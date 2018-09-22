-module(elixir_expand).
-export([expand/2, expand_args/2, expand_arg/2, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% =

expand({'=', Meta, [Left, Right]}, E) ->
  assert_no_guard_scope(Meta, "=", E),
  {ERight, ER} = expand(Right, E),
  {ELeft, EL} = elixir_clauses:match(fun expand/2, Left, E),
  refute_parallel_bitstring_match(ELeft, ERight, E, ?key(E, context) == match),
  {{'=', Meta, [ELeft, ERight]}, elixir_env:mergev(EL, ER)};

%% Literal operators

expand({'{}', Meta, Args}, E) ->
  {EArgs, EA} = expand_args(Args, E),
  {{'{}', Meta, EArgs}, EA};

expand({'%{}', Meta, Args}, E) ->
  elixir_map:expand_map(Meta, Args, E);

expand({'%', Meta, [Left, Right]}, E) ->
  elixir_map:expand_struct(Meta, Left, Right, E);

expand({'<<>>', Meta, Args}, E) ->
  elixir_bitstring:expand(Meta, Args, E, false);

expand({'->', Meta, _Args}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, unhandled_arrow_op);

%% __block__

expand({'__block__', _Meta, []}, E) ->
  {nil, E};
expand({'__block__', _Meta, [Arg]}, E) ->
  expand(Arg, E);
expand({'__block__', Meta, Args}, E) when is_list(Args) ->
  {EArgs, EA} = expand_block(Args, [], Meta, E),
  {{'__block__', Meta, EArgs}, EA};

%% __aliases__

expand({'__aliases__', _, _} = Alias, E) ->
  expand_aliases(Alias, E, true);

%% alias

expand({Kind, Meta, [{{'.', _, [Base, '{}']}, _, Refs} | Rest]}, E)
    when Kind == alias; Kind == require; Kind == import ->
  case Rest of
    [] ->
      expand_multi_alias_call(Kind, Meta, Base, Refs, [], E);
    [Opts] ->
      case lists:keymember(as, 1, Opts) of
        true ->
          form_error(Meta, ?key(E, file), ?MODULE, as_in_multi_alias_call);
        false ->
          expand_multi_alias_call(Kind, Meta, Base, Refs, Opts, E)
      end
  end;
expand({alias, Meta, [Ref]}, E) ->
  expand({alias, Meta, [Ref, []]}, E);
expand({alias, Meta, [Ref, Opts]}, E) ->
  assert_no_match_or_guard_scope(Meta, "alias", E),
  {ERef, ER} = expand_without_aliases_report(Ref, E),
  {EOpts, ET}  = expand_opts(Meta, alias, [as, warn], no_alias_opts(Opts), ER),

  if
    is_atom(ERef) ->
      {ERef, expand_alias(Meta, true, ERef, EOpts, ET)};
    true ->
      form_error(Meta, ?key(E, file), ?MODULE, {expected_compile_time_module, alias, Ref})
  end;

expand({require, Meta, [Ref]}, E) ->
  expand({require, Meta, [Ref, []]}, E);
expand({require, Meta, [Ref, Opts]}, E) ->
  assert_no_match_or_guard_scope(Meta, "require", E),

  {ERef, ER} = expand_without_aliases_report(Ref, E),
  {EOpts, ET}  = expand_opts(Meta, require, [as, warn], no_alias_opts(Opts), ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      {ERef, expand_require(Meta, ERef, EOpts, ET)};
    true ->
      form_error(Meta, ?key(E, file), ?MODULE, {expected_compile_time_module, require, Ref})
  end;

expand({import, Meta, [Left]}, E) ->
  expand({import, Meta, [Left, []]}, E);

expand({import, Meta, [Ref, Opts]}, E) ->
  assert_no_match_or_guard_scope(Meta, "import", E),
  {ERef, ER} = expand_without_aliases_report(Ref, E),
  {EOpts, ET}  = expand_opts(Meta, import, [only, except, warn], Opts, ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      {Functions, Macros} = elixir_import:import(Meta, ERef, EOpts, ET),
      {ERef, expand_require(Meta, ERef, EOpts, ET#{functions := Functions, macros := Macros})};
    true ->
      form_error(Meta, ?key(E, file), ?MODULE, {expected_compile_time_module, import, Ref})
  end;

%% Compilation environment macros

expand({'__MODULE__', _, Atom}, E) when is_atom(Atom) ->
  {?key(E, module), E};
expand({'__DIR__', _, Atom}, E) when is_atom(Atom) ->
  {filename:dirname(?key(E, file)), E};
expand({'__CALLER__', Meta, Atom} = Caller, E) when is_atom(Atom) ->
  assert_contextual_var(Meta, '__CALLER__', E, caller_not_allowed),
  {Caller, E};
expand({'__STACKTRACE__', Meta, Atom} = Stacktrace, E) when is_atom(Atom) ->
  assert_contextual_var(Meta, '__STACKTRACE__', E, stacktrace_not_allowed),
  {Stacktrace, E};
expand({'__ENV__', Meta, Atom}, E) when is_atom(Atom) ->
  {escape_map(escape_env_entries(Meta, E)), E};
expand({{'.', DotMeta, [{'__ENV__', Meta, Atom}, Field]}, CallMeta, []}, E) when is_atom(Atom), is_atom(Field) ->
  Env = escape_env_entries(Meta, E),
  case maps:is_key(Field, Env) of
    true  -> {maps:get(Field, Env), E};
    false -> {{{'.', DotMeta, [escape_map(Env), Field]}, CallMeta, []}, E}
  end;

%% Quote

expand({Unquote, Meta, [_]}, E) when Unquote == unquote; Unquote == unquote_splicing ->
  form_error(Meta, ?key(E, file), ?MODULE, {unquote_outside_quote, Unquote});

expand({quote, Meta, [Opts]}, E) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    {do, Do} ->
      expand({quote, Meta, [lists:keydelete(do, 1, Opts), [{do, Do}]]}, E);
    false ->
      form_error(Meta, ?key(E, file), ?MODULE, {missing_option, 'quote', [do]})
  end;

expand({quote, Meta, [_]}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_args, 'quote'});

expand({quote, Meta, [Opts, Do]}, E) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      {do, Expr} -> Expr;
      false -> form_error(Meta, ?key(E, file), ?MODULE, {missing_option, 'quote', [do]})
    end,

  ValidOpts = [context, location, line, file, unquote, bind_quoted, generated],
  {EOpts, ET} = expand_opts(Meta, quote, ValidOpts, Opts, E),

  Context = case lists:keyfind(context, 1, EOpts) of
    {context, Ctx} when is_atom(Ctx) and (Ctx /= nil) ->
      Ctx;
    {context, Ctx} ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_context_opt_for_quote, Ctx});
    false ->
      case ?key(E, module) of
        nil -> 'Elixir';
        Mod -> Mod
      end
  end,

  {File, Line} = case lists:keyfind(location, 1, EOpts) of
    {location, keep} ->
      {elixir_utils:relative_to_cwd(?key(E, file)), false};
    false ->
      { case lists:keyfind(file, 1, EOpts) of
          {file, F} -> F;
          false -> nil
        end,

        case lists:keyfind(line, 1, EOpts) of
          {line, L} -> L;
          false -> false
        end }
  end,

  {Binding, DefaultUnquote} = case lists:keyfind(bind_quoted, 1, EOpts) of
    {bind_quoted, BQ} -> {BQ, false};
    false -> {nil, true}
  end,

  Unquote = case lists:keyfind(unquote, 1, EOpts) of
    {unquote, U} when is_boolean(U) -> U;
    false -> DefaultUnquote
  end,

  Generated = lists:keyfind(generated, 1, EOpts) == {generated, true},

  Q = #elixir_quote{line=Line, file=File, unquote=Unquote,
                    context=Context, generated=Generated},

  Quoted = elixir_quote:quote(Meta, Exprs, Binding, Q, ET),
  expand(Quoted, ET);

expand({quote, Meta, [_, _]}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_args, 'quote'});

%% Functions

expand({'&', Meta, [Arg]}, E) ->
  assert_no_match_or_guard_scope(Meta, "&", E),
  case elixir_fn:capture(Meta, Arg, E) of
    {{remote, Remote, Fun, Arity}, EE} ->
      is_atom(Remote) andalso
        elixir_lexical:record_remote(Remote, Fun, Arity, ?key(E, function), ?line(Meta), ?key(E, lexical_tracker)),
      {{'&', Meta, [{'/', [], [{{'.', [], [Remote, Fun]}, [], []}, Arity]}]}, EE};
    {{local, Fun, Arity}, EE} ->
      {{'&', Meta, [{'/', [], [{Fun, [], nil}, Arity]}]}, EE};
    {expand, Expr, EE} ->
      expand(Expr, EE)
  end;

expand({fn, Meta, Pairs}, E) ->
  assert_no_match_or_guard_scope(Meta, "fn", E),
  elixir_fn:expand(Meta, Pairs, E);

%% Case/Receive/Try

expand({'cond', Meta, [Opts]}, E) ->
  assert_no_match_or_guard_scope(Meta, "cond", E),
  assert_no_underscore_clause_in_cond(Opts, E),
  {EClauses, EC} = elixir_clauses:'cond'(Meta, Opts, E),
  {{'cond', Meta, [EClauses]}, EC};

expand({'case', Meta, [Expr, Options]}, E) ->
  assert_no_match_or_guard_scope(Meta, "case", E),
  expand_case(Meta, Expr, Options, E);

expand({'receive', Meta, [Opts]}, E) ->
  assert_no_match_or_guard_scope(Meta, "receive", E),
  {EClauses, EC} = elixir_clauses:'receive'(Meta, Opts, E),
  {{'receive', Meta, [EClauses]}, EC};

expand({'try', Meta, [Opts]}, E) ->
  assert_no_match_or_guard_scope(Meta, "try", E),
  {EClauses, EC} = elixir_clauses:'try'(Meta, Opts, E),
  {{'try', Meta, [EClauses]}, EC};

%% Comprehensions

expand({for, Meta, [_ | _] = Args}, E) ->
  assert_no_match_or_guard_scope(Meta, "for", E),
  {Cases, Block} =
    case elixir_utils:split_last(Args) of
      {OuterCases, OuterOpts} when is_list(OuterOpts) ->
        case elixir_utils:split_last(OuterCases) of
          {InnerCases, InnerOpts} when is_list(InnerOpts) ->
            {InnerCases, InnerOpts ++ OuterOpts};
          _ ->
            {OuterCases, OuterOpts}
        end;
      _ ->
        {Args, []}
    end,

  validate_opts(Meta, for, [do, into, uniq], Block, E),

  {Expr, Opts} =
    case lists:keytake(do, 1, Block) of
      {value, {do, Do}, DoOpts} ->
        {Do, DoOpts};
      false ->
        form_error(Meta, ?key(E, file), ?MODULE, {missing_option, for, [do]})
    end,

  case lists:keyfind(uniq, 1, Opts) of
    false -> ok;
    {uniq, Value} when is_boolean(Value) -> ok;
    {uniq, Value} -> form_error(Meta, ?key(E, file), ?MODULE, {for_invalid_uniq, Value})
  end,

  {EOpts, EO} = expand(Opts, E),
  {ECases, EC} = lists:mapfoldl(fun expand_for/2, EO, Cases),
  {EExpr, EE} = expand(Expr, EC),
  assert_generator_start(Meta, ECases, E),
  {{for, Meta, ECases ++ [[{do, EExpr} | EOpts]]}, elixir_env:merge_and_check_unused_vars(E, EE)};

%% With

expand({with, Meta, [_ | _] = Args}, E) ->
  assert_no_match_or_guard_scope(Meta, "with", E),
  elixir_clauses:with(Meta, Args, E);

%% Super

expand({super, Meta, Args}, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "super", E),
  Module = assert_module_scope(Meta, super, E),
  Function = assert_function_scope(Meta, super, E),
  {_, Arity} = Function,

  case length(Args) of
    Arity ->
      {Kind, Name, SuperMeta} = elixir_overridable:super(Meta, Module, Function, E),

      %% TODO: Remove this on Elixir v2.0 and make all GenServer callbacks optional
      case lists:keyfind(context, 1, SuperMeta) of
        {context, 'Elixir.GenServer'} ->
          Message =
            io_lib:format(
              "calling super for GenServer callback ~ts/~B is deprecated",
              [element(1, Function), Arity]
            ),

          elixir_errors:warn(?line(Meta), ?key(E, file), Message);

        _ ->
          ok
      end,

      {EArgs, EA} = expand_args(Args, E),
      {{super, Meta, [{Kind, Name} | EArgs]}, EA};
    _ ->
      form_error(Meta, ?key(E, file), ?MODULE, wrong_number_of_args_for_super)
  end;

%% Vars

expand({'^', Meta, [Arg]}, #{context := match} = E) ->
  #{current_vars := Current, prematch_vars := Prematch} = E,

  %% We need to rollback to a no match context.
  NoMatchE = E#{context := nil, current_vars := Prematch, prematch_vars := pin},

  case expand(Arg, NoMatchE) of
    {{Name, _, Kind} = Var, ExpandedE} when is_atom(Name), is_atom(Kind) ->
      EA = ExpandedE#{
        context := match,
        current_vars := Current,
        prematch_vars := ?key(ExpandedE, current_vars)
      },

      {{'^', Meta, [Var]}, EA};
    _ ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_arg_for_pin, Arg})
  end;
expand({'^', Meta, [Arg]}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {pin_outside_of_match, Arg});

expand({'_', _Meta, Kind} = Var, #{context := match} = E) when is_atom(Kind) ->
  {Var, E};
expand({'_', Meta, Kind}, E) when is_atom(Kind) ->
  form_error(Meta, ?key(E, file), ?MODULE, unbound_underscore);

expand({Name, Meta, Kind} = Var, #{context := match} = E) when is_atom(Name), is_atom(Kind) ->
  #{unused_vars := Unused, current_vars := Current, prematch_vars := Prematch} = E,
  Pair = {Name, var_context(Meta, Kind)},
  PrematchVersion = var_version(Prematch, Pair),

  EE =
    case Current of
      %% Variable is being overridden now
      #{Pair := {PrematchVersion, _}} ->
        NewUnused = var_unused(Pair, Meta, PrematchVersion + 1, Unused),
        NewCurrent = Current#{Pair => {PrematchVersion + 1, term}},
        E#{unused_vars := NewUnused, current_vars := NewCurrent};

      %% Variable was already overriden
      #{Pair := {CurrentVersion, _}} ->
        maybe_warn_underscored_var_repeat(Meta, Name, Kind, E),
        NewUnused = Unused#{{Pair, CurrentVersion} => false},
        E#{unused_vars := NewUnused};

      %% Variable defined for the first time
      _ ->
        NewVars = ordsets:add_element(Pair, ?key(E, vars)),
        NewUnused = var_unused(Pair, Meta, 0, Unused),
        NewCurrent = Current#{Pair => {0, term}},
        E#{vars := NewVars, unused_vars := NewUnused, current_vars := NewCurrent}
    end,

  {Var, EE};
expand({Name, Meta, Kind} = Var, E) when is_atom(Name), is_atom(Kind) ->
  #{unused_vars := Unused, current_vars := Current} = E,
  Pair = {Name, var_context(Meta, Kind)},

  case Current of
    #{Pair := {Version, _}} ->
      maybe_warn_underscored_var_access(Meta, Name, Kind, E),
      UnusedKey = {Pair, Version},

      case Unused of
        #{UnusedKey := Entry} when Entry /= false ->
          {Var, E#{unused_vars := Unused#{UnusedKey := false}}};
        _ ->
          {Var, E}
      end;

    _ ->
      case lists:keyfind(var, 1, Meta) of
        {var, true} ->
          form_error(Meta, ?key(E, file), ?MODULE, {undefined_var_bang, Name, Kind});

        _ ->
          case ?key(E, prematch_vars) of
            warn ->
              Message =
                io_lib:format("variable \"~ts\" does not exist and is being expanded to \"~ts()\","
                  " please use parentheses to remove the ambiguity or change the variable name", [Name, Name]),
              elixir_errors:warn(?line(Meta), ?key(E, file), Message),
              expand({Name, Meta, []}, E);

            raise ->
              form_error(Meta, ?key(E, file), ?MODULE, {undefined_var, Name, Kind});

            pin ->
              form_error(Meta, ?key(E, file), ?MODULE, {undefined_var_pin, Name, Kind});

            apply ->
              expand({Name, Meta, []}, E)
          end
      end
  end;

%% Local calls

expand({Atom, Meta, Args}, E) when is_atom(Atom), is_list(Meta), is_list(Args) ->
  assert_no_ambiguous_op(Atom, Meta, Args, E),

  elixir_dispatch:dispatch_import(Meta, Atom, Args, E, fun() ->
    expand_local(Meta, Atom, Args, E)
  end);

%% Remote calls

expand({{'.', DotMeta, [Left, Right]}, Meta, Args}, E)
    when (is_tuple(Left) orelse is_atom(Left)), is_atom(Right), is_list(Meta), is_list(Args) ->
  {ELeft, EL} = expand(Left, E),

  elixir_dispatch:dispatch_require(Meta, ELeft, Right, Args, EL, fun(AR, AF, AA) ->
    expand_remote(AR, DotMeta, AF, Meta, AA, E, EL)
  end);

%% Anonymous calls

expand({{'.', DotMeta, [Expr]}, Meta, Args}, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "anonymous call", E),
  {EExpr, EE} = expand(Expr, E),
  if
    is_atom(EExpr) ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_function_call, EExpr});
    true ->
      {EArgs, EA} = expand_args(Args, elixir_env:mergea(E, EE)),
      {{{'.', DotMeta, [EExpr]}, Meta, EArgs}, elixir_env:mergev(EE, EA)}
  end;

%% Invalid calls

expand({_, Meta, Args} = Invalid, E) when is_list(Meta) and is_list(Args) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_call, Invalid});

expand({_, _, _} = Tuple, E) ->
  form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Tuple});

%% Literals

expand({Left, Right}, E) ->
  {[ELeft, ERight], EE} = expand_args([Left, Right], E),
  {{ELeft, ERight}, EE};

expand(List, #{context := match} = E) when is_list(List) ->
  expand_list(List, fun expand/2, E, []);

expand(List, E) when is_list(List) ->
  {EArgs, {EC, EV}} = expand_list(List, fun expand_arg/2, {E, E}, []),
  {EArgs, elixir_env:mergea(EV, EC)};

expand(Function, E) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
       (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {Function, E};
    false ->
      form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Function})
  end;

expand(Pid, E) when is_pid(Pid) ->
  case ?key(E, function) of
    nil ->
      {Pid, E};
    Function ->
      %% TODO: Make me an error on 2.0
      elixir_errors:form_warn([], ?key(E, file), ?MODULE,
                              {invalid_pid_in_function, Pid, Function}),
      {Pid, E}
  end;

expand(Other, E) when is_number(Other); is_atom(Other); is_binary(Other) ->
  {Other, E};

expand(Other, E) ->
  form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Other}).

%% Helpers

escape_env_entries(Meta, #{unused_vars := Unused, current_vars := Current} = Env0) ->
  Env1 = case Env0 of
    #{function := nil} -> Env0;
    _ -> Env0#{lexical_tracker := nil}
  end,
  Env2 = Env1#{unused_vars := escape_map(Unused), current_vars := escape_map(Current)},
  Env3 = elixir_env:linify({?line(Meta), Env2}),
  Env3.

escape_map(Map) ->
  {'%{}', [], maps:to_list(Map)}.

expand_multi_alias_call(Kind, Meta, Base, Refs, Opts, E) ->
  {BaseRef, EB} = expand_without_aliases_report(Base, E),
  Fun = fun
    ({'__aliases__', _, Ref}, ER) ->
      expand({Kind, Meta, [elixir_aliases:concat([BaseRef | Ref]), Opts]}, ER);
    (Ref, ER) when is_atom(Ref) ->
      expand({Kind, Meta, [elixir_aliases:concat([BaseRef, Ref]), Opts]}, ER);
    (Other, _ER) ->
      form_error(Meta, ?key(E, file), ?MODULE, {expected_compile_time_module, Kind, Other})
  end,
  lists:mapfoldl(Fun, EB, Refs).

expand_list([{'|', Meta, [_, _] = Args}], Fun, Acc, List) ->
  {EArgs, EAcc} = lists:mapfoldl(Fun, Acc, Args),
  expand_list([], Fun, EAcc, [{'|', Meta, EArgs} | List]);
expand_list([H | T], Fun, Acc, List) ->
  {EArg, EAcc} = Fun(H, Acc),
  expand_list(T, Fun, EAcc, [EArg | List]);
expand_list([], _Fun, Acc, List) ->
  {lists:reverse(List), Acc}.

expand_block([], Acc, _Meta, E) ->
  {lists:reverse(Acc), E};
expand_block([H], Acc, Meta, E) ->
  {EH, EE} = expand(H, E),
  expand_block([], [EH | Acc], Meta, EE);
expand_block([H | T], Acc, Meta, E) ->
  {EH, EE} = expand(H, E),

  %% Notice checks rely on the code BEFORE expansion
  %% instead of relying on Erlang checks.
  %%
  %% That's because expansion may generate useless
  %% terms on their own (think compile time removed
  %% logger calls) and we don't want to catch those.
  %%
  %% Or, similarly, the work is all in the expansion
  %% (for example, to register something) and it is
  %% simply returning something as replacement.
  case is_useless_building(H, EH, Meta) of
    {UselessMeta, UselessTerm} ->
      elixir_errors:form_warn(UselessMeta, ?key(E, file), ?MODULE, UselessTerm);
    false ->
      ok
  end,

  expand_block(T, [EH | Acc], Meta, EE).

%% Notice we don't handle atoms on purpose. They are common
%% when unquoting AST and it is unlikely that we would catch
%% bugs as we don't do binary operations on them like in
%% strings or numbers.
is_useless_building(H, _, Meta) when is_binary(H); is_number(H) ->
  {Meta, {useless_literal, H}};
is_useless_building({'@', Meta, [{Var, _, Ctx}]}, _, _) when is_atom(Ctx); Ctx == [] ->
  {Meta, {useless_attr, Var}};
is_useless_building({Var, Meta, Ctx}, {Var, _, Ctx}, _) when is_atom(Ctx) ->
  {Meta, {useless_var, Var}};
is_useless_building(_, _, _) ->
  false.

%% Variables in arguments are not propagated from one
%% argument to the other. For instance:
%%
%%   x = 1
%%   foo(x = x + 2, x)
%%   x
%%
%% Should be the same as:
%%
%%   foo(3, 1)
%%   3
%%
%% However, lexical information is.
expand_arg(Arg, Acc) when is_number(Arg); is_atom(Arg); is_binary(Arg); is_pid(Arg) ->
  {Arg, Acc};
expand_arg(Arg, {Acc1, Acc2}) ->
  {EArg, EAcc} = expand(Arg, Acc1),
  {EArg, {elixir_env:mergea(Acc1, EAcc), elixir_env:mergev(Acc2, EAcc)}}.

expand_args([Arg], E) ->
  {EArg, EE} = expand(Arg, E),
  {[EArg], EE};
expand_args(Args, #{context := match} = E) ->
  lists:mapfoldl(fun expand/2, E, Args);
expand_args(Args, E) ->
  {EArgs, {_EC, EV}} = lists:mapfoldl(fun expand_arg/2, {E, E}, Args),
  {EArgs, EV}.

%% Match/var helpers

var_unused({_Name, Kind} = Pair, Meta, Version, Unused) ->
  case (Kind == nil) andalso should_warn(Meta) of
    true -> Unused#{{Pair, Version} => ?line(Meta)};
    false -> Unused
  end.

var_version(Map, Pair) ->
  case Map of
    #{Pair := {Version, _}} -> Version;
    _ -> -1
  end.

var_context(Meta, Kind) ->
  case lists:keyfind(counter, 1, Meta) of
    {counter, Counter} -> Counter;
    false -> Kind
  end.

maybe_warn_underscored_var_repeat(Meta, Name, Kind, E) ->
  case should_warn(Meta) andalso atom_to_list(Name) of
    "_" ++ _ ->
      elixir_errors:form_warn(Meta, ?key(E, file), ?MODULE, {underscored_var_repeat, Name, Kind});
    _ ->
      ok
  end.

maybe_warn_underscored_var_access(Meta, Name, Kind, E) ->
  case (Kind == nil) andalso should_warn(Meta) andalso atom_to_list(Name) of
    "_" ++ _ ->
      elixir_errors:form_warn(Meta, ?key(E, file), ?MODULE, {underscored_var_access, Name});
    _ ->
      ok
  end.

context_info(Kind) when Kind == nil; is_integer(Kind) -> "";
context_info(Kind) -> io_lib:format(" (context ~ts)", [elixir_aliases:inspect(Kind)]).

should_warn(Meta) ->
  lists:keyfind(generated, 1, Meta) /= {generated, true}.

%% Case

expand_case(Meta, Expr, Opts, E) ->
  {EExpr, EE} = expand(Expr, E),

  ROpts =
    case proplists:get_value(optimize_boolean, Meta, false) of
      true ->
        case elixir_utils:returns_boolean(EExpr) of
          true -> rewrite_case_clauses(Opts);
          false -> generated_case_clauses(Opts)
        end;

      false ->
        Opts
    end,

  {EOpts, EO} = elixir_clauses:'case'(Meta, ROpts, EE),
  {{'case', Meta, [EExpr, EOpts]}, EO}.

rewrite_case_clauses([{do, [
  {'->', FalseMeta, [
    [{'when', _, [Var, {{'.', _, ['Elixir.Kernel', 'in']}, _, [Var, [false, nil]]}]}],
    FalseExpr
  ]},
  {'->', TrueMeta, [
    [{'_', _, _}],
    TrueExpr
  ]}
]}]) ->
  rewrite_case_clauses(FalseMeta, FalseExpr, TrueMeta, TrueExpr);

rewrite_case_clauses([{do, [
  {'->', FalseMeta, [[false], FalseExpr]},
  {'->', TrueMeta, [[true], TrueExpr]} | _
]}]) ->
  rewrite_case_clauses(FalseMeta, FalseExpr, TrueMeta, TrueExpr);

rewrite_case_clauses(Other) ->
  generated_case_clauses(Other).

rewrite_case_clauses(FalseMeta, FalseExpr, TrueMeta, TrueExpr) ->
  [{do, [
    {'->', ?generated(FalseMeta), [[false], FalseExpr]},
    {'->', ?generated(TrueMeta), [[true], TrueExpr]}
  ]}].

generated_case_clauses([{do, Clauses}]) ->
  RClauses = [{'->', ?generated(Meta), Args} || {'->', Meta, Args} <- Clauses],
  [{do, RClauses}].

%% Locals

assert_no_ambiguous_op(Name, Meta, [Arg], E) ->
  case lists:keyfind(ambiguous_op, 1, Meta) of
    {ambiguous_op, Kind} ->
      Pair = {Name, Kind},
      case ?key(E, current_vars) of
        #{Pair := _} ->
          form_error(Meta, ?key(E, file), ?MODULE, {op_ambiguity, Name, Arg});
        _ ->
          ok
      end;
    _ ->
      ok
  end;
assert_no_ambiguous_op(_Atom, _Meta, _Args, _E) ->
  ok.

assert_no_clauses(_Name, _Meta, [], _E) ->
  ok;
assert_no_clauses(Name, Meta, Args, E) ->
  assert_arg_with_no_clauses(Name, Meta, lists:last(Args), E).

assert_arg_with_no_clauses(Name, Meta, [{Key, Value} | Rest], E) when is_atom(Key) ->
  case Value of
    [{'->', _, _} | _] ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_clauses, Name});
    _ ->
      assert_arg_with_no_clauses(Name, Meta, Rest, E)
  end;
assert_arg_with_no_clauses(_Name, _Meta, _Arg, _E) ->
  ok.

expand_local(Meta, Name, Args, #{function := nil} = E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {undefined_function, Name, Args});
expand_local(Meta, Name, Args, #{context := Context} = E) when Context == match; Context == guard ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_local_invocation, Context, {Name, Meta, Args}});
expand_local(Meta, Name, Args, #{module := Module, function := Function} = E) ->
  assert_no_clauses(Name, Meta, Args, E),

  elixir_locals:record_local({Name, length(Args)}, Module, Function),
  {EArgs, EA} = expand_args(Args, E),
  {{Name, Meta, EArgs}, EA}.

%% Remote

expand_remote(Receiver, DotMeta, Right, Meta, Args, #{context := Context} = E, EL) when is_atom(Receiver) or is_tuple(Receiver) ->
  assert_no_clauses(Right, Meta, Args, E),

  Arity = length(Args),
  is_atom(Receiver) andalso
    elixir_lexical:record_remote(Receiver, Right, Arity,
                                 ?key(E, function), ?line(Meta), ?key(E, lexical_tracker)),
  {EArgs, EA} = expand_args(Args, E),
  case rewrite(Context, Receiver, DotMeta, Right, Meta, EArgs) of
    {ok, Rewritten} ->
      maybe_warn_comparison(Rewritten, Args, E),
      {Rewritten, elixir_env:mergev(EL, EA)};
    {error, Error} -> form_error(Meta, ?key(E, file), elixir_rewrite, Error)
  end;
expand_remote(Receiver, DotMeta, Right, Meta, Args, E, _) ->
  Call = {{'.', DotMeta, [Receiver, Right]}, Meta, Args},
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_call, Call}).

rewrite(match, Receiver, _DotMeta, Right, _Meta, EArgs) ->
  {error, {invalid_match, Receiver, Right, length(EArgs)}};
rewrite(guard, Receiver, DotMeta, Right, Meta, EArgs) ->
  elixir_rewrite:guard_rewrite(Receiver, DotMeta, Right, Meta, EArgs);
rewrite(_, Receiver, DotMeta, Right, Meta, EArgs) ->
  {ok, elixir_rewrite:rewrite(Receiver, DotMeta, Right, Meta, EArgs)}.

maybe_warn_comparison({{'.', _, [erlang, Op]}, Meta, [ELeft, ERight]}, [Left, Right], E)
    when Op =:= '>'; Op =:= '<'; Op =:= '=<'; Op =:= '>=' ->
  case is_struct_comparison(ELeft, ERight, Left, Right) of
    false ->
      case is_nested_comparison(Op, ELeft, ERight, Left, Right) of
        false -> ok;
        CompExpr ->
          elixir_errors:form_warn(Meta, ?key(E, file), ?MODULE, {nested_comparison, CompExpr})
      end;
    StructExpr ->
      elixir_errors:form_warn(Meta, ?key(E, file), ?MODULE, {struct_comparison, StructExpr})
  end;
maybe_warn_comparison(_, _, _) ->
  ok.

is_struct_comparison(ELeft, ERight, Left, Right) ->
  case is_struct_expression(ELeft) of
    true -> Left;
    false ->
      case is_struct_expression(ERight) of
        true -> Right;
        false -> false
      end
  end.

is_struct_expression({'%', _, [Struct, _]}) when is_atom(Struct) ->
  true;
is_struct_expression({'%{}', _, KVs}) ->
  case lists:keyfind('__struct__', 1, KVs) of
    {'__struct__', Struct} when is_atom(Struct) -> true;
    false -> false
  end;
is_struct_expression(_Other) -> false.

is_nested_comparison(Op, ELeft, ERight, Left, Right) ->
  NestedExpr = {elixir_utils:erlang_comparison_op_to_elixir(Op), [], [Left, Right]},
  case is_comparison_expression(ELeft) of
    true ->
      NestedExpr;
    false ->
      case is_comparison_expression(ERight) of
        true -> NestedExpr;
        false -> false
      end
  end.
is_comparison_expression({{'.',_,[erlang,Op]},_,_})
  when Op =:= '>'; Op =:= '<'; Op =:= '=<'; Op =:= '>=' -> true;
is_comparison_expression(_Other) -> false.

%% Lexical helpers

expand_opts(Meta, Kind, Allowed, Opts, E) ->
  {EOpts, EE} = expand(Opts, E),
  validate_opts(Meta, Kind, Allowed, EOpts, EE),
  {EOpts, EE}.

validate_opts(Meta, Kind, Allowed, Opts, E) when is_list(Opts) ->
  [begin
    form_error(Meta, ?key(E, file), ?MODULE, {unsupported_option, Kind, Key})
  end || {Key, _} <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, Opts, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {options_are_not_keyword, Kind, Opts}).

no_alias_opts(Opts) when is_list(Opts) ->
  case lists:keyfind(as, 1, Opts) of
    {as, As} -> lists:keystore(as, 1, Opts, {as, no_alias_expansion(As)});
    false -> Opts
  end;
no_alias_opts(Opts) -> Opts.

no_alias_expansion({'__aliases__', _, [H | T]}) when is_atom(H) ->
  elixir_aliases:concat([H | T]);
no_alias_expansion(Other) ->
  Other.

expand_require(Meta, Ref, Opts, E) ->
  %% We always record requires when they are defined
  %% as they expect the reference at compile time.
  elixir_lexical:record_remote(Ref, nil, ?key(E, lexical_tracker)),
  RE = E#{requires := ordsets:add_element(Ref, ?key(E, requires))},
  expand_alias(Meta, false, Ref, Opts, RE).

expand_alias(Meta, IncludeByDefault, Ref, Opts, #{context_modules := Context} = E) ->
  New = expand_as(lists:keyfind(as, 1, Opts), Meta, IncludeByDefault, Ref, E),

  %% Add the alias to context_modules if defined is set.
  %% This is used by defmodule in order to store the defined
  %% module in context modules.
  NewContext =
    case lists:keyfind(defined, 1, Meta) of
      {defined, Mod} when is_atom(Mod) -> [Mod | Context];
      false -> Context
    end,

  {Aliases, MacroAliases} = elixir_aliases:store(Meta, New, Ref, Opts, ?key(E, aliases),
                                ?key(E, macro_aliases), ?key(E, lexical_tracker)),

  E#{aliases := Aliases, macro_aliases := MacroAliases, context_modules := NewContext}.

expand_as({as, nil}, _Meta, _IncludeByDefault, Ref, _E) ->
  Ref;
expand_as({as, Atom}, Meta, _IncludeByDefault, _Ref, E) when is_atom(Atom), not is_boolean(Atom) ->
  case atom_to_list(Atom) of
    "Elixir." ++ Rest ->
      case string:tokens(Rest, ".") of
        [Rest] ->
          Atom;
        _ ->
          form_error(Meta, ?key(E, file), ?MODULE, {invalid_alias_for_as, nested_alias, Atom})
      end;
    _ ->
      form_error(Meta, ?key(E, file), ?MODULE, {invalid_alias_for_as, not_alias, Atom})
  end;
expand_as(false, _Meta, IncludeByDefault, Ref, _E) ->
  if IncludeByDefault -> elixir_aliases:last(Ref);
     true -> Ref
  end;
expand_as({as, Other}, Meta, _IncludeByDefault, _Ref, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_alias_for_as, not_alias, Other}).

%% Aliases

expand_without_aliases_report({'__aliases__', _, _} = Alias, E) ->
  expand_aliases(Alias, E, false);
expand_without_aliases_report(Other, E) ->
  expand(Other, E).

expand_aliases({'__aliases__', Meta, _} = Alias, E, Report) ->
  case elixir_aliases:expand(Alias, ?key(E, aliases), ?key(E, macro_aliases), ?key(E, lexical_tracker)) of
    Receiver when is_atom(Receiver) ->
      Report andalso
        elixir_lexical:record_remote(Receiver, ?key(E, function), ?key(E, lexical_tracker)),
      {Receiver, E};
    Aliases ->
      {EAliases, EA} = expand_args(Aliases, E),

      case lists:all(fun is_atom/1, EAliases) of
        true ->
          Receiver = elixir_aliases:concat(EAliases),
          Report andalso
            elixir_lexical:record_remote(Receiver, ?key(E, function), ?key(E, lexical_tracker)),
          {Receiver, EA};
        false ->
          form_error(Meta, ?key(E, file), ?MODULE, {invalid_alias, Alias})
      end
  end.

%% Comprehensions

expand_for({'<-', Meta, [Left, Right]}, E) ->
  {ERight, ER} = expand(Right, E),
  {[ELeft], EL}  = elixir_clauses:head([Left], E),
  {{'<-', Meta, [ELeft, ERight]}, elixir_env:mergev(EL, ER)};
expand_for({'<<>>', Meta, Args} = X, E) when is_list(Args) ->
  case elixir_utils:split_last(Args) of
    {LeftStart, {'<-', OpMeta, [LeftEnd, Right]}} ->
      {ERight, ER} = expand(Right, E),
      {ELeft, EL} = elixir_clauses:match(fun(BArg, BE) ->
        elixir_bitstring:expand(Meta, BArg, BE, true)
      end, LeftStart ++ [LeftEnd], E),
      {{'<<>>', [], [{'<-', OpMeta, [ELeft, ERight]}]}, elixir_env:mergev(EL, ER)};
    _ ->
      expand(X, E)
  end;
expand_for(X, E) ->
  expand(X, E).

assert_generator_start(_, [{'<-', _, [_, _]} | _], _) ->
  ok;
assert_generator_start(_, [{'<<>>', _, [{'<-', _, [_, _]}]} | _], _) ->
  ok;
assert_generator_start(Meta, _, E) ->
  elixir_errors:form_error(Meta, ?key(E, file), ?MODULE, for_generator_start).

%% Assertions

refute_parallel_bitstring_match({'<<>>', _, _}, {'<<>>', Meta, _} = Arg, E, true) ->
  form_error(Meta, ?key(E, file), ?MODULE, {parallel_bitstring_match, Arg});
refute_parallel_bitstring_match(Left, {'=', _Meta, [MatchLeft, MatchRight]}, E, Parallel) ->
  refute_parallel_bitstring_match(Left, MatchLeft, E, true),
  refute_parallel_bitstring_match(Left, MatchRight, E, Parallel);
refute_parallel_bitstring_match([_ | _] = Left, [_ | _] = Right, E, Parallel) ->
  refute_parallel_bitstring_match_each(Left, Right, E, Parallel);
refute_parallel_bitstring_match({Left1, Left2}, {Right1, Right2}, E, Parallel) ->
  refute_parallel_bitstring_match_each([Left1, Left2], [Right1, Right2], E, Parallel);
refute_parallel_bitstring_match({'{}', _, Args1}, {'{}', _, Args2}, E, Parallel) ->
  refute_parallel_bitstring_match_each(Args1, Args2, E, Parallel);
refute_parallel_bitstring_match({'%{}', _, Args1}, {'%{}', _, Args2}, E, Parallel) ->
  refute_parallel_bitstring_match_map_field(lists:sort(Args1), lists:sort(Args2), E, Parallel);
refute_parallel_bitstring_match({'%', _, [_, Args]}, Right, E, Parallel) ->
  refute_parallel_bitstring_match(Args, Right, E, Parallel);
refute_parallel_bitstring_match(Left, {'%', _, [_, Args]}, E, Parallel) ->
  refute_parallel_bitstring_match(Left, Args, E, Parallel);
refute_parallel_bitstring_match(_Left, _Right, _E, _Parallel) ->
  ok.

refute_parallel_bitstring_match_each([Arg1 | Rest1], [Arg2 | Rest2], E, Parallel) ->
  refute_parallel_bitstring_match(Arg1, Arg2, E, Parallel),
    refute_parallel_bitstring_match_each(Rest1, Rest2, E, Parallel);
refute_parallel_bitstring_match_each(_List1, _List2, _E, _Parallel) ->
  ok.

refute_parallel_bitstring_match_map_field([{Key, Val1} | Rest1], [{Key, Val2} | Rest2], E, Parallel) ->
  refute_parallel_bitstring_match(Val1, Val2, E, Parallel),
  refute_parallel_bitstring_match_map_field(Rest1, Rest2, E, Parallel);
refute_parallel_bitstring_match_map_field([Field1 | Rest1] = Args1, [Field2 | Rest2] = Args2, E, Parallel) ->
  case Field1 > Field2 of
    true ->
      refute_parallel_bitstring_match_map_field(Args1, Rest2, E, Parallel);
    false ->
      refute_parallel_bitstring_match_map_field(Rest1, Args2, E, Parallel)
  end;
refute_parallel_bitstring_match_map_field(_Args1, _Args2, _E, _Parallel) ->
  ok.

assert_module_scope(Meta, Kind, #{module := nil, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_expr_in_scope, "module", Kind});
assert_module_scope(_Meta, _Kind, #{module:=Module}) -> Module.

assert_function_scope(Meta, Kind, #{function := nil, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_expr_in_scope, "function", Kind});
assert_function_scope(_Meta, _Kind, #{function := Function}) -> Function.

assert_no_match_or_guard_scope(Meta, Kind, E) ->
  assert_no_match_scope(Meta, Kind, E),
  assert_no_guard_scope(Meta, Kind, E).
assert_no_match_scope(Meta, Kind, #{context := match, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_pattern_in_match, Kind});
assert_no_match_scope(_Meta, _Kind, _E) -> [].
assert_no_guard_scope(Meta, Kind, #{context := guard, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_expr_in_guard, Kind});
assert_no_guard_scope(_Meta, _Kind, _E) -> [].

assert_contextual_var(Meta, Name, #{contextual_vars := Vars, file := File}, Error) ->
  case lists:member(Name, Vars) of
    true -> ok;
    false -> form_error(Meta, File, ?MODULE, Error)
  end.

%% Here we look into the Clauses "optimistically", that is, we don't check for
%% multiple "do"s and similar stuff. After all, the error we're gonna give here
%% is just a friendlier version of the "undefined variable _" error that we
%% would raise if we found a "_ -> ..." clause in a "cond". For this reason, if
%% Clauses has a bad shape, we just do nothing and let future functions catch
%% this.
assert_no_underscore_clause_in_cond([{do, Clauses}], E) when is_list(Clauses) ->
  case lists:last(Clauses) of
    {'->', Meta, [[{'_', _, Atom}], _]} when is_atom(Atom) ->
      form_error(Meta, ?key(E, file), ?MODULE, underscore_in_cond);
    _Other ->
      ok
  end;
assert_no_underscore_clause_in_cond(_Other, _E) ->
  ok.

%% Errors

format_error({useless_literal, Term}) ->
  io_lib:format("code block contains unused literal ~ts "
                "(remove the literal or assign it to _ to avoid warnings)",
                ['Elixir.Macro':to_string(Term)]);
format_error({useless_var, Var}) ->
  io_lib:format("variable ~ts in code block has no effect as it is never returned "
                "(remove the variable or assign it to _ to avoid warnings)",
                [Var]);
format_error({useless_attr, Attr}) ->
  io_lib:format("module attribute @~ts in code block has no effect as it is never returned "
                "(remove the attribute or assign it to _ to avoid warnings)",
                [Attr]);
format_error({missing_option, Construct, Opts}) when is_list(Opts) ->
  StringOpts = lists:map(fun(Opt) -> [$: | atom_to_list(Opt)] end, Opts),
  io_lib:format("missing ~ts option in \"~ts\"", [string:join(StringOpts, "/"), Construct]);
format_error({invalid_args, Construct}) ->
  io_lib:format("invalid arguments for \"~ts\"", [Construct]);
format_error({for_invalid_uniq, Value}) ->
  io_lib:format(":uniq option for comprehensions only accepts a boolean, got: ~ts", ['Elixir.Macro':to_string(Value)]);
format_error(for_generator_start) ->
  "for comprehensions must start with a generator";
format_error(unhandled_arrow_op) ->
  "unhandled operator ->";
format_error(as_in_multi_alias_call) ->
  ":as option is not supported by multi-alias call";
format_error({expected_compile_time_module, Kind, GivenTerm}) ->
  io_lib:format("invalid argument for ~ts, expected a compile time atom or alias, got: ~ts",
                [Kind, 'Elixir.Macro':to_string(GivenTerm)]);
format_error({unquote_outside_quote, Unquote}) ->
  %% Unquote can be "unquote" or "unquote_splicing".
  io_lib:format("~p called outside quote", [Unquote]);
format_error({invalid_context_opt_for_quote, Context}) ->
  io_lib:format("invalid :context for quote, expected non-nil compile time atom or alias, got: ~ts",
                ['Elixir.Macro':to_string(Context)]);
format_error(wrong_number_of_args_for_super) ->
  "super must be called with the same number of arguments as the current definition";
format_error({invalid_arg_for_pin, Arg}) ->
  io_lib:format("invalid argument for unary operator ^, expected an existing variable, got: ^~ts",
                ['Elixir.Macro':to_string(Arg)]);
format_error({pin_outside_of_match, Arg}) ->
  io_lib:format("cannot use ^~ts outside of match clauses", ['Elixir.Macro':to_string(Arg)]);
format_error(unbound_underscore) ->
  "invalid use of _. \"_\" represents a value to be ignored in a pattern and cannot be used in expressions";
format_error({undefined_var, Name, Kind}) ->
  io_lib:format("undefined variable \"~ts\"~ts", [Name, context_info(Kind)]);
format_error({undefined_var_pin, Name, Kind}) ->
  Message = "undefined variable ^~ts. No variable \"~ts\"~ts has been defined before the current pattern",
  io_lib:format(Message, [Name, Name, context_info(Kind)]);
format_error({undefined_var_bang, Name, Kind}) ->
  Message = "expected \"~ts\"~ts to expand to an existing variable or be part of a match",
  io_lib:format(Message, [Name, context_info(Kind)]);
format_error(underscore_in_cond) ->
  "invalid use of _ inside \"cond\". If you want the last clause to always match, "
    "you probably meant to use: true ->";
format_error({invalid_expr_in_guard, Kind}) ->
  Message =
    "invalid expression in guard, ~ts is not allowed in guards. To learn more about "
    "guards, visit: https://hexdocs.pm/elixir/guards.html",
  io_lib:format(Message, [Kind]);
format_error({invalid_pattern_in_match, Kind}) ->
  io_lib:format("invalid pattern in match, ~ts is not allowed in matches", [Kind]);
format_error({invalid_expr_in_scope, Scope, Kind}) ->
  io_lib:format("cannot invoke ~ts outside ~ts", [Kind, Scope]);
format_error({invalid_alias, Expr}) ->
  Message =
    "invalid alias: \"~ts\". If you wanted to define an alias, an alias must expand "
    "to an atom at compile time but it did not, you may use Module.concat/2 to build "
    "it at runtime. If instead you wanted to invoke a function or access a field, "
    "wrap the function or field name in double quotes",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]);
format_error({op_ambiguity, Name, Arg}) ->
  Message =
    "\"~ts ~ts\" looks like a function call but there is a variable named \"~ts\", "
    "please use explicit parentheses or even spaces",
  io_lib:format(Message, [Name, 'Elixir.Macro':to_string(Arg), Name]);
format_error({invalid_clauses, Name}) ->
  Message =
    "the function \"~ts\" cannot handle clauses with the -> operator because it is not macro. "
    "Please make sure you are invoking the proper name and that it is a macro",
  io_lib:format(Message, [Name]);
format_error({invalid_alias_for_as, Reason, Value}) ->
  ExpectedGot =
    case Reason of
      not_alias -> "expected an alias, got";
      nested_alias -> "expected a simple alias, got nested alias"
    end,
  io_lib:format("invalid value for option :as, ~ts: ~ts",
                [ExpectedGot, 'Elixir.Macro':to_string(Value)]);
format_error({invalid_function_call, Expr}) ->
  io_lib:format("invalid function call :~ts.()", [Expr]);
format_error({invalid_call, Call}) ->
  io_lib:format("invalid call ~ts", ['Elixir.Macro':to_string(Call)]);
format_error({invalid_quoted_expr, Expr}) ->
  io_lib:format("invalid quoted expression: ~ts", ['Elixir.Kernel':inspect(Expr, [])]);
format_error({invalid_local_invocation, Context, {Name, _, Args} = Call}) ->
  Message =
    "cannot find or invoke local ~ts/~B inside ~ts. "
    "Only macros can be invoked in a ~ts and they must be defined before their invocation. Called as: ~ts",
  io_lib:format(Message, [Name, length(Args), Context, Context, 'Elixir.Macro':to_string(Call)]);
format_error({invalid_pid_in_function, Pid, {Name, Arity}}) ->
  io_lib:format("cannot compile PID ~ts inside quoted expression for function ~ts/~B",
                ['Elixir.Kernel':inspect(Pid, []), Name, Arity]);
format_error({unsupported_option, Kind, Key}) ->
  io_lib:format("unsupported option ~ts given to ~s",
                ['Elixir.Macro':to_string(Key), Kind]);
format_error({options_are_not_keyword, Kind, Opts}) ->
  io_lib:format("invalid options for ~s, expected a keyword list, got: ~ts",
                [Kind, 'Elixir.Macro':to_string(Opts)]);
format_error({undefined_function, '|', [_, _]}) ->
  "misplaced operator |/2\n\n"
  "The | operator is typically used between brackets as the cons operator:\n\n"
  "    [head | tail]\n\n"
  "where head is a single element and the tail is the remaining of a list.\n"
  "It is also used to update maps and structs, via the %{map | key: value} notation,\n"
  "and in typespecs, such as @type and @spec, to express the union of two types";
format_error({undefined_function, '::', [_, _]}) ->
  "misplaced operator ::/2\n\n"
  "The :: operator is typically used in bitstrings to specify types and sizes of segments:\n\n"
  "    <<size::32-integer, letter::utf8, rest::binary>>\n\n"
  "It is also used in typespecs, such as @type and @spec, to describe inputs and outputs";
format_error({undefined_function, Name, Args}) ->
  io_lib:format("undefined function ~ts/~B", [Name, length(Args)]);
format_error({underscored_var_repeat, Name, Kind}) ->
  io_lib:format("the underscored variable \"~ts\"~ts appears more than once in a "
                "match. This means the pattern will only match if all \"~ts\" bind "
                "to the same value. If this is the intended behaviour, please "
                "remove the leading underscore from the variable name, otherwise "
                "give the variables different names", [Name, context_info(Kind), Name]);
format_error({underscored_var_access, Name}) ->
  io_lib:format("the underscored variable \"~ts\" is used after being set. "
                "A leading underscore indicates that the value of the variable "
                "should be ignored. If this is intended please rename the "
                "variable to remove the underscore", [Name]);
format_error({struct_comparison, StructExpr}) ->
  String = 'Elixir.Macro':to_string(StructExpr),
  io_lib:format("invalid comparison with struct literal ~ts. Comparison operators "
                "(>, <, >=, <=) perform structural and not semantic comparison. "
                "Comparing with a struct literal is unlikely to give a meaningful result. "
                "Modules typically define a compare/2 function that can be used for "
                "semantic comparison", [String]);
format_error({nested_comparison, CompExpr}) ->
  String = 'Elixir.Macro':to_string(CompExpr),
  io_lib:format("Elixir does not support nested comparisons. Something like\n\n"
                "     x < y < z\n\n"
                "is equivalent to\n\n"
                "     (x < y) < z\n\n"
                "which ultimately compares z with the boolean result of (x < y). "
                "Instead, consider joining together each comparison segment with an \"and\", e.g.\n\n"
                "     x < y and y < z\n\n"
                "You wrote: ~ts", [String]);

format_error(caller_not_allowed) ->
  "__CALLER__ is available only inside defmacro and defmacrop";
format_error(stacktrace_not_allowed) ->
  "__STACKTRACE__ is available only inside catch and rescue clauses of try expressions";
format_error({parallel_bitstring_match, Expr}) ->
  Message =
    "binary patterns cannot be matched in parallel using \"=\", excess pattern: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]).
