-module(elixir_expand).
-export([expand/2, expand_args/2, expand_arg/2, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% =

expand({'=', Meta, [Left, Right]}, E) ->
  assert_no_guard_scope(Meta, "=", E),
  {ERight, ER} = expand(Right, E),
  {ELeft, EL} = elixir_clauses:match(fun expand/2, Left, ER, E),
  refute_parallel_bitstring_match(ELeft, ERight, E, ?key(E, context) == match),
  {{'=', Meta, [ELeft, ERight]}, EL};

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
  form_error(Meta, E, ?MODULE, unhandled_arrow_op);

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
          form_error(Meta, E, ?MODULE, as_in_multi_alias_call);
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
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, alias, Ref})
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
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, require, Ref})
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
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, import, Ref})
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
expand({'__ENV__', Meta, Atom}, #{context := match} = E) when is_atom(Atom) ->
  form_error(Meta, E, ?MODULE, env_not_allowed);
expand({'__ENV__', Meta, Atom}, E) when is_atom(Atom) ->
  {maybe_escape_map(escape_env_entries(Meta, E)), E};
expand({{'.', DotMeta, [{'__ENV__', Meta, Atom}, Field]}, CallMeta, []}, E) when is_atom(Atom), is_atom(Field) ->
  Env = escape_env_entries(Meta, E),
  case maps:is_key(Field, Env) of
    true  -> {maps:get(Field, Env), E};
    false -> {{{'.', DotMeta, [maybe_escape_map(Env), Field]}, CallMeta, []}, E}
  end;

%% Quote

expand({Unquote, Meta, [_]}, E) when Unquote == unquote; Unquote == unquote_splicing ->
  form_error(Meta, E, ?MODULE, {unquote_outside_quote, Unquote});

expand({quote, Meta, [Opts]}, E) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    {do, Do} ->
      expand({quote, Meta, [lists:keydelete(do, 1, Opts), [{do, Do}]]}, E);
    false ->
      form_error(Meta, E, ?MODULE, {missing_option, 'quote', [do]})
  end;

expand({quote, Meta, [_]}, E) ->
  form_error(Meta, E, ?MODULE, {invalid_args, 'quote'});

expand({quote, Meta, [Opts, Do]}, E) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      {do, Expr} -> Expr;
      false -> form_error(Meta, E, ?MODULE, {missing_option, 'quote', [do]})
    end,

  ValidOpts = [context, location, line, file, unquote, bind_quoted, generated],
  {EOpts, ET} = expand_opts(Meta, quote, ValidOpts, Opts, E),

  Context = proplists:get_value(context, EOpts, case ?key(E, module) of
    nil -> 'Elixir';
    Mod -> Mod
  end),

  {File, Line} = case lists:keyfind(location, 1, EOpts) of
    {location, keep} ->
      {elixir_utils:relative_to_cwd(?key(E, file)), false};
    false ->
      {proplists:get_value(file, EOpts, nil), proplists:get_value(line, EOpts, false)}
  end,

  {Binding, DefaultUnquote} = case lists:keyfind(bind_quoted, 1, EOpts) of
    {bind_quoted, BQ} ->
      case is_list(BQ) andalso
            lists:all(fun({Key, _}) when is_atom(Key) -> true; (_) -> false end, BQ) of
        true -> {BQ, false};
        false -> form_error(Meta, E, ?MODULE, {invalid_bind_quoted_for_quote, BQ})
      end;
    false ->
      {[], true}
  end,

  Unquote = proplists:get_value(unquote, EOpts, DefaultUnquote),
  Generated = proplists:get_value(generated, EOpts, false),

  {Q, Prelude} = elixir_quote:build(Meta, Line, File, Context, Unquote, Generated),
  Quoted = elixir_quote:quote(Meta, Exprs, Binding, Q, Prelude, ET),
  expand(Quoted, ET);

expand({quote, Meta, [_, _]}, E) ->
  form_error(Meta, E, ?MODULE, {invalid_args, 'quote'});

%% Functions

expand({'&', Meta, [{super, SuperMeta, Args} = Expr]}, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "&", E),

  case resolve_super(Meta, length(Args), E) of
    {Kind, Name, _} when Kind == def; Kind == defp ->
      expand_fn_capture(Meta, {Name, SuperMeta, Args}, E);
    _ ->
      expand_fn_capture(Meta, Expr, E)
  end;

expand({'&', Meta, [{'/', _, [{super, _, Context}, Arity]} = Expr]}, E) when is_atom(Context), is_integer(Arity) ->
  assert_no_match_or_guard_scope(Meta, "&", E),

  case resolve_super(Meta, Arity, E) of
    {Kind, Name, _} when Kind == def; Kind == defp ->
      {{'&', Meta, [{'/', [], [{Name, [], Context}, Arity]}]}, E};
    _ ->
      expand_fn_capture(Meta, Expr, E)
  end;

expand({'&', Meta, [Arg]}, E) ->
  assert_no_match_or_guard_scope(Meta, "&", E),
  expand_fn_capture(Meta, Arg, E);

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

  validate_opts(Meta, for, [do, into, uniq, reduce], Block, E),

  {Expr, Opts} =
    case lists:keytake(do, 1, Block) of
      {value, {do, Do}, DoOpts} ->
        {Do, DoOpts};
      false ->
        form_error(Meta, E, ?MODULE, {missing_option, for, [do]})
    end,

  {EOpts, EO} = expand(Opts, elixir_env:reset_unused_vars(E)),
  {ECases, EC} = lists:mapfoldl(fun expand_for/2, EO, Cases),
  assert_generator_start(Meta, ECases, E),

  {EExpr, EE} =
    case validate_for_options(EOpts, false, false, false) of
      {ok, MaybeReduce} -> expand_for_do_block(Meta, Expr, EC, MaybeReduce);
      {error, Error} -> form_error(Meta, E, ?MODULE, Error)
    end,

  {{for, Meta, ECases ++ [[{do, EExpr} | EOpts]]}, elixir_env:merge_and_check_unused_vars(E, EE)};

%% With

expand({with, Meta, [_ | _] = Args}, E) ->
  assert_no_match_or_guard_scope(Meta, "with", E),
  elixir_clauses:with(Meta, Args, E);

%% Super

expand({super, Meta, Args}, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "super", E),
  {Kind, Name, _} = resolve_super(Meta, length(Args), E),
  {EArgs, EA} = expand_args(Args, E),
  {{super, [{super, {Kind, Name}} | Meta], EArgs}, EA};

%% Vars

expand({'^', Meta, [Arg]}, #{context := match} = E) ->
  #{current_vars := {_ReadCurrent, WriteCurrent}, prematch_vars := {Prematch, _}} = E,

  %% We need to rollback to a no match context.
  NoMatchE = E#{context := nil, current_vars := {Prematch, WriteCurrent}, prematch_vars := pin},

  case expand(Arg, NoMatchE) of
    {{Name, _, Kind} = Var, #{unused_vars := Unused}} when is_atom(Name), is_atom(Kind) ->
      {{'^', Meta, [Var]}, E#{unused_vars := Unused}};
    _ ->
      form_error(Meta, E, ?MODULE, {invalid_arg_for_pin, Arg})
  end;
expand({'^', Meta, [Arg]}, E) ->
  form_error(Meta, E, ?MODULE, {pin_outside_of_match, Arg});

expand({'_', _Meta, Kind} = Var, #{context := match} = E) when is_atom(Kind) ->
  {Var, E};
expand({'_', Meta, Kind}, E) when is_atom(Kind) ->
  form_error(Meta, E, ?MODULE, unbound_underscore);

expand({Name, Meta, Kind}, #{context := match} = E) when is_atom(Name), is_atom(Kind) ->
  #{
    current_vars := {ReadCurrent, WriteCurrent},
    unused_vars := {Unused, Version},
    prematch_vars := {_, PrematchVersion}
  } = E,

  Pair = {Name, elixir_utils:var_context(Meta, Kind)},

  case ReadCurrent of
    %% Variable was already overridden
    #{Pair := VarVersion} when VarVersion >= PrematchVersion ->
      maybe_warn_underscored_var_repeat(Meta, Name, Kind, E),
      NewUnused = var_used(Pair, VarVersion, Unused),
      Var = {Name, [{version, VarVersion} | Meta], Kind},
      {Var, E#{unused_vars := {NewUnused, Version}}};

    %% Variable is being overridden now
    #{Pair := _} ->
      NewUnused = var_unused(Pair, Meta, Version, Unused, true),
      NewReadCurrent = ReadCurrent#{Pair => Version},
      NewWriteCurrent = (WriteCurrent /= false) andalso WriteCurrent#{Pair => Version},
      Var = {Name, [{version, Version} | Meta], Kind},
      {Var, E#{current_vars := {NewReadCurrent, NewWriteCurrent}, unused_vars := {NewUnused, Version + 1}}};

    %% Variable defined for the first time
    _ ->
      NewVars = ordsets:add_element(Pair, ?key(E, vars)),
      NewUnused = var_unused(Pair, Meta, Version, Unused, false),
      NewReadCurrent = ReadCurrent#{Pair => Version},
      NewWriteCurrent = (WriteCurrent /= false) andalso WriteCurrent#{Pair => Version},
      Var = {Name, [{version, Version} | Meta], Kind},
      {Var, E#{vars := NewVars, current_vars := {NewReadCurrent, NewWriteCurrent}, unused_vars := {NewUnused, Version + 1}}}
  end;

expand({Name, Meta, Kind}, E) when is_atom(Name), is_atom(Kind) ->
  #{current_vars := {ReadCurrent, _WriteCurrent}, unused_vars := {Unused, Version}} = E,
  Pair = {Name, elixir_utils:var_context(Meta, Kind)},

  case ReadCurrent of
    #{Pair := PairVersion} ->
      maybe_warn_underscored_var_access(Meta, Name, Kind, E),
      Var = {Name, [{version, PairVersion} | Meta], Kind},
      {Var, E#{unused_vars := {var_used(Pair, PairVersion, Unused), Version}}};

    _ ->
      %% TODO: Remove this check on v2.0 as we can always raise undefined_var
      case lists:keyfind(var, 1, Meta) of
        {var, true} ->
          form_error(Meta, E, ?MODULE, {undefined_var_bang, Name, Kind});

        _ ->
          case ?key(E, prematch_vars) of
            warn ->
              %% TODO: Remove warn option on v2.0
              elixir_errors:form_warn(Meta, E, ?MODULE, {unknown_variable, Name}),
              expand({Name, Meta, []}, E);

            raise ->
              form_error(Meta, E, ?MODULE, {undefined_var, Name, Kind});

            pin ->
              form_error(Meta, E, ?MODULE, {undefined_var_pin, Name, Kind});

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
  {ELeft, EL} = expand(Left, elixir_env:prepare_write(E)),

  elixir_dispatch:dispatch_require(Meta, ELeft, Right, Args, EL, fun(AR, AF, AA) ->
    expand_remote(AR, DotMeta, AF, Meta, AA, E, EL)
  end);

%% Anonymous calls

expand({{'.', DotMeta, [Expr]}, Meta, Args}, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "anonymous call", E),

  case expand_args([Expr | Args], E) of
    {[EExpr | _], _} when is_atom(EExpr) ->
      form_error(Meta, E, ?MODULE, {invalid_function_call, EExpr});

    {[EExpr | EArgs], EA} ->
      {{{'.', DotMeta, [EExpr]}, Meta, EArgs}, EA}
  end;

%% Invalid calls

expand({_, Meta, Args} = Invalid, E) when is_list(Meta) and is_list(Args) ->
  form_error(Meta, E, ?MODULE, {invalid_call, Invalid});

expand({_, _, _} = Tuple, E) ->
  form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Tuple});

%% Literals

expand({Left, Right}, E) ->
  {[ELeft, ERight], EE} = expand_args([Left, Right], E),
  {{ELeft, ERight}, EE};

expand(List, #{context := match} = E) when is_list(List) ->
  expand_list(List, fun expand/2, E, []);

expand(List, E) when is_list(List) ->
  {EArgs, {EE, _}} = expand_list(List, fun expand_arg/2, {elixir_env:prepare_write(E), E}, []),
  {EArgs, elixir_env:close_write(EE, E)};

expand(Function, E) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
       (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {elixir_quote:fun_to_quoted(Function), E};
    false ->
      form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Function})
  end;

expand(Pid, E) when is_pid(Pid) ->
  case ?key(E, function) of
    nil ->
      {Pid, E};
    Function ->
      %% TODO: Make me an error on v2.0
      elixir_errors:form_warn([], E, ?MODULE, {invalid_pid_in_function, Pid, Function}),
      {Pid, E}
  end;

expand(Other, E) when is_number(Other); is_atom(Other); is_binary(Other) ->
  {Other, E};

expand(Other, E) ->
  form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Other}).

%% Helpers

escape_env_entries(Meta, #{current_vars := {Read, Write}, unused_vars := {Unused, Version}} = Env0) ->
  Env1 = case Env0 of
    #{function := nil} -> Env0;
    _ -> Env0#{lexical_tracker := nil, tracers := []}
  end,
  Current = {maybe_escape_map(Read), maybe_escape_map(Write)},
  Env2 = Env1#{current_vars := Current, unused_vars := {maybe_escape_map(Unused), Version}},
  Env3 = elixir_env:linify({?line(Meta), Env2}),
  Env3.

maybe_escape_map(#{} = Map) -> {'%{}', [], maps:to_list(Map)};
maybe_escape_map(Other) -> Other.

expand_multi_alias_call(Kind, Meta, Base, Refs, Opts, E) ->
  {BaseRef, EB} = expand_without_aliases_report(Base, E),
  Fun = fun
    ({'__aliases__', _, Ref}, ER) ->
      expand({Kind, Meta, [elixir_aliases:concat([BaseRef | Ref]), Opts]}, ER);
    (Ref, ER) when is_atom(Ref) ->
      expand({Kind, Meta, [elixir_aliases:concat([BaseRef, Ref]), Opts]}, ER);
    (Other, _ER) ->
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, Kind, Other})
  end,
  lists:mapfoldl(Fun, EB, Refs).

resolve_super(Meta, Arity, E) ->
  Module = assert_module_scope(Meta, super, E),
  Function = assert_function_scope(Meta, super, E),

  case Function of
    {_, Arity} ->
      {Kind, Name, SuperMeta} = elixir_overridable:super(Meta, Module, Function, E),
      maybe_warn_deprecated_super_in_gen_server_callback(Meta, Function, SuperMeta, E),
      {Kind, Name, SuperMeta};

    _ ->
      form_error(Meta, E, ?MODULE, wrong_number_of_args_for_super)
  end.

expand_fn_capture(Meta, Arg, E) ->
  case elixir_fn:capture(Meta, Arg, E) of
    {{remote, Remote, Fun, Arity}, EE} ->
      is_atom(Remote) andalso
        elixir_env:trace({remote_function, Meta, Remote, Fun, Arity}, E),
      AttachedMeta = attach_context_module(Remote, Meta, E),
      {{'&', AttachedMeta, [{'/', [], [{{'.', [], [Remote, Fun]}, [], []}, Arity]}]}, EE};
    {{local, Fun, Arity}, #{function := nil}} ->
      form_error(Meta, E, ?MODULE, {undefined_local_capture, Fun, Arity});
    {{local, Fun, Arity}, EE} ->
      {{'&', Meta, [{'/', [], [{Fun, [], nil}, Arity]}]}, EE};
    {expand, Expr, EE} ->
      expand(Expr, EE)
  end.

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

  %% Note that checks rely on the code BEFORE expansion
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
      elixir_errors:form_warn(UselessMeta, E, ?MODULE, UselessTerm);
    false ->
      ok
  end,

  expand_block(T, [EH | Acc], Meta, EE).

%% Note that we don't handle atoms on purpose. They are common
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
expand_arg(Arg, {Acc, E}) ->
  {EArg, EAcc} = expand(Arg, elixir_env:reset_read(Acc, E)),
  {EArg, {EAcc, E}}.

expand_args([Arg], E) ->
  {EArg, EE} = expand(Arg, E),
  {[EArg], EE};
expand_args(Args, #{context := match} = E) ->
  lists:mapfoldl(fun expand/2, E, Args);
expand_args(Args, E) ->
  {EArgs, {EA, _}} = lists:mapfoldl(fun expand_arg/2, {elixir_env:prepare_write(E), E}, Args),
  {EArgs, elixir_env:close_write(EA, E)}.

%% Match/var helpers

var_unused({Name, Kind}, Meta, Version, Unused, Override) ->
  case (Kind == nil) andalso should_warn(Meta) of
    true -> Unused#{{Name, Version} => {?line(Meta), Override}};
    false -> Unused
  end.

var_used({Name, Kind}, Version, Unused) ->
  case Kind of
    nil -> Unused#{{Name, Version} => false};
    _ -> Unused
  end.

maybe_warn_underscored_var_repeat(Meta, Name, Kind, E) ->
  case should_warn(Meta) andalso atom_to_list(Name) of
    "_" ++ _ ->
      elixir_errors:form_warn(Meta, E, ?MODULE, {underscored_var_repeat, Name, Kind});
    _ ->
      ok
  end.

maybe_warn_underscored_var_access(Meta, Name, Kind, E) ->
  case (Kind == nil) andalso should_warn(Meta) andalso atom_to_list(Name) of
    "_" ++ _ ->
      elixir_errors:form_warn(Meta, E, ?MODULE, {underscored_var_access, Name});
    _ ->
      ok
  end.

%% TODO: Remove this on Elixir v2.0 and make all GenServer callbacks optional
maybe_warn_deprecated_super_in_gen_server_callback(Meta, Function, SuperMeta, E) ->
  case lists:keyfind(context, 1, SuperMeta) of
    {context, 'Elixir.GenServer'} ->
      case Function of
        {child_spec, 1} ->
          ok;

        _ ->
          elixir_errors:form_warn(Meta, E, ?MODULE, {super_in_genserver, Function})
      end;

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

%% Comprehensions

validate_for_options([{into, _} = Pair | Opts], _Into, Uniq, Reduce) ->
  validate_for_options(Opts, Pair, Uniq, Reduce);
validate_for_options([{uniq, Boolean} = Pair | Opts], Into, _Uniq, Reduce) when is_boolean(Boolean) ->
  validate_for_options(Opts, Into, Pair, Reduce);
validate_for_options([{uniq, Value} | _], _, _, _) ->
  {error, {for_invalid_uniq, Value}};
validate_for_options([{reduce, _} = Pair | Opts], Into, Uniq, _Reduce) ->
  validate_for_options(Opts, Into, Uniq, Pair);
validate_for_options([], Into, Uniq, {reduce, _}) when Into /= false; Uniq /= false ->
  {error, for_conflicting_reduce_into_uniq};
validate_for_options([], _Into, _Uniq, Reduce) ->
  {ok, Reduce}.

expand_for_do_block(Meta, Expr, E, false) ->
  case Expr of
    [{'->', _, _} | _] -> form_error(Meta, E, ?MODULE, for_without_reduce_bad_block);
    _ -> expand(Expr, E)
  end;
expand_for_do_block(Meta, Clauses, E, {reduce, _}) ->
  Transformer = fun({_, _, [Left, _Right]} = Clause, Acc) ->
    case Left of
      [_] ->
        EReset = elixir_env:reset_unused_vars(Acc),
        {EClause, EAcc} = elixir_clauses:clause(Meta, fn, fun elixir_clauses:head/2, Clause, EReset),
        {EClause, elixir_env:merge_and_check_unused_vars(Acc, EAcc)};
      _ ->
        form_error(Meta, E, ?MODULE, for_with_reduce_bad_block)
    end
  end,

  case Clauses of
    [{'->', _, _} | _] -> lists:mapfoldl(Transformer, E, Clauses);
    _ -> form_error(Meta, E, ?MODULE, for_with_reduce_bad_block)
  end.

%% Locals

assert_no_ambiguous_op(Name, Meta, [Arg], E) ->
  case lists:keyfind(ambiguous_op, 1, Meta) of
    {ambiguous_op, Kind} ->
      Pair = {Name, Kind},
      case ?key(E, current_vars) of
        {#{Pair := _}, _} ->
          form_error(Meta, E, ?MODULE, {op_ambiguity, Name, Arg});
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
      form_error(Meta, E, ?MODULE, {invalid_clauses, Name});
    _ ->
      assert_arg_with_no_clauses(Name, Meta, Rest, E)
  end;
assert_arg_with_no_clauses(_Name, _Meta, _Arg, _E) ->
  ok.

expand_local(Meta, Name, Args, #{module := Module, function := Function, context := nil} = E)
    when Function /= nil ->
  assert_no_clauses(Name, Meta, Args, E),
  Arity = length(Args),
  elixir_env:trace({local_function, Meta, Name, Arity}, E),
  elixir_locals:record_local({Name, Arity}, Module, Function, Meta, false),
  {EArgs, EA} = expand_args(Args, E),
  {{Name, Meta, EArgs}, EA};
expand_local(Meta, Name, Args, E) when Name == '|'; Name == '::' ->
  form_error(Meta, E, ?MODULE, {undefined_function, Name, Args});
expand_local(Meta, Name, Args, #{function := nil} = E) ->
  form_error(Meta, E, ?MODULE, {undefined_function, Name, Args});
expand_local(Meta, Name, Args, #{context := Context} = E) when Context == match; Context == guard ->
  form_error(Meta, E, ?MODULE, {invalid_local_invocation, Context, {Name, Meta, Args}}).

%% Remote

expand_remote(Receiver, DotMeta, Right, Meta, Args, #{context := Context} = E, EL) when is_atom(Receiver) or is_tuple(Receiver) ->
  assert_no_clauses(Right, Meta, Args, E),

  case {Context, lists:keyfind(no_parens, 1, Meta)} of
    {guard, {no_parens, true}} when is_tuple(Receiver) ->
      {{{'.', DotMeta, [Receiver, Right]}, Meta, []}, EL};

    {guard, _} when is_tuple(Receiver) ->
      form_error(Meta, E, ?MODULE, {parens_map_lookup_guard, Receiver, Right});

    _ ->
      AttachedDotMeta = attach_context_module(Receiver, DotMeta, E),

      is_atom(Receiver) andalso
        elixir_env:trace({remote_function, DotMeta, Receiver, Right, length(Args)}, E),

      {EArgs, {EA, _}} = lists:mapfoldl(fun expand_arg/2, {EL, E}, Args),

      case rewrite(Context, Receiver, AttachedDotMeta, Right, Meta, EArgs) of
        {ok, Rewritten} ->
          maybe_warn_comparison(Rewritten, Args, E),
          {Rewritten, elixir_env:close_write(EA, E)};
        {error, Error} ->
          form_error(Meta, E, elixir_rewrite, Error)
      end
  end;
expand_remote(Receiver, DotMeta, Right, Meta, Args, E, _) ->
  Call = {{'.', DotMeta, [Receiver, Right]}, Meta, Args},
  form_error(Meta, E, ?MODULE, {invalid_call, Call}).

attach_context_module(_Receiver, Meta, #{function := nil}) ->
  Meta;
attach_context_module(Receiver, Meta, #{context_modules := ContextModules}) ->
  case lists:member(Receiver, ContextModules) of
    true -> [{context_module, true} | Meta];
    false -> Meta
  end.

rewrite(match, Receiver, DotMeta, Right, Meta, EArgs) ->
  elixir_rewrite:match_rewrite(Receiver, DotMeta, Right, Meta, EArgs);
rewrite(guard, Receiver, DotMeta, Right, Meta, EArgs) ->
  elixir_rewrite:guard_rewrite(Receiver, DotMeta, Right, Meta, EArgs);
rewrite(_, Receiver, DotMeta, Right, Meta, EArgs) ->
  {ok, elixir_rewrite:rewrite(Receiver, DotMeta, Right, Meta, EArgs)}.

maybe_warn_comparison({{'.', _, [erlang, Op]}, Meta, [ELeft, ERight]}, [Left, Right], E)
    when Op =:= '>'; Op =:= '<'; Op =:= '=<'; Op =:= '>='; Op =:= min; Op =:= max ->
  case is_struct_comparison(ELeft, ERight, Left, Right) of
    false ->
      case is_nested_comparison(Op, ELeft, ERight, Left, Right) of
        false -> ok;
        CompExpr ->
          elixir_errors:form_warn(Meta, E, ?MODULE, {nested_comparison, CompExpr})
      end;
    StructExpr ->
      elixir_errors:form_warn(Meta, E, ?MODULE, {struct_comparison, StructExpr})
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
    form_error(Meta, E, ?MODULE, {unsupported_option, Kind, Key})
  end || {Key, _} <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, Opts, E) ->
  form_error(Meta, E, ?MODULE, {options_are_not_keyword, Kind, Opts}).

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
  elixir_env:trace({require, Meta, Ref, Opts}, E),
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

  {Aliases, MacroAliases} = elixir_aliases:store(Meta, New, Ref, Opts, E),
  E#{aliases := Aliases, macro_aliases := MacroAliases, context_modules := NewContext}.

expand_as({as, nil}, _Meta, _IncludeByDefault, Ref, _E) ->
  Ref;
expand_as({as, Atom}, Meta, _IncludeByDefault, _Ref, E) when is_atom(Atom), not is_boolean(Atom) ->
  case atom_to_list(Atom) of
    "Elixir." ++ ([FirstLetter | _] = Rest) when FirstLetter >= $A, FirstLetter =< $Z ->
      case string:tokens(Rest, ".") of
        [_] ->
          Atom;
        _ ->
          form_error(Meta, E, ?MODULE, {invalid_alias_for_as, nested_alias, Atom})
      end;
    _ ->
      form_error(Meta, E, ?MODULE, {invalid_alias_for_as, not_alias, Atom})
  end;
expand_as(false, Meta, IncludeByDefault, Ref, E) ->
  if 
    IncludeByDefault -> 
      case elixir_aliases:last(Ref) of
        {ok, NewRef} -> NewRef;
        error -> form_error(Meta, E, ?MODULE, {invalid_alias_module, Ref})
      end;
    true -> Ref
  end;
expand_as({as, Other}, Meta, _IncludeByDefault, _Ref, E) ->
  form_error(Meta, E, ?MODULE, {invalid_alias_for_as, not_alias, Other}).

%% Aliases

expand_without_aliases_report({'__aliases__', _, _} = Alias, E) ->
  expand_aliases(Alias, E, false);
expand_without_aliases_report(Other, E) ->
  expand(Other, E).

expand_aliases({'__aliases__', Meta, _} = Alias, E, Report) ->
  case elixir_aliases:expand(Alias, E) of
    Receiver when is_atom(Receiver) ->
      Report andalso elixir_env:trace({alias_reference, Meta, Receiver}, E),
      {Receiver, E};

    Aliases ->
      {EAliases, EA} = expand_args(Aliases, E),

      case lists:all(fun is_atom/1, EAliases) of
        true ->
          Receiver = elixir_aliases:concat(EAliases),
          Report andalso elixir_env:trace({alias_reference, Meta, Receiver}, E),
          {Receiver, EA};
        false ->
          form_error(Meta, E, ?MODULE, {invalid_alias, Alias})
      end
  end.

%% Comprehensions

expand_for({'<-', Meta, [Left, Right]}, E) ->
  {ERight, ER} = expand(Right, E),
  {[ELeft], EL} = elixir_clauses:head([Left], ER, E),
  {{'<-', Meta, [ELeft, ERight]}, EL};
expand_for({'<<>>', Meta, Args} = X, E) when is_list(Args) ->
  case elixir_utils:split_last(Args) of
    {LeftStart, {'<-', OpMeta, [LeftEnd, Right]}} ->
      {ERight, ER} = expand(Right, E),
      {ELeft, EL} = elixir_clauses:match(fun(BArg, BE) ->
        elixir_bitstring:expand(Meta, BArg, BE, true)
      end, LeftStart ++ [LeftEnd], ER, E),
      {{'<<>>', [], [{'<-', OpMeta, [ELeft, ERight]}]}, EL};
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
  elixir_errors:form_error(Meta, E, ?MODULE, for_generator_start).

%% Assertions

refute_parallel_bitstring_match({'<<>>', _, _}, {'<<>>', Meta, _} = Arg, E, true) ->
  form_error(Meta, E, ?MODULE, {parallel_bitstring_match, Arg});
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
      form_error(Meta, E, ?MODULE, underscore_in_cond);
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
format_error(for_conflicting_reduce_into_uniq) ->
  "cannot use :reduce alongside :into/:uniq in comprehension";
format_error(for_with_reduce_bad_block) ->
  "when using :reduce with comprehensions, the do block must be written using acc -> expr clauses, where each clause expects the accumulator as a single argument";
format_error(for_without_reduce_bad_block) ->
  "the do block was written using acc -> expr clauses but the :reduce option was not given";
format_error(for_generator_start) ->
  "for comprehensions must start with a generator";
format_error(unhandled_arrow_op) ->
  "unhandled operator ->";
format_error(as_in_multi_alias_call) ->
  ":as option is not supported by multi-alias call";
format_error({invalid_alias_module, Ref}) ->
  io_lib:format("alias cannot be inferred automatically for module: ~ts, please use the :as option. Implicit aliasing is only supported with Elixir modules",
                ['Elixir.Macro':to_string(Ref)]);
format_error({expected_compile_time_module, Kind, GivenTerm}) ->
  io_lib:format("invalid argument for ~ts, expected a compile time atom or alias, got: ~ts",
                [Kind, 'Elixir.Macro':to_string(GivenTerm)]);
format_error({unquote_outside_quote, Unquote}) ->
  %% Unquote can be "unquote" or "unquote_splicing".
  io_lib:format("~p called outside quote", [Unquote]);
format_error({invalid_bind_quoted_for_quote, BQ}) ->
  io_lib:format("invalid :bind_quoted for quote, expected a keyword list of variable names, got: ~ts",
                ['Elixir.Macro':to_string(BQ)]);
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
    "guards, visit: https://hexdocs.pm/elixir/patterns-and-guards.html",
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
format_error({op_ambiguity, Name, {Op, _, [Arg]}}) ->
  NameString = atom_to_binary(Name, utf8),
  OpString = atom_to_binary(Op, utf8),
  ArgString = 'Elixir.Macro':to_string(Arg),

  Message =
    "\"~ts ~ts~ts\" looks like a function call but there is a variable named \"~ts\".\n"
    "If you want to perform a function call, use parentheses:\n"
    "\n"
    "    ~ts(~ts~ts)\n"
    "\n"
    "If you want to perform an operation on the variable ~ts, use even spaces instead:\n"
    "\n"
    "    ~ts ~ts ~ts",
  io_lib:format(Message, [NameString, OpString, ArgString, NameString,
                          NameString, OpString, ArgString, NameString,
                          NameString, OpString, ArgString]);
format_error({invalid_clauses, Name}) ->
  Message =
    "the function \"~ts\" cannot handle clauses with the -> operator because it is not a macro. "
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
  Message =
    "invalid quoted expression: ~ts\n\n"
    "Please make sure your quoted expressions are made of valid AST nodes. "
    "If you would like to introduce a value into the AST, such as a four-element "
    "tuple or a map, make sure to call Macro.escape/1 before",
  io_lib:format(Message, ['Elixir.Kernel':inspect(Expr, [])]);
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
  io_lib:format("undefined function ~ts/~B (there is no such import)", [Name, length(Args)]);
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
                "(>, <, >=, <=, min, and max) perform structural and not semantic comparison. "
                "Comparing with a struct literal is unlikely to give a meaningful result. "
                "Struct modules typically define a compare/2 function that can be used for "
                "semantic comparison", [String]);
format_error({nested_comparison, CompExpr}) ->
  String = 'Elixir.Macro':to_string(CompExpr),
  io_lib:format("Elixir does not support nested comparisons. Something like\n\n"
                "     x < y < z\n\n"
                "is equivalent to\n\n"
                "     (x < y) < z\n\n"
                "which ultimately compares z with the boolean result of (x < y). "
                "Instead, consider joining together each comparison segment with an \"and\", for example,\n\n"
                "     x < y and y < z\n\n"
                "You wrote: ~ts", [String]);
format_error({undefined_local_capture, Fun, Arity}) ->
  io_lib:format("undefined function ~ts/~B (there is no such import)", [Fun, Arity]);
format_error(env_not_allowed) ->
  "__ENV__ is not allowed inside a match";
format_error(caller_not_allowed) ->
  "__CALLER__ is available only inside defmacro and defmacrop";
format_error(stacktrace_not_allowed) ->
  "__STACKTRACE__ is available only inside catch and rescue clauses of try expressions";
format_error({unknown_variable, Name}) ->
  io_lib:format("variable \"~ts\" does not exist and is being expanded to \"~ts()\","
                " please use parentheses to remove the ambiguity or change the variable name", [Name, Name]);
format_error({parens_map_lookup_guard, Map, Field}) ->
  io_lib:format("cannot invoke remote function in guard. "
                "If you want to do a map lookup instead, please remove parens from ~ts.~ts()",
                ['Elixir.Macro':to_string(Map), Field]);
format_error({super_in_genserver, {Name, Arity}}) ->
  io_lib:format("calling super for GenServer callback ~ts/~B is deprecated", [Name, Arity]);
format_error({parallel_bitstring_match, Expr}) ->
  Message =
    "binary patterns cannot be matched in parallel using \"=\", excess pattern: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]).
