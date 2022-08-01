-module(elixir_expand).
-export([expand/3, expand_args/3, expand_arg/3, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% =

expand({'=', Meta, [Left, Right]}, S, E) ->
  assert_no_guard_scope(Meta, "=", S, E),
  {ERight, SR, ER} = expand(Right, S, E),
  {ELeft, SL, EL} = elixir_clauses:match(fun expand/3, Left, SR, S, ER),
  refute_parallel_bitstring_match(ELeft, ERight, E, ?key(E, context) == match),
  {{'=', Meta, [ELeft, ERight]}, SL, EL};

%% Literal operators

expand({'{}', Meta, Args}, S, E) ->
  {EArgs, SA, EA} = expand_args(Args, S, E),
  {{'{}', Meta, EArgs}, SA, EA};

expand({'%{}', Meta, Args}, S, E) ->
  elixir_map:expand_map(Meta, Args, S, E);

expand({'%', Meta, [Left, Right]}, S, E) ->
  elixir_map:expand_struct(Meta, Left, Right, S, E);

expand({'<<>>', Meta, Args}, S, E) ->
  elixir_bitstring:expand(Meta, Args, S, E, false);

expand({'->', Meta, _Args}, _S, E) ->
  form_error(Meta, E, ?MODULE, unhandled_arrow_op);

%% __block__

expand({'__block__', _Meta, []}, S, E) ->
  {nil, S, E};
expand({'__block__', _Meta, [Arg]}, S, E) ->
  expand(Arg, S, E);
expand({'__block__', Meta, Args}, S, E) when is_list(Args) ->
  {EArgs, SA, EA} = expand_block(Args, [], Meta, S, E),
  {{'__block__', Meta, EArgs}, SA, EA};

%% __aliases__

expand({'__aliases__', _, _} = Alias, S, E) ->
  expand_aliases(Alias, S, E, true);

%% alias

expand({Kind, Meta, [{{'.', _, [Base, '{}']}, _, Refs} | Rest]}, S, E)
    when Kind == alias; Kind == require; Kind == import ->
  case Rest of
    [] ->
      expand_multi_alias_call(Kind, Meta, Base, Refs, [], S, E);
    [Opts] ->
      case lists:keymember(as, 1, Opts) of
        true ->
          form_error(Meta, E, ?MODULE, as_in_multi_alias_call);
        false ->
          expand_multi_alias_call(Kind, Meta, Base, Refs, Opts, S, E)
      end
  end;
expand({alias, Meta, [Ref]}, S, E) ->
  expand({alias, Meta, [Ref, []]}, S, E);
expand({alias, Meta, [Ref, Opts]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "alias", S, E),
  {ERef, SR, ER} = expand_without_aliases_report(Ref, S, E),
  {EOpts, ST, ET} = expand_opts(Meta, alias, [as, warn], no_alias_opts(Opts), SR, ER),
  case lists:member(ERef, ['Elixir.True', 'Elixir.False', 'Elixir.Nil']) of
    true ->  elixir_errors:form_warn(Meta, E, ?MODULE, {commonly_mistaken_alias, Ref});
    false -> ok
  end,
  if
    is_atom(ERef) ->
      {ERef, ST, expand_alias(Meta, true, ERef, EOpts, ET)};
    true ->
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, alias, Ref})
  end;

expand({require, Meta, [Ref]}, S, E) ->
  expand({require, Meta, [Ref, []]}, S, E);
expand({require, Meta, [Ref, Opts]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "require", S, E),

  {ERef, SR, ER} = expand_without_aliases_report(Ref, S, E),
  {EOpts, ST, ET}  = expand_opts(Meta, require, [as, warn], no_alias_opts(Opts), SR, ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      {ERef, ST, expand_require(Meta, ERef, EOpts, ET)};
    true ->
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, require, Ref})
  end;

expand({import, Meta, [Left]}, S, E) ->
  expand({import, Meta, [Left, []]}, S, E);

expand({import, Meta, [Ref, Opts]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "import", S, E),
  {ERef, SR, ER} = expand_without_aliases_report(Ref, S, E),
  {EOpts, ST, ET} = expand_opts(Meta, import, [only, except, warn], Opts, SR, ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      {Functions, Macros} = elixir_import:import(Meta, ERef, EOpts, ET),
      {ERef, ST, expand_require(Meta, ERef, EOpts, ET#{functions := Functions, macros := Macros})};
    true ->
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, import, Ref})
  end;

%% Compilation environment macros

expand({'__MODULE__', _, Atom}, S, E) when is_atom(Atom) ->
  {?key(E, module), S, E};
expand({'__DIR__', _, Atom}, S, E) when is_atom(Atom) ->
  {filename:dirname(?key(E, file)), S, E};
expand({'__CALLER__', Meta, Atom} = Caller, S, E) when is_atom(Atom) ->
  assert_no_match_scope(Meta, "__CALLER__", E),
  case S#elixir_ex.caller of
    true -> {Caller, S, E};
    false -> form_error(Meta, E, ?MODULE, caller_not_allowed)
  end;
expand({'__STACKTRACE__', Meta, Atom} = Stacktrace, S, E) when is_atom(Atom) ->
  assert_no_match_scope(Meta, "__STACKTRACE__", E),
  case S#elixir_ex.stacktrace of
    true -> {Stacktrace, S, E};
    false -> form_error(Meta, E, ?MODULE, stacktrace_not_allowed)
  end;
expand({'__ENV__', Meta, Atom}, S, E) when is_atom(Atom) ->
  assert_no_match_scope(Meta, "__ENV__", E),
  {escape_map(escape_env_entries(Meta, S, E)), S, E};
expand({{'.', DotMeta, [{'__ENV__', Meta, Atom}, Field]}, CallMeta, []}, S, E)
    when is_atom(Atom), is_atom(Field) ->
  assert_no_match_scope(Meta, "__ENV__", E),
  Env = escape_env_entries(Meta, S, E),
  case maps:is_key(Field, Env) of
    true  -> {maps:get(Field, Env), S, E};
    false -> {{{'.', DotMeta, [escape_map(Env), Field]}, CallMeta, []}, S, E}
  end;

%% Quote

expand({Unquote, Meta, [_]}, _S, E) when Unquote == unquote; Unquote == unquote_splicing ->
  form_error(Meta, E, ?MODULE, {unquote_outside_quote, Unquote});

expand({quote, Meta, [Opts]}, S, E) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    {do, Do} ->
      expand({quote, Meta, [lists:keydelete(do, 1, Opts), [{do, Do}]]}, S, E);
    false ->
      form_error(Meta, E, ?MODULE, {missing_option, 'quote', [do]})
  end;

expand({quote, Meta, [_]}, _S, E) ->
  form_error(Meta, E, ?MODULE, {invalid_args, 'quote'});

expand({quote, Meta, [Opts, Do]}, S, E) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      {do, Expr} -> Expr;
      false -> form_error(Meta, E, ?MODULE, {missing_option, 'quote', [do]})
    end,

  ValidOpts = [context, location, line, file, unquote, bind_quoted, generated],
  {EOpts, ST, ET} = expand_opts(Meta, quote, ValidOpts, Opts, S, E),

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
  expand(Quoted, ST, ET);

expand({quote, Meta, [_, _]}, _S, E) ->
  form_error(Meta, E, ?MODULE, {invalid_args, 'quote'});

%% Functions

expand({'&', Meta, [{super, SuperMeta, Args} = Expr]}, S, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "&", S, E),

  case resolve_super(Meta, length(Args), E) of
    {Kind, Name, _} when Kind == def; Kind == defp ->
      expand_fn_capture(Meta, {Name, SuperMeta, Args}, S, E);
    _ ->
      expand_fn_capture(Meta, Expr, S, E)
  end;

expand({'&', Meta, [{'/', _, [{super, _, Context}, Arity]} = Expr]}, S, E) when is_atom(Context), is_integer(Arity) ->
  assert_no_match_or_guard_scope(Meta, "&", S, E),

  case resolve_super(Meta, Arity, E) of
    {Kind, Name, _} when Kind == def; Kind == defp ->
      {{'&', Meta, [{'/', [], [{Name, [], Context}, Arity]}]}, S, E};
    _ ->
      expand_fn_capture(Meta, Expr, S, E)
  end;

expand({'&', Meta, [Arg]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "&", S, E),
  expand_fn_capture(Meta, Arg, S, E);

expand({fn, Meta, Pairs}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "fn", S, E),
  elixir_fn:expand(Meta, Pairs, S, E);

%% Case/Receive/Try

expand({'cond', Meta, [Opts]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "cond", S, E),
  assert_no_underscore_clause_in_cond(Opts, E),
  {EClauses, SC, EC} = elixir_clauses:'cond'(Meta, Opts, S, E),
  {{'cond', Meta, [EClauses]}, SC, EC};

expand({'case', Meta, [Expr, Options]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "case", S, E),
  expand_case(Meta, Expr, Options, S, E);

expand({'receive', Meta, [Opts]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "receive", S, E),
  {EClauses, SC, EC} = elixir_clauses:'receive'(Meta, Opts, S, E),
  {{'receive', Meta, [EClauses]}, SC, EC};

expand({'try', Meta, [Opts]}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "try", S, E),
  {EClauses, SC, EC} = elixir_clauses:'try'(Meta, Opts, S, E),
  {{'try', Meta, [EClauses]}, SC, EC};

%% Comprehensions

expand({for, Meta, [_ | _] = Args}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "for", S, E),

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

  {EOpts, SO, EO} = expand(Opts, elixir_env:reset_unused_vars(S), E),
  {ECases, SC, EC} = mapfold(fun expand_for/3, SO, EO, Cases),
  assert_generator_start(Meta, ECases, E),

  {EExpr, SE, EE} =
    case validate_for_options(EOpts, false, false, false) of
      {ok, MaybeReduce} -> expand_for_do_block(Meta, Expr, SC, EC, MaybeReduce);
      {error, Error} -> form_error(Meta, E, ?MODULE, Error)
    end,

  {{for, Meta, ECases ++ [[{do, EExpr} | EOpts]]},
   elixir_env:merge_and_check_unused_vars(SE, S, EE),
   E};

%% With

expand({with, Meta, [_ | _] = Args}, S, E) ->
  assert_no_match_or_guard_scope(Meta, "with", S, E),
  elixir_clauses:with(Meta, Args, S, E);

%% Super

expand({super, Meta, Args}, S, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "super", S, E),
  {Kind, Name, _} = resolve_super(Meta, length(Args), E),
  {EArgs, SA, EA} = expand_args(Args, S, E),
  {{super, [{super, {Kind, Name}} | Meta], EArgs}, SA, EA};

%% Vars

expand({'^', Meta, [Arg]}, S, #{context := match} = E) ->
  #elixir_ex{prematch={Prematch, _}, vars={_Read, Write}} = S,

  %% We need to rollback to a no match context
  NoMatchE = E#{context := nil},
  NoMatchS = S#elixir_ex{prematch=pin, vars={Prematch, Write}},

  case expand(Arg, NoMatchS, NoMatchE) of
    {{Name, _, Kind} = Var, #elixir_ex{unused=Unused}, _} when is_atom(Name), is_atom(Kind) ->
      {{'^', Meta, [Var]}, S#elixir_ex{unused=Unused}, E};

    _ ->
      form_error(Meta, E, ?MODULE, {invalid_arg_for_pin, Arg})
  end;
expand({'^', Meta, [Arg]}, _S, E) ->
  form_error(Meta, E, ?MODULE, {pin_outside_of_match, Arg});

expand({'_', _Meta, Kind} = Var, S, #{context := match} = E) when is_atom(Kind) ->
  {Var, S, E};
expand({'_', Meta, Kind}, _S, E) when is_atom(Kind) ->
  form_error(Meta, E, ?MODULE, unbound_underscore);

expand({Name, Meta, Kind}, S, #{context := match} = E) when is_atom(Name), is_atom(Kind) ->
  #elixir_ex{
    prematch={_, PrematchVersion},
    unused={Unused, Version},
    vars={Read, Write}
  } = S,

  Pair = {Name, elixir_utils:var_context(Meta, Kind)},

  case Read of
    %% Variable was already overridden
    #{Pair := VarVersion} when VarVersion >= PrematchVersion ->
      maybe_warn_underscored_var_repeat(Meta, Name, Kind, E),
      NewUnused = var_used(Pair, VarVersion, Unused),
      Var = {Name, [{version, VarVersion} | Meta], Kind},
      {Var, S#elixir_ex{unused={NewUnused, Version}}, E};

    %% Variable is being overridden now
    #{Pair := _} ->
      NewUnused = var_unused(Pair, Meta, Version, Unused, true),
      NewRead = Read#{Pair => Version},
      NewWrite = (Write /= false) andalso Write#{Pair => Version},
      Var = {Name, [{version, Version} | Meta], Kind},
      {Var, S#elixir_ex{vars={NewRead, NewWrite}, unused={NewUnused, Version + 1}}, E};

    %% Variable defined for the first time
    _ ->
      NewUnused = var_unused(Pair, Meta, Version, Unused, false),
      NewRead = Read#{Pair => Version},
      NewWrite = (Write /= false) andalso Write#{Pair => Version},
      Var = {Name, [{version, Version} | Meta], Kind},
      {Var, S#elixir_ex{vars={NewRead, NewWrite}, unused={NewUnused, Version + 1}}, E}
  end;

expand({Name, Meta, Kind}, S, E) when is_atom(Name), is_atom(Kind) ->
  #elixir_ex{vars={Read, _Write}, unused={Unused, Version}, prematch=Prematch} = S,
  Pair = {Name, elixir_utils:var_context(Meta, Kind)},

  Result =
    case Read of
      #{Pair := CurrentVersion} ->
        case Prematch of
          {bitsize, Pre, Original} ->
            if
              is_map_key(Pair, Pre); not is_map_key(Pair, Original) -> {ok, CurrentVersion};
              true -> raise
            end;

          _ ->
            {ok, CurrentVersion}
        end;

      _ ->
        Prematch
    end,

  case Result of
    {ok, PairVersion} ->
      maybe_warn_underscored_var_access(Meta, Name, Kind, E),
      Var = {Name, [{version, PairVersion} | Meta], Kind},
      {Var, S#elixir_ex{unused={var_used(Pair, PairVersion, Unused), Version}}, E};

    Error ->
      case lists:keyfind(if_undefined, 1, Meta) of
        %% TODO: Remove this clause on v2.0 as we will always raise by default
        {if_undefined, raise} ->
          form_error(Meta, E, ?MODULE, {undefined_var, Name, Kind});

        {if_undefined, apply} ->
          expand({Name, Meta, []}, S, E);

        %% TODO: Remove this clause on v2.0
        _ when Error == warn ->
          elixir_errors:form_warn(Meta, E, ?MODULE, {unknown_variable, Name}),
          expand({Name, Meta, []}, S, E);

        _ when Error == pin ->
          form_error(Meta, E, ?MODULE, {undefined_var_pin, Name, Kind});

        _ ->
          form_error(Meta, E, ?MODULE, {undefined_var, Name, Kind})
      end
  end;

%% Local calls

expand({Atom, Meta, Args}, S, E) when is_atom(Atom), is_list(Meta), is_list(Args) ->
  assert_no_ambiguous_op(Atom, Meta, Args, S, E),

  elixir_dispatch:dispatch_import(Meta, Atom, Args, S, E, fun() ->
    expand_local(Meta, Atom, Args, S, E)
  end);

%% Remote calls

expand({{'.', DotMeta, [Left, Right]}, Meta, Args}, S, E)
    when (is_tuple(Left) orelse is_atom(Left)), is_atom(Right), is_list(Meta), is_list(Args) ->
  {ELeft, SL, EL} = expand(Left, elixir_env:prepare_write(S), E),

  elixir_dispatch:dispatch_require(Meta, ELeft, Right, Args, S, EL, fun(AR, AF, AA) ->
    expand_remote(AR, DotMeta, AF, Meta, AA, S, SL, EL)
  end);

%% Anonymous calls

expand({{'.', DotMeta, [Expr]}, Meta, Args}, S, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, "anonymous call", S, E),

  case expand_args([Expr | Args], S, E) of
    {[EExpr | _], _, _} when is_atom(EExpr) ->
      form_error(Meta, E, ?MODULE, {invalid_function_call, EExpr});

    {[EExpr | EArgs], SA, EA} ->
      {{{'.', DotMeta, [EExpr]}, Meta, EArgs}, SA, EA}
  end;

%% Invalid calls

expand({_, Meta, Args} = Invalid, _S, E) when is_list(Meta) and is_list(Args) ->
  form_error(Meta, E, ?MODULE, {invalid_call, Invalid});

%% Literals

expand({Left, Right}, S, E) ->
  {[ELeft, ERight], SE, EE} = expand_args([Left, Right], S, E),
  {{ELeft, ERight}, SE, EE};

expand(List, S, #{context := match} = E) when is_list(List) ->
  expand_list(List, fun expand/3, S, E, []);

expand(List, S, E) when is_list(List) ->
  {EArgs, {SE, _}, EE} =
    expand_list(List, fun expand_arg/3, {elixir_env:prepare_write(S), S}, E, []),

  {EArgs, elixir_env:close_write(SE, S), EE};

expand(Function, S, E) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
       (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {elixir_quote:fun_to_quoted(Function), S, E};
    false ->
      form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Function})
  end;

expand(Pid, S, E) when is_pid(Pid) ->
  case ?key(E, function) of
    nil ->
      {Pid, S, E};
    Function ->
      %% TODO: Make me an error on v2.0
      elixir_errors:form_warn([], E, ?MODULE, {invalid_pid_in_function, Pid, Function}),
      {Pid, E}
  end;

expand(Other, S, E) when is_number(Other); is_atom(Other); is_binary(Other) ->
  {Other, S, E};

expand(Other, _S, E) ->
  form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Other}).

%% Helpers

escape_env_entries(Meta, #elixir_ex{vars={Read, _}}, Env0) ->
  Env1 = case Env0 of
    #{function := nil} -> Env0;
    _ -> Env0#{lexical_tracker := nil, tracers := []}
  end,

  Env1#{versioned_vars := escape_map(Read), line := ?line(Meta)}.

escape_map(Map) -> {'%{}', [], maps:to_list(Map)}.

expand_multi_alias_call(Kind, Meta, Base, Refs, Opts, S, E) ->
  {BaseRef, SB, EB} = expand_without_aliases_report(Base, S, E),

  Fun = fun
    ({'__aliases__', _, Ref}, SR, ER) ->
      expand({Kind, Meta, [elixir_aliases:concat([BaseRef | Ref]), Opts]}, SR, ER);

    (Ref, SR, ER) when is_atom(Ref) ->
      expand({Kind, Meta, [elixir_aliases:concat([BaseRef, Ref]), Opts]}, SR, ER);

    (Other, _SR, _ER) ->
      form_error(Meta, E, ?MODULE, {expected_compile_time_module, Kind, Other})
  end,

  mapfold(Fun, SB, EB, Refs).

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

expand_fn_capture(Meta, Arg, S, E) ->
  case elixir_fn:capture(Meta, Arg, S, E) of
    {{remote, Remote, Fun, Arity}, RemoteMeta, SE, EE} ->
      is_atom(Remote) andalso
        elixir_env:trace({remote_function, RemoteMeta, Remote, Fun, Arity}, E),
      AttachedMeta = attach_context_module(Remote, Meta, E),
      {{'&', AttachedMeta, [{'/', [], [{{'.', [], [Remote, Fun]}, [], []}, Arity]}]}, SE, EE};
    {{local, Fun, Arity}, _LocalMeta, _SE, #{function := nil}} ->
      form_error(Meta, E, ?MODULE, {undefined_local_capture, Fun, Arity});
    {{local, Fun, Arity}, _LocalMeta, SE, EE} ->
      {{'&', Meta, [{'/', [], [{Fun, [], nil}, Arity]}]}, SE, EE};
    {expand, Expr, SE, EE} ->
      expand(Expr, SE, EE)
  end.

expand_list([{'|', Meta, [_, _] = Args}], Fun, S, E, List) ->
  {EArgs, SAcc, EAcc} = mapfold(Fun, S, E, Args),
  expand_list([], Fun, SAcc, EAcc, [{'|', Meta, EArgs} | List]);
expand_list([H | T], Fun, S, E, List) ->
  {EArg, SAcc, EAcc} = Fun(H, S, E),
  expand_list(T, Fun, SAcc, EAcc, [EArg | List]);
expand_list([], _Fun, S, E, List) ->
  {lists:reverse(List), S, E}.

expand_block([], Acc, _Meta, S, E) ->
  {lists:reverse(Acc), S, E};
expand_block([H], Acc, Meta, S, E) ->
  {EH, SE, EE} = expand(H, S, E),
  expand_block([], [EH | Acc], Meta, SE, EE);
expand_block([H | T], Acc, Meta, S, E) ->
  {EH, SE, EE} = expand(H, S, E),

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

  expand_block(T, [EH | Acc], Meta, SE, EE).

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
expand_arg(Arg, Acc, E) when is_number(Arg); is_atom(Arg); is_binary(Arg); is_pid(Arg) ->
  {Arg, Acc, E};
expand_arg(Arg, {Acc, S}, E) ->
  {EArg, SAcc, EAcc} = expand(Arg, elixir_env:reset_read(Acc, S), E),
  {EArg, {SAcc, S}, EAcc}.

expand_args([Arg], S, E) ->
  {EArg, SE, EE} = expand(Arg, S, E),
  {[EArg], SE, EE};
expand_args(Args, S, #{context := match} = E) ->
  mapfold(fun expand/3, S, E, Args);
expand_args(Args, S, E) ->
  {EArgs, {SA, _}, EA} = mapfold(fun expand_arg/3, {elixir_env:prepare_write(S), S}, E, Args),
  {EArgs, elixir_env:close_write(SA, S), EA}.

mapfold(Fun, S, E, List) ->
  mapfold(Fun, S, E, List, []).

mapfold(Fun, S, E, [H | T], Acc) ->
  {RH, RS, RE} = Fun(H, S, E),
  mapfold(Fun, RS, RE, T, [RH | Acc]);
mapfold(_Fun, S, E, [], Acc) ->
  {lists:reverse(Acc), S, E}.

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

expand_case(Meta, Expr, Opts, S, E) ->
  {EExpr, SE, EE} = expand(Expr, S, E),

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

  {EOpts, SO, EO} = elixir_clauses:'case'(Meta, ROpts, SE, EE),
  {{'case', Meta, [EExpr, EOpts]}, SO, EO}.

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

expand_for_do_block(Meta, [{'->', _, _} | _], _S, E, false) ->
  form_error(Meta, E, ?MODULE, for_without_reduce_bad_block);
expand_for_do_block(_Meta, Expr, S, E, false) ->
  expand(Expr, S, E);
expand_for_do_block(Meta, [{'->', _, _} | _] = Clauses, S, E, {reduce, _}) ->
  Transformer = fun
    ({_, _, [[_], _]} = Clause, SA) ->
      SReset = elixir_env:reset_unused_vars(SA),

      {EClause, SAcc, EAcc} =
        elixir_clauses:clause(Meta, fn, fun elixir_clauses:head/3, Clause, SReset, E),

      {EClause, elixir_env:merge_and_check_unused_vars(SAcc, SA, EAcc)};

    (_, _) ->
      form_error(Meta, E, ?MODULE, for_with_reduce_bad_block)
  end,

  {Do, SA} = lists:mapfoldl(Transformer, S, Clauses),
  {Do, SA, E};
expand_for_do_block(Meta, _Expr, _S, E, {reduce, _}) ->
  form_error(Meta, E, ?MODULE, for_with_reduce_bad_block).

%% Locals

assert_no_ambiguous_op(Name, Meta, [Arg], S, E) ->
  case lists:keyfind(ambiguous_op, 1, Meta) of
    {ambiguous_op, Kind} ->
      Pair = {Name, Kind},
      case S#elixir_ex.vars of
        {#{Pair := _}, _} ->
          form_error(Meta, E, ?MODULE, {op_ambiguity, Name, Arg});
        _ ->
          ok
      end;
    _ ->
      ok
  end;
assert_no_ambiguous_op(_Atom, _Meta, _Args, _S, _E) ->
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

expand_local(Meta, Name, Args, S, #{module := Module, function := Function, context := nil} = E)
    when Function /= nil ->
  assert_no_clauses(Name, Meta, Args, E),
  Arity = length(Args),
  elixir_env:trace({local_function, Meta, Name, Arity}, E),
  elixir_locals:record_local({Name, Arity}, Module, Function, Meta, false),
  {EArgs, SA, EA} = expand_args(Args, S, E),
  {{Name, Meta, EArgs}, SA, EA};
expand_local(Meta, Name, Args, _S, E) when Name == '|'; Name == '::' ->
  form_error(Meta, E, ?MODULE, {undefined_function, Name, Args});
expand_local(Meta, Name, Args, _S, #{function := nil} = E) ->
  form_error(Meta, E, ?MODULE, {undefined_function, Name, Args});
expand_local(Meta, Name, Args, _S, #{context := match} = E) ->
  form_error(Meta, E, ?MODULE, {invalid_local_invocation, match, {Name, Meta, Args}});
expand_local(Meta, Name, Args, S, #{context := guard} = E) ->
  form_error(Meta, E, ?MODULE, {invalid_local_invocation, guard_context(S), {Name, Meta, Args}}).

%% Remote

expand_remote(Receiver, DotMeta, Right, Meta, Args, S, SL, #{context := Context} = E) when is_atom(Receiver) or is_tuple(Receiver) ->
  assert_no_clauses(Right, Meta, Args, E),

  case {Context, lists:keyfind(no_parens, 1, Meta)} of
    {guard, {no_parens, true}} when is_tuple(Receiver) ->
      {{{'.', DotMeta, [Receiver, Right]}, Meta, []}, SL, E};

    {guard, _} when is_tuple(Receiver) ->
      form_error(Meta, E, ?MODULE, {parens_map_lookup, Receiver, Right, guard_context(S)});

    _ ->
      AttachedDotMeta = attach_context_module(Receiver, DotMeta, E),

      is_atom(Receiver) andalso
        elixir_env:trace({remote_function, Meta, Receiver, Right, length(Args)}, E),

      {EArgs, {SA, _}, EA} = mapfold(fun expand_arg/3, {SL, S}, E, Args),

      case rewrite(Context, Receiver, AttachedDotMeta, Right, Meta, EArgs, S) of
        {ok, Rewritten} ->
          maybe_warn_comparison(Rewritten, Args, E),
          {Rewritten, elixir_env:close_write(SA, S), EA};
        {error, Error} ->
          form_error(Meta, E, elixir_rewrite, Error)
      end
  end;
expand_remote(Receiver, DotMeta, Right, Meta, Args, _, _, E) ->
  Call = {{'.', DotMeta, [Receiver, Right]}, Meta, Args},
  form_error(Meta, E, ?MODULE, {invalid_call, Call}).

attach_context_module(_Receiver, Meta, #{function := nil}) ->
  Meta;
attach_context_module(Receiver, Meta, #{context_modules := [Ctx | _], module := Mod})
    %% If the context is the current module or
    %% if the receiver is the current module,
    %% then there is no context module.
    when Ctx == Mod; Receiver == Mod ->
  Meta;
attach_context_module(Receiver, Meta, #{context_modules := ContextModules}) ->
  case lists:member(Receiver, ContextModules) of
    true -> [{context_module, true} | Meta];
    false -> Meta
  end.

rewrite(match, Receiver, DotMeta, Right, Meta, EArgs, _S) ->
  elixir_rewrite:match_rewrite(Receiver, DotMeta, Right, Meta, EArgs);
rewrite(guard, Receiver, DotMeta, Right, Meta, EArgs, S) ->
  elixir_rewrite:guard_rewrite(Receiver, DotMeta, Right, Meta, EArgs, guard_context(S));
rewrite(_, Receiver, DotMeta, Right, Meta, EArgs, _S) ->
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

expand_opts(Meta, Kind, Allowed, Opts, S, E) ->
  {EOpts, SE, EE} = expand(Opts, S, E),
  validate_opts(Meta, Kind, Allowed, EOpts, EE),
  {EOpts, SE, EE}.

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

expand_without_aliases_report({'__aliases__', _, _} = Alias, S, E) ->
  expand_aliases(Alias, S, E, false);
expand_without_aliases_report(Other, S, E) ->
  expand(Other, S, E).

expand_aliases({'__aliases__', Meta, _} = Alias, S, E, Report) ->
  case elixir_aliases:expand_or_concat(Alias, E) of
    Receiver when is_atom(Receiver) ->
      Report andalso elixir_env:trace({alias_reference, Meta, Receiver}, E),
      {Receiver, S, E};

    Aliases ->
      {EAliases, SA, EA} = expand_args(Aliases, S, E),

      case lists:all(fun is_atom/1, EAliases) of
        true ->
          Receiver = elixir_aliases:concat(EAliases),
          Report andalso elixir_env:trace({alias_reference, Meta, Receiver}, E),
          {Receiver, SA, EA};

        false ->
          form_error(Meta, E, ?MODULE, {invalid_alias, Alias})
      end
  end.

%% Comprehensions

expand_for({'<-', Meta, [Left, Right]}, S, E) ->
  {ERight, SR, ER} = expand(Right, S, E),
  SM = elixir_env:reset_read(SR, S),
  {[ELeft], SL, EL} = elixir_clauses:head([Left], SM, ER),
  {{'<-', Meta, [ELeft, ERight]}, SL, EL};
expand_for({'<<>>', Meta, Args} = X, S, E) when is_list(Args) ->
  case elixir_utils:split_last(Args) of
    {LeftStart, {'<-', OpMeta, [LeftEnd, Right]}} ->
      {ERight, SR, ER} = expand(Right, S, E),
      SM = elixir_env:reset_read(SR, S),
      {ELeft, SL, EL} = elixir_clauses:match(fun(BArg, BS, BE) ->
        elixir_bitstring:expand(Meta, BArg, BS, BE, true)
      end, LeftStart ++ [LeftEnd], SM, SM, ER),
      {{'<<>>', Meta, [{'<-', OpMeta, [ELeft, ERight]}]}, SL, EL};
    _ ->
      expand(X, S, E)
  end;
expand_for(X, S, E) ->
  expand(X, S, E).

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

assert_no_match_or_guard_scope(Meta, Kind, S, E) ->
  assert_no_match_scope(Meta, Kind, E),
  assert_no_guard_scope(Meta, Kind, S, E).
assert_no_match_scope(Meta, Kind, #{context := match, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_pattern_in_match, Kind});
assert_no_match_scope(_Meta, _Kind, _E) -> [].
assert_no_guard_scope(Meta, Kind, S, #{context := guard, file := File}) ->
  Key =
    case S#elixir_ex.prematch of
      {bitsize, _, _}  -> invalid_expr_in_bitsize;
      _ -> invalid_expr_in_guard
    end,
  form_error(Meta, File, ?MODULE, {Key, Kind});
assert_no_guard_scope(_Meta, _Kind, _S, _E) -> [].

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

guard_context(#elixir_ex{prematch={bitsize, _, _}}) -> "bitstring size specifier";
guard_context(_) -> "guards".

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
format_error({commonly_mistaken_alias, Ref}) ->
  Module = 'Elixir.Macro':to_string(Ref),
  io_lib:format("reserved alias \"~ts\" expands to the atom :\"Elixir.~ts\". Perhaps you meant to write \"~ts\" instead?", [Module, Module, string:casefold(Module)]);
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
format_error(underscore_in_cond) ->
  "invalid use of _ inside \"cond\". If you want the last clause to always match, "
    "you probably meant to use: true ->";
format_error({invalid_pattern_in_match, Kind}) ->
  io_lib:format("invalid pattern in match, ~ts is not allowed in matches", [Kind]);
format_error({invalid_expr_in_scope, Scope, Kind}) ->
  io_lib:format("cannot invoke ~ts outside ~ts", [Kind, Scope]);
format_error({invalid_expr_in_guard, Kind}) ->
  Message =
    "invalid expression in guards, ~ts is not allowed in guards. To learn more about "
    "guards, visit: https://hexdocs.pm/elixir/patterns-and-guards.html",
  io_lib:format(Message, [Kind]);
format_error({invalid_expr_in_bitsize, Kind}) ->
  Message =
    "~ts is not allowed in bitstring size specifier. The size specifier in matches works like guards. "
    "To learn more about guards, visit: https://hexdocs.pm/elixir/patterns-and-guards.html",
  io_lib:format(Message, [Kind]);
format_error({invalid_alias, Expr}) ->
  Message =
    "invalid alias: \"~ts\". If you wanted to define an alias, an alias must expand "
    "to an atom at compile time but it did not, you may use Module.concat/2 to build "
    "it at runtime. If instead you wanted to invoke a function or access a field, "
    "wrap the function or field name in double quotes",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]);
format_error({op_ambiguity, Name, Arg}) ->
  NameString = atom_to_binary(Name),
  ArgString = 'Elixir.Macro':to_string(Arg),

  Message =
    "\"~ts ~ts\" looks like a function call but there is a variable named \"~ts\". "
    "If you want to perform a function call, use parentheses:\n"
    "\n"
    "    ~ts(~ts)\n"
    "\n"
    "If you want to perform an operation on the variable ~ts, use spaces "
    "around the unary operator",
  io_lib:format(Message, [NameString, ArgString, NameString, NameString, ArgString, NameString]);
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
format_error(caller_not_allowed) ->
  "__CALLER__ is available only inside defmacro and defmacrop";
format_error(stacktrace_not_allowed) ->
  "__STACKTRACE__ is available only inside catch and rescue clauses of try expressions";
format_error({unknown_variable, Name}) ->
  io_lib:format("variable \"~ts\" does not exist and is being expanded to \"~ts()\","
                " please use parentheses to remove the ambiguity or change the variable name", [Name, Name]);
format_error({parens_map_lookup, Map, Field, Context}) ->
  io_lib:format("cannot invoke remote function in ~ts. "
                "If you want to do a map lookup instead, please remove parens from ~ts.~ts()",
                [Context, 'Elixir.Macro':to_string(Map), Field]);
format_error({super_in_genserver, {Name, Arity}}) ->
  io_lib:format("calling super for GenServer callback ~ts/~B is deprecated", [Name, Arity]);
format_error({parallel_bitstring_match, Expr}) ->
  Message =
    "binary patterns cannot be matched in parallel using \"=\", excess pattern: ~ts",
  io_lib:format(Message, ['Elixir.Macro':to_string(Expr)]).
