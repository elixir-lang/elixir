-module(elixir_exp).
-export([expand/2, expand_args/2, expand_arg/2]).
-import(elixir_errors, [compile_error/3, compile_error/4]).
-include("elixir.hrl").

%% =

expand({'=', Meta, [Left, Right]}, E) ->
  assert_no_guard_scope(Meta, '=', E),
  {ERight, ER} = expand(Right, E),
  {ELeft, EL}  = elixir_exp_clauses:match(fun expand/2, Left, E),
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
  elixir_bitstring:expand(Meta, Args, E);

%% Other operators

expand({'__op__', Meta, [_, _] = Args}, E) ->
  {EArgs, EA} = expand_args(Args, E),
  {{'__op__', Meta, EArgs}, EA};

expand({'__op__', Meta, [_, _, _] = Args}, E) ->
  {EArgs, EA} = expand_args(Args, E),
  {{'__op__', Meta, EArgs}, EA};

expand({'->', Meta, _Args}, E) ->
  compile_error(Meta, ?m(E, file), "unhandled operator ->");

%% __block__

expand({'__block__', _Meta, []}, E) ->
  {nil, E};
expand({'__block__', _Meta, [Arg]}, E) ->
  expand(Arg, E);
expand({'__block__', Meta, Args}, E) when is_list(Args) ->
  {EArgs, EA} = expand_many(Args, E),
  {{'__block__', Meta, EArgs}, EA};

%% __aliases__

expand({'__aliases__', Meta, _} = Alias, E) ->
  case elixir_aliases:expand(Alias, ?m(E, aliases),
                             ?m(E, macro_aliases), ?m(E, lexical_tracker)) of
    Receiver when is_atom(Receiver) ->
      elixir_lexical:record_remote(Receiver, ?m(E, lexical_tracker)),
      {Receiver, E};
    Aliases ->
      {EAliases, EA} = expand_args(Aliases, E),

      case lists:all(fun is_atom/1, EAliases) of
        true ->
          Receiver = elixir_aliases:concat(EAliases),
          elixir_lexical:record_remote(Receiver, ?m(E, lexical_tracker)),
          {Receiver, EA};
        false ->
          compile_error(Meta, ?m(E, file), "an alias must expand to an atom "
            "at compilation time, but did not in \"~ts\". Use Module.concat/2 "
            "if you want to dynamically generate aliases", ['Elixir.Macro':to_string(Alias)])
      end
  end;

%% alias

expand({alias, Meta, [Ref]}, E) ->
  expand({alias, Meta, [Ref,[]]}, E);
expand({alias, Meta, [Ref, KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, alias, E),
  {ERef, ER} = expand(Ref, E),
  {EKV, ET}  = expand_opts(Meta, alias, [as, warn], no_alias_opts(KV), ER),

  if
    is_atom(ERef) ->
      {{alias, Meta, [ERef, EKV]},
        expand_alias(Meta, true, ERef, EKV, ET)};
    true ->
      compile_error(Meta, ?m(E, file),
        "invalid argument for alias, expected a compile time atom or alias, got: ~ts",
        ['Elixir.Kernel':inspect(ERef)])
  end;

expand({require, Meta, [Ref]}, E) ->
  expand({require, Meta, [Ref, []]}, E);
expand({require, Meta, [Ref, KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, require, E),

  {ERef, ER} = expand(Ref, E),
  {EKV, ET}  = expand_opts(Meta, require, [as, warn], no_alias_opts(KV), ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      {{require, Meta, [ERef, EKV]},
        expand_require(Meta, ERef, EKV, ET)};
    true ->
      compile_error(Meta, ?m(E, file),
        "invalid argument for require, expected a compile time atom or alias, got: ~ts",
        ['Elixir.Kernel':inspect(ERef)])
  end;

expand({import, Meta, [Left]}, E) ->
  expand({import, Meta, [Left, []]}, E);

expand({import, Meta, [Ref, KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, import, E),
  {ERef, ER} = expand(Ref, E),
  {EKV, ET}  = expand_opts(Meta, import, [only, except, warn], KV, ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      {Functions, Macros} = elixir_import:import(Meta, ERef, EKV, ET),
      {{import, Meta, [ERef, EKV]},
        expand_require(Meta, ERef, EKV, ET#{functions := Functions, macros := Macros})};
    true ->
      compile_error(Meta, ?m(E, file),
        "invalid argument for import, expected a compile time atom or alias, got: ~ts",
        ['Elixir.Kernel':inspect(ERef)])
  end;

%% Pseudo vars

expand({'__MODULE__', _, Atom}, E) when is_atom(Atom) ->
  {?m(E, module), E};
expand({'__DIR__', _, Atom}, E) when is_atom(Atom) ->
  {filename:dirname(?m(E, file)), E};
expand({'__CALLER__', _, Atom} = Caller, E) when is_atom(Atom) ->
  {Caller, E};
expand({'__ENV__', Meta, Atom}, E) when is_atom(Atom) ->
  Env = elixir_env:linify({?line(Meta), E}),
  {{'%{}', [], maps:to_list(Env)}, E};
expand({{'.', DotMeta, [{'__ENV__', Meta, Atom}, Field]}, CallMeta, []}, E) when is_atom(Atom), is_atom(Field) ->
  Env = elixir_env:linify({?line(Meta), E}),
  case maps:is_key(Field, Env) of
    true  -> {maps:get(Field, Env), E};
    false -> {{{'.', DotMeta, [{'%{}', [], maps:to_list(Env)}, Field]}, CallMeta, []}, E}
  end;

%% Quote

expand({Unquote, Meta, [_]}, E) when Unquote == unquote; Unquote == unquote_splicing ->
  compile_error(Meta, ?m(E, file), "~p called outside quote", [Unquote]);

expand({quote, Meta, [Opts]}, E) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    {do, Do} ->
      expand({quote, Meta, [lists:keydelete(do, 1, Opts), [{do,Do}]]}, E);
    false ->
      compile_error(Meta, ?m(E, file), "missing do keyword in quote")
  end;

expand({quote, Meta, [_]}, E) ->
  compile_error(Meta, ?m(E, file), "invalid arguments for quote");

expand({quote, Meta, [KV, Do]}, E) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      {do, Expr} -> Expr;
      false -> compile_error(Meta, E#elixir_scope.file, "missing do keyword in quote")
    end,

  ValidOpts = [context, location, line, file, unquote, bind_quoted],
  {EKV, ET} = expand_opts(Meta, quote, ValidOpts, KV, E),

  Context = case lists:keyfind(context, 1, EKV) of
    {context, Ctx} when is_atom(Ctx) and (Ctx /= nil) ->
      Ctx;
    {context, Ctx} ->
      compile_error(Meta, ?m(E, file), "invalid :context for quote, "
        "expected non nil compile time atom or alias, got: ~ts", ['Elixir.Kernel':inspect(Ctx)]);
    false ->
      case ?m(E, module) of
        nil -> 'Elixir';
        Mod -> Mod
      end
  end,

  {File, Line} = case lists:keyfind(location, 1, EKV) of
    {location, keep} ->
      {elixir_utils:relative_to_cwd(?m(E, file)), false};
    false ->
      { case lists:keyfind(file, 1, EKV) of
          {file, F} -> F;
          false -> nil
        end,

        case lists:keyfind(line, 1, EKV) of
          {line, L} -> L;
          false -> false
        end }
  end,

  {Binding, DefaultUnquote} = case lists:keyfind(bind_quoted, 1, EKV) of
    {bind_quoted, BQ} -> {BQ, false};
    false -> {nil, true}
  end,

  Unquote = case lists:keyfind(unquote, 1, EKV) of
    {unquote, U} when is_boolean(U) -> U;
    false -> DefaultUnquote
  end,

  Q = #elixir_quote{line=Line, file=File, unquote=Unquote, context=Context},

  {Quoted, _Q} = elixir_quote:quote(Exprs, Binding, Q, ET),
  expand(Quoted, ET);

expand({quote, Meta, [_, _]}, E) ->
  compile_error(Meta, ?m(E, file), "invalid arguments for quote");

%% Functions

expand({'&', _, [Arg]} = Original, E) when is_integer(Arg) ->
  {Original, E};
expand({'&', Meta, [Arg]}, E) ->
  assert_no_match_or_guard_scope(Meta, '&', E),
  case elixir_fn:capture(Meta, Arg, E) of
    {local, Fun, Arity} ->
      {{'&', Meta, [{'/', [], [{Fun, [], nil}, Arity]}]}, E};
    {expanded, Expr, EE} ->
      expand(Expr, EE)
  end;

expand({fn, Meta, Pairs}, E) ->
  assert_no_match_or_guard_scope(Meta, fn, E),
  elixir_fn:expand(Meta, Pairs, E);

%% Case/Receive/Try

expand({'cond', Meta, [KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, 'cond', E),
  {EClauses, EC} = elixir_exp_clauses:'cond'(Meta, KV, E),
  {{'cond', Meta, [EClauses]}, EC};

expand({'case', Meta, [Expr, KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, 'case', E),
  {EExpr, EE} = expand(Expr, E),
  {EClauses, EC} = elixir_exp_clauses:'case'(Meta, KV, EE),
  FClauses =
    case (lists:keyfind(optimize_boolean, 1, Meta) == {optimize_boolean, true}) and
         elixir_utils:returns_boolean(EExpr) of
      true  -> rewrite_case_clauses(EClauses);
      false -> EClauses
    end,
  {{'case', Meta, [EExpr, FClauses]}, EC};

expand({'receive', Meta, [KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, 'receive', E),
  {EClauses, EC} = elixir_exp_clauses:'receive'(Meta, KV, E),
  {{'receive', Meta, [EClauses]}, EC};

expand({'try', Meta, [KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, 'try', E),
  {EClauses, EC} = elixir_exp_clauses:'try'(Meta, KV, E),
  {{'try', Meta, [EClauses]}, EC};

%% Comprehensions

expand({for, Meta, [_|_] = Args}, E) ->
  elixir_for:expand(Meta, Args, E);

%% Super

expand({super, Meta, Args}, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, super, E),
  {EArgs, EA} = expand_args(Args, E),
  {{super, Meta, EArgs}, EA};

%% Vars

expand({'^', Meta, [Arg]}, #{context := match} = E) ->
  case expand(Arg, E) of
    {{Name, _, Kind} = EArg, EA} when is_atom(Name), is_atom(Kind) ->
      {{'^', Meta, [EArg]}, EA};
    _ ->
    Msg = "invalid argument for unary operator ^, expected an existing variable, got: ^~ts",
    compile_error(Meta, ?m(E, file), Msg, ['Elixir.Macro':to_string(Arg)])
  end;
expand({'^', Meta, [Arg]}, E) ->
  compile_error(Meta, ?m(E, file),
    "cannot use ^~ts outside of match clauses", ['Elixir.Macro':to_string(Arg)]);

expand({'_', _, Kind} = Var, E) when is_atom(Kind) ->
  {Var, E};
expand({Name, Meta, Kind} = Var, #{context := match, export_vars := Export} = E) when is_atom(Name), is_atom(Kind) ->
  Pair      = {Name, var_kind(Meta, Kind)},
  NewVars   = ordsets:add_element(Pair, ?m(E, vars)),
  NewExport = case (Export /= nil) of
    true  -> ordsets:add_element(Pair, Export);
    false -> Export
  end,
  {Var, E#{vars := NewVars, export_vars := NewExport}};
expand({Name, Meta, Kind} = Var, #{vars := Vars} = E) when is_atom(Name), is_atom(Kind) ->
  case lists:member({Name, var_kind(Meta, Kind)}, Vars) of
    true ->
      {Var, E};
    false ->
      VarMeta = lists:keyfind(var, 1, Meta),
      if
        VarMeta == {var, true} ->
          compile_error(Meta, ?m(E, file), "expected var \"~ts\"~ts to expand to an existing variable "
                        "or be part of a match", [Name, elixir_scope:context_info(Kind)]);
        true ->
          expand({Name, Meta, []}, E)
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
  {EExpr, EE} = expand(Expr, E),
  if
    is_atom(EExpr) ->
      compile_error(Meta, ?m(E, file), "invalid function call :~ts.()", [EExpr]);
    true ->
      {EArgs, EA} = expand_args(Args, elixir_env:mergea(E, EE)),
      {{{'.', DotMeta, [EExpr]}, Meta, EArgs}, elixir_env:mergev(EE, EA)}
  end;

%% Invalid calls

expand({_, Meta, Args} = Invalid, E) when is_list(Meta) and is_list(Args) ->
  compile_error(Meta, ?m(E, file), "invalid call ~ts",
    ['Elixir.Macro':to_string(Invalid)]);

expand({_, _, _} = Tuple, E) ->
  compile_error([{line,0}], ?m(E, file), "invalid quoted expression: ~ts",
    ['Elixir.Kernel':inspect(Tuple, [])]);

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
      compile_error([{line,0}], ?m(E, file),
        "invalid quoted expression: ~ts", ['Elixir.Kernel':inspect(Function)])
  end;

expand(Other, E) when is_number(Other); is_atom(Other); is_binary(Other); is_pid(Other) ->
  {Other, E};

expand(Other, E) ->
  compile_error([{line,0}], ?m(E, file),
    "invalid quoted expression: ~ts", ['Elixir.Kernel':inspect(Other)]).

%% Helpers

expand_list([{'|', Meta, [_, _] = Args}], Fun, Acc, List) ->
  {EArgs, EAcc} = lists:mapfoldl(Fun, Acc, Args),
  expand_list([], Fun, EAcc, [{'|', Meta, EArgs}|List]);
expand_list([H|T], Fun, Acc, List) ->
  {EArg, EAcc} = Fun(H, Acc),
  expand_list(T, Fun, EAcc, [EArg|List]);
expand_list([], _Fun, Acc, List) ->
  {lists:reverse(List), Acc}.

expand_many(Args, E) ->
  lists:mapfoldl(fun expand/2, E, Args).

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
  expand_many(Args, E);
expand_args(Args, E) ->
  {EArgs, {EC, EV}} = lists:mapfoldl(fun expand_arg/2, {E, E}, Args),
  {EArgs, elixir_env:mergea(EV, EC)}.

%% Match/var helpers

var_kind(Meta, Kind) ->
  case lists:keyfind(counter, 1, Meta) of
    {counter, Counter} -> Counter;
    false -> Kind
  end.

%% Locals

assert_no_ambiguous_op(Name, Meta, [Arg], E) ->
  case lists:keyfind(ambiguous_op, 1, Meta) of
    {ambiguous_op, Kind} ->
      case lists:member({Name, Kind}, ?m(E, vars)) of
        true ->
          compile_error(Meta, ?m(E, file), "\"~ts ~ts\" looks like a function call but "
                        "there is a variable named \"~ts\", please use explicit parenthesis or even spaces",
                        [Name, 'Elixir.Macro':to_string(Arg), Name]);
        false ->
          ok
      end;
    _ ->
      ok
  end;
assert_no_ambiguous_op(_Atom, _Meta, _Args, _E) ->
  ok.

expand_local(Meta, Name, Args, #{function := nil} = E) ->
  compile_error(Meta, ?m(E, file), "undefined function ~ts/~B", [Name, length(Args)]);
expand_local(Meta, Name, Args, #{module := Module, function := Function} = E) ->
  elixir_locals:record_local({Name, length(Args)}, Module, Function),
  {EArgs, EA} = expand_args(Args, E),
  {{Name, Meta, EArgs}, EA}.

%% Remote

expand_remote(Receiver, DotMeta, Right, Meta, Args, E, EL) ->
  if
    is_atom(Receiver) -> elixir_lexical:record_remote(Receiver, ?m(E, lexical_tracker));
    true -> ok
  end,
  {EArgs, EA} = expand_args(Args, E),
  {elixir_rewrite:rewrite(Receiver, DotMeta, Right, Meta, EArgs),
   elixir_env:mergev(EL, EA)}.

%% Lexical helpers

expand_opts(Meta, Kind, Allowed, Opts, E) ->
  {EOpts, EE} = expand(Opts, E),
  validate_opts(Meta, Kind, Allowed, EOpts, EE),
  {EOpts, EE}.

validate_opts(Meta, Kind, Allowed, Opts, E) when is_list(Opts) ->
  [begin
    compile_error(Meta, ?m(E, file),
                  "unsupported option ~ts given to ~s", ['Elixir.Kernel':inspect(Key), Kind])
  end || {Key, _} <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, _Opts, E) ->
  compile_error(Meta, ?m(E, file), "invalid options for ~s, expected a keyword list", [Kind]).

no_alias_opts(KV) when is_list(KV) ->
  case lists:keyfind(as, 1, KV) of
    {as, As} -> lists:keystore(as, 1, KV, {as, no_alias_expansion(As)});
    false -> KV
  end;
no_alias_opts(KV) -> KV.

no_alias_expansion({'__aliases__', Meta, [H|T]}) when (H /= 'Elixir') and is_atom(H) ->
  {'__aliases__', Meta, ['Elixir',H|T]};
no_alias_expansion(Other) ->
  Other.

expand_require(Meta, Ref, KV, E) ->
  RE = E#{requires := ordsets:add_element(Ref, ?m(E, requires))},
  expand_alias(Meta, false, Ref, KV, RE).

expand_alias(Meta, IncludeByDefault, Ref, KV, #{context_modules := Context} = E) ->
  New = expand_as(lists:keyfind(as, 1, KV), Meta, IncludeByDefault, Ref, E),

  %% Add the alias to context_modules if defined is true.
  %% This is used by defmodule in order to store the defined
  %% module in context modules.
  NewContext =
    case lists:keyfind(defined, 1, Meta) of
      {defined, Mod} when is_atom(Mod) -> [Mod|Context];
      false -> Context
    end,

  {Aliases, MacroAliases} = elixir_aliases:store(Meta, New, Ref, KV, ?m(E, aliases),
                                ?m(E, macro_aliases), ?m(E, lexical_tracker)),

  E#{aliases := Aliases, macro_aliases := MacroAliases, context_modules := NewContext}.

expand_as({as, true}, _Meta, _IncludeByDefault, Ref, _E) ->
  elixir_aliases:last(Ref);
expand_as({as, false}, _Meta, _IncludeByDefault, Ref, _E) ->
  Ref;
expand_as({as, Atom}, Meta, _IncludeByDefault, _Ref, E) when is_atom(Atom) ->
  case length(string:tokens(atom_to_list(Atom), ".")) of
    1 -> compile_error(Meta, ?m(E, file),
           "invalid value for keyword :as, expected an alias, got atom: ~ts", [elixir_aliases:inspect(Atom)]);
    2 -> Atom;
    _ -> compile_error(Meta, ?m(E, file),
           "invalid value for keyword :as, expected an alias, got nested alias: ~ts", [elixir_aliases:inspect(Atom)])
  end;
expand_as(false, _Meta, IncludeByDefault, Ref, _E) ->
  if IncludeByDefault -> elixir_aliases:last(Ref);
     true -> Ref
  end;
expand_as({as, Other}, Meta, _IncludeByDefault, _Ref, E) ->
  compile_error(Meta, ?m(E, file),
    "invalid value for keyword :as, expected an alias, got: ~ts", ['Elixir.Macro':to_string(Other)]).

%% Assertions

rewrite_case_clauses([{do,[
  {'->', FalseMeta, [
    [{'when', _, [Var, {'__op__', _,[
      'orelse',
      {{'.', _, [erlang, '=:=']}, _, [Var, nil]},
      {{'.', _, [erlang, '=:=']}, _, [Var, false]}
    ]}]}],
    FalseExpr
  ]},
  {'->', TrueMeta, [
    [{'_', _, _}],
    TrueExpr
  ]}
]}]) ->
  [{do, [
    {'->', FalseMeta, [[false], FalseExpr]},
    {'->', TrueMeta, [[true], TrueExpr]}
  ]}];
rewrite_case_clauses(Clauses) ->
  Clauses.

assert_no_match_or_guard_scope(Meta, Kind, E) ->
  assert_no_match_scope(Meta, Kind, E),
  assert_no_guard_scope(Meta, Kind, E).
assert_no_match_scope(Meta, _Kind, #{context := match, file := File}) ->
  compile_error(Meta, File, "invalid expression in match");
assert_no_match_scope(_Meta, _Kind, _E) -> [].
assert_no_guard_scope(Meta, _Kind, #{context := guard, file := File}) ->
  compile_error(Meta, File, "invalid expression in guard");
assert_no_guard_scope(_Meta, _Kind, _E) -> [].
