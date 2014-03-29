-module(elixir_exp).
-export([expand/2, expand_args/2, expand_arg/2]).
-import(elixir_errors, [compile_error/3, compile_error/4]).
-include("elixir.hrl").

%% =

expand({ '=', Meta, [Left, Right] }, E) ->
  assert_no_guard_scope(Meta, '=', E),
  { ERight, ER } = expand(Right, E),
  { ELeft, EL }  = elixir_exp_clauses:match(fun expand/2, Left, E),
  { { '=', Meta, [ELeft, ERight] }, elixir_env:mergev(EL, ER) };

%% Literal operators

expand({ '{}', Meta, Args }, E) ->
  { EArgs, EA } = expand_args(Args, E),
  { { '{}', Meta, EArgs }, EA };

expand({ '%{}', Meta, Args }, E) ->
  elixir_map:expand_map(Meta, Args, E);

expand({ '%', Meta, [Left, Right] }, E) ->
  elixir_map:expand_struct(Meta, Left, Right, E);

expand({ '<<>>', Meta, Args }, E) ->
  elixir_bitstring:expand(Meta, Args, E);

%% Other operators

expand({ '__op__', Meta, [_, _] = Args }, E) ->
  { EArgs, EA } = expand_args(Args, E),
  { { '__op__', Meta, EArgs }, EA };

expand({ '__op__', Meta, [_, _, _] = Args }, E) ->
  { EArgs, EA } = expand_args(Args, E),
  { { '__op__', Meta, EArgs }, EA };

expand({ '->', Meta, _Args }, E) ->
  compile_error(Meta, E#elixir_env.file, "unhandled operator ->");

%% __block__

expand({ '__block__', _Meta, [] }, E) ->
  { nil, E };
expand({ '__block__', _Meta, [Arg] }, E) ->
  expand(Arg, E);
expand({ '__block__', Meta, Args }, E) when is_list(Args) ->
  { EArgs, EA } = expand_many(Args, E),
  { { '__block__', Meta, EArgs }, EA };

%% __aliases__

expand({ '__aliases__', _, _ } = Alias, E) ->
  case elixir_aliases:expand(Alias, E#elixir_env.aliases,
                             E#elixir_env.macro_aliases, E#elixir_env.lexical_tracker) of
    Receiver when is_atom(Receiver) ->
      elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
      { Receiver, E };
    Aliases ->
      { EAliases, EA } = expand_args(Aliases, E),

      case lists:all(fun is_atom/1, EAliases) of
        true ->
          Receiver = elixir_aliases:concat(EAliases),
          elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker),
          { Receiver, EA };
        false ->
          { { { '.', [], [elixir_aliases, concat] }, [], [EAliases] }, EA }
      end
  end;

%% alias

expand({ alias, Meta, [Ref] }, E) ->
  expand({ alias, Meta, [Ref,[]] }, E);
expand({ alias, Meta, [Ref, KV] }, E) ->
  assert_no_match_or_guard_scope(Meta, alias, E),
  { ERef, ER } = expand(Ref, E),
  { EKV, ET }  = expand_opts(Meta, alias, [as, warn], no_alias_opts(KV), ER),

  if
    is_atom(ERef) ->
      { { alias, Meta, [ERef, EKV] },
        expand_alias(Meta, true, ERef, EKV, ET) };
    true ->
      compile_error(Meta, E#elixir_env.file,
        "invalid argument for alias, expected a compile time atom or alias, got: ~ts",
        ['Elixir.Kernel':inspect(ERef)])
  end;

expand({ require, Meta, [Ref] }, E) ->
  expand({ require, Meta, [Ref, []] }, E);
expand({ require, Meta, [Ref, KV] }, E) ->
  assert_no_match_or_guard_scope(Meta, require, E),

  { ERef, ER } = expand(Ref, E),
  { EKV, ET }  = expand_opts(Meta, require, [as, warn], no_alias_opts(KV), ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      { { require, Meta, [ERef, EKV] },
        expand_require(Meta, ERef, EKV, ET) };
    true ->
      compile_error(Meta, E#elixir_env.file,
        "invalid argument for require, expected a compile time atom or alias, got: ~ts",
        ['Elixir.Kernel':inspect(ERef)])
  end;

expand({ import, Meta, [Left] }, E) ->
  expand({ import, Meta, [Left, []]}, E);

expand({ import, Meta, [Ref, KV] }, E) ->
  assert_no_match_or_guard_scope(Meta, import, E),
  { ERef, ER } = expand(Ref, E),
  { EKV, ET }  = expand_opts(Meta, import, [only, except, warn], KV, ER),

  if
    is_atom(ERef) ->
      elixir_aliases:ensure_loaded(Meta, ERef, ET),
      { Functions, Macros } = elixir_import:import(Meta, ERef, EKV, ET),
      { { import, Meta, [ERef, EKV] },
        expand_require(Meta, ERef, EKV, ET#elixir_env{functions=Functions, macros=Macros}) };
    true ->
      compile_error(Meta, E#elixir_env.file,
        "invalid argument for import, expected a compile time atom or alias, got: ~ts",
        ['Elixir.Kernel':inspect(ERef)])
  end;

%% Pseudo vars

expand({ '__MODULE__', _, Atom }, E) when is_atom(Atom) ->
  { E#elixir_env.module, E };
expand({ '__DIR__', _, Atom }, E) when is_atom(Atom) ->
  { filename:dirname(E#elixir_env.file), E };
expand({ '__CALLER__', _, Atom } = Caller, E) when is_atom(Atom) ->
  { Caller, E };
expand({ '__ENV__', Meta, Atom }, E) when is_atom(Atom) ->
  Env = elixir_env:env_to_ex({ ?line(Meta), E }),
  { { '{}', [], tuple_to_list(Env) }, E };
expand({ { '.', DotMeta, [{ '__ENV__', Meta, Atom }, Field] }, CallMeta, [] }, E) when is_atom(Atom), is_atom(Field) ->
  Env = elixir_env:env_to_ex({ ?line(Meta), E }),
  case erlang:function_exported('Elixir.Macro.Env', Field, 1) of
    true  -> { Env:Field(), E };
    false -> { { { '.', DotMeta, [{ '{}', [], tuple_to_list(Env) }, Field] }, CallMeta, [] }, E }
  end;

%% Quote

expand({ Unquote, Meta, [_] }, E) when Unquote == unquote; Unquote == unquote_splicing ->
  compile_error(Meta, E#elixir_env.file, "~p called outside quote", [Unquote]);

expand({ quote, Meta, [Opts] }, E) when is_list(Opts) ->
  case lists:keyfind(do, 1, Opts) of
    { do, Do } ->
      expand({ quote, Meta, [lists:keydelete(do, 1, Opts), [{do,Do}]] }, E);
    false ->
      compile_error(Meta, E#elixir_env.file, "missing do keyword in quote")
  end;

expand({ quote, Meta, [_] }, E) ->
  compile_error(Meta, E#elixir_env.file, "invalid arguments for quote");

expand({ quote, Meta, [KV, Do] }, E) when is_list(Do) ->
  Exprs =
    case lists:keyfind(do, 1, Do) of
      { do, Expr } -> Expr;
      false -> compile_error(Meta, E#elixir_scope.file, "missing do keyword in quote")
    end,

  ValidOpts   = [hygiene, context, var_context, location, line, unquote, bind_quoted],
  { EKV, ET } = expand_opts(Meta, quote, ValidOpts, KV, E),

  Hygiene = case lists:keyfind(hygiene, 1, EKV) of
    { hygiene, List } when is_list(List) ->
      List;
    false ->
      []
  end,

  Context = case lists:keyfind(context, 1, EKV) of
    { context, Atom } when is_atom(Atom) ->
      Atom;
    { context, Ctx } ->
      compile_error(Meta, E#elixir_env.file, "invalid :context for quote, "
        "expected a compile time atom or alias, got: ~ts", ['Elixir.Kernel':inspect(Ctx)]);
    false ->
      case E#elixir_env.module of
        nil -> 'Elixir';
        Mod -> Mod
      end
  end,

  Vars    = lists:keyfind(vars, 1, Hygiene) /= { vars, false },
  Aliases = lists:keyfind(aliases, 1, Hygiene) /= { aliases, false },
  Imports = lists:keyfind(imports, 1, Hygiene) /= { imports, false },

  Keep = lists:keyfind(location, 1, EKV) == { location, keep },
  Line = proplists:get_value(line, EKV, false),

  { Binding, DefaultUnquote } = case lists:keyfind(bind_quoted, 1, EKV) of
    { bind_quoted, BQ } -> { BQ, false };
    false -> { nil, true }
  end,

  Unquote = case lists:keyfind(unquote, 1, EKV) of
    { unquote, Bool } when is_boolean(Bool) -> Bool;
    false -> DefaultUnquote
  end,

  Q = #elixir_quote{vars_hygiene=Vars, line=Line, keep=Keep, unquote=Unquote,
        aliases_hygiene=Aliases, imports_hygiene=Imports, context=Context},

  { Quoted, _Q } = elixir_quote:quote(Exprs, Binding, Q, ET),
  expand(Quoted, ET);

expand({ quote, Meta, [_, _] }, E) ->
  compile_error(Meta, E#elixir_env.file, "invalid arguments for quote");

%% Functions

expand({ '&', _, [Arg] } = Original, E) when is_integer(Arg) ->
  { Original, E };
expand({ '&', Meta, [Arg] }, E) ->
  assert_no_match_or_guard_scope(Meta, '&', E),
  case elixir_fn:capture(Meta, Arg, E) of
    { local, Fun, Arity } ->
      { { '&', Meta, [{ '/', [], [{ Fun, [], nil }, Arity] }] }, E };
    { expanded, Expr, EE } ->
      expand(Expr, EE)
  end;

expand({ fn, Meta, Pairs }, E) ->
  assert_no_match_or_guard_scope(Meta, fn, E),
  elixir_fn:expand(Meta, Pairs, E);

%% Case/Receive/Try

expand({'case', Meta, [Expr, KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, 'case', E),
  { EExpr, EE } = expand(Expr, E),
  { EClauses, EC } = elixir_exp_clauses:'case'(Meta, KV, EE),
  { { 'case', Meta, [EExpr, EClauses] }, EC };

expand({'receive', Meta, [KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, 'receive', E),
  { EClauses, EC } = elixir_exp_clauses:'receive'(Meta, KV, E),
  { { 'receive', Meta, [EClauses] }, EC };

expand({'try', Meta, [KV]}, E) ->
  assert_no_match_or_guard_scope(Meta, 'try', E),
  { EClauses, EC } = elixir_exp_clauses:'try'(Meta, KV, E),
  { { 'try', Meta, [EClauses] }, EC };

%% Comprehensions

expand({ Kind, Meta, Args }, E) when is_list(Args), (Kind == lc) orelse (Kind == bc) ->
  expand_comprehension(Meta, Kind, Args, E);

expand({ for, Meta, Args }, E) when is_list(Args) ->
  elixir_for:expand(Meta, Args, E);

%% Super

expand({ super, Meta, Args }, E) when is_list(Args) ->
  assert_no_match_or_guard_scope(Meta, super, E),
  { EArgs, EA } = expand_args(Args, E),
  { { super, Meta, EArgs }, EA };

%% Vars

expand({ '^', Meta, [Arg] }, #elixir_env{context=match} = E) ->
  case expand(Arg, E) of
    { { Name, _, Kind } = EArg, EA } when is_atom(Name), is_atom(Kind) ->
      { { '^', Meta, [EArg] }, EA };
    _ ->
    Msg = "invalid argument for unary operator ^, expected an existing variable, got: ^~ts",
    compile_error(Meta, E#elixir_env.file, Msg, ['Elixir.Macro':to_string(Arg)])
  end;
expand({ '^', Meta, [Arg] }, E) ->
  compile_error(Meta, E#elixir_env.file,
    "cannot use ^~ts outside of match clauses", ['Elixir.Macro':to_string(Arg)]);

expand({ '_', _, Kind } = Var, E) when is_atom(Kind) ->
  { Var, E };
expand({ Name, Meta, Kind } = Var, #elixir_env{context=match, export_vars=Export} = E) when is_atom(Name), is_atom(Kind) ->
  Pair      = { Name, var_kind(Meta, Kind) },
  NewVars   = ordsets:add_element(Pair, E#elixir_env.vars),
  NewExport = case (Export /= nil) andalso (lists:keyfind(export, 1, Meta) /= { export, false }) of
    true  -> ordsets:add_element(Pair, Export);
    false -> Export
  end,
  { Var, E#elixir_env{vars=NewVars, export_vars=NewExport} };
expand({ Name, Meta, Kind } = Var, #elixir_env{vars=Vars} = E) when is_atom(Name), is_atom(Kind) ->
  case lists:member({ Name, var_kind(Meta, Kind) }, Vars) of
    true ->
      { Var, E };
    false ->
      VarMeta = lists:keyfind(var, 1, Meta),
      if
        VarMeta == { var, true } ->
          Extra = case Kind of
            nil -> "";
            _   -> io_lib:format(" (context ~ts)", [elixir_aliases:inspect(Kind)])
          end,

          compile_error(Meta, E#elixir_env.file, "expected var ~ts~ts to expand to an existing "
                        "variable or be a part of a match", [Name, Extra]);
        true ->
          expand({ Name, Meta, [] }, E)
      end
  end;

%% Local calls

expand({ Atom, Meta, Args }, E) when is_atom(Atom), is_list(Meta), is_list(Args) ->
  assert_no_ambiguous_op(Atom, Meta, Args, E),

  elixir_dispatch:dispatch_import(Meta, Atom, Args, E, fun() ->
    expand_local(Meta, Atom, Args, E)
  end);

%% Remote calls

expand({ { '.', DotMeta, [Left, Right] }, Meta, Args }, E)
    when (is_tuple(Left) orelse is_atom(Left)), is_atom(Right), is_list(Meta), is_list(Args) ->
  { ELeft, EL } = expand(Left, E),

  elixir_dispatch:dispatch_require(Meta, ELeft, Right, Args, EL, fun(AR, AF, AA) ->
    expand_remote(AR, DotMeta, AF, Meta, AA, E, EL)
  end);

%% Anonymous calls

expand({ { '.', DotMeta, [Expr] }, Meta, Args }, E) when is_list(Args) ->
  { EExpr, EE } = expand(Expr, E),
  if
    is_atom(EExpr) ->
      compile_error(Meta, E#elixir_env.file, "invalid function call :~ts.()", [EExpr]);
    true ->
      { EArgs, EA } = expand_args(Args, elixir_env:mergea(E, EE)),
      { { { '.', DotMeta, [EExpr] }, Meta, EArgs }, elixir_env:mergev(EE, EA) }
  end;

%% Invalid calls

expand({ _, Meta, Args } = Invalid, E) when is_list(Meta) and is_list(Args) ->
  compile_error(Meta, E#elixir_env.file, "invalid call ~ts",
    ['Elixir.Macro':to_string(Invalid)]);

expand({ _, _, _ } = Tuple, E) ->
  compile_error([{line,0}], E#elixir_env.file, "invalid quoted expression: ~ts",
    ['Elixir.Kernel':inspect(Tuple, [{records,false}])]);

%% Literals

expand({ Left, Right }, E) ->
  { [ELeft, ERight], EE } = expand_args([Left, Right], E),
  { { ELeft, ERight }, EE };

expand(List, #elixir_env{context=match} = E) when is_list(List) ->
  expand_list(List, fun expand/2, E, []);

expand(List, E) when is_list(List) ->
  { EArgs, { EC, EV } } = expand_list(List, fun expand_arg/2, {E, E}, []),
  { EArgs, elixir_env:mergea(EV, EC) };

expand(Function, E) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == { type, external }) andalso
       (erlang:fun_info(Function, env) == { env, [] }) of
    true ->
      { Function, E };
    false ->
      compile_error([{line,0}], E#elixir_env.file,
        "invalid quoted expression: ~ts", ['Elixir.Kernel':inspect(Function)])
  end;

expand(Other, E) when is_number(Other); is_atom(Other); is_binary(Other); is_pid(Other) ->
  { Other, E };

expand(Other, E) ->
  compile_error([{line,0}], E#elixir_env.file,
    "invalid quoted expression: ~ts", ['Elixir.Kernel':inspect(Other)]).

%% Helpers

expand_list([{ '|', Meta, [_, _] = Args }], Fun, Acc, List) ->
  { EArgs, EAcc } = lists:mapfoldl(Fun, Acc, Args),
  expand_list([], Fun, EAcc, [{ '|', Meta, EArgs }|List]);
expand_list([H|T], Fun, Acc, List) ->
  { EArg, EAcc } = Fun(H, Acc),
  expand_list(T, Fun, EAcc, [EArg|List]);
expand_list([], _Fun, Acc, List) ->
  { lists:reverse(List), Acc }.

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
  { Arg, Acc };
expand_arg(Arg, { Acc1, Acc2 }) ->
  { EArg, EAcc } = expand(Arg, Acc1),
  { EArg, { elixir_env:mergea(Acc1, EAcc), elixir_env:mergev(Acc2, EAcc) } }.

expand_args(Args, #elixir_env{context=match} = E) ->
  expand_many(Args, E);
expand_args(Args, E) ->
  { EArgs, { EC, EV } } = lists:mapfoldl(fun expand_arg/2, {E, E}, Args),
  { EArgs, elixir_env:mergea(EV, EC) }.

%% Match/var helpers

var_kind(Meta, Kind) ->
  case lists:keyfind(counter, 1, Meta) of
    { counter, Counter } -> Counter;
    false -> Kind
  end.

%% Locals

assert_no_ambiguous_op(Name, Meta, [Arg], E) ->
  case lists:keyfind(ambiguous_op, 1, Meta) of
    { ambiguous_op, Kind } ->
      case lists:member({ Name, Kind }, E#elixir_env.vars) of
        true ->
          compile_error(Meta, E#elixir_env.file, "\"~ts ~ts\" looks like a function call but "
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

expand_local(Meta, Name, Args, #elixir_env{local=nil, function=nil} = E) ->
  { EArgs, EA } = expand_args(Args, E),
  { { Name, Meta, EArgs }, EA };
expand_local(Meta, Name, Args, #elixir_env{local=nil, module=Module, function=Function} = E) ->
  elixir_locals:record_local({ Name, length(Args) }, Module, Function),
  { EArgs, EA } = expand_args(Args, E),
  { { Name, Meta, EArgs }, EA };
expand_local(Meta, Name, Args, E) ->
  expand({ { '.', Meta, [E#elixir_env.local, Name] }, Meta, Args }, E).

%% Remote

expand_remote(Receiver, DotMeta, Right, Meta, Args, E, EL) ->
  if
    is_atom(Receiver) -> elixir_lexical:record_remote(Receiver, E#elixir_env.lexical_tracker);
    true -> ok
  end,
  { EArgs, EA } = expand_args(Args, E),
  { { { '.', DotMeta, [Receiver, Right] }, Meta, EArgs }, elixir_env:mergev(EL, EA) }.

%% Lexical helpers

expand_opts(Meta, Kind, Allowed, Opts, E) ->
  { EOpts, EE } = expand(Opts, E),
  validate_opts(Meta, Kind, Allowed, EOpts, EE),
  { EOpts, EE }.

validate_opts(Meta, Kind, Allowed, Opts, E) when is_list(Opts) ->
  [begin
    compile_error(Meta, E#elixir_env.file,
                  "unsupported option ~ts given to ~s", ['Elixir.Kernel':inspect(Key), Kind])
  end || { Key, _ } <- Opts, not lists:member(Key, Allowed)];

validate_opts(Meta, Kind, _Allowed, _Opts, S) ->
  compile_error(Meta, S#elixir_scope.file, "invalid options for ~s, expected a keyword list", [Kind]).

no_alias_opts(KV) when is_list(KV) ->
  case lists:keyfind(as, 1, KV) of
    { as, As } -> lists:keystore(as, 1, KV, { as, no_alias_expansion(As) });
    false -> KV
  end;
no_alias_opts(KV) -> KV.

no_alias_expansion({ '__aliases__', Meta, [H|T] }) when (H /= 'Elixir') and is_atom(H) ->
  { '__aliases__', Meta, ['Elixir',H|T] };
no_alias_expansion(Other) ->
  Other.

expand_require(Meta, Ref, KV, E) ->
  RE = E#elixir_env{requires=ordsets:add_element(Ref, E#elixir_env.requires)},
  expand_alias(Meta, false, Ref, KV, RE).

expand_alias(Meta, IncludeByDefault, Ref, KV, #elixir_env{context_modules=Context} = E) ->
  New = expand_as(lists:keyfind(as, 1, KV), Meta, IncludeByDefault, Ref, E),

  %% Add the alias to context_modules if defined is true.
  %% This is used by defmodule in order to store the defined
  %% module in context modules.
  NewContext =
    case lists:keyfind(defined, 1, Meta) of
      { defined, Mod } when is_atom(Mod) -> [Mod|Context];
      false -> Context
    end,

  { Aliases, MacroAliases } = elixir_aliases:store(Meta, New, Ref, KV, E#elixir_env.aliases,
                                E#elixir_env.macro_aliases, E#elixir_env.lexical_tracker),

  E#elixir_env{aliases=Aliases, macro_aliases=MacroAliases, context_modules=NewContext}.

expand_as({ as, true }, _Meta, _IncludeByDefault, Ref, _E) ->
  elixir_aliases:last(Ref);
expand_as({ as, false }, _Meta, _IncludeByDefault, Ref, _E) ->
  Ref;
expand_as({ as, Atom }, Meta, _IncludeByDefault, _Ref, E) when is_atom(Atom) ->
  case length(string:tokens(atom_to_list(Atom), ".")) of
    1 -> compile_error(Meta, E#elixir_env.file,
           "invalid value for keyword :as, expected an alias, got atom: ~ts", [elixir_aliases:inspect(Atom)]);
    2 -> Atom;
    _ -> compile_error(Meta, E#elixir_env.file,
           "invalid value for keyword :as, expected an alias, got nested alias: ~ts", [elixir_aliases:inspect(Atom)])
  end;
expand_as(false, _Meta, IncludeByDefault, Ref, _E) ->
  if IncludeByDefault -> elixir_aliases:last(Ref);
     true -> Ref
  end;
expand_as({ as, Other }, Meta, _IncludeByDefault, _Ref, E) ->
  compile_error(Meta, E#elixir_env.file,
    "invalid value for keyword :as, expected an alias, got: ~ts", ['Elixir.Macro':to_string(Other)]).

%% Comprehensions

expand_comprehension(Meta, Kind, Args, E) ->
  case elixir_utils:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { ECases, EC } = lists:mapfoldl(fun expand_comprehension_clause/2, E, Cases),
      { EExpr, _ }   = expand(Expr, EC),
      { { Kind, Meta, ECases ++ [[{do,EExpr}]] }, E };
    _ ->
      compile_error(Meta, E#elixir_env.file, "missing do keyword in comprehension ~ts", [Kind])
  end.

expand_comprehension_clause({Gen, Meta, [Left, Right]}, E) when Gen == inbits; Gen == inlist ->
  { ERight, ER } = expand(Right, E),
  { ELeft, EL }  = elixir_exp_clauses:match(fun expand/2, Left, E),
  { { Gen, Meta, [ELeft, ERight] }, elixir_env:mergev(EL, ER) };
expand_comprehension_clause(X, E) ->
  expand(X, E).

%% Assertions

assert_no_match_or_guard_scope(Meta, Kind, E) ->
  assert_no_match_scope(Meta, Kind, E),
  assert_no_guard_scope(Meta, Kind, E).
assert_no_match_scope(Meta, _Kind, #elixir_env{context=match,file=File}) ->
  compile_error(Meta, File, "invalid expression in match");
assert_no_match_scope(_Meta, _Kind, _E) -> [].
assert_no_guard_scope(Meta, _Kind, #elixir_env{context=guard,file=File}) ->
  compile_error(Meta, File, "invalid expression in guard");
assert_no_guard_scope(_Meta, _Kind, _E) -> [].
