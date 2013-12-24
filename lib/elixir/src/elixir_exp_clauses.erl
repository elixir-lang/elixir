%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_exp_clauses).
-export([match/3]).
-export([expand_clause/5, expand_case/3]).
-import(elixir_errors, [compile_error/3, compile_error/4]).
-include("elixir.hrl").

match(Fun, Expr, #elixir_env{context=Context} = E) ->
  { EExpr, EE } = Fun(Expr, E#elixir_env{context=match}),
  { EExpr, EE#elixir_env{context=Context} }.

expand_clause(_Meta, _Kind, Fun, { '->', Meta, [Left, Right] }, E) ->
  { ELeft, EL }  = expand_head(Fun, Left, E),
  { ERight, ER } = elixir_exp:expand(Right, EL),
  { { '->', Meta, [ELeft, ERight] }, ER };
expand_clause(Meta, Kind, _Fun, _, E) ->
  compile_error(Meta, E#elixir_env.file, "expected -> clauses in ~ts", [Kind]).

expand_head(Fun, [{ 'when', Meta, [_,_|_] = All }], E) ->
  { Args, Guard } = elixir_utils:split_last(All),
  { EArgs, EA }   = match(Fun, Args, E),
  { EGuard, EG }  = expand_guard(Guard, EA#elixir_env{context=guard}),
  { [{ 'when', Meta, EArgs ++ [EGuard] }], EG#elixir_env{context=E#elixir_env.context} };
expand_head(Fun, Args, E) ->
  match(Fun, Args, E).

expand_guard({ 'when', Meta, [Left, Right] }, E) ->
  { ELeft, EL }  = expand_guard(Left, E),
  { ERight, ER } = expand_guard(Right, EL),
  { { 'when', Meta, [ELeft, ERight] }, ER };
expand_guard(Other, E) ->
  elixir_exp:expand(Other, E).

%% Case

expand_case(Meta, [], E) ->
  elixir_errors:compile_error(Meta, E#elixir_scope.file, "missing do keyword in case");
expand_case(Meta, KV, E) when not is_list(KV) ->
  elixir_errors:compile_error(Meta, E#elixir_scope.file, "invalid arguments for case");
expand_case(Meta, KV, E) ->
  { EClauses, { _, EV } } =
    lists:mapfoldl(fun(X, Acc) -> do_expand_case(Meta, X, Acc) end, { E, E }, KV),
  { EClauses, EV }.

do_expand_case(Meta, { do, Clauses }, Acc) when is_list(Clauses) ->
  Transformer = fun(Clause, { Acc1, Acc2 }) ->
    { EClause, EC } =
      elixir_exp_clauses:expand_clause(Meta, 'case', fun elixir_exp:expand_many/2, Clause, Acc1),
    { EClause, { elixir_env:mergec(Acc1, EC), elixir_env:mergev(Acc2, EC) } }
  end,
  { EClauses, EAcc } = lists:mapfoldl(Transformer, Acc, Clauses),
  { { do, EClauses }, EAcc };
do_expand_case(Meta, { do, _ }, { E, _ }) ->
  compile_error(Meta, E#elixir_env.file, "expected -> clauses for do in case");
do_expand_case(Meta, { Kind, _ }, { E, _ }) ->
  compile_error(Meta, E#elixir_env.file, "unexpected keyword ~ts in case", [Kind]).
