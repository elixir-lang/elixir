%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_exp_clauses).
-export([match/3]).
-export([expand_clause/3]).
-include("elixir.hrl").

match(Fun, Expr, #elixir_env{context=Context} = E) ->
  { EExpr, EE } = Fun(Expr, E#elixir_env{context=match}),
  { EExpr, EE#elixir_env{context=Context} }.

expand_clause(Fun, { '->', Meta, [Left, Right] }, E) ->
  { ELeft, EL }  = expand_head(Fun, Left, E),
  { ERight, ER } = elixir_exp:expand(Right, EL),
  { { '->', Meta, [ELeft, ERight] }, ER }.

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
