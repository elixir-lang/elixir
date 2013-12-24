%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_exp_clauses).
-export([match/3, clause/5, 'case'/3, 'receive'/3]).
-import(elixir_errors, [compile_error/3, compile_error/4]).
-include("elixir.hrl").

match(Fun, Expr, #elixir_env{context=Context} = E) ->
  { EExpr, EE } = Fun(Expr, E#elixir_env{context=match}),
  { EExpr, EE#elixir_env{context=Context} }.

clause(_Meta, _Kind, Fun, { '->', Meta, [Left, Right] }, E) ->
  { ELeft, EL }  = head(Fun, Left, E),
  { ERight, ER } = elixir_exp:expand(Right, EL),
  { { '->', Meta, [ELeft, ERight] }, ER };
clause(Meta, Kind, _Fun, _, E) ->
  compile_error(Meta, E#elixir_env.file, "expected -> clauses in ~ts", [Kind]).

head(Fun, [{ 'when', Meta, [_,_|_] = All }], E) ->
  { Args, Guard } = elixir_utils:split_last(All),
  { EArgs, EA }   = match(Fun, Args, E),
  { EGuard, EG }  = guard(Guard, EA#elixir_env{context=guard}),
  { [{ 'when', Meta, EArgs ++ [EGuard] }], EG#elixir_env{context=E#elixir_env.context} };
head(Fun, Args, E) ->
  match(Fun, Args, E).

guard({ 'when', Meta, [Left, Right] }, E) ->
  { ELeft, EL }  = guard(Left, E),
  { ERight, ER } = guard(Right, EL),
  { { 'when', Meta, [ELeft, ERight] }, ER };
guard(Other, E) ->
  elixir_exp:expand(Other, E).

%% Case

'case'(Meta, [], E) ->
  elixir_errors:compile_error(Meta, E#elixir_scope.file, "missing do keyword in case");
'case'(Meta, KV, E) when not is_list(KV) ->
  elixir_errors:compile_error(Meta, E#elixir_scope.file, "invalid arguments for case");
'case'(Meta, KV, E) ->
  { EClauses, { _, EV } } =
    lists:mapfoldl(fun(X, Acc) -> do_case(Meta, X, Acc) end, { E, E }, KV),
  { EClauses, EV }.

do_case(Meta, { 'do', _ } = Do, Acc) ->
  do_key(Meta, 'case', expand_one(Meta, 'case'), Do, Acc);
do_case(Meta, { Key, _ }, { E, _ }) ->
  compile_error(Meta, E#elixir_env.file, "unexpected keyword ~ts in case", [Key]).

%% Receive

'receive'(Meta, [], E) ->
  elixir_errors:compile_error(Meta, E#elixir_scope.file, "missing do or after keywords in receive");
'receive'(Meta, KV, E) when not is_list(KV) ->
  elixir_errors:compile_error(Meta, E#elixir_scope.file, "invalid arguments for receive");
'receive'(Meta, KV, E) ->
  { EClauses, { _, EV } } =
    lists:mapfoldl(fun(X, Acc) -> do_receive(Meta, X, Acc) end, { E, E }, KV),
  { EClauses, EV }.

do_receive(Meta, { 'do', _ } = Do, Acc) ->
  do_key(Meta, 'receive', expand_one(Meta, 'receive'), Do, Acc);
do_receive(_Meta, { 'after', [{ '->', Meta, [[Left], Right] }] }, { Acc1, Acc2 }) ->
  { ELeft, EL }  = elixir_exp:expand(Left, Acc1),
  { ERight, ER } = elixir_exp:expand(Right, EL),
  EClause = { 'after', [{ '->', Meta, [[ELeft], ERight] }] },
  { EClause, { elixir_env:mergec(Acc1, ER), elixir_env:mergev(Acc2, ER) } };
do_receive(Meta, { 'after', _ }, { E, _ }) ->
  compile_error(Meta, E#elixir_env.file, "expected a single -> clause for after in receive");
do_receive(Meta, { Key, _ }, { E, _ }) ->
  compile_error(Meta, E#elixir_env.file, "unexpected keyword ~ts in receive", [Key]).

%% Expansion helpers

do_key(Meta, Kind, Fun, { Key, Clauses }, Acc) when is_list(Clauses) ->
  Transformer = fun(Clause, { Acc1, Acc2 }) ->
    { EClause, EC } = clause(Meta, Kind, Fun, Clause, Acc1),
    { EClause, { elixir_env:mergec(Acc1, EC), elixir_env:mergev(Acc2, EC) } }
  end,
  { EClauses, EAcc } = lists:mapfoldl(Transformer, Acc, Clauses),
  { { Key, EClauses }, EAcc };
do_key(Meta, Kind, _Fun, { Key, _ }, { E, _ }) ->
  compile_error(Meta, E#elixir_env.file, "expected -> clauses for ~ts in ~ts", [Key, Kind]).

expand_one(Meta, Kind) ->
  fun
    ([Arg], E) ->
      { EArg, EA } = elixir_exp:expand(Arg, E),
      { [EArg], EA };
    (_, E) ->
      compile_error(Meta, E#elixir_env.file, "expected one arg for -> clauses in ~ts", [Kind])
  end.
