%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_erl_try.
-module(elixir_erl_clauses).
-export([match/3, clause/6, clauses/2, guards/3, get_clauses/3]).
-include("elixir.hrl").

%% Get clauses under the given key.

get_clauses(Key, Keyword, As) ->
  case lists:keyfind(Key, 1, Keyword) of
    {Key, Clauses} when is_list(Clauses) ->
      [{As, Meta, Left, Right} || {'->', Meta, [Left, Right]} <- Clauses];
    _ ->
      []
  end.

%% Translate matches

match(Fun, Args, #elixir_erl{context=Context} = S) when Context =/= match ->
  {Result, NewS} = match(Fun, Args, S#elixir_erl{context=match}),
  {Result, NewS#elixir_erl{context=Context}};
match(Fun, Args, S) ->
  Fun(Args, S).

%% Translate clauses with args, guards and expressions

clause(Meta, Fun, Args, Expr, Guards, S) when is_list(Meta) ->
  {TArgs, SA} = match(Fun, Args, S),
  SG = SA#elixir_erl{extra_guards=[]},
  TGuards = guards(Guards, SA#elixir_erl.extra_guards, SG),
  {TExpr, SE} = elixir_erl_pass:translate(Expr, SG),
  {{clause, ?ann(Meta), TArgs, TGuards, unblock(TExpr)}, SE}.

% Translate/Extract guards from the given expression.

guards(Guards, Extra, S) ->
  SG = S#elixir_erl{context=guard},
  case Guards of
    [] -> case Extra of [] -> []; _ -> [Extra] end;
    _  -> [translate_guard(Guard, Extra, SG) || Guard <- Guards]
  end.

translate_guard(Guard, Extra, S) ->
  [element(1, elixir_erl_pass:translate(Guard, S)) | Extra].

% Function for translating macros with match style like case and receive.

clauses([], S) ->
  {[], S};

clauses(Clauses, S) ->
  lists:mapfoldl(fun each_clause/2, S, Clauses).

each_clause({match, Meta, [Condition], Expr}, S) ->
  {Arg, Guards} = elixir_utils:extract_guards(Condition),
  clause(Meta, fun elixir_erl_pass:translate_args/2, [Arg], Expr, Guards, S);

each_clause({expr, Meta, [Condition], Expr}, S) ->
  {TCondition, SC} = elixir_erl_pass:translate(Condition, S),
  {TExpr, SB} = elixir_erl_pass:translate(Expr, SC),
  {{clause, ?ann(Meta), [TCondition], [], unblock(TExpr)}, SB}.

unblock({'block', _, Exprs}) -> Exprs;
unblock(Exprs) -> [Exprs].
