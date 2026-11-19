%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_erl_try.
-module(elixir_erl_clauses).
-export([match/4, clause/6, clauses/2, guards/4, get_clauses/3]).
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

match(Ann, Fun, Match, #elixir_erl{context=Context} = S) when Context =/= match ->
  {Result, NewS} = Fun(Match, Ann, S#elixir_erl{context=match}),
  {Result, NewS#elixir_erl{context=Context}};
match(Ann, Fun, Match, S) ->
  Fun(Match, Ann, S).

%% Translate clauses with args, guards and expressions

clause(Ann, Fun, Match, Expr, Guards, S) ->
  {TMatch, SA} = match(Ann, Fun, Match, S),
  SG = SA#elixir_erl{extra_guards=[]},
  {TGuards, SC} = guards(Ann, Guards, SA#elixir_erl.extra_guards, SG),
  {TExpr, SE} = elixir_erl_pass:translate(Expr, Ann, SC),
  {{clause, Ann, TMatch, TGuards, unblock(TExpr)}, SE}.

% Translate/Extract guards from the given expression.

guards(Ann, Guards, Extra, S) ->
  SG = S#elixir_erl{context=guard},
  {TGuards, TS} = case Guards of
    [] -> {case Extra of [] -> []; _ -> [Extra] end, SG};
    _  -> lists:mapfoldl(fun(Guard, Acc) -> translate_guard(Guard, Ann, Acc, Extra) end, SG, Guards)
  end,
  {TGuards, TS#elixir_erl{context=S#elixir_erl.context}}.

translate_guard(Guard, Ann, S, Extra) ->
  {TGuard, TS} = elixir_erl_pass:translate(Guard, Ann, S),
  {[TGuard | Extra], TS}.

% Function for translating macros with match style like case and receive.

clauses([], S) ->
  {[], S};

clauses(Clauses, S) ->
  lists:mapfoldl(fun each_clause/2, S, Clauses).

each_clause({match, Meta, [Condition], Expr}, S) ->
  {Arg, Guards} = elixir_utils:extract_guards(Condition),
  clause(?ann(Meta), fun elixir_erl_pass:translate_args/3, [Arg], Expr, Guards, S);

each_clause({expr, Meta, [Condition], Expr}, S) ->
  Ann = ?ann(Meta),
  {TCondition, SC} = elixir_erl_pass:translate(Condition, Ann, S),
  {TExpr, SB} = elixir_erl_pass:translate(Expr, Ann, SC),
  {{clause, Ann, [TCondition], [], unblock(TExpr)}, SB}.

unblock({'block', _, Exprs}) -> Exprs;
unblock(Exprs) -> [Exprs].
