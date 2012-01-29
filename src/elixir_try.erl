%% Handle code related to match/after/else and guard
%% clauses for receive/case/fn and friends. try is
%% handled in elixir_try.
-module(elixir_try).
-export([clauses/3]).
-import(elixir_variables, [umergec/2]).
-include("elixir.hrl").

clauses(Line, Clauses, S) ->
  DecoupledClauses = elixir_kv_block:decouple(Clauses),
  % Just pass the variable counter forward between each clause.
  Transformer = fun(X, Acc) -> translate_each(Line, X, umergec(S, Acc)) end,
  lists:mapfoldl(Transformer, S, DecoupledClauses).

translate_each(Line, {'catch',Raw,Expr}, S) ->
  { Args, Guards } = elixir_clauses:extract_last_guards(Raw),

  Final = case Args of
    [X]     -> [throw, X, { '_', Line, nil }];
    [X,Y]   -> [X, Y, { '_', Line, nil }];
    [_,_,_] -> Args;
    [] ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no condition given for: ", "catch");
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "too many conditions given for: ", "catch")
  end,

  Condition = { '{}', Line, Final },
  elixir_clauses:assigns_block(Line, fun elixir_translator:translate_each/2, Condition, [Expr], Guards, S);

translate_each(Line, {Key,_,_}, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid key: ", atom_to_list(Key)).