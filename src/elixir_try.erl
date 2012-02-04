%% Handle code related to match/after/else and guard
%% clauses for receive/case/fn and friends. try is
%% handled in elixir_try.
-module(elixir_try).
-export([clauses/3]).
-import(elixir_translator, [translate/2]).
-import(elixir_variables, [umergec/2]).
-include("elixir.hrl").

clauses(Line, Clauses, S) ->
  DecoupledClauses = elixir_kv_block:decouple(Clauses),
  { Catch, Rescue } = lists:partition(fun(X) -> element(1, X) == 'catch' end, DecoupledClauses),
  Transformer = fun(X, Acc) -> each_clause(Line, X, umergec(S, Acc)) end,
  lists:mapfoldl(Transformer, S, Rescue ++ Catch).

each_clause(Line, {'catch',Raw,Expr}, S) ->
  { Args, Guards } = elixir_clauses:extract_last_guards(Raw),
  validate_args('catch', Line, Args, 3, S),

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

each_clause(Line, { rescue, Args, Expr }, S) ->
  validate_args(rescue, Line, Args, 3, S),
  [Condition] = Args,
  { Left, Right } = normalize_rescue(Line, Condition, S),
  case Left of
    { '_', _, Atom } when is_atom(Atom) ->
      each_clause(Line, { 'catch', [error, Left], Expr }, S)
  end;

each_clause(Line, {Key,_,_}, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid key: ", atom_to_list(Key)).

%% Helpers

validate_args(Clause, Line, [], _, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no condition given for: ", atom_to_list(Clause));

validate_args(Clause, Line, List, Max, S) when length(List) > Max ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "too many conditions given for: ", atom_to_list(Clause));

validate_args(_, _, _, _, _) -> [].

%% rescue [Error] -> _ in [Error]
normalize_rescue(Line, List, S) when is_list(List) ->
  normalize_rescue(Line, { in, Line, [{ '_', Line, nil }, List] }, S);

%% rescue _    -> _ in _
%% rescue var  -> var in _
normalize_rescue(_, { Name, Line, Atom } = Rescue, S) when is_atom(Name), is_atom(Atom) ->
  normalize_rescue(Line, { in, Line, [Rescue, { '_', Line, nil }] }, S);

%% rescue var in _
%% rescue var in [Exprs]
normalize_rescue(_, { in, Line, [Left, Right] }, S) ->
  case Right of
    { '_', _, _ } ->
      { Left, nil };
    _ when is_list(Right) ->
      { _, Refs } = lists:partition(fun(X) -> element(1, X) == '^' end, Right),
      { TRefs, _ } = translate(Refs, S),
      case lists:all(fun is_atom/1, TRefs) of
        true -> { Left, Right };
        false -> normalize_rescue(Line, nil, S)
      end;
    _ -> normalize_rescue(Line, nil, S)
  end;

%% rescue ^var -> _ in [^var]
%% rescue ErlangError -> _ in [ErlangError]
normalize_rescue(_, { Name, Line, _ } = Rescue, S) when is_atom(Name) ->
  normalize_rescue(Line, { in, Line, [{ '_', Line, nil }, Rescue] }, S);

normalize_rescue(Line, _, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid condition for: ", "rescue").