-module(elixir_translator).
-export([translate/2]).
-include("elixir.hrl").

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Math Operators

translate_each({ Op, Line, Left, Right }, S) when Op == '+'; Op == '-'; Op == '*'; Op == '/' ->
  { TLeft, SL }  = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  { { op, Line, Op, TLeft, TRight }, umergev(SL, SR) };

%% Short-circuit operators

translate_each({'||', Line, Left, Right}, S) ->
  { Var, NS } = elixir_tree_helpers:build_var_name(Line, S),
  { TLeft, SL } = translate_each(Left, NS),
  { TRight, SR } = translate_each(Right, umergec(NS, SL)),

  Match = {match, Line, Var, TLeft},
  True  = [{atom,Line,true}],
  False = [{atom,Line,false}],

  { { 'case', Line, elixir_tree_helpers:convert_to_boolean(Line, Match, true), [
    { clause, Line, False, [], [TRight] },
    { clause, Line, True, [], [Var] }
  ] }, umergev(SL, SR) };

translate_each({'&&', Line, Left, Right}, S) ->
  { TLeft, SL } = translate_each(Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),

  Any   = [{var, Line,'_'}],
  Nil   = [{atom,Line,nil}],
  False = [{atom,Line,false}],

  { { 'case', Line, TLeft, [
    { clause, Line, False, [], False },
    { clause, Line, Nil, [], Nil },
    { clause, Line, Any, [], [TRight] }
  ] }, umergev(SL, SR) };

%% Methods

translate_each({Atom, Line, Args}, S) when is_atom(Atom) ->
  { TArgs, NS } = translate(Args, S),
  { { call, Line, { atom, Line, Atom }, TArgs }, NS };

%% Block expressions

translate_each([], S) ->
  { { atom, 0, nil }, S };

translate_each([Expr], S) ->
  translate_each(Expr, S);

translate_each(List, S) when is_list(List) ->
  { TList, NS } = translate(List, S),
  { { block, 0, TList }, NS };

%% Literals

translate_each(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate_each(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate_each(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S }.

%% Helpers

% Receives two scopes and return a new scope based on the second
% with their variables merged.
umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  A1 = S1#elixir_scope.assigned_vars,
  A2 = S2#elixir_scope.assigned_vars,
  S2#elixir_scope{
    vars=dict:merge(fun var_merger/3, V1, V2),
    clause_vars=dict:merge(fun var_merger/3, C1, C2),
    assigned_vars=dict:merge(fun unique_var_merge/3, A1, A2)
  }.

% Receives two scopes and return a new scope based on the first
% with the counter values from the first one.
umergec(S1, S2) ->
  S1#elixir_scope{counter=S2#elixir_scope.counter}.

% Merge variables and keep them only if they are equal.
unique_var_merge(_, V, V) -> V;
unique_var_merge(_, _, _) -> [].

% Merge variables trying to find the most recently created.
var_merger(Var, Var, K2) -> K2;
var_merger(Var, K1, Var) -> K1;
var_merger(Var, K1, K2) ->
  V1 = list_to_integer(tl(atom_to_list(K1))),
  V2 = list_to_integer(tl(atom_to_list(K2))),
  if V1 > V2 -> K1;
     true -> K2
  end.