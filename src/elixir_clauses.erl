-module(elixir_clauses).
-export([translate/4]).
-include("elixir.hrl").

% Helpers for translating clauses

translate(Line, Fun, [Clause], S) ->
  { TClause, TS } = translate_each(Fun, Clause, S),
  { [TClause], TS };

translate(Line, Fun, Clauses, RawS) ->
  S = RawS#elixir_scope{clause_vars=dict:new()},

  % Transform tree just passing the variables counter forward
  % and storing variables defined inside each clause.
  Transformer = fun(X, {Acc, CV}) ->
    { TX, TAcc } = translate_each(Fun, X, Acc),
    { TX, { umergec(S, TAcc), [TAcc#elixir_scope.clause_vars|CV] } }
  end,

  { TClauses, { TS, RawCV } } = lists:mapfoldl(Transformer, {S, []}, Clauses),

  % Now get all the variables defined inside each clause
  CV = lists:reverse(RawCV),
  NewVars = lists:umerge([lists:sort(dict:fetch_keys(X)) || X <- CV]),

  case NewVars of
    [] -> { TClauses, TS };
    _  ->
      % Create a new scope that contains a list of all variables
      % defined inside all the clauses. It returns this new scope and
      % a list of tuples where the first element is the variable name,
      % the second one is the new pointer to the variable and the third
      % is the old pointer.
      { FinalVars, FS } = lists:mapfoldl(fun normalize_vars/2, TS, NewVars),

      % Defines a tuple that will be used as left side of the match operator
      LeftTuple = { tuple, Line, [{var, Line, NewValue} || {_, NewValue,_} <- FinalVars] },
      { StorageVar, SS } = elixir_tree_helpers:build_var_name(Line, FS),

      % Expand all clauses by adding a match operation at the end that assigns
      % variables missing in one clause to the others.
      Expander = fun(Clause, Counter) ->
        ClauseVars = lists:nth(Counter, CV),
        RightTuple = [normalize_clause_var(Var, OldValue, ClauseVars) || {Var, _, OldValue} <- FinalVars],

        AssignExpr = { match, Line, LeftTuple, { tuple, Line, RightTuple } },
        [Final|RawClauses] = lists:reverse(Clause),

        % If the last sentence has a match clause, we need to assign its value
        % in the variable list. If not, we insert the variable list before the
        % final clause in order to keep it tail call optimized.
        FinalClause = case has_match_tuple(Final) of
          true ->
            StorageExpr = { match, Line, StorageVar, Final },
            [StorageVar,AssignExpr,StorageExpr|RawClauses];
          false ->
            [Final,AssignExpr|RawClauses]
        end,

        { lists:reverse(FinalClause), Counter + 1 }
      end,

      { FClauses, _ } = lists:mapfoldl(Expander, 1, TClauses),
      { FClauses, SS }
  end.

% Handle each key/value clause pair and translate them accordingly.

translate_each(Fun, {Key,Line,Expr}, S) when not is_list(Expr) ->
  translate_each(Fun, {Key,Line,[Expr]}, S);

translate_each(Fun, {else,_,Expr}, S) ->
  Fun(Expr, S);

translate_each(Fun, {Key,Line,[Expr]}, S) ->
  translate_each(Fun, {Key,Line,[Expr,nil]}, S);

translate_each(Fun, {Key,_Line,[Condition|Exprs]} = T, S) when Key == do; Key == elsif; Key == match ->
  { [TCondition], SC } = Fun([Condition], S),
  { TExprs, SE } = Fun(Exprs, SC),
  { [TCondition|TExprs], SE }.

% Check if the given expression is a match tuple.
% This is a small optimization to allow us to change
% existing assignments instead of creating new ones every time.

has_match_tuple({match, _, _, _}) ->
  true;

has_match_tuple(H) when is_tuple(H) ->
  has_match_tuple(tuple_to_list(H));

has_match_tuple(H) when is_list(H) ->
  lists:any(fun has_match_tuple/1, H);

has_match_tuple(H) -> false.

% Normalize the given var checking its existence in the scope var dictionary.

normalize_vars(Var, #elixir_scope{vars=Dict} = S) ->
  { { _, _, NewValue }, NS } = elixir_tree_helpers:build_var_name(0, S),
  FS = NS#elixir_scope{vars=dict:store(Var, NewValue, Dict)},

  Expr = case dict:find(Var, Dict) of
    { ok, OldValue } -> { var, 0, OldValue };
    error -> { atom, 0, nil }
  end,

  { { Var, NewValue, Expr }, FS }.

% Normalize a var by checking if it was defined in the clause.
% If so, use it, otherwise use from main scope.

normalize_clause_var(Var, OldValue, ClauseVars) ->
  case dict:find(Var, ClauseVars) of
    { ok, ClauseValue } -> { var, 0, ClauseValue };
    error -> OldValue
  end.

% Merge variable counters.

umergec(S1, S2) ->
  S1#elixir_scope{counter=S2#elixir_scope.counter}.
