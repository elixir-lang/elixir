-module(elixir_translator).
-export([translate/2]).
-include("elixir.hrl").

translate(Forms, S) ->
  lists:mapfoldl(fun translate_each/2, S, Forms).

%% Assignment operator

translate_each({'=', Line, Left, Right}, S) ->
  { TLeft, SL } = translate_assigns(fun translate_each/2, Left, S),
  { TRight, SR } = translate_each(Right, umergec(S, SL)),
  SM = umergev(SL, SR),
  SF = case TLeft of
    { var, _, Name } ->
      Current = SM#elixir_scope.assigned_vars,
      SM#elixir_scope{assigned_vars=dict:store(Name, {Right, TRight}, Current)};
    _ -> SM
  end,
  { {match, Line, TLeft, TRight }, SF };

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

%% If

translate_each({'if', Line, [Condition, {':', _, [{do,_}|_] = Keywords}]}, S) ->
  [{ do, Exprs }|ElsesKeywords] = Keywords,

  IfKeywords = case is_list(Exprs) of
    true  -> {do, [Condition|Exprs]};
    false -> {do, [Condition,Exprs]}
  end,

  case ElsesKeywords of
    [{else,_} = ElseKeywords|ElsifsKeywords] -> [];
    ElsifsKeywords -> ElseKeywords = {else,[nil]}
  end,

  { Clauses, FS } = translate_clauses(Line, [IfKeywords|ElsifsKeywords] ++ [ElseKeywords], S),
  [Else|Others] = lists:reverse(Clauses),
  { build_if_clauses(Line, Others, Else), FS };

% TODO: Handle tree errors properly
translate_each({'if', Line, _}, S) ->
  error(invalid_arguments_for_if);

%% Functions

translate_each({fn, Line, [{'[]', _, Args}, {':', _, [{do,_}] = Keywords}]}, S) ->
  { TArgs, NS } = translate_assigns(fun translate/2, Args, S),
  { TKeywords, FS } = translate_keywords(Keywords, NS),
  [{ do, TExprs}] = TKeywords,
  { { 'fun', Line, {clauses, [{clause, Line, TArgs, [], TExprs}]} }, FS };

% TODO: Handle tree errors properly
translate_each({fn, Line, _}, S) ->
  error(invalid_arguments_for_fn);

%% Variables & Methods

translate_each({Name, Line, false}, S) when is_atom(Name) ->
  Match = S#elixir_scope.assign,
  Vars = S#elixir_scope.vars,
  TempVars = S#elixir_scope.temp_vars,
  ClauseVars = S#elixir_scope.clause_vars,

  case Name of
    '_' -> { {var, Line, Name}, S };
    _ ->
      case { Match, dict:is_key(Name, Vars), lists:member(Name, TempVars) } of
        { true, true, true } -> { {var, Line, dict:fetch(Name, Vars) }, S };
        { true, Else, _ } ->
          % If it was already assigned or in a noname scope, build a new var.
          % TODO: Potentially get rid of noname scopes.
          { NewVar, NS } = case Else or S#elixir_scope.noname of
            true -> elixir_tree_helpers:build_var_name(Line, S);
            false -> { {var, Line, Name}, S }
          end,
          RealName = element(3, NewVar),
          { NewVar, NS#elixir_scope{
            vars=dict:store(Name, RealName, Vars),
            temp_vars=[RealName|TempVars],
            clause_vars=dict:store(Name, RealName, ClauseVars)
          } };
        { false, false, _ } -> translate_each({Name, Line, []}, S);
        { false, true, _ }  -> { {var, Line, dict:fetch(Name, Vars) }, S }
      end
  end;

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

%% Assigns helpers

translate_assigns(Fun, Args, Scope) ->
  { Result, NewScope } = Fun(Args, Scope#elixir_scope{assign=true}),
  { Result, NewScope#elixir_scope{assign=false, temp_vars=[] } }.

%% Keyword helpers

translate_keywords(Keywords, S) ->
  lists:mapfoldl(fun translate_each_keyword/2, S, Keywords).

translate_each_keyword({Key,Expr}, S) when not is_list(Expr) ->
  translate_each_keyword({Key,[Expr]}, S);

translate_each_keyword({Key,Expr}, S) when is_atom(Key) ->
  { TExpr, NS } = translate(Expr, S),
  { { Key, TExpr }, NS }.

%% Clauses

translate_clauses(Line, [Clause], S) ->
  { TClause, TS } = translate_each_clause(Clause, S),
  { [TClause], TS };

translate_clauses(Line, Clauses, RawS) ->
  S = RawS#elixir_scope{clause_vars=dict:new()},

  % Transform tree just passing the variables counter forward
  % and storing variables defined inside each clause.
  Transformer = fun(X, {Acc, CV}) ->
    { TX, TAcc } = translate_each_clause(X, Acc),
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

translate_each_clause({Key,Expr}, S) when not is_list(Expr) ->
  translate_each_clause({Key,[Expr]}, S);

translate_each_clause({else,Expr}, S) ->
  translate(Expr, S);

translate_each_clause({Key,[Expr]}, S) ->
  translate_each_clause({Key,[Expr,nil]}, S);

translate_each_clause({Key,[Condition|Exprs]} = T, S) when Key == do; Key == elsif ->
  { TCondition, SC } = translate_each(Condition, S),
  { TExprs, SE } = translate(Exprs, SC),
  { [TCondition|TExprs], SE }.

% Helpers to translate clauses

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

% If the var was defined in the clause, use it, otherwise use from main scope

normalize_clause_var(Var, OldValue, ClauseVars) ->
  case dict:find(Var, ClauseVars) of
    { ok, ClauseValue } -> { var, 0, ClauseValue };
    error -> OldValue
  end.

%% Build if clauses by nesting

build_if_clauses(Line, [], Acc) ->
  Acc;

build_if_clauses(Line, [[Condition|Exprs]|Others], Acc) ->
  True  = [{atom,Line,true}],
  False = [{atom,Line,false}],

  Case = { 'case', Line, elixir_tree_helpers:convert_to_boolean(Line, Condition, true), [
    { clause, Line, True,  [], Exprs },
    { clause, Line, False, [], Acc }
  ] },

  build_if_clauses(Line, Others, Case).

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