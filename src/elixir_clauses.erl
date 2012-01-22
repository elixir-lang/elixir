%% Handle code related to match/after/catch/else clauses
%% for receive/try/fn and friends.
-module(elixir_clauses).
-export([match/3, try_catch/3,
  assigns/3, assigns_block/5, assigns_block/6,
  extract_args/1, extract_guards/1, extract_guard_clauses/1, extract_last_guards/1]).
-import(elixir_variables, [umergec/2]).
-include("elixir.hrl").

% Function for translating assigns.
assigns(Fun, Args, #elixir_scope{assign=false} = S) ->
  { Result, NewS } = assigns(Fun, Args, S#elixir_scope{assign=true, temp_vars=dict:new()}),
  { Result, NewS#elixir_scope{assign=false} };

assigns(Fun, Args, S) -> Fun(Args, S).

%% Function for translating a block that is preceeded by an
%% assignment and optional guards. This is used by def* and fn.

assigns_block(Line, Fun, BareArgs, Exprs, S) ->
  { Args, Guards } = extract_guards(BareArgs),
  assigns_block(Line, Fun, Args, Exprs, extract_guard_clauses(Guards), S).

assigns_block(Line, Fun, Args, Exprs, Guards, S) ->
  { TArgs, SA }  = elixir_clauses:assigns(Fun, Args, S),
  { TExprs, SE } = elixir_translator:translate(Exprs, SA),

  FArgs   = listify(TArgs),
  FGuards = [element(1, elixir_translator:translate(Guard, SA#elixir_scope{guard=true})) || Guard <- Guards],

  % Uncompact expressions from the block.
  case TExprs of
    [{ block, _, FExprs }] -> [];
    _ -> FExprs = TExprs
  end,

  { { clause, Line, FArgs, FGuards, FExprs }, SE }.

% Extract guards from the given expression.

extract_guards({ 'when', _, [Left, Right] }) -> { Left, Right };
extract_guards(Else) -> { Else, true }.

extract_guard_clauses(true) -> [];
extract_guard_clauses(Term) ->
  Or = extract_or_clauses(Term, []),
  [extract_and_clauses(Item, []) || Item <- Or].

extract_or_clauses({ '|', _, [Left, Right] }, Acc) -> extract_or_clauses(Left, [Right|Acc]);
extract_or_clauses(Term, Acc) -> [Term|Acc].

extract_and_clauses({ '&', _, [Left, Right] }, Acc) -> extract_and_clauses(Left, [Right|Acc]);
extract_and_clauses(Term, Acc) -> [Term|Acc].

% Extract guards when it is in the last element of the args

extract_last_guards([]) -> { [], [] };
extract_last_guards(Args) ->
  { Left, [Right] } = lists:split(length(Args) - 1, Args),
  { Bare, Guards } = extract_guards(Right),
  { Left ++ [Bare], extract_guard_clauses(Guards) }.

% Extract name and args from the given expression.

extract_args({ { '.', _, [Name] }, _, Args }) when is_atom(Name), is_list(Args) -> { Name, Args };
extract_args({ Name, _, Args }) when is_atom(Name), is_atom(Args) -> { Name, [] };
extract_args({ Name, _, Args }) when is_atom(Name), is_list(Args) -> { Name, Args }.

% Function for translating macros for try's catch.

try_catch(Line, Clauses, S) ->
  DecoupledClauses = elixir_kv_block:decouple(Clauses),
  % Just pass the variable counter forward between each clause.
  Transformer = fun(X, Acc) -> translate_each(Line, X, umergec(S, Acc)) end,
  lists:mapfoldl(Transformer, S, DecoupledClauses).

% Function for translating macros with match style like case and receive.

match(Line, Clauses, RawS) ->
  S = RawS#elixir_scope{clause_vars=dict:new()},
  DecoupledClauses = elixir_kv_block:decouple(Clauses, fun(X) -> handle_else(match, Line, X) end),
  case DecoupledClauses of
    [DecoupledClause] ->
      { TDecoupledClause, TS } = translate_each(Line, DecoupledClause, S),
      { [TDecoupledClause], TS };
    _ ->
      % Transform tree just passing the variables counter forward
      % and storing variables defined inside each clause.
      Transformer = fun(X, {Acc, CV}) ->
        { TX, TAcc } = translate_each(Line, X, Acc),
        { TX, { umergec(S, TAcc), [TAcc#elixir_scope.clause_vars|CV] } }
      end,

      { TClauses, { TS, RawCV } } = lists:mapfoldl(Transformer, {S, []}, DecoupledClauses),

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
          { StorageVar, SS } = elixir_variables:build_erl(Line, FS),

          % Expand all clauses by adding a match operation at the end that assigns
          % variables missing in one clause to the others.
          Expander = fun(Clause, Counter) ->
            ClauseVars = lists:nth(Counter, CV),
            RightTuple = [normalize_clause_var(Var, OldValue, ClauseVars) || {Var, _, OldValue} <- FinalVars],

            AssignExpr = { match, Line, LeftTuple, { tuple, Line, RightTuple } },
            ClauseExprs = element(5, Clause),
            [Final|RawClauseExprs] = lists:reverse(ClauseExprs),

            % If the last sentence has a match clause, we need to assign its value
            % in the variable list. If not, we insert the variable list before the
            % final clause in order to keep it tail call optimized.
            FinalClauseExprs = case has_match_tuple(Final) of
              true ->
                StorageExpr = { match, Line, StorageVar, Final },
                [StorageVar,AssignExpr,StorageExpr|RawClauseExprs];
              false ->
                [Final,AssignExpr|RawClauseExprs]
            end,

            FinalClause = setelement(5, Clause, lists:reverse(FinalClauseExprs)),
            { FinalClause, Counter + 1 }
          end,

          { FClauses, _ } = lists:mapfoldl(Expander, 1, TClauses),
          { FClauses, SS }
      end
  end.

% Handle else clauses by moving them under the given Kind.
handle_else(Kind, Line, Clauses) ->
  case orddict:find(else, Clauses) of
    { ok, Else } ->
      %% Get else expression from the kv_block and normalize it
      %% TODO: raise an error for else foo, bar
      Expr = case Else of
        { '__KVBLOCK__', _, [{ [Left], nil }] }   -> Left;
        { '__KVBLOCK__', _, [{ [], Right }] }     -> Right;
        { '__KVBLOCK__', _, [{ [Left], Right }] } -> prepend_to_block(Line, Left, Right)
      end,

      TClauses = orddict:erase(else, Clauses),
      ElseExpr = {[{'_', Line, nil}], Expr},

      FinalClause = case orddict:find(Kind, TClauses) of
        { ok, KindClause } ->
          { '__KVBLOCK__', _, KindExprs } = KindClause,
          setelement(3, KindClause, KindExprs ++ [ElseExpr]);
        _ ->
          { '__KVBLOCK__', Line, [ElseExpr] }
      end,

      orddict:store(Kind, FinalClause, TClauses);
    _ -> Clauses
  end.

% Handle each key/value clause pair and translate them accordingly.

% Do clauses have no conditions. So we are done.
translate_each(_Line, {Key,[],Expr}, S) when Key == do ->
  elixir_translator:translate_each(Expr, S);

translate_each(Line, {'catch',Raw,Expr}, S) ->
  { Args, Guards } = extract_last_guards(Raw),

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
  assigns_block(Line, fun elixir_translator:translate_each/2, Condition, [Expr], Guards, S);

translate_each(Line, {Key,[Condition],Expr}, S) when Key == match; Key == 'catch'; Key == 'after' ->
  assigns_block(Line, fun elixir_translator:translate_each/2, Condition, [Expr], S);

translate_each(Line, {Key,[],_}, S) when Key == match; Key == 'catch'; Key == 'after' ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no condition given for: ", atom_to_list(Key));

translate_each(Line, {Key,_,_}, S) when Key == match; Key == 'after' ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid comma arguments for: ", atom_to_list(Key));

translate_each(Line, {Key,_,_}, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid key: ", atom_to_list(Key)).

% Check if the given expression is a match tuple.
% This is a small optimization to allow us to change
% existing assignments instead of creating new ones every time.

has_match_tuple({match, _, _, _}) ->
  true;

has_match_tuple(H) when is_tuple(H) ->
  has_match_tuple(tuple_to_list(H));

has_match_tuple(H) when is_list(H) ->
  lists:any(fun has_match_tuple/1, H);

has_match_tuple(_) -> false.

% Normalize the given var checking its existence in the scope var dictionary.

normalize_vars(Var, #elixir_scope{vars=Dict} = S) ->
  { { _, _, NewValue }, NS } = elixir_variables:build_erl(0, S),
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

%% Listify

listify(Expr) when not is_list(Expr) -> [Expr];
listify(Expr) -> Expr.

%% Prepend a given expression to a block.

prepend_to_block(_Line, Expr, { '__BLOCK__', Line, Args }) ->
  { '__BLOCK__', Line, [Expr|Args] };

prepend_to_block(Line, Expr, Args) ->
  { '__BLOCK__', Line, [Expr, Args] }.
