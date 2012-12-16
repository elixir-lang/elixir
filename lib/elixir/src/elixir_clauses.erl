%% Handle code related to rocket args, guard and -> matching
%% for case, fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([
  assigns/3, assigns_block/5, assigns_block/6, extract_last_guards/1,
  get_pairs/4, get_pairs/5, match/3, extract_args/1, extract_guards/1]).
-import(elixir_scope, [umergec/2]).
-include("elixir.hrl").

%% Get pairs from a clause.

get_pairs(Line, Key, Clauses, S) ->
  get_pairs(Line, Key, Clauses, S, false).

get_pairs(Line, Key, Clauses, S, AllowNil) ->
  case lists:keyfind(Key, 1, Clauses) of
    { Key, { '->', _, Pairs } } ->
      [{ Key, Left, Right } || { Left, Right } <- Pairs];
    { Key, nil } when AllowNil ->
      [];
    { Key, _ } ->
      elixir_errors:syntax_error(Line, S#elixir_scope.file, "expected pairs with -> for key ~s", [Key]);
    _ ->
      []
  end.

% Function for translating assigns.

assigns(Fun, Args, #elixir_scope{context=Context} = S) when Context /= assign ->
  { Result, NewS } = assigns(Fun, Args, S#elixir_scope{context=assign, temp_vars=orddict:new()}),
  { Result, NewS#elixir_scope{context=Context} };

assigns(Fun, Args, S) -> Fun(Args, S).

%% Function for translating a block that is preceeded by an
%% assignment and optional guards. This is used by def* and fn.

assigns_block(Line, Fun, BareArgs, Exprs, S) ->
  { Args, Guards } = extract_guards(BareArgs),
  assigns_block(Line, Fun, Args, Exprs, Guards, S).

assigns_block(Line, Fun, Args, Exprs, Guards, S) ->
  { TArgs, SA }  = assigns(Fun, Args, S#elixir_scope{extra_guards=[]}),
  { TExprs, SE } = elixir_translator:translate(Exprs, SA#elixir_scope{extra_guards=nil}),

  FArgs   = listify(TArgs),
  SG      = SA#elixir_scope{context=guard, extra_guards=nil},
  Extra   = SA#elixir_scope.extra_guards,

  FGuards = case Guards of
    [] -> case Extra of [] -> []; _ -> [Extra] end;
    _  -> [translate_guard(Line, Guard, Extra, SG) || Guard <- Guards]
  end,

  % Uncompact expressions from the block.
  case TExprs of
    [{ block, _, FExprs }] -> [];
    _ -> FExprs = TExprs
  end,

  { { clause, Line, FArgs, FGuards, FExprs }, SE }.

% Translate/Extract guards from the given expression.

translate_guard(Line, Guard, Extra, S) ->
  [element(1, elixir_translator:translate_each(elixir_quote:linify(Line, Guard), S))|Extra].

extract_guards({ 'when', _, [Left, Right] }) -> { Left, extract_or_clauses(Right, []) };
extract_guards(Else) -> { Else, [] }.

extract_or_clauses({ 'when', _, [Left, Right] }, Acc) -> extract_or_clauses(Right, [Left|Acc]);
extract_or_clauses(Term, Acc) -> [Term|Acc].

% Extract name and args from the given expression.

extract_args({ { '.', _, [Name] }, _, Args }) when is_atom(Name), is_list(Args) -> { Name, Args };
extract_args({ Name, _, Args }) when is_atom(Name), is_atom(Args) -> { Name, [] };
extract_args({ Name, _, Args }) when is_atom(Name), is_list(Args) -> { Name, Args };
extract_args(_) -> error.

% Extract guards when it is in the last element of the args

extract_last_guards([]) -> { [], [] };
extract_last_guards(Args) ->
  { Left, Right }  = elixir_tree_helpers:split_last(Args),
  { Bare, Guards } = extract_guards(Right),
  { Left ++ [Bare], Guards }.

% Function for translating macros with match style like case and receive.

match(Line, Clauses, #elixir_scope{clause_vars=C1} = S) ->
  { TC, TS } = do_match(Line, Clauses, S#elixir_scope{clause_vars=orddict:new()}),
  C2 = TS#elixir_scope.clause_vars,
  { TC, TS#elixir_scope{clause_vars=elixir_scope:merge_clause_vars(C1, C2)} }.

do_match(_Line, [], S) ->
  { [], S };

do_match(Line, [DecoupledClause], S) ->
  { TDecoupledClause, TS } = each_clause(Line, DecoupledClause, S),
  { [TDecoupledClause], TS };

do_match(Line, DecoupledClauses, S) ->
  % Transform tree just passing the variables counter forward
  % and storing variables defined inside each clause.
  Transformer = fun(X, {Acc, CV}) ->
    { TX, TAcc } = each_clause(Line, X, Acc),
    { TX, { umergec(S, TAcc), [TAcc#elixir_scope.clause_vars|CV] } }
  end,

  { TClauses, { TS, RawCV } } = lists:mapfoldl(Transformer, {S, []}, DecoupledClauses),

  % Now get all the variables defined inside each clause
  CV = lists:reverse(RawCV),

  AllVars    = lists:umerge([orddict:fetch_keys(X) || X <- CV]),
  SharedVars = ordsets:intersection(CV),

  case AllVars of
    [] -> { TClauses, TS };
    _  ->
      % Create a new scope that contains a list of all variables
      % defined inside all the clauses. It returns this new scope and
      % a list of tuples where the first element is the variable name,
      % the second one is the new pointer to the variable and the third
      % is the old pointer.
      { FinalVars, FS } = lists:mapfoldl(fun(X, Acc) -> normalize_vars(X, SharedVars, Acc) end, TS, AllVars),

      % Defines a tuple that will be used as left side of the match operator
      LeftVars = [{var, Line, NewValue} || { _, _, NewValue, _ } <- FinalVars],

      % Expand all clauses by adding a match operation at the end
      % that assigns variables missing in one clause to the others.
      expand_clauses(Line, TClauses, CV, LeftVars, FinalVars, [], FS)
  end.

expand_clauses(Line, [Clause|T], [ClauseVars|V], LeftVars, FinalVars, Acc, S) ->
  RightVars = [normalize_clause_var(Var, Kind, OldValue, ClauseVars) ||
                 { Var, Kind, _, OldValue } <- FinalVars],

  AssignExpr = generate_match(Line, LeftVars, RightVars),
  ClauseExprs = element(5, Clause),
  [Final|RawClauseExprs] = lists:reverse(ClauseExprs),

  % If the last sentence has a match clause, we need to assign its value
  % in the variable list. If not, we insert the variable list before the
  % final clause in order to keep it tail call optimized.
  { FinalClauseExprs, FS } = case has_match_tuple(Final) of
    true ->
      case Final of
        { match, _, { var, _, UserVarName } = UserVar, _ } when UserVarName /= '_' ->
          { [UserVar,AssignExpr,Final|RawClauseExprs], S };
        _ ->
          { StorageVar, SS } = elixir_scope:build_erl_var(Line, S),
          StorageExpr = { match, Line, StorageVar, Final },
          { [StorageVar,AssignExpr,StorageExpr|RawClauseExprs], SS }
      end;
    false ->
      { [Final,AssignExpr|RawClauseExprs], S }
  end,

  FinalClause = setelement(5, Clause, lists:reverse(FinalClauseExprs)),
  expand_clauses(Line, T, V, LeftVars, FinalVars, [FinalClause|Acc], FS);

expand_clauses(_Line, [], [], _LeftVars, _FinalVars, Acc, S) ->
  { lists:reverse(Acc), S }.

% Handle each key/value clause pair and translate them accordingly.

each_clause(Line, { do, [Condition], Expr }, S) ->
  assigns_block(Line, fun elixir_translator:translate_each/2, Condition, [Expr], S);

each_clause(Line, { else, [Condition], Expr }, S) ->
  assigns_block(Line, fun elixir_translator:translate_each/2, Condition, [Expr], S);

each_clause(Line, { 'after', [Condition], Expr }, S) ->
  { TCondition, SC } = elixir_translator:translate_each(Condition, S),
  { TBody, SB } = elixir_translator:translate([Expr], SC),
  { { clause, Line, [TCondition], [], TBody }, SB };

each_clause(Line, { Key, [_|_], _ }, S) when Key == do; Key == 'after' ->
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "too many arguments given for ~s", [Key]);

each_clause(Line, { Key, _, _ }, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "invalid key ~s", [Key]).

% Check if the given expression is a match tuple.
% This is a small optimization to allow us to change
% existing assignments instead of creating new ones every time.

has_match_tuple({'receive', _, _, _, _}) ->
  true;

has_match_tuple({'receive', _, _}) ->
  true;

has_match_tuple({'case', _, _, _}) ->
  true;

has_match_tuple({match, _, _, _}) ->
  true;

has_match_tuple(H) when is_tuple(H) ->
  has_match_tuple(tuple_to_list(H));

has_match_tuple(H) when is_list(H) ->
  lists:any(fun has_match_tuple/1, H);

has_match_tuple(_) -> false.

% Normalize the given var checking its existence in the scope var dictionary.

normalize_vars({ Var, Kind } = Key, Shared, #elixir_scope{vars=Vars,clause_vars=ClauseVars} = S) ->
  { NewValue, S1 } =
    case orddict:find(Key, Shared) of
      { ok, SharedValue } ->
        { SharedValue, S };
      error ->
        { { _, _, ErlValue }, ErlS } = case (Kind == quoted) or (S#elixir_scope.noname) of
          true  -> elixir_scope:build_erl_var(0, S);
          false -> elixir_scope:build_erl_var(0, Var, "_@" ++ atom_to_list(Var), S)
        end,
        { ErlValue, ErlS }
    end,

  S2 = S1#elixir_scope{
    vars=orddict:store(Key, NewValue, Vars),
    clause_vars=orddict:store(Key, NewValue, ClauseVars)
  },

  Expr = case orddict:find(Key, Vars) of
    { ok, OldValue } -> { var, 0, OldValue };
    error -> { atom, 0, nil }
  end,

  { { Var, Kind, NewValue, Expr }, S2 }.

% Normalize a var by checking if it was defined in the clause.
% If so, use it, otherwise use from main scope.

normalize_clause_var(Var, Kind, OldValue, ClauseVars) ->
  case orddict:find({ Var, Kind }, ClauseVars) of
    { ok, ClauseValue } -> { var, 0, ClauseValue };
    error -> OldValue
  end.

%% generate_match

generate_match(Line, [Left], [Right]) ->
  { match, Line, Left, Right };

generate_match(Line, LeftVars, RightVars) ->
  { match, Line, { tuple, Line, LeftVars }, { tuple, Line, RightVars } }.

%% Listify

listify(Expr) when not is_list(Expr) -> [Expr];
listify(Expr) -> Expr.