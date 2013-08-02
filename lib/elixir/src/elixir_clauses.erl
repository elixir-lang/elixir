%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([
  assigns/3, assigns_block/5, assigns_block/6, extract_splat_guards/1,
  get_pairs/4, get_pairs/5, match/3, extract_args/1, extract_guards/1]).
-include("elixir.hrl").

%% Get pairs from a clause.

get_pairs(Meta, Key, Clauses, S) ->
  get_pairs(Meta, Key, Clauses, S, false).

get_pairs(Meta, Key, Clauses, S, AllowNil) ->
  case lists:keyfind(Key, 1, Clauses) of
    { Key, { '->', _, Pairs } } ->
      [{ Key, PMeta, Left, Right } || { Left, PMeta, Right } <- Pairs];
    { Key, nil } when AllowNil ->
      [];
    { Key, _ } ->
      elixir_errors:syntax_error(Meta, S#elixir_scope.file, "expected pairs with -> for key ~ts", [Key]);
    _ ->
      []
  end.

% Function for translating assigns.

assigns(Fun, Args, #elixir_scope{context=Context, temp_vars=TempVars,
    backup_vars=BackupVars, vars=Vars} = S) when Context /= match ->
  { Result, NewS } = assigns(Fun, Args, S#elixir_scope{context=match,
                       temp_vars=ordsets:new(), backup_vars=Vars}),
  { Result, NewS#elixir_scope{context=Context,
      temp_vars=TempVars, backup_vars=BackupVars} };

assigns(Fun, Args, S) -> Fun(Args, S).

%% Function for translating a block that is preceeded by an
%% assignment and optional guards. This is used by def* and fn.

assigns_block(Line, Fun, BareArgs, Exprs, S) ->
  { Args, Guards } = extract_guards(BareArgs),
  assigns_block(Line, Fun, Args, Exprs, Guards, S).

assigns_block(Line, Fun, Args, Exprs, Guards, S) when is_integer(Line) ->
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

% Extract guards when multiple left side args are allowed.

extract_splat_guards([{ 'when', _, [_,_|_] = Args }]) ->
  { Left, Right } = elixir_tree_helpers:split_last(Args),
  { Left, extract_or_clauses(Right, []) };
extract_splat_guards(Else) ->
  { Else, [] }.

% Function for translating macros with match style like case and receive.

match(Meta, Clauses, #elixir_scope{clause_vars=C1} = S) ->
  { TC, TS } = do_match(Meta, Clauses, S#elixir_scope{clause_vars=orddict:new()}),
  C2 = TS#elixir_scope.clause_vars,
  { TC, TS#elixir_scope{clause_vars=elixir_scope:merge_clause_vars(C1, C2)} }.

do_match(_Meta, [], S) ->
  { [], S };

do_match(_Meta, [DecoupledClause], S) ->
  { TDecoupledClause, TS } = each_clause(DecoupledClause, S),
  { [TDecoupledClause], TS };

do_match(Meta, DecoupledClauses, S) ->
  % Transform tree just passing the variables counter forward
  % and storing variables defined inside each clause.
  Transformer = fun(X, {Acc, CV}) ->
    { TX, TAcc } = each_clause(X, Acc),
    { TX, { merge_clauses_scope(S, TAcc), [TAcc#elixir_scope.clause_vars|CV] } }
  end,

  { TClauses, { TS, ReverseCV } } = lists:mapfoldl(Transformer, {S, []}, DecoupledClauses),

  % Now get all the variables defined inside each clause
  CV = lists:reverse(ReverseCV),
  AllVars = lists:foldl(fun(KV, Acc) ->
    elixir_scope:merge_clause_vars(Acc, KV)
  end, orddict:new(), CV),

  % Create a new scope that contains a list of all variables
  % defined inside all the clauses. It returns this new scope and
  % a list of tuples where the first element is the variable name,
  % the second one is the new pointer to the variable and the third
  % is the old pointer.
  { FinalVars, FS } = lists:mapfoldl(fun({ Key, Ref }, Acc) ->
    normalize_vars(Key, Ref, Acc)
  end, TS, AllVars),

  % Expand all clauses by adding a match operation at the end
  % that assigns variables missing in one clause to the others.
  expand_clauses(?line(Meta), TClauses, CV, FinalVars, [], FS).

expand_clauses(Line, [Clause|T], [ClauseVars|V], FinalVars, Acc, S) ->
  case generate_match_vars(FinalVars, ClauseVars, [], []) of
    { [], [] } ->
      expand_clauses(Line, T, V, FinalVars, [Clause|Acc], S);
    { Left, Right } ->
      MatchExpr   = generate_match(Line, Left, Right),
      ClauseExprs = element(5, Clause),
      [Final|RawClauseExprs] = lists:reverse(ClauseExprs),

      % If the last sentence has a match clause, we need to assign its value
      % in the variable list. If not, we insert the variable list before the
      % final clause in order to keep it tail call optimized.
      { FinalClauseExprs, FS } = case has_match_tuple(Final) of
        true ->
          case Final of
            { match, _, { var, _, UserVarName } = UserVar, _ } when UserVarName /= '_' ->
              { [UserVar,MatchExpr,Final|RawClauseExprs], S };
            _ ->
              { StorageVar, SS } = elixir_scope:build_erl_var(Line, S),
              StorageExpr = { match, Line, StorageVar, Final },
              { [StorageVar,MatchExpr,StorageExpr|RawClauseExprs], SS }
          end;
        false ->
          { [Final,MatchExpr|RawClauseExprs], S }
      end,

      FinalClause = setelement(5, Clause, lists:reverse(FinalClauseExprs)),
      expand_clauses(Line, T, V, FinalVars, [FinalClause|Acc], FS)
  end;

expand_clauses(_Line, [], [], _FinalVars, Acc, S) ->
  { lists:reverse(Acc), S }.

% Handle each key/value clause pair and translate them accordingly.

each_clause({ do, Meta, [Condition], Expr }, S) ->
  assigns_block(?line(Meta), fun elixir_translator:translate_each/2, Condition, [Expr], S);

each_clause({ else, Meta, [Condition], Expr }, S) ->
  assigns_block(?line(Meta), fun elixir_translator:translate_each/2, Condition, [Expr], S);

each_clause({ 'after', Meta, [Condition], Expr }, S) ->
  { TCondition, SC } = elixir_translator:translate_each(Condition, S),
  { TBody, SB } = elixir_translator:translate([Expr], SC),
  { { clause, ?line(Meta), [TCondition], [], TBody }, SB };

each_clause({ Key, Meta, [_|_], _ }, S) when Key == do; Key == 'after' ->
  elixir_errors:syntax_error(Meta, S#elixir_scope.file, "too many arguments given for ~ts", [Key]);

each_clause({ Key, Meta, _, _ }, S) ->
  elixir_errors:syntax_error(Meta, S#elixir_scope.file, "invalid key ~ts", [Key]).

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

has_match_tuple({'fun', _, { clauses, _ }}) ->
  false;

has_match_tuple(H) when is_tuple(H) ->
  has_match_tuple(tuple_to_list(H));

has_match_tuple(H) when is_list(H) ->
  lists:any(fun has_match_tuple/1, H);

has_match_tuple(_) -> false.

% Normalize the given var in between clauses
% by picking one value as reference and retriving
% its previous value.

normalize_vars(Key, Value, #elixir_scope{vars=Vars,clause_vars=ClauseVars} = S) ->
  FS = S#elixir_scope{
    vars=orddict:store(Key, Value, Vars),
    clause_vars=orddict:store(Key, Value, ClauseVars)
  },

  Expr = case orddict:find(Key, Vars) of
    { ok, OldValue } -> { var, 0, OldValue };
    error -> { atom, 0, nil }
  end,

  { { Key, Value, Expr }, FS }.

% Generate match vars by checking if they were updated
% or not and assigning the previous value.

generate_match_vars([{ Key, NewValue, OldValue }|T], ClauseVars, Left, Right) ->
  case orddict:find(Key, ClauseVars) of
    { ok, NewValue } ->
      generate_match_vars(T, ClauseVars, Left, Right);
    { ok, ClauseValue } ->
      generate_match_vars(T, ClauseVars, [{ var, 0, NewValue }|Left], [{ var, 0, ClauseValue }|Right]);
    error ->
      generate_match_vars(T, ClauseVars, [{ var, 0, NewValue }|Left], [OldValue|Right])
  end;

generate_match_vars([], _ClauseVars, Left, Right) ->
  { Left, Right }.

generate_match(Line, [Left], [Right]) ->
  { match, Line, Left, Right };

generate_match(Line, LeftVars, RightVars) ->
  { match, Line, { tuple, Line, LeftVars }, { tuple, Line, RightVars } }.

listify(Expr) when not is_list(Expr) -> [Expr];
listify(Expr) -> Expr.

%% We don't use umergec because imports, aliases and
%% what not are not passed from one clause to the other.
merge_clauses_scope(S1, S2) ->
  S1#elixir_scope{
    counter=S2#elixir_scope.counter,
    extra_guards=S2#elixir_scope.extra_guards,
    super=S1#elixir_scope.super orelse S2#elixir_scope.super,
    caller=S1#elixir_scope.caller orelse S2#elixir_scope.caller
  }.