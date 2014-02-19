%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([match/3, clause/7, clauses/4, get_pairs/2, get_pairs/3,
  extract_splat_guards/1, extract_guards/1]).
-include("elixir.hrl").

%% Get pairs from a clause.

get_pairs(Key, Clauses) ->
  get_pairs(Key, Clauses, false).
get_pairs(Key, Clauses, AllowNil) ->
  case lists:keyfind(Key, 1, Clauses) of
    { Key, Pairs } when is_list(Pairs) ->
      [{ Key, Meta, Left, Right } || { '->', Meta, [Left, Right] } <- Pairs];
    { Key, nil } when AllowNil ->
      [];
    false ->
      []
  end.

%% Translate matches

match(Fun, Args, #elixir_scope{context=Context, match_vars=MatchVars,
    backup_vars=BackupVars, vars=Vars} = S) when Context /= match ->
  { Result, NewS } = match(Fun, Args, S#elixir_scope{context=match,
                       match_vars=ordsets:new(), backup_vars=Vars}),
  { Result, NewS#elixir_scope{context=Context,
      match_vars=MatchVars, backup_vars=BackupVars} };
match(Fun, Args, S) -> Fun(Args, S).

%% Translate clauses with args, guards and expressions

clause(Line, Fun, Args, Expr, Guards, Return, S) when is_integer(Line) ->
  { TArgs, SA } = match(Fun, Args, S#elixir_scope{extra_guards=[]}),
  { TExpr, SE } = elixir_translator:translate_block(Expr, Return, SA#elixir_scope{extra_guards=nil}),

  SG    = SA#elixir_scope{context=guard, extra_guards=nil},
  Extra = SA#elixir_scope.extra_guards,

  TGuards = case Guards of
    [] -> case Extra of [] -> []; _ -> [Extra] end;
    _  -> [translate_guard(Line, Guard, Extra, SG) || Guard <- Guards]
  end,

  { { clause, Line, TArgs, TGuards, unblock(TExpr) }, SE }.

% Translate/Extract guards from the given expression.

translate_guard(Line, Guard, Extra, S) ->
  [element(1, elixir_translator:translate(elixir_quote:linify(Line, Guard), S))|Extra].

extract_guards({ 'when', _, [Left, Right] }) -> { Left, extract_or_guards(Right) };
extract_guards(Else) -> { Else, [] }.

extract_or_guards({ 'when', _, [Left, Right] }) -> [Left|extract_or_guards(Right)];
extract_or_guards(Term) -> [Term].

% Extract guards when multiple left side args are allowed.

extract_splat_guards([{ 'when', _, [_,_|_] = Args }]) ->
  { Left, Right } = elixir_utils:split_last(Args),
  { Left, extract_or_guards(Right) };
extract_splat_guards(Else) ->
  { Else, [] }.

% Function for translating macros with match style like case and receive.

clauses(Meta, Clauses, Return, #elixir_scope{export_vars=CV} = S) ->
  { TC, TS } = do_clauses(Meta, Clauses, Return, S#elixir_scope{export_vars=[]}),
  { TC, TS#elixir_scope{export_vars=elixir_scope:merge_opt_vars(CV, TS#elixir_scope.export_vars)} }.

do_clauses(_Meta, [], _Return, S) ->
  { [], S };

do_clauses(Meta, DecoupledClauses, Return, S) ->
  % Transform tree just passing the variables counter forward
  % and storing variables defined inside each clause.
  Transformer = fun(X, {SAcc, VAcc}) ->
    { TX, TS } = each_clause(X, Return, SAcc),
    { TX,
      { elixir_scope:mergec(S, TS), [TS#elixir_scope.export_vars|VAcc] } }
  end,

  { TClauses, { TS, ReverseCV } } =
    lists:mapfoldl(Transformer, {S, []}, DecoupledClauses),

  % Now get all the variables defined inside each clause
  CV = lists:reverse(ReverseCV),
  AllVars = lists:foldl(fun elixir_scope:merge_vars/2, [], CV),

  % Create a new scope that contains a list of all variables
  % defined inside all the clauses. It returns this new scope and
  % a list of tuples where the first element is the variable name,
  % the second one is the new pointer to the variable and the third
  % is the old pointer.
  { FinalVars, FS } = lists:mapfoldl(fun({ Key, Val }, Acc) ->
    normalize_vars(Key, Val, Acc)
  end, TS, AllVars),

  % Expand all clauses by adding a match operation at the end
  % that defines variables missing in one clause to the others.
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
              { VarName, _, SS } = elixir_scope:build_var('_', S),
              StorageVar  = { var, Line, VarName },
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

each_clause({ do, Meta, [Condition], Expr }, Return, S) ->
  { Arg, Guards } = extract_guards(Condition),
  clause(?line(Meta), fun elixir_translator:translate_args/2, [Arg], Expr, Guards, Return, S);

each_clause({ else, Meta, [Condition], Expr }, Return, S) ->
  { Arg, Guards } = extract_guards(Condition),
  clause(?line(Meta), fun elixir_translator:translate_args/2, [Arg], Expr, Guards, Return, S);

each_clause({ 'after', Meta, [Condition], Expr }, Return, S) ->
  { TCondition, SC } = elixir_translator:translate(Condition, S),
  { TExpr, SB } = elixir_translator:translate_block(Expr, Return, SC),
  { { clause, ?line(Meta), [TCondition], [], unblock(TExpr) }, SB }.

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

normalize_vars(Key, Value, #elixir_scope{vars=Vars,export_vars=ClauseVars} = S) ->
  VS = S#elixir_scope{
    vars=orddict:store(Key, Value, Vars),
    export_vars=orddict:store(Key, Value, ClauseVars)
  },

  Expr = case orddict:find(Key, Vars) of
    { ok, { PreValue, _ } } -> { var, 0, PreValue };
    error -> { atom, 0, nil }
  end,

  { { Key, Value, Expr }, VS }.

% Generate match vars by checking if they were updated
% or not and assigning the previous value.

generate_match_vars([{ Key, Value, Expr }|T], ClauseVars, Left, Right) ->
  case orddict:find(Key, ClauseVars) of
    { ok, Value } ->
      generate_match_vars(T, ClauseVars, Left, Right);
    { ok, Clause } ->
      generate_match_vars(T, ClauseVars,
        [{ var, 0, element(1, Value) }|Left],
        [{ var, 0, element(1, Clause) }|Right]);
    error ->
      generate_match_vars(T, ClauseVars,
        [{ var, 0, element(1, Value) }|Left], [Expr|Right])
  end;

generate_match_vars([], _ClauseVars, Left, Right) ->
  { Left, Right }.

generate_match(Line, [Left], [Right]) ->
  { match, Line, Left, Right };

generate_match(Line, LeftVars, RightVars) ->
  { match, Line, { tuple, Line, LeftVars }, { tuple, Line, RightVars } }.

unblock({ 'block', _, Exprs }) -> Exprs;
unblock(Exprs) -> [Exprs].
