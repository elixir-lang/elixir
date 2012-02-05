%% Handle code related to match/after/else and guard
%% clauses for receive/case/fn and friends. try is
%% handled in elixir_try.
-module(elixir_try).
-export([clauses/3]).
-import(elixir_translator, [translate/2, translate_each/2]).
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
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no condition given for catch");
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "too many conditions given for catch")
  end,

  Condition = { '{}', Line, Final },
  elixir_clauses:assigns_block(Line, fun elixir_translator:translate_each/2, Condition, [Expr], Guards, S);

each_clause(Line, { rescue, Args, Expr }, S) ->
  validate_args(rescue, Line, Args, 3, S),
  [Condition] = Args,
  { Left, Right } = normalize_rescue(Line, Condition, S),

  case Left of
    { '_', _, _ } ->
      case Right of
        nil ->
          each_clause(Line, { 'catch', [error, Left], Expr }, S);
        _ ->
          { ClauseVar, CS } = elixir_variables:build_ex(Line, S),
          { Clause, _ } = rescue_guards(Line, ClauseVar, Right, S),
          each_clause(Line, { 'catch', [error, Clause], Expr }, CS)
      end;
    _ ->
      { Clause, Safe } = rescue_guards(Line, Left, Right, S),
      case Safe of
        true ->
          each_clause(Line, { 'catch', [error, Clause], Expr }, S);
        false ->
          { ClauseVar, CS }  = elixir_variables:build_ex(Line, S),
          { FinalClause, _ } = rescue_guards(Line, ClauseVar, Right, S),
          Match = { '=', Line, [
            Left,
            { { '.', Line, ['::Exception', normalize] }, Line, [ClauseVar] }
          ] },
          FinalExpr = prepend_to_block(Line, Match, Expr),
          each_clause(Line, { 'catch', [error, FinalClause], FinalExpr }, CS)
      end
  end;

each_clause(Line, {Key,_,_}, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid key ~s in try", [Key]).

%% Helpers

validate_args(Clause, Line, [], _, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no condition given for ~s in try", [Clause]);

validate_args(Clause, Line, List, Max, S) when length(List) > Max ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "too many conditions given for ~s in try", [Clause]);

validate_args(_, _, _, _, _) -> [].

%% rescue [Error] -> _ in [Error]
normalize_rescue(Line, List, S) when is_list(List) ->
  normalize_rescue(Line, { in, Line, [{ '_', Line, nil }, List] }, S);

%% rescue ^var -> _ in [var]
normalize_rescue(_, { '^', Line, [Var] }, S) ->
  normalize_rescue(Line, { in, Line, [{ '_', Line, nil }, [Var]] }, S);

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
    _ when is_list(Right), Right /= [] ->
      { _, Refs } = lists:partition(fun(X) -> is_var(X) end, Right),
      { TRefs, _ } = translate(Refs, S),
      case lists:all(fun(X) -> is_tuple(X) andalso element(1, X) == atom end, TRefs) of
        true -> { Left, Right };
        false -> normalize_rescue(Line, nil, S)
      end;
    _ -> normalize_rescue(Line, nil, S)
  end;

%% rescue ErlangError -> _ in [ErlangError]
normalize_rescue(_, { Name, Line, _ } = Rescue, S) when is_atom(Name) ->
  normalize_rescue(Line, { in, Line, [{ '_', Line, nil }, [Rescue]] }, S);

normalize_rescue(Line, _, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid condition given for rescue in try").

%% Convert rescue clauses into guards.
rescue_guards(_, Var, nil, _) -> { Var, false };

rescue_guards(Line, Var, Guards, S) ->
  { RawElixir, RawErlang } = rescue_each_var(Line, Var, Guards),
  { Elixir, Erlang, Safe } = rescue_each_ref(Line, Var, Guards, RawElixir, RawErlang, RawErlang == [], S),

  Final = case Elixir == [] of
    true  -> Erlang;
    false ->
      IsTuple     = { is_tuple, Line, [Var] },
      IsException = { '==', Line, [
        { element, Line, [2, Var] },
        { '__EXCEPTION__', Line, nil }
      ] },
      OrElse = join(Line, 'orelse', Elixir),
      [join(Line, '&', [IsTuple, IsException, OrElse])|Erlang]
  end,
  {
    { 'when', Line, [Var, join(Line, '|', Final)] },
    Safe
  }.

%% Handle variables in the right side of rescue.

rescue_each_var(Line, ClauseVar, Guards) ->
  Vars = [Var || Var <- Guards, is_var(Var)],

  case Vars == [] of
    true  -> { [], [] };
    false ->
      Elixir = [exception_compare(Line, ClauseVar, Var) || Var <- Vars],
      Erlang = lists:map(fun(Rescues) ->
        Compares = [{ '==', Line, [Rescue, Var] } || Var <- Vars, Rescue <- Rescues],
        { 'andalso', Line, [
          join(Line, 'orelse', Compares),
          erlang_rescue_guard_for(Line, ClauseVar, Rescues)
        ] }
      end, erlang_rescues()),
      { Elixir, Erlang }
  end.

%% Rescue each reference considering their Erlang or Elixir matches.
%% Matching of variables is done separatewith Erlang exceptions is done in another
%% method for optimization.

%% Ignore variables
rescue_each_ref(Line, Var, [{ Name, _, Atom }|T], Elixir, Erlang, Safe, S) when is_atom(Name), is_atom(Atom) ->
  rescue_each_ref(Line, Var, T, Elixir, Erlang, Safe, S);

rescue_each_ref(Line, Var, [H|T], Elixir, Erlang, _Safe, S) when
  H == '::UndefinedFunctionError'; H == '::ErlangError';
  H == '::ArgumentError'; H == '::ArithmeticError';
  H == '::BadArityError'; H == '::BadFunctionError';
  H == '::MatchError'; H == '::CaseClauseError';
  H == '::FunctionClauseError'; H == '::SystemLimitError' ->
  Expr = erlang_rescue_guard_for(Line, Var, H),
  rescue_each_ref(Line, Var, T, Elixir, [Expr|Erlang], false, S);

rescue_each_ref(Line, Var, [H|T], Elixir, Erlang, Safe, S) when is_atom(H) ->
  rescue_each_ref(Line, Var, T, [exception_compare(Line, Var, H)|Elixir], Erlang, Safe, S);

rescue_each_ref(Line, Var, [H|T], Elixir, Erlang, Safe, S) ->
  case translate_each(H, S) of
    { { atom, _, Atom }, _ } ->
      rescue_each_ref(Line, Var, [Atom|T], Elixir, Erlang, Safe, S);
    _ ->
      rescue_each_ref(Line, Var, T, [exception_compare(Line, Var, H)|Elixir], Erlang, Safe, S)
  end;

rescue_each_ref(_, _, [], Elixir, Erlang, Safe, _) ->
  { Elixir, Erlang, Safe }.

%% Handle erlang rescue matches.

erlang_rescues() ->
  [
    ['::UndefinedFunctionError', '::ArgumentError', '::ArithmeticError', '::BadArityError',
     '::BadFunctionError', '::MatchError', '::CaseClauseError', '::FunctionClauseError',
     '::SystemLimitError'],
    ['::ErlangError']
  ].

erlang_rescue_guard_for(Line, Var, List) when is_list(List) ->
  join(Line, 'orelse', [erlang_rescue_guard_for(Line, Var, X) || X <- List]);

erlang_rescue_guard_for(Line, Var, '::UndefinedFunctionError') ->
  { '==', Line, [Var, undef] };

erlang_rescue_guard_for(Line, Var, '::FunctionClauseError') ->
  { '==', Line, [Var, function_clause] };

erlang_rescue_guard_for(Line, Var, '::SystemLimitError') ->
  { '==', Line, [Var, system_limit] };

erlang_rescue_guard_for(Line, Var, '::ArithmeticError') ->
  { '==', Line, [Var, badarith] };

erlang_rescue_guard_for(Line, Var, '::BadArityError') ->
  { 'andalso', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badarity)
  ] };

erlang_rescue_guard_for(Line, Var, '::BadFunctionError') ->
  { 'andalso', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badfun)
  ] };

erlang_rescue_guard_for(Line, Var, '::MatchError') ->
  { 'andalso', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badmatch)
  ] };

erlang_rescue_guard_for(Line, Var, '::CaseClauseError') ->
  { 'andalso', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, case_clause)
  ] };

erlang_rescue_guard_for(Line, Var, '::ArgumentError') ->
  { 'orelse', Line, [
    { '==', Line, [Var, badarg] },
    { 'andalso', Line, [
      { is_tuple, Line, [Var] },
      exception_compare(Line, Var, badarg)
    ] }
  ] };

erlang_rescue_guard_for(Line, Var, '::ErlangError') ->
  IsNotTuple  = { 'not', Line, [{ is_tuple, Line, [Var] }] },
  IsException = { '!=', Line, [
    { element, Line, [2, Var] },
    { '__EXCEPTION__', Line, nil }
  ] },
  { 'orelse', Line, [IsNotTuple, IsException] }.

%% Join the given expression forming a tree according to the given kind.

is_var({ Name, _, Atom }) when is_atom(Name), is_atom(Atom) -> true;
is_var(_) -> false.

exception_compare(Line, Var, Expr) ->
  { '==', Line, [
    { element, Line, [1, Var] },
    Expr
  ] }.

join(Line, Kind, [H|T]) ->
  lists:foldl(fun(X, Acc) -> { Kind, Line, [Acc, X] } end, H, T).

prepend_to_block(_Line, Expr, { '__BLOCK__', Line, Args }) ->
  { '__BLOCK__', Line, [Expr|Args] };

prepend_to_block(Line, Expr, Args) ->
  { '__BLOCK__', Line, [Expr, Args] }.