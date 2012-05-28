-module(elixir_try).
-export([clauses/3]).
-import(elixir_scope, [umergec/2]).
-include("elixir.hrl").

clauses(Line, Clauses, S) ->
  Catch  = elixir_clauses:get_pairs(Line, 'catch', Clauses, S),
  Rescue = elixir_clauses:get_pairs(Line, rescue, Clauses, S),
  Transformer = fun(X, Acc) -> each_clause(Line, X, umergec(S, Acc)) end,
  lists:mapfoldl(Transformer, S, Rescue ++ Catch).

each_clause(Line, { 'catch', Raw, Expr }, S) ->
  { Args, Guards } = elixir_clauses:extract_last_guards(Raw),

  Final = case Args of
    [X]     -> [throw, X, { '_', Line, nil }];
    [X,Y]   -> [X, Y, { '_', Line, nil }];
    [_,_,_] -> Args;
    _       ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "too many arguments given for catch")
  end,

  Condition = { '{}', Line, Final },
  elixir_clauses:assigns_block(Line, fun elixir_translator:translate_each/2, Condition, [Expr], Guards, S);

each_clause(Line, { rescue, [Condition], Expr }, S) ->
  case normalize_rescue(Line, Condition, S) of
    { Left, Right } ->
      case Left of
        { '_', _, _ } ->
          { ClauseVar, CS } = elixir_scope:build_ex_var(Line, S),
          { Clause, _ } = rescue_guards(Line, ClauseVar, Right, S),
          each_clause(Line, { 'catch', [error, Clause], Expr }, CS);
        _ ->
          { Clause, Safe } = rescue_guards(Line, Left, Right, S),
          case Safe of
            true ->
              each_clause(Line, { 'catch', [error, Clause], Expr }, S);
            false ->
              { ClauseVar, CS }  = elixir_scope:build_ex_var(Line, S),
              { FinalClause, _ } = rescue_guards(Line, ClauseVar, Right, S),
              Match = { '=', Line, [
                Left,
                { { '.', Line, ['__MAIN__.Exception', normalize] }, Line, [ClauseVar] }
              ] },
              FinalExpr = prepend_to_block(Line, Match, Expr),
              each_clause(Line, { 'catch', [error, FinalClause], FinalExpr }, CS)
          end
      end;
    _ ->
      Result = each_clause(Line, { 'catch', [error, Condition], Expr }, S),
      validate_rescue_access(Line, Condition, S),
      Result
  end;

each_clause(Line, {rescue,_,_}, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "too many arguments given for rescue");

each_clause(Line, {Key,_,_}, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid key ~s in try", [Key]).

%% Helpers

%% rescue [Error] -> _ in [Error]
normalize_rescue(Line, List, S) when is_list(List) ->
  normalize_rescue(Line, { in, Line, [{ '_', Line, nil }, List] }, S);

%% rescue var -> var in _
normalize_rescue(_, { Name, Line, Atom } = Rescue, S) when is_atom(Name), is_atom(Atom), Name /= '_' ->
  normalize_rescue(Line, { in, Line, [Rescue, { '_', Line, nil }] }, S);

%% rescue var in [Exprs]
normalize_rescue(_, { in, Line, [Left, Right] }, S) ->
  case Right of
    { '_', _, _ } ->
      { Left, nil };
    _ when is_list(Right), Right /= [] ->
      { _, Aliases } = lists:partition(fun(X) -> is_var(X) end, Right),
      { TAliases, _ } = elixir_translator:translate(Aliases, S),
      case lists:all(fun(X) -> is_tuple(X) andalso element(1, X) == atom end, TAliases) of
        true -> { Left, Right };
        false -> normalize_rescue(Line, nil, S)
      end;
    _ ->
      elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid use of operator \"in\" in rescue inside try")
  end;

normalize_rescue(Line, Condition, S) ->
  case elixir_translator:translate_each(Condition, S#elixir_scope{context=assign}) of
    { { atom, _, Atom }, _ } ->
      normalize_rescue(Line, { in, Line, [{ '_', Line, nil }, [Atom]] }, S);
    _ ->
      false
  end.

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
        { { '.', Line, [erlang, element] }, Line, [2, Var] }, '__exception__'
      ] },
      OrElse = join(Line, 'or', Elixir),
      [join(Line, 'and', [IsTuple, IsException, OrElse])|Erlang]
  end,
  {
    { 'when', Line, [Var, reverse_join(Line, 'when', Final)] },
    Safe
  }.

%% Handle variables in the right side of rescue.

rescue_each_var(Line, ClauseVar, Guards) ->
  Vars = [Var || Var <- Guards, is_var(Var)],

  case Vars == [] of
    true  -> { [], [] };
    false ->
      Elixir = [exception_compare(Line, ClauseVar, Var) || Var <- Vars],
      Erlang = lists:map(fun(Rescue) ->
        Compares = [{ '==', Line, [Rescue, Var] } || Var <- Vars],
        { 'and', Line, [
          erlang_rescue_guard_for(Line, ClauseVar, Rescue),
          join(Line, 'or', Compares)
        ] }
      end, erlang_rescues()),
      { Elixir, Erlang }
  end.

%% Rescue each atom name considering their Erlang or Elixir matches.
%% Matching of variables is done with Erlang exceptions is done in another
%% method for optimization.

%% Ignore variables
rescue_each_ref(Line, Var, [{ Name, _, Atom }|T], Elixir, Erlang, Safe, S) when is_atom(Name), is_atom(Atom) ->
  rescue_each_ref(Line, Var, T, Elixir, Erlang, Safe, S);

rescue_each_ref(Line, Var, [H|T], Elixir, Erlang, _Safe, S) when
  H == '__MAIN__.UndefinedFunctionError'; H == '__MAIN__.ErlangError';
  H == '__MAIN__.ArgumentError'; H == '__MAIN__.ArithmeticError';
  H == '__MAIN__.BadArityError'; H == '__MAIN__.BadFunctionError';
  H == '__MAIN__.MatchError'; H == '__MAIN__.CaseClauseError';
  H == '__MAIN__.FunctionClauseError'; H == '__MAIN__.SystemLimitError' ->
  Expr = { 'or', Line, [
    erlang_rescue_guard_for(Line, Var, H),
    exception_compare(Line, Var, H)
  ] },
  rescue_each_ref(Line, Var, T, Elixir, [Expr|Erlang], false, S);

rescue_each_ref(Line, Var, [H|T], Elixir, Erlang, Safe, S) when is_atom(H) ->
  rescue_each_ref(Line, Var, T, [exception_compare(Line, Var, H)|Elixir], Erlang, Safe, S);

rescue_each_ref(Line, Var, [H|T], Elixir, Erlang, Safe, S) ->
  case elixir_translator:translate_each(H, S) of
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
    '__MAIN__.UndefinedFunctionError', '__MAIN__.ArgumentError', '__MAIN__.ArithmeticError', '__MAIN__.BadArityError',
    '__MAIN__.BadFunctionError', '__MAIN__.MatchError', '__MAIN__.CaseClauseError', '__MAIN__.FunctionClauseError',
    '__MAIN__.SystemLimitError', '__MAIN__.ErlangError'
  ].

erlang_rescue_guard_for(Line, Var, List) when is_list(List) ->
  join(Line, 'or', [erlang_rescue_guard_for(Line, Var, X) || X <- List]);

erlang_rescue_guard_for(Line, Var, '__MAIN__.UndefinedFunctionError') ->
  { '==', Line, [Var, undef] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.FunctionClauseError') ->
  { '==', Line, [Var, function_clause] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.SystemLimitError') ->
  { '==', Line, [Var, system_limit] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.ArithmeticError') ->
  { '==', Line, [Var, badarith] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.BadArityError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badarity)
  ] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.BadFunctionError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badfun)
  ] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.MatchError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badmatch)
  ] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.CaseClauseError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, case_clause)
  ] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.ArgumentError') ->
  { 'or', Line, [
    { '==', Line, [Var, badarg] },
    { 'and', Line, [
      { is_tuple, Line, [Var] },
      exception_compare(Line, Var, badarg)
    ] }
  ] };

erlang_rescue_guard_for(Line, Var, '__MAIN__.ErlangError') ->
  IsNotTuple  = { 'not', Line, [{ is_tuple, Line, [Var] }] },
  IsException = { '!=', Line, [
    { { '.', Line, [ erlang, element ] }, Line, [2, Var] }, '__exception__'
  ] },
  { 'or', Line, [IsNotTuple, IsException] }.

%% Validate rescue access

validate_rescue_access(Line, { '=', _, [Left, Right] }, S) ->
  validate_rescue_access(Line, Left, S),
  validate_rescue_access(Line, Right, S);

validate_rescue_access(Line, { 'access', _, [Element, _] }, S) ->
  case elixir_translator:translate_each(Element, S) of
    { { atom, _, Atom }, _ } ->
      case lists:member(Atom, erlang_rescues()) of
        false -> [];
        true -> elixir_errors:syntax_error(Line, S#elixir_scope.filename, "cannot (yet) pattern match against erlang exceptions")
      end;
    _ -> []
  end;

validate_rescue_access(_, _, _) -> [].

%% Helpers

is_var({ Name, _, Atom }) when is_atom(Name), is_atom(Atom) -> true;
is_var(_) -> false.

exception_compare(Line, Var, Expr) ->
  { '==', Line, [
    { { '.', Line, [ erlang, element ] }, Line, [1, Var] },
    Expr
  ] }.

join(Line, Kind, [H|T]) ->
  lists:foldl(fun(X, Acc) -> { Kind, Line, [Acc, X] } end, H, T).

reverse_join(Line, Kind, [H|T]) ->
  lists:foldl(fun(X, Acc) -> { Kind, Line, [X, Acc] } end, H, T).

prepend_to_block(_Line, Expr, { '__block__', Line, Args }) ->
  { '__block__', Line, [Expr|Args] };

prepend_to_block(Line, Expr, Args) ->
  { '__block__', Line, [Expr, Args] }.