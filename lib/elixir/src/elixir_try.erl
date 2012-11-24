-module(elixir_try).
-export([clauses/3, format_error/1]).
-import(elixir_scope, [umergec/2]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

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
      elixir_errors:syntax_error(Line, S#elixir_scope.file, "too many arguments given for catch")
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
                { { '.', Line, ['Elixir.Exception', normalize] }, Line, [ClauseVar] }
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
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "too many arguments given for rescue");

each_clause(Line, {Key,_,_}, S) ->
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "invalid key ~s in try", [Key]).

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
    _ when is_list(Right) ->
      is_valid_rescue_list(Right, S) andalso { Left, Right };
    _ ->
      Expanded = 'Elixir.Macro':expand(Right, elixir_scope:to_ex_env({ Line, S })),
      case is_valid_rescue_list(Expanded, S) of
        true  -> { Left, Expanded };
        false ->
          elixir_errors:syntax_error(Line, S#elixir_scope.file, "invalid use of operator \"in\" in rescue inside try")
      end
  end;

normalize_rescue(_, { '=', Line, [{ '__aliases__', _, _ } = Alias, { Name, _, Atom } = Var] }, S)
    when is_atom(Name) and is_atom(Atom) ->
  elixir_errors:handle_file_warning(S#elixir_scope.file, { Line, ?MODULE, { rescue_no_match, Var, Alias } }),
  false;

normalize_rescue(_, { '=', Line, [{ Name, _, Atom } = Var, { '__aliases__', _, _ } = Alias] }, S)
    when is_atom(Name) and is_atom(Atom) ->
  elixir_errors:handle_file_warning(S#elixir_scope.file, { Line, ?MODULE, { rescue_no_match, Var, Alias } }),
  false;

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
  H == 'Elixir.UndefinedFunctionError'; H == 'Elixir.ErlangError';
  H == 'Elixir.ArgumentError'; H == 'Elixir.ArithmeticError';
  H == 'Elixir.BadArityError'; H == 'Elixir.BadFunctionError';
  H == 'Elixir.MatchError'; H == 'Elixir.CaseClauseError';
  H == 'Elixir.FunctionClauseError'; H == 'Elixir.SystemLimitError' ->
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
    'Elixir.UndefinedFunctionError', 'Elixir.ArgumentError', 'Elixir.ArithmeticError', 'Elixir.BadArityError',
    'Elixir.BadFunctionError', 'Elixir.MatchError', 'Elixir.CaseClauseError', 'Elixir.FunctionClauseError',
    'Elixir.SystemLimitError', 'Elixir.ErlangError'
  ].

erlang_rescue_guard_for(Line, Var, List) when is_list(List) ->
  join(Line, 'or', [erlang_rescue_guard_for(Line, Var, X) || X <- List]);

erlang_rescue_guard_for(Line, Var, 'Elixir.UndefinedFunctionError') ->
  { '==', Line, [Var, undef] };

erlang_rescue_guard_for(Line, Var, 'Elixir.FunctionClauseError') ->
  { '==', Line, [Var, function_clause] };

erlang_rescue_guard_for(Line, Var, 'Elixir.SystemLimitError') ->
  { '==', Line, [Var, system_limit] };

erlang_rescue_guard_for(Line, Var, 'Elixir.ArithmeticError') ->
  { '==', Line, [Var, badarith] };

erlang_rescue_guard_for(Line, Var, 'Elixir.BadArityError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badarity)
  ] };

erlang_rescue_guard_for(Line, Var, 'Elixir.BadFunctionError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badfun)
  ] };

erlang_rescue_guard_for(Line, Var, 'Elixir.MatchError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, badmatch)
  ] };

erlang_rescue_guard_for(Line, Var, 'Elixir.CaseClauseError') ->
  { 'and', Line, [
    { is_tuple, Line, [Var] },
    exception_compare(Line, Var, case_clause)
  ] };

erlang_rescue_guard_for(Line, Var, 'Elixir.ArgumentError') ->
  { 'or', Line, [
    { '==', Line, [Var, badarg] },
    { 'and', Line, [
      { is_tuple, Line, [Var] },
      exception_compare(Line, Var, badarg)
    ] }
  ] };

erlang_rescue_guard_for(Line, Var, 'Elixir.ErlangError') ->
  IsNotTuple  = { 'not', Line, [{ is_tuple, Line, [Var] }] },
  IsException = { '!=', Line, [
    { { '.', Line, [ erlang, element ] }, Line, [2, Var] }, '__exception__'
  ] },
  { 'or', Line, [IsNotTuple, IsException] }.

%% Validate rescue access

validate_rescue_access(Line, { '=', _, [Left, Right] }, S) ->
  validate_rescue_access(Line, Left, S),
  validate_rescue_access(Line, Right, S);

validate_rescue_access(Line, { { '.', _, ['Elixir.Kernel', 'access'] }, _, [Element, _] }, S) ->
  case elixir_translator:translate_each(Element, S) of
    { { atom, _, Atom }, _ } ->
      case lists:member(Atom, erlang_rescues()) of
        false -> [];
        true -> elixir_errors:syntax_error(Line, S#elixir_scope.file, "cannot (yet) pattern match against erlang exceptions")
      end;
    _ -> []
  end;

validate_rescue_access(_, _, _) -> [].

%% Helpers

format_error({ rescue_no_match, Var, Alias }) ->
  VarBinary   = 'Elixir.Macro':to_binary(Var),
  AliasBinary = 'Elixir.Macro':to_binary(Alias),
  Message = "rescue clause (~s = ~s) can never match, maybe you meant to write: ~s in [~s] ?",
  io_lib:format(Message, [AliasBinary, VarBinary, VarBinary, AliasBinary]).

is_var({ Name, _, Atom }) when is_atom(Name), is_atom(Atom) -> true;
is_var(_) -> false.

is_erl_var_or_atom({ Kind, Line, Name }) when Kind == var orelse Kind == atom, is_integer(Line), is_atom(Name) -> true;
is_erl_var_or_atom(_) -> false.

is_valid_rescue_list(Right, S) when is_list(Right) ->
  { TRight, _ } = elixir_translator:translate(Right, S),
  lists:all(fun is_erl_var_or_atom/1, TRight).

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