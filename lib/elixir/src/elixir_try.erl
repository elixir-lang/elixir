-module(elixir_try).
-export([clauses/3, format_error/1]).
-import(elixir_scope, [umergec/2]).
-include("elixir.hrl").

clauses(Meta, Clauses, S) ->
  Catch  = elixir_clauses:get_pairs(Meta, 'catch', Clauses, S),
  Rescue = elixir_clauses:get_pairs(Meta, rescue, Clauses, S),
  Transformer = fun(X, Acc) -> each_clause(X, umergec(S, Acc)) end,
  lists:mapfoldl(Transformer, S, Rescue ++ Catch).

each_clause({ 'catch', Meta, Raw, Expr }, S) ->
  { Args, Guards } = elixir_clauses:extract_splat_guards(Raw),

  Final = case Args of
    [X]     -> [throw, X, { '_', Meta, nil }];
    [X,Y]   -> [X, Y, { '_', Meta, nil }];
    [_,_,_] -> Args;
    _       ->
      elixir_errors:syntax_error(Meta, S#elixir_scope.file, "too many arguments given for catch")
  end,

  Condition = { '{}', Meta, Final },
  elixir_clauses:assigns_block(?line(Meta), fun elixir_translator:translate_each/2, Condition, [Expr], Guards, S);

each_clause({ rescue, Meta, [Condition], Expr }, S) ->
  case normalize_rescue(Meta, Condition, S) of
    { Left, Right } ->
      case Left of
        { '_', _, _ } ->
          { ClauseVar, CS } = elixir_scope:build_ex_var(?line(Meta), S),
          { Clause, _ } = rescue_guards(Meta, ClauseVar, Right, S),
          each_clause({ 'catch', Meta, Clause, Expr }, CS);
        _ ->
          { Clause, Safe } = rescue_guards(Meta, Left, Right, S),
          case Safe of
            true ->
              each_clause({ 'catch', Meta, Clause, Expr }, S);
            false ->
              { ClauseVar, CS }  = elixir_scope:build_ex_var(?line(Meta), S),
              { FinalClause, _ } = rescue_guards(Meta, ClauseVar, Right, S),
              Match = { '=', Meta, [
                Left,
                { { '.', Meta, ['Elixir.Exception', normalize] }, Meta, [ClauseVar] }
              ] },
              FinalExpr = prepend_to_block(Meta, Match, Expr),
              each_clause({ 'catch', Meta, FinalClause, FinalExpr }, CS)
          end
      end;
    _ ->
      each_clause({ 'catch', Meta, [error, Condition], Expr }, S)
  end;

each_clause({rescue,Meta,_,_}, S) ->
  elixir_errors:syntax_error(Meta, S#elixir_scope.file, "too many arguments given for rescue");

each_clause({Key,Meta,_,_}, S) ->
  elixir_errors:syntax_error(Meta, S#elixir_scope.file, "invalid key ~ts in try", [Key]).

%% Helpers

%% rescue [Error] -> _ in [Error]
normalize_rescue(Meta, List, S) when is_list(List) ->
  normalize_rescue(Meta, { in, Meta, [{ '_', Meta, nil }, List] }, S);

%% rescue _
normalize_rescue(_, { '_', _, Atom }, _S) when is_atom(Atom) ->
  false;

%% rescue var -> var in _
normalize_rescue(_, { Name, Meta, Atom } = Rescue, S) when is_atom(Name), is_atom(Atom) ->
  normalize_rescue(Meta, { in, Meta, [Rescue, { '_', Meta, nil }] }, S);

%% rescue var in [Exprs]
normalize_rescue(_, { in, Meta, [Left, Right] }, S) ->
  case Right of
    { '_', _, _ } ->
      { Left, nil };
    _ when is_list(Right) ->
      is_valid_rescue_list(Right, S) andalso { Left, Right };
    _ ->
      { Expanded, ES } = 'Elixir.Macro':expand_all(Right, elixir_scope:to_ex_env({ ?line(Meta), S }), S),
      case is_valid_rescue_list(Expanded, ES) of
        true  -> { Left, Expanded };
        false ->
          elixir_errors:syntax_error(Meta, ES#elixir_scope.file, "invalid use of operator \"in\" in rescue inside try")
      end
  end;

normalize_rescue(Meta, Condition, S) ->
  case elixir_translator:translate_each(Condition, S#elixir_scope{context=match}) of
    { { atom, _, Atom }, _ } ->
      normalize_rescue(Meta, { in, Meta, [{ '_', Meta, nil }, [Atom]] }, S);
    _ ->
      elixir_errors:syntax_error(Meta, S#elixir_scope.file, "invalid rescue clause. The clause should "
                                 "match on an alias, a variable or be in the `var in [alias]` format")
  end.

%% Convert rescue clauses into guards.
rescue_guards(_, Var, nil, _) -> { [error, Var], false };

rescue_guards(Meta, Var, Guards, S) ->
  { RawElixir, RawErlang } = rescue_each_var(Meta, Var, Guards),
  { Elixir, Erlang, Safe } = rescue_each_ref(Meta, Var, Guards, RawElixir, RawErlang, RawErlang == [], S),

  Final = case Elixir == [] of
    true  -> Erlang;
    false ->
      IsTuple     = { is_tuple, Meta, [Var] },
      IsException = { '==', Meta, [
        { { '.', Meta, [erlang, element] }, Meta, [2, Var] }, '__exception__'
      ] },
      OrElse = join(Meta, 'or', Elixir),
      [join(Meta, 'and', [IsTuple, IsException, OrElse])|Erlang]
  end,
  {
    [{ 'when', Meta, [error, Var, reverse_join(Meta, 'when', Final)] }],
    Safe
  }.

%% Handle variables in the right side of rescue.

rescue_each_var(Meta, ClauseVar, Guards) ->
  Vars = [Var || Var <- Guards, is_var(Var)],

  case Vars == [] of
    true  -> { [], [] };
    false ->
      Elixir = [exception_compare(Meta, ClauseVar, Var) || Var <- Vars],
      Erlang = lists:map(fun(Rescue) ->
        Compares = [{ '==', Meta, [Rescue, Var] } || Var <- Vars],
        { 'and', Meta, [
          erlang_rescue_guard_for(Meta, ClauseVar, Rescue),
          join(Meta, 'or', Compares)
        ] }
      end, erlang_rescues()),
      { Elixir, Erlang }
  end.

%% Rescue each atom name considering their Erlang or Elixir matches.
%% Matching of variables is done with Erlang exceptions is done in another
%% method for optimization.

%% Ignore variables
rescue_each_ref(Meta, Var, [{ Name, _, Atom }|T], Elixir, Erlang, Safe, S) when is_atom(Name), is_atom(Atom) ->
  rescue_each_ref(Meta, Var, T, Elixir, Erlang, Safe, S);

rescue_each_ref(Meta, Var, [H|T], Elixir, Erlang, _Safe, S) when
  H == 'Elixir.UndefinedFunctionError'; H == 'Elixir.ErlangError';
  H == 'Elixir.ArgumentError'; H == 'Elixir.ArithmeticError';
  H == 'Elixir.BadArityError'; H == 'Elixir.BadFunctionError';
  H == 'Elixir.MatchError'; H == 'Elixir.CaseClauseError';
  H == 'Elixir.TryClauseError'; H == 'Elixir.FunctionClauseError';
  H == 'Elixir.SystemLimitError' ->
  Expr = { 'or', Meta, [
    erlang_rescue_guard_for(Meta, Var, H),
    exception_compare(Meta, Var, H)
  ] },
  rescue_each_ref(Meta, Var, T, Elixir, [Expr|Erlang], false, S);

rescue_each_ref(Meta, Var, [H|T], Elixir, Erlang, Safe, S) when is_atom(H) ->
  rescue_each_ref(Meta, Var, T, [exception_compare(Meta, Var, H)|Elixir], Erlang, Safe, S);

rescue_each_ref(Meta, Var, [H|T], Elixir, Erlang, Safe, S) ->
  case elixir_translator:translate_each(H, S) of
    { { atom, _, Atom }, _ } ->
      rescue_each_ref(Meta, Var, [Atom|T], Elixir, Erlang, Safe, S);
    _ ->
      rescue_each_ref(Meta, Var, T, [exception_compare(Meta, Var, H)|Elixir], Erlang, Safe, S)
  end;

rescue_each_ref(_, _, [], Elixir, Erlang, Safe, _) ->
  { Elixir, Erlang, Safe }.

%% Handle erlang rescue matches.

erlang_rescues() ->
  [
    'Elixir.UndefinedFunctionError', 'Elixir.ArgumentError', 'Elixir.ArithmeticError', 'Elixir.BadArityError',
    'Elixir.BadFunctionError', 'Elixir.MatchError', 'Elixir.CaseClauseError', 'Elixir.TryClauseError',
    'Elixir.FunctionClauseError', 'Elixir.SystemLimitError', 'Elixir.ErlangError'
  ].

erlang_rescue_guard_for(Meta, Var, List) when is_list(List) ->
  join(Meta, 'or', [erlang_rescue_guard_for(Meta, Var, X) || X <- List]);

erlang_rescue_guard_for(Meta, Var, 'Elixir.UndefinedFunctionError') ->
  { '==', Meta, [Var, undef] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.FunctionClauseError') ->
  { '==', Meta, [Var, function_clause] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.SystemLimitError') ->
  { '==', Meta, [Var, system_limit] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.ArithmeticError') ->
  { '==', Meta, [Var, badarith] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.BadArityError') ->
  { 'and', Meta, [
    { is_tuple, Meta, [Var] },
    exception_compare(Meta, Var, badarity)
  ] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.BadFunctionError') ->
  { 'and', Meta, [
    { is_tuple, Meta, [Var] },
    exception_compare(Meta, Var, badfun)
  ] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.MatchError') ->
  { 'and', Meta, [
    { is_tuple, Meta, [Var] },
    exception_compare(Meta, Var, badmatch)
  ] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.CaseClauseError') ->
  { 'and', Meta, [
    { is_tuple, Meta, [Var] },
    exception_compare(Meta, Var, case_clause)
  ] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.TryClauseError') ->
  { 'and', Meta, [
    { is_tuple, Meta, [Var] },
    exception_compare(Meta, Var, try_clause)
  ] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.ArgumentError') ->
  { 'or', Meta, [
    { '==', Meta, [Var, badarg] },
    { 'and', Meta, [
      { is_tuple, Meta, [Var] },
      exception_compare(Meta, Var, badarg)
    ] }
  ] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.ErlangError') ->
  IsNotTuple  = { 'not', Meta, [{ is_tuple, Meta, [Var] }] },
  IsException = { '!=', Meta, [
    { { '.', Meta, [ erlang, element ] }, Meta, [2, Var] }, '__exception__'
  ] },
  { 'or', Meta, [IsNotTuple, IsException] }.

%% Helpers

format_error({ rescue_no_match, Var, Alias }) ->
  VarBinary   = 'Elixir.Macro':to_string(Var),
  AliasBinary = 'Elixir.Macro':to_string(Alias),
  Message = "rescue clause (~ts = ~ts) can never match, maybe you meant to write: ~ts in [~ts] ?",
  io_lib:format(Message, [AliasBinary, VarBinary, VarBinary, AliasBinary]).

is_var({ Name, _, Atom }) when is_atom(Name), is_atom(Atom) -> true;
is_var(_) -> false.

is_erl_var_or_atom({ Kind, Line, Name }) when Kind == var orelse Kind == atom, is_integer(Line), is_atom(Name) -> true;
is_erl_var_or_atom(_) -> false.

is_valid_rescue_list(Right, S) when is_list(Right) ->
  { TRight, _ } = elixir_translator:translate(Right, S),
  lists:all(fun is_erl_var_or_atom/1, TRight).

exception_compare(Meta, Var, Expr) ->
  { '==', Meta, [
    { { '.', Meta, [ erlang, element ] }, Meta, [1, Var] },
    Expr
  ] }.

join(Meta, Kind, [H|T]) ->
  lists:foldl(fun(X, Acc) -> { Kind, Meta, [Acc, X] } end, H, T).

reverse_join(Meta, Kind, [H|T]) ->
  lists:foldl(fun(X, Acc) -> { Kind, Meta, [X, Acc] } end, H, T).

prepend_to_block(_Meta, Expr, { '__block__', Meta, Args }) ->
  { '__block__', Meta, [Expr|Args] };

prepend_to_block(Meta, Expr, Args) ->
  { '__block__', Meta, [Expr, Args] }.
