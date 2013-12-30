-module(elixir_try).
-export([clauses/3, format_error/1]).
-import(elixir_scope, [mergec/2]).
-include("elixir.hrl").

clauses(_Meta, Clauses, S) ->
  Catch  = elixir_clauses:get_pairs('catch', Clauses),
  Rescue = elixir_clauses:get_pairs(rescue, Clauses),
  Transformer = fun(X, Acc) -> each_clause(X, mergec(S, Acc)) end,
  lists:mapfoldl(Transformer, S, Rescue ++ Catch).

each_clause({ 'catch', Meta, Raw, Expr }, S) ->
  { Args, Guards } = elixir_clauses:extract_splat_guards(Raw),

  Final = case Args of
    [X]     -> [throw, X, { '_', Meta, nil }];
    [X,Y]   -> [X, Y, { '_', Meta, nil }];
    [_,_,_] -> Args;
    _       ->
      elixir_errors:compile_error(Meta, S#elixir_scope.file, "too many arguments given for catch")
  end,

  Condition = [{ '{}', Meta, Final }],
  elixir_clauses:clause(?line(Meta), fun elixir_translator:translate_many/2, Condition, Expr, Guards, S);

each_clause({ rescue, Meta, [{ in, _, [Left, Right]}], Expr }, S) ->
  case Left of
    { '_', _, LAtom } when is_atom(LAtom) ->
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

each_clause({rescue,Meta,_,_}, S) ->
  elixir_errors:compile_error(Meta, S#elixir_scope.file, "invalid arguments for rescue in try");

each_clause({Key,Meta,_,_}, S) ->
  elixir_errors:compile_error(Meta, S#elixir_scope.file, "invalid key ~ts in try", [Key]).

%% Helpers

%% Convert rescue clauses into guards.
rescue_guards(_, Var, { '_', _, _ }, _) -> { [error, Var], false };

rescue_guards(Meta, Var, Guards, S) ->
  { RawElixir, RawErlang } = rescue_each_var(Meta, Var, Guards),
  { Elixir, Erlang, Safe } = rescue_each_ref(Meta, Var, Guards, RawElixir, RawErlang, RawErlang == [], S),

  Final = case Elixir == [] of
    true  -> Erlang;
    false ->
      IsTuple     = { erl(Meta, is_tuple), Meta, [Var] },
      IsException = { erl(Meta, '=='), Meta, [
        { erl(Meta, element), Meta, [2, Var] }, '__exception__'
      ] },
      OrElse = join(Meta, fun do_or/3, Elixir),
      [join(Meta, fun do_and/3, [IsTuple, IsException, OrElse])|Erlang]
  end,
  {
    [{ 'when', Meta, [error, Var, join_when(Meta, Final)] }],
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
        Compares = [{ erl(Meta, '=='), Meta, [Rescue, Var] } || Var <- Vars],
        do_and(Meta,
               erlang_rescue_guard_for(Meta, ClauseVar, Rescue),
               join(Meta, fun do_or/3, Compares))
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
  Expr = do_or(Meta,
               erlang_rescue_guard_for(Meta, Var, H),
               exception_compare(Meta, Var, H)),
  rescue_each_ref(Meta, Var, T, Elixir, [Expr|Erlang], false, S);

rescue_each_ref(Meta, Var, [H|T], Elixir, Erlang, Safe, S) when is_atom(H) ->
  rescue_each_ref(Meta, Var, T, [exception_compare(Meta, Var, H)|Elixir], Erlang, Safe, S);

rescue_each_ref(Meta, Var, [H|T], Elixir, Erlang, Safe, S) ->
  case elixir_translator:translate(H, S) of
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
  join(Meta, fun do_or/3, [erlang_rescue_guard_for(Meta, Var, X) || X <- List]);

erlang_rescue_guard_for(Meta, Var, 'Elixir.UndefinedFunctionError') ->
  { erl(Meta, '=='), Meta, [Var, undef] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.FunctionClauseError') ->
  { erl(Meta, '=='), Meta, [Var, function_clause] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.SystemLimitError') ->
  { erl(Meta, '=='), Meta, [Var, system_limit] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.ArithmeticError') ->
  { erl(Meta, '=='), Meta, [Var, badarith] };

erlang_rescue_guard_for(Meta, Var, 'Elixir.BadArityError') ->
  do_and(Meta,
         { erl(Meta, is_tuple), Meta, [Var] },
         exception_compare(Meta, Var, badarity));

erlang_rescue_guard_for(Meta, Var, 'Elixir.BadFunctionError') ->
  do_and(Meta,
         { erl(Meta, is_tuple), Meta, [Var] },
         exception_compare(Meta, Var, badfun));

erlang_rescue_guard_for(Meta, Var, 'Elixir.MatchError') ->
  do_and(Meta,
         { erl(Meta, is_tuple), Meta, [Var] },
         exception_compare(Meta, Var, badmatch));

erlang_rescue_guard_for(Meta, Var, 'Elixir.CaseClauseError') ->
  do_and(Meta,
         { erl(Meta, is_tuple), Meta, [Var] },
         exception_compare(Meta, Var, case_clause));

erlang_rescue_guard_for(Meta, Var, 'Elixir.TryClauseError') ->
  do_and(Meta,
         { erl(Meta, is_tuple), Meta, [Var] },
         exception_compare(Meta, Var, try_clause));

erlang_rescue_guard_for(Meta, Var, 'Elixir.ArgumentError') ->
  do_or(Meta,
        { erl(Meta, '=='), Meta, [Var, badarg] },
        do_and(Meta,
               { erl(Meta, is_tuple), Meta, [Var] },
               exception_compare(Meta, Var, badarg)));

erlang_rescue_guard_for(Meta, Var, 'Elixir.ErlangError') ->
  IsNotTuple  = { erl(Meta, 'not'), Meta, [{ erl(Meta, is_tuple), Meta, [Var] }] },
  IsException = { erl(Meta, '/='), Meta, [
    { erl(Meta, element), Meta, [2, Var] }, '__exception__'
  ] },
  do_or(Meta, IsNotTuple, IsException).

%% Helpers

format_error({ rescue_no_match, Var, Alias }) ->
  VarBinary   = 'Elixir.Macro':to_string(Var),
  AliasBinary = 'Elixir.Macro':to_string(Alias),
  Message = "rescue clause (~ts = ~ts) can never match, maybe you meant to write: ~ts in [~ts] ?",
  io_lib:format(Message, [AliasBinary, VarBinary, VarBinary, AliasBinary]).

is_var({ Name, _, Atom }) when is_atom(Name), is_atom(Atom) -> true;
is_var(_) -> false.

exception_compare(Meta, Var, Expr) ->
  { erl(Meta, '=='), Meta, [
    { erl(Meta, element), Meta, [1, Var] },
    Expr
  ] }.

join(Meta, Kind, [H|T]) ->
  lists:foldl(fun(X, Acc) -> Kind(Meta, Acc, X) end, H, T).

join_when(Meta, [H|T]) ->
  lists:foldl(fun(X, Acc) -> { 'when', Meta, [X, Acc] } end, H, T).

prepend_to_block(_Meta, Expr, { '__block__', Meta, Args }) ->
  { '__block__', Meta, [Expr|Args] };

prepend_to_block(Meta, Expr, Args) ->
  { '__block__', Meta, [Expr, Args] }.

erl(Meta, Op)      -> { '.', Meta, [erlang, Op] }.
do_or(Meta, Left, Right) -> { '__op__', Meta, ['orelse', Left, Right] }.
do_and(Meta, Left, Right) -> { '__op__', Meta, ['andalso', Left, Right] }.
