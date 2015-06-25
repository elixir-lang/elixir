-module(elixir_try).
-export([clauses/4]).
-include("elixir.hrl").

clauses(_Meta, Clauses, Return, S) ->
  Catch  = elixir_clauses:get_pairs('catch', Clauses, 'catch'),
  Rescue = elixir_clauses:get_pairs(rescue, Clauses, rescue),
  reduce_clauses(Rescue ++ Catch, [], S, Return, S).

reduce_clauses([H|T], Acc, SAcc, Return, S) ->
  {TH, TS} = each_clause(H, Return, SAcc),
  reduce_clauses(T, TH ++ Acc, elixir_scope:mergec(S, TS), Return, S);
reduce_clauses([], Acc, SAcc, _Return, _S) ->
  {lists:reverse(Acc), SAcc}.

each_clause({'catch', Meta, Raw, Expr}, Return, S) ->
  {Args, Guards} = elixir_clauses:extract_splat_guards(Raw),

  Final = case Args of
    [X]   -> [throw, X, {'_', Meta, nil}];
    [X, Y] -> [X, Y, {'_', Meta, nil}]
  end,

  Condition = [{'{}', Meta, Final}],
  {TC, TS} = elixir_clauses:clause(?line(Meta), fun elixir_translator:translate_args/2,
                                   Condition, Expr, Guards, Return, S),
  {[TC], TS};

each_clause({rescue, Meta, [{in, _, [Left, Right]}], Expr}, Return, S) ->
  {VarName, _, CS} = elixir_scope:build_var('_', S),
  Var = {VarName, Meta, nil},
  {Parts, Safe, FS} = rescue_guards(Meta, Var, Right, CS),

  Body =
    case Left of
      {'_', _, Atom} when is_atom(Atom) ->
        Expr;
      _ ->
        Normalized =
          case Safe of
            true  -> Var;
            false -> {{'.', Meta, ['Elixir.Exception', normalize]}, Meta, [error, Var]}
          end,
        prepend_to_block(Meta, {'=', Meta, [Left, Normalized]}, Expr)
    end,

  build_rescue(Meta, Parts, Body, Return, FS);

each_clause({rescue, Meta, _, _}, _Return, S) ->
  elixir_errors:compile_error(Meta, S#elixir_scope.file, "invalid arguments for rescue in try");

each_clause({Key, Meta, _, _}, _Return, S) ->
  elixir_errors:compile_error(Meta, S#elixir_scope.file, "invalid key ~ts in try", [Key]).

%% Helpers

build_rescue(Meta, Parts, Body, Return, S) ->
  Matches = [Match || {Match, _} <- Parts],

  {{clause, Line, TMatches, _, TBody}, TS} =
    elixir_clauses:clause(?line(Meta), fun elixir_translator:translate_args/2,
                          Matches, Body, [], Return, S),

  TClauses =
    [begin
      TArgs   = [{tuple, Line, [{atom, Line, error}, TMatch, {var, Line, '_'}]}],
      TGuards = elixir_clauses:guards(Line, Guards, [], TS),
      {clause, Line, TArgs, TGuards, TBody}
     end || {TMatch, {_, Guards}} <- lists:zip(TMatches, Parts)],

  {TClauses, TS}.

%% Convert rescue clauses into guards.
rescue_guards(_, Var, {'_', _, _}, S) -> {[{Var, []}], false, S};

rescue_guards(Meta, Var, Aliases, S) ->
  {Elixir, Erlang} = rescue_each_ref(Meta, Var, Aliases, [], [], S),

  {ElixirParts, ES} =
    case Elixir of
      [] -> {[], S};
      _  ->
        {VarName, _, CS} = elixir_scope:build_var('_', S),
        StructVar = {VarName, Meta, nil},
        Map = {'%{}', Meta, [{'__struct__', StructVar}, {'__exception__', true}]},
        Match = {'=', Meta, [Map, Var]},
        Guards = [{erl(Meta, '=='), Meta, [StructVar, Mod]} || Mod <- Elixir],
        {[{Match, Guards}], CS}
    end,

  ErlangParts =
    case Erlang of
      [] -> [];
      _  -> [{Var, Erlang}]
    end,

  {ElixirParts ++ ErlangParts, ErlangParts == [], ES}.

%% Rescue each atom name considering their Erlang or Elixir matches.
%% Matching of variables is done with Erlang exceptions is done in
%% function for optimization.

rescue_each_ref(Meta, Var, [H|T], Elixir, Erlang, S) when is_atom(H) ->
  case erl_rescue_guard_for(Meta, Var, H) of
    false -> rescue_each_ref(Meta, Var, T, [H|Elixir], Erlang, S);
    Expr  -> rescue_each_ref(Meta, Var, T, [H|Elixir], [Expr|Erlang], S)
  end;

rescue_each_ref(_, _, [], Elixir, Erlang, _) ->
  {Elixir, Erlang}.

%% Handle erlang rescue matches.

erl_rescue_guard_for(Meta, Var, 'Elixir.UndefinedFunctionError') ->
  {erl(Meta, '=='), Meta, [Var, undef]};

erl_rescue_guard_for(Meta, Var, 'Elixir.FunctionClauseError') ->
  {erl(Meta, '=='), Meta, [Var, function_clause]};

erl_rescue_guard_for(Meta, Var, 'Elixir.SystemLimitError') ->
  {erl(Meta, '=='), Meta, [Var, system_limit]};

erl_rescue_guard_for(Meta, Var, 'Elixir.ArithmeticError') ->
  {erl(Meta, '=='), Meta, [Var, badarith]};

erl_rescue_guard_for(Meta, Var, 'Elixir.CondClauseError') ->
  {erl(Meta, '=='), Meta, [Var, cond_clause]};

erl_rescue_guard_for(Meta, Var, 'Elixir.BadArityError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, badarity));

erl_rescue_guard_for(Meta, Var, 'Elixir.BadFunctionError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, badfun));

erl_rescue_guard_for(Meta, Var, 'Elixir.MatchError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, badmatch));

erl_rescue_guard_for(Meta, Var, 'Elixir.CaseClauseError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, case_clause));

erl_rescue_guard_for(Meta, Var, 'Elixir.TryClauseError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, try_clause));

erl_rescue_guard_for(Meta, Var, 'Elixir.BadStructError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 3),
          erl_record_compare(Meta, Var, badstruct));

erl_rescue_guard_for(Meta, Var, 'Elixir.ArgumentError') ->
  erl_or(Meta,
         {erl(Meta, '=='), Meta, [Var, badarg]},
         erl_and(Meta,
                 erl_tuple_size(Meta, Var, 2),
                 erl_record_compare(Meta, Var, badarg)));

erl_rescue_guard_for(Meta, Var, 'Elixir.ErlangError') ->
  IsNotTuple  = {erl(Meta, 'not'), Meta, [{erl(Meta, is_tuple), Meta, [Var]}]},
  IsException = {erl(Meta, '/='), Meta, [
    {erl(Meta, element), Meta, [2, Var]}, '__exception__'
  ]},
  erl_or(Meta, IsNotTuple, IsException);

erl_rescue_guard_for(_, _, _) ->
  false.

%% Helpers

erl_tuple_size(Meta, Var, Size) ->
  {erl(Meta, '=='), Meta, [{erl(Meta, tuple_size), Meta, [Var]}, Size]}.

erl_record_compare(Meta, Var, Expr) ->
  {erl(Meta, '=='), Meta, [
    {erl(Meta, element), Meta, [1, Var]},
    Expr
  ]}.

prepend_to_block(_Meta, Expr, {'__block__', Meta, Args}) ->
  {'__block__', Meta, [Expr|Args]};

prepend_to_block(Meta, Expr, Args) ->
  {'__block__', Meta, [Expr, Args]}.

erl(Meta, Op)      -> {'.', Meta, [erlang, Op]}.
erl_or(Meta, Left, Right) -> {'__op__', Meta, ['orelse', Left, Right]}.
erl_and(Meta, Left, Right) -> {'__op__', Meta, ['andalso', Left, Right]}.
