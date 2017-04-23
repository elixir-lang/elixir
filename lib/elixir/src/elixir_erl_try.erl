-module(elixir_erl_try).
-export([clauses/3]).
-include("elixir.hrl").

clauses(_Meta, Args, S) ->
  Catch = elixir_erl_clauses:get_clauses('catch', Args, 'catch'),
  Rescue = elixir_erl_clauses:get_clauses(rescue, Args, rescue),
  reduce_clauses(Rescue ++ Catch, [], S, S).

reduce_clauses([H | T], Acc, SAcc, S) ->
  {TH, TS} = each_clause(H, SAcc),
  reduce_clauses(T, TH ++ Acc, elixir_erl_var:mergec(S, TS), S);
reduce_clauses([], Acc, SAcc, _S) ->
  {lists:reverse(Acc), SAcc}.

each_clause({'catch', Meta, Raw, Expr}, S) ->
  {Args, Guards} = elixir_utils:extract_splat_guards(Raw),

  Final = case Args of
    [X]   -> [throw, X, {'_', Meta, nil}];
    [X, Y] -> [X, Y, {'_', Meta, nil}]
  end,

  Condition = [{'{}', Meta, Final}],
  {TC, TS} = elixir_erl_clauses:clause(Meta, fun elixir_erl_pass:translate_args/2,
                                   Condition, Expr, Guards, S),
  {[TC], TS};

each_clause({rescue, Meta, [{in, _, [Left, Right]}], Expr}, S) ->
  {VarName, _, CS} = elixir_erl_var:build('_', S),
  Var = {VarName, Meta, nil},
  {Parts, Safe, FS} = rescue_guards(Meta, Var, Right, CS),
  Body = rescue_clause_body(Left, Expr, Safe, Var, Meta),
  build_rescue(Meta, Parts, Body, FS);

each_clause({rescue, Meta, [{VarName, _, Atom} = Var], Expr}, S) when is_atom(VarName), is_atom(Atom) ->
  Body = rescue_clause_body(Var, Expr, false, Var, Meta),
  build_rescue(Meta, _Parts = [{Var, []}], Body, S).

rescue_clause_body({'_', _, Atom}, Expr, _Safe, _Var, _Meta) when is_atom(Atom) ->
  Expr;
rescue_clause_body(Pattern, Expr, Safe, Var, Meta) ->
  Normalized =
    case Safe of
      true -> Var;
      false -> {{'.', Meta, ['Elixir.Exception', normalize]}, Meta, [error, Var]}
    end,
  prepend_to_block(Meta, {'=', Meta, [Pattern, Normalized]}, Expr).

%% Helpers

build_rescue(Meta, Parts, Body, S) ->
  Matches = [Match || {Match, _} <- Parts],

  {{clause, Line, TMatches, _, TBody}, TS} =
    elixir_erl_clauses:clause(Meta, fun elixir_erl_pass:translate_args/2,
                          Matches, Body, [], S),

  Fun = fun({TMatch, {_, Guards}}, SG) ->
    TArgs   = [{tuple, Line, [{atom, Line, error}, TMatch, {var, Line, '_'}]}],
    {TGuards, ST} = elixir_erl_clauses:guards(Guards, [], SG),
    {{clause, Line, TArgs, TGuards, TBody}, ST}
  end,
  lists:mapfoldl(Fun, TS, lists:zip(TMatches, Parts)).

%% Convert rescue clauses ("var in [alias1, alias2]") into guards.
rescue_guards(Meta, Var, Aliases, S) ->
  {Elixir, Erlang} = rescue_each_ref(Meta, Var, Aliases, [], [], S),

  {ElixirParts, ES} =
    case Elixir of
      [] -> {[], S};
      _  ->
        {VarName, _, CS} = elixir_erl_var:build('_', S),
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

rescue_each_ref(Meta, Var, [H | T], Elixir, Erlang, S) when is_atom(H) ->
  case erl_rescue_guard_for(Meta, Var, H) of
    false -> rescue_each_ref(Meta, Var, T, [H | Elixir], Erlang, S);
    Expr  -> rescue_each_ref(Meta, Var, T, [H | Elixir], [Expr | Erlang], S)
  end;

rescue_each_ref(_, _, [], Elixir, Erlang, _) ->
  {Elixir, Erlang}.

%% Handle Erlang rescue matches.

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

erl_rescue_guard_for(Meta, Var, 'Elixir.WithClauseError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, with_clause));

erl_rescue_guard_for(Meta, Var, 'Elixir.TryClauseError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, try_clause));

erl_rescue_guard_for(Meta, Var, 'Elixir.BadStructError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 3),
          erl_record_compare(Meta, Var, badstruct));

erl_rescue_guard_for(Meta, Var, 'Elixir.BadMapError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 2),
          erl_record_compare(Meta, Var, badmap));

erl_rescue_guard_for(Meta, Var, 'Elixir.BadBooleanError') ->
  erl_and(Meta,
          erl_tuple_size(Meta, Var, 3),
          erl_record_compare(Meta, Var, badbool));

erl_rescue_guard_for(Meta, Var, 'Elixir.KeyError') ->
  erl_and(Meta,
          erl_or(Meta,
            erl_tuple_size(Meta, Var, 2),
            erl_tuple_size(Meta, Var, 3)),
          erl_record_compare(Meta, Var, badkey));

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
  {'__block__', Meta, [Expr | Args]};

prepend_to_block(Meta, Expr, Args) ->
  {'__block__', Meta, [Expr, Args]}.

erl(Meta, Op)      -> {'.', Meta, [erlang, Op]}.
erl_or(Meta, Left, Right) -> {{'.', Meta, [erlang, 'orelse']}, Meta, [Left, Right]}.
erl_and(Meta, Left, Right) -> {{'.', Meta, [erlang, 'andalso']}, Meta, [Left, Right]}.
