-module(elixir_erl_try).
-export([clauses/3]).
-include("elixir.hrl").

clauses(_Meta, Args, S) ->
  Catch = elixir_erl_clauses:get_clauses('catch', Args, 'catch'),
  Rescue = elixir_erl_clauses:get_clauses(rescue, Args, rescue),
  {StackName, _Counter, SV} = elixir_erl_var:build('__STACKTRACE__', S),
  OldStack = SV#elixir_erl.stacktrace,
  SS = SV#elixir_erl{stacktrace={StackName,false}},
  reduce_clauses(Rescue ++ Catch, [], OldStack, SS, SS).

reduce_clauses([H | T], Acc, OldStack, SAcc, S) ->
  {TH, TS} = each_clause(H, SAcc),
  reduce_clauses(T, TH ++ Acc, OldStack, elixir_erl_var:mergec(S, TS), S);
reduce_clauses([], Acc, OldStack, SAcc, _S) ->
  {lists:reverse(Acc), SAcc#elixir_erl{stacktrace = OldStack}}.

each_clause({'catch', Meta, Raw, Expr}, S) ->
  {Args, Guards} = elixir_utils:extract_splat_guards(Raw),

  Final = case Args of
    [X]   -> [throw, X, {'_', Meta, nil}];
    [X, Y] -> [X, Y, {'_', Meta, nil}]
  end,

  Condition = [{'{}', Meta, Final}],
  {TC, TS} = elixir_erl_clauses:clause(Meta, fun elixir_erl_pass:translate_args/2,
                                       Condition, Expr, Guards, S),
  {[maybe_add_stracktrace(TC, TS)], TS};

each_clause({rescue, Meta, [{in, _, [Left, Right]}], Expr}, S) ->
  in_clause(Meta, Left, Right, Expr, S, in);

each_clause({rescue, Meta, [{'not', _, [{in, _, [Left, Right]}]}], Expr}, S) ->
  in_clause(Meta, Left, Right, Expr, S, 'not in');

each_clause({rescue, Meta, [{VarName, _, Context} = Left], Expr}, S) when is_atom(VarName), is_atom(Context) ->
  {TempName, _, CS} = elixir_erl_var:build('_', S),
  TempVar = {TempName, Meta, ?var_context},
  Body = rescue_clause_body(Left, Expr, false, TempVar, Meta),
  build_rescue(Meta, [{TempVar, []}], Body, CS).

in_clause(Meta, Left, Right, Expr, S, Op) ->
  {TempName, _, CS} = elixir_erl_var:build('_', S),
  TempVar = {TempName, Meta, ?var_context},
  {Parts, Safe, FS} = rescue_guards(Meta, TempVar, Right, CS, Op),
  Body = rescue_clause_body(Left, Expr, Safe, TempVar, Meta),
  build_rescue(Meta, Parts, Body, FS).

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

  {TC, TS} =
    elixir_erl_clauses:clause(Meta, fun elixir_erl_pass:translate_args/2,
                              Matches, Body, [], S),

  {clause, Line, TMatches, _, TBody} = maybe_add_stracktrace(TC, TS),

  TClauses =
    [begin
      TArgs   = [{tuple, Line, [{atom, Line, error}, TMatch, {var, Line, '_'}]}],
      TGuards = elixir_erl_clauses:guards(Guards, [], TS),
      {clause, Line, TArgs, TGuards, TBody}
     end || {TMatch, {_, Guards}} <- lists:zip(TMatches, Parts)],

  {TClauses, TS}.

%% Convert rescue clauses ("var in [alias1, alias2]") into guards.
rescue_guards(Meta, Var, Aliases, S, Op) ->
  {Elixir, Erlang} = rescue_each_ref(Meta, Var, Aliases, [], false, S),

  {ElixirParts, ES} =
    case Elixir of
      [] -> {[], S};
      _  ->
        {VarName, _, CS} = elixir_erl_var:build('_', S),
        StructVar = {VarName, Meta, 'Elixir'},
        Map = {'%{}', Meta, [{'__struct__', StructVar}, {'__exception__', true}]},
        Match = {'=', Meta, [Map, Var]},
        Guards =
          case Op of
            in -> rescue_guards_ors(Elixir, Meta, StructVar);
            'not in' -> {erl(Meta, 'not'), Meta, [rescue_guards_ors(Elixir, Meta, StructVar)]}
          end,
        {[{Match, [Guards]}], CS}
    end,

  ErlangParts =
    case Erlang of
      [] -> [];
      _  -> [{Var, Erlang}]
    end,

  {ElixirParts ++ ErlangParts, ErlangParts == [], ES}.

rescue_guards_ors([FirstMod | Mods], Meta, StructVar) ->
  Acc0 = {erl(Meta, '=='), Meta, [StructVar, FirstMod]},

  lists:foldl(fun(Mod, Acc) ->
    erl_or(Meta, Acc, {erl(Meta, '=='), Meta, [StructVar, Mod]})
  end, Acc0, Mods).

maybe_add_stracktrace({clause, Line, Args, Guards, Body}, #elixir_erl{stacktrace = {Var,true}}) ->
  GetStacktrace = elixir_erl:remote(Line, erlang, get_stacktrace, []),
  Stack = {match, Line, {var, Line, Var}, GetStacktrace},
  {clause, Line, Args, Guards, [Stack|Body]};
maybe_add_stracktrace(Clause, _) ->
  Clause.

%% Rescue each atom name considering their Erlang or Elixir matches.
%% Matching of variables is done with Erlang exceptions is done in
%% function for optimization.

rescue_each_ref(Meta, Var, [H | T], Elixir, Erlang, S) when is_atom(H) ->
  case erl_rescue_guard_for(Meta, Var, H) of
    false -> rescue_each_ref(Meta, Var, T, [H | Elixir], Erlang, S);
    Expr -> rescue_each_ref(Meta, Var, T, [H | Elixir], erl_or(Meta, Erlang, Expr), S)
  end;

rescue_each_ref(_, _, [], Elixir, Erlang, _) ->
  case Erlang of
    false -> {Elixir, []};
    _ -> {Elixir, [Erlang]}
  end.

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
