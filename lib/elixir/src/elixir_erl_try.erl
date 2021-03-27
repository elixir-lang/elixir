-module(elixir_erl_try).
-export([clauses/3]).
-include("elixir.hrl").
-define(REQUIRES_STACKTRACE,
        ['Elixir.FunctionClauseError', 'Elixir.UndefinedFunctionError',
         'Elixir.KeyError', 'Elixir.ArgumentError', 'Elixir.SystemLimitError']).

clauses(_Meta, Args, S) ->
  Catch = elixir_erl_clauses:get_clauses('catch', Args, 'catch'),
  Rescue = elixir_erl_clauses:get_clauses(rescue, Args, rescue),
  {StackName, SV} = elixir_erl_var:build('__STACKTRACE__', S),
  OldStack = SV#elixir_erl.stacktrace,
  SS = SV#elixir_erl{stacktrace=StackName},
  reduce_clauses(Rescue ++ Catch, [], OldStack, SS, SS).

reduce_clauses([H | T], Acc, OldStack, SAcc, S) ->
  {TH, TS} = each_clause(H, SAcc),
  reduce_clauses(T, [TH | Acc], OldStack, TS, S);
reduce_clauses([], Acc, OldStack, SAcc, _S) ->
  {lists:reverse(Acc), SAcc#elixir_erl{stacktrace=OldStack}}.

each_clause({'catch', Meta, Raw, Expr}, S) ->
  {Args, Guards} = elixir_utils:extract_splat_guards(Raw),

  Match =
    case Args of
      [X] -> [throw, X];
      [X, Y] -> [X, Y]
    end,

  {{clause, Line, [TKind, TMatches], TGuards, TBody}, TS} =
    elixir_erl_clauses:clause(Meta, fun elixir_erl_pass:translate_args/2, Match, Expr, Guards, S),

  build_clause(Line, TKind, TMatches, TGuards, TBody, TS);

each_clause({rescue, Meta, [{in, _, [Left, Right]}], Expr}, S) ->
  {TempVar, _, CS} = elixir_erl_var:assign(Meta, S),
  {Guards, ErlangAliases} = rescue_guards(Meta, TempVar, Right),
  Body = normalize_rescue(Meta, TempVar, Left, Expr, ErlangAliases),
  build_rescue(Meta, TempVar, Guards, Body, CS);

each_clause({rescue, Meta, [{VarName, _, Context} = Left], Expr}, S) when is_atom(VarName), is_atom(Context) ->
  {TempVar, _, CS} = elixir_erl_var:assign(Meta, S),
  Body = normalize_rescue(Meta, TempVar, Left, Expr, ['Elixir.ErlangError']),
  build_rescue(Meta, TempVar, [], Body, CS).

normalize_rescue(_Meta, _Var, {'_', _, Atom}, Expr, _) when is_atom(Atom) ->
  Expr;
normalize_rescue(Meta, Var, Pattern, Expr, []) ->
  prepend_to_block(Meta, {'=', Meta, [Pattern, Var]}, Expr);
normalize_rescue(Meta, Var, Pattern, Expr, ErlangAliases) ->
  Stacktrace =
    case lists:member('Elixir.ErlangError', ErlangAliases) of
      true ->
        dynamic_normalize(Meta, Var, ?REQUIRES_STACKTRACE);

      false ->
        case lists:splitwith(fun is_normalized_with_stacktrace/1, ErlangAliases) of
          {[], _} -> [];
          {_, []} -> {'__STACKTRACE__', Meta, nil};
          {Some, _} -> dynamic_normalize(Meta, Var, Some)
        end
    end,

  Normalized = {{'.', Meta, ['Elixir.Exception', normalize]}, Meta, [error, Var, Stacktrace]},
  prepend_to_block(Meta, {'=', Meta, [Pattern, Normalized]}, Expr).

dynamic_normalize(Meta, Var, [H | T]) ->
  Generated = ?generated(Meta),

  Guards =
    lists:foldl(fun(Alias, Acc) ->
      {'when', Generated, [erl_rescue_stacktrace_for(Generated, Var, Alias), Acc]}
    end, erl_rescue_stacktrace_for(Generated, Var, H), T),

  {'case', Generated, [
    Var,
    [{do, [
      {'->', Generated, [[{'when', Generated, [{'_', Generated, nil}, Guards]}], {'__STACKTRACE__', Generated, nil}]},
      {'->', Generated, [[{'_', Generated, nil}], []]}
    ]}]
  ]}.

erl_rescue_stacktrace_for(_Meta, _Var, 'Elixir.ErlangError') ->
  %% ErlangError is a "meta" exception, we should never expand it here.
  error(badarg);
erl_rescue_stacktrace_for(Meta, Var, 'Elixir.KeyError') ->
  %% Only the two-element tuple requires stacktrace.
  erl_and(Meta, erl_tuple_size(Meta, Var, 2), erl_record_compare(Meta, Var, badkey));
erl_rescue_stacktrace_for(Meta, Var, Module) ->
  erl_rescue_guard_for(Meta, Var, Module).

is_normalized_with_stacktrace(Module) ->
  lists:member(Module, ?REQUIRES_STACKTRACE).

%% Helpers

build_rescue(Meta, Var, Guards, Body, S) ->
  {{clause, Line, [TMatch], TGuards, TBody}, TS} =
    elixir_erl_clauses:clause(Meta, fun elixir_erl_pass:translate_args/2, [Var], Body, Guards, S),

  build_clause(Line, {atom, Line, error}, TMatch, TGuards, TBody, TS).

%% Convert rescue clauses ("var in [alias1, alias2]") into guards.
rescue_guards(_Meta, _Var, []) ->
  {[], []};
rescue_guards(Meta, Var, Aliases) ->
  {ErlangGuards, ErlangAliases} = erl_rescue(Meta, Var, Aliases, [], []),

  ElixirGuards =
    [erl_and(Meta,
       {erl(Meta, '=='), Meta, [{erl(Meta, map_get), Meta, ['__struct__', Var]}, Alias]},
       {erl(Meta, map_get), Meta, ['__exception__', Var]}
     ) || Alias <- Aliases],

  {ElixirGuards ++ ErlangGuards, ErlangAliases}.

build_clause(Line, Kind, Expr, Guards, Body, #elixir_erl{stacktrace=Var} = TS) ->
  Match = {tuple, Line, [Kind, Expr, {var, Line, Var}]},
  {{clause, Line, [Match], Guards, Body}, TS}.

%% Rescue each atom name considering their Erlang or Elixir matches.
%% Matching of variables is done with Erlang exceptions is done in
%% function for optimization.

erl_rescue(Meta, Var, [H | T], Guards, Aliases) when is_atom(H) ->
  case erl_rescue_guard_for(Meta, Var, H) of
    false -> erl_rescue(Meta, Var, T, Guards, Aliases);
    Expr  -> erl_rescue(Meta, Var, T, [Expr | Guards], [H | Aliases])
  end;
erl_rescue(_, _, [], Guards, Aliases) ->
  {Guards, Aliases}.

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
  Condition =
    erl_and(
      Meta,
      {erl(Meta, is_map), Meta, [Var]},
      {erl(Meta, is_map_key), Meta, ['__exception__', Var]}
    ),
  {erl(Meta, 'not'), Meta, [Condition]};

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

erl(Meta, Op) -> {'.', Meta, [erlang, Op]}.
erl_or(Meta, Left, Right) -> {erl(Meta, 'orelse'), Meta, [Left, Right]}.
erl_and(Meta, Left, Right) -> {erl(Meta, 'andalso'), Meta, [Left, Right]}.
