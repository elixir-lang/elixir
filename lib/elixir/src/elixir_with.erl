-module(elixir_with).
-export([expand/3, translate/3]).
-include("elixir.hrl").

%% Expansion

expand(Meta, Args, E) ->
  {Cases, Block} =
    case elixir_utils:split_last(Args) of
      {OuterCases, OuterOpts} when is_list(OuterOpts) ->
        case elixir_utils:split_last(OuterCases) of
          {InnerCases, InnerOpts} when is_list(InnerOpts) ->
            {InnerCases, InnerOpts ++ OuterOpts};
          _ ->
            {OuterCases, OuterOpts}
        end;
      _ ->
        {Args, []}
    end,

  {DoExpr, DoOpts} =
    case lists:keytake(do, 1, Block) of
      {value, {do, Do}, DoRest} ->
        {Do, DoRest};
      false ->
        elixir_errors:compile_error(Meta, ?m(E, file),
          "missing do keyword in with")
    end,

  {ElseExpr, ElseOpts} =
    case lists:keytake(else, 1, DoOpts) of
      {value, {else, Else}, ElseRest} ->
        {Else, ElseRest};
      false ->
        {nil, DoOpts}
    end,

  case ElseOpts of
    [{Key, _}|_] ->
      elixir_errors:compile_error(Meta, ?m(E, file),
        "unexpected keyword ~ts in with", [Key]);
    [] ->
      ok
  end,

  {ECases, EC} = lists:mapfoldl(fun expand/2, E, Cases),
  {EDoExpr, _} = elixir_exp:expand(DoExpr, EC),
  {EElseExpr, _} = expand_else(ElseExpr, E),
  {{with, Meta, ECases ++ [[{do, EDoExpr} | EElseExpr]]}, E}.

expand({'<-', Meta, [Left, Right]}, E) ->
  {ERight, ER} = elixir_exp:expand(Right, E),
  {[ELeft], EL}  = elixir_exp_clauses:head([Left], E),
  {{'<-', Meta, [ELeft, ERight]}, elixir_env:mergev(EL, ER)};
expand(X, E) ->
  elixir_exp:expand(X, E).

expand_else(KV, E) when is_list(KV) ->
  {[{do, EClauses}], EC} = elixir_exp_clauses:'case'([], [{do, KV}], E),
  {[{else, EClauses}], EC};
expand_else(nil, E) ->
  {[], E}.

%% Translation

translate(Meta, Args, S) ->
  {Parts, [{do, Expr} | ExprList]} = elixir_utils:split_last(Args),
  CaseExpr =
    case ExprList of
      [{else, ElseExpr}] ->
        build_else(Meta, build_cases(Parts, {ok, Expr}, fun(X) -> {error, X} end), ElseExpr);
      [] ->
        build_cases(Parts, Expr, fun(X) -> X end)
    end,
  {TC, TS} = elixir_translator:translate(CaseExpr, S),
  {TC, elixir_scope:mergec(S, TS)}.

build_cases([{'<-', Meta, [Left, Right]} | Rest], DoExpr, Wrapper) ->
  Other = {'other', Meta, ?MODULE},
  Clauses = [
    {'->', Meta, [[Left], build_cases(Rest, DoExpr, Wrapper)]},
    {'->', Meta, [[Other], Wrapper(Other)]}
  ],
  {'case', Meta, [Right, [{do, Clauses}]]};
build_cases([Expr | Rest], DoExpr, Wrapper) ->
  {'__block__', [], [Expr, build_cases(Rest, DoExpr, Wrapper)]};
build_cases([], DoExpr, _Wrapper) ->
  DoExpr.

build_else(Meta, WithCases, ElseClauses) ->
  Result = {'result', Meta, ?MODULE},
  Clauses = [
    {'->', Meta, [[{ok, Result}], Result]}
    | else_to_error_clause(ElseClauses)
  ] ++ [build_raise(Meta)],
  {'case', Meta, [WithCases, [{do, Clauses}]]}.

else_to_error_clause(Clauses) ->
  [{'->', Meta, [[{error, Match}], Expr]} ||
    {'->', Meta, [[Match], Expr]} <- Clauses].

build_raise(Meta) ->
  Other = {'raise', Meta, ?MODULE},
  {'->', ?generated, [[{error, Other}], {{'.', Meta, [erlang, error]}, Meta, [{with_clause, Other}]}]}.
