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

  {Expr, Opts} =
    case lists:keytake(do, 1, Block) of
      {value, {do, Do}, Rest} ->
        {Do, Rest};
      false ->
        elixir_errors:compile_error(Meta, ?m(E, file),
          "missing do keyword in with")
    end,

  {EOpts, EO} = elixir_exp:expand(Opts, E),
  {ECases, EC} = lists:mapfoldl(fun expand/2, EO, Cases),
  {EExpr, _} = elixir_exp:expand(Expr, EC),
  {{with, Meta, ECases ++ [[{do, EExpr} | EOpts]]}, E}.

expand({'<-', Meta, [Left, Right]}, E) ->
  {ERight, ER} = elixir_exp:expand(Right, E),
  {ELeft, EL}  = elixir_exp_clauses:match(fun elixir_exp:expand/2, Left, E),
  {{'<-', Meta, [ELeft, ERight]}, elixir_env:mergev(EL, ER)};
expand(X, E) ->
  elixir_exp:expand(X, E).

%% Translation

translate(_Meta, Args, S) ->
  {Parts, [{do, Expr} | _Opts]} = elixir_utils:split_last(Args),
  Cases = build_cases(Parts, Expr),
  elixir_translator:translate(Cases, S).

build_cases([{'<-', Meta, [Left, Right]} | Rest], DoExpr) ->
  Other = {'other', Meta, nil},
  Clauses = [
    {'->', Meta, [[Left], build_cases(Rest, DoExpr)]},
    {'->', Meta, [[Other], Other]}
  ],
  {'case', Meta, [Right, [{do, Clauses}]]};
build_cases([], DoExpr) ->
  DoExpr;
build_cases([{_, Meta, _} = Expr | Rest], DoExpr) ->
  {'__block__', Meta, [Expr, build_cases(Rest, DoExpr)]}.
