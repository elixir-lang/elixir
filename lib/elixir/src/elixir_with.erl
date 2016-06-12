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
    [{Key, _} | _] ->
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
  case ExprList of
    [{else, ElseExpr}] ->
      {TCases, TS} = translate_case(Parts, {ok, Expr}, fun(X) -> {error, X} end, S),
      translate_else(Meta, TCases, ElseExpr, TS);
    [] ->
      translate_case(Parts, Expr, fun(X) -> X end, S)
  end.

translate_case(Parts, DoExpr, Wrapper, S) ->
  Cases = build_case(Parts, DoExpr, Wrapper),
  {TCases, TS} = elixir_translator:translate(Cases, S#elixir_scope{extra=nil}),
  {TCases, elixir_scope:mergec(S, TS)}.

translate_else(Meta, WithCases, ElseExpr, S) ->
  ElseClauses = build_else(Meta, ElseExpr),
  {TClauses, TS} = elixir_clauses:clauses(Meta, ElseClauses, S#elixir_scope{extra=nil}),
  {{'case', ?ann(Meta), WithCases, TClauses}, elixir_scope:mergec(S, TS)}.

build_case([{'<-', Meta, [{Name, _, Ctx}, _] = Args} | Rest], DoExpr, Wrapper)
    when is_atom(Name) andalso is_atom(Ctx) ->
  build_case([{'=', Meta, Args} | Rest], DoExpr, Wrapper);
build_case([{'<-', Meta, [Left, Right]} | Rest], DoExpr, Wrapper) ->
  Other = {other, Meta, ?MODULE},
  Clauses = [
    {'->', ?generated, [[Left], build_case(Rest, DoExpr, Wrapper)]},
    {'->', ?generated, [[Other], Wrapper(Other)]}
  ],
  {'case', ?generated, [Right, [{do, Clauses}]]};
build_case([Expr | Rest], DoExpr, Wrapper) ->
  {'__block__', [], [Expr, build_case(Rest, DoExpr, Wrapper)]};
build_case([], DoExpr, _Wrapper) ->
  DoExpr.

build_else(Meta, ElseClauses) ->
  Result = {result, Meta, ?MODULE},
  [{match, Meta, [{ok, Result}], Result} |
    each_clause_to_error_match(ElseClauses)] ++ [build_raise(Meta)].

each_clause_to_error_match(Clauses) ->
  [{match, Meta, [{error, Match}], Expr} ||
    {'->', Meta, [[Match], Expr]} <- Clauses].

build_raise(Meta) ->
  Other = {other, Meta, ?MODULE},
  {match, ?generated, [{error, Other}], {{'.', Meta, [erlang, error]}, Meta, [{with_clause, Other}]}}.
