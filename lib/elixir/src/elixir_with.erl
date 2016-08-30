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
  {EElseExpr, _} = expand_else(Meta, ElseExpr, E),
  {{with, Meta, ECases ++ [[{do, EDoExpr} | EElseExpr]]}, E}.

expand({'<-', Meta, [Left, Right]}, E) ->
  {ERight, ER} = elixir_exp:expand(Right, E),
  {[ELeft], EL}  = elixir_exp_clauses:head([Left], E),
  {{'<-', Meta, [ELeft, ERight]}, elixir_env:mergev(EL, ER)};
expand(X, E) ->
  elixir_exp:expand(X, E).

expand_else(_Meta, KV, E) when is_list(KV) ->
  {[{do, EClauses}], EC} = elixir_exp_clauses:'case'([], [{do, KV}], E),
  {[{else, EClauses}], EC};
expand_else(_Meta, nil, E) ->
  {[], E};
expand_else(Meta, _KV, E) ->
  Message = "expected -> clauses for else in with",
  elixir_errors:compile_error(Meta, ?m(E, file), Message, []).

%% Translation

translate(Meta, Args, S) ->
  {Parts, [{do, Expr} | ExprList]} = elixir_utils:split_last(Args),
  case ExprList of
    [{else, ElseExpr}] ->
      {TCase, TS, HasMatch} = translate_case(Parts, {ok, Expr}, fun(X) -> {error, X} end, S),
      translate_else(Meta, TCase, ElseExpr, TS, HasMatch);
    [] ->
      {TCase, TS, _HasMatch} = translate_case(Parts, Expr, fun(X) -> X end, S),
      {TCase, TS}
  end.

translate_case(Parts, DoExpr, Wrapper, S) ->
  {Case, HasMatch} = build_case(Parts, DoExpr, Wrapper, false),
  {TCase, TS} = elixir_translator:translate(Case, S#elixir_scope{extra=nil}),
  {TCase, elixir_scope:mergec(S, TS), HasMatch}.

translate_else(Meta, WithCases, _ElseExpr, S, false) ->
  Warning =
    "\"else\" clauses will never match"
    " because all patterns in \"with\" will always match",
  elixir_errors:warn(?line(Meta), S#elixir_scope.file, Warning),
  Ann = ?ann(Meta),
  Call = {call, Ann,
    {remote, Ann, {atom, Ann, erlang}, {atom, Ann, element}},
    [{integer, Ann, 2}, WithCases]
  },
  {Call, S};
translate_else(Meta, WithCases, ElseExpr, S, true) ->
  ElseClauses = build_else(Meta, ElseExpr),
  {TClauses, TS} = elixir_clauses:clauses(Meta, ElseClauses, S#elixir_scope{extra=nil}),
  {{'case', ?ann(Meta), WithCases, TClauses}, elixir_scope:mergec(S, TS)}.

build_case([{'<-', Meta, [{Name, _, Ctx}, _] = Args} | Rest], DoExpr, Wrapper, HasMatch)
    when is_atom(Name) andalso is_atom(Ctx) ->
  build_case([{'=', Meta, Args} | Rest], DoExpr, Wrapper, HasMatch);
build_case([{'<-', Meta, [Left, Right]} | Rest], DoExpr, Wrapper, _HasMatch) ->
  {InnerCase, true} = build_case(Rest, DoExpr, Wrapper, true),
  Other = {other, Meta, ?MODULE},
  Generated = ?generated(Meta),
  Clauses = [
    {'->', Generated, [[Left], InnerCase]},
    {'->', Generated, [[Other], Wrapper(Other)]}
  ],
  {{'case', Generated, [Right, [{do, Clauses}]]}, true};
build_case([Expr | Rest], DoExpr, Wrapper, HasMatch) ->
  {InnerCase, InnerHasMatch} = build_case(Rest, DoExpr, Wrapper, HasMatch),
  {{'__block__', [], [Expr, InnerCase]}, InnerHasMatch};
build_case([], DoExpr, _Wrapper, HasMatch) ->
  {DoExpr, HasMatch}.

build_else(Meta, ElseClauses) ->
  Result = {result, Meta, ?MODULE},
  [{match, Meta, [{ok, Result}], Result} |
    each_clause_to_error_match(ElseClauses)] ++ [build_raise(Meta)].

each_clause_to_error_match(Clauses) ->
  [{match, Meta, [error_match_for_match(Match)], Expr} ||
    {'->', Meta, [[Match], Expr]} <- Clauses].

error_match_for_match({'when', Meta, [Left, Right]}) ->
  {'when', Meta, [{error, Left}, Right]};
error_match_for_match(Match) ->
  {error, Match}.

build_raise(Meta) ->
  Other = {other, Meta, ?MODULE},
  {match, ?generated(Meta), [{error, Other}], {{'.', Meta, [erlang, error]}, Meta, [{with_clause, Other}]}}.
