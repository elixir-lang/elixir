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

  {DoExpr, BlockWithoutDo} =
    case lists:keytake(do, 1, Block) of
      {value, {do, Do}, ElseRest} ->
        {Do, ElseRest};
      false ->
        elixir_errors:compile_error(Meta, ?m(E, file),
          "missing do keyword in with")
    end,
  {ElseExpr, Opts} =
    case lists:keytake(else, 1, BlockWithoutDo) of
      {value, {else, Else}, Rest} ->
        {Else, Rest};
      false ->
        {nil, BlockWithoutDo}
    end,

  {EOpts, EO} = elixir_exp:expand(Opts, E),
  {ECases, EC} = lists:mapfoldl(fun expand/2, EO, Cases),
  {EExpr, _} = elixir_exp:expand(DoExpr, EC),
  {EEExpr, _} = expand_else(ElseExpr, E),
  {{with, Meta, ECases ++ [[{do, EExpr} | EEExpr] ++ EOpts]}, E}.

expand({'<-', Meta, [Left, Right]}, E) ->
  {ERight, ER} = elixir_exp:expand(Right, E),
  {ELeft, EL}  = elixir_exp_clauses:match(fun elixir_exp:expand/2, Left, E),
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
      [{else, ElseExpr} | _Opts] ->
        RefVar = {ref, Meta, elixir_with},
        MakeRefExpr = make_ref(RefVar, Meta),
        Cases = build_cases(Parts, {RefVar, Expr}),
        Else = build_else(Meta, Cases, RefVar, ElseExpr),
        {'__block__', Meta, [MakeRefExpr, Else]};
      _Opts ->
        build_cases(Parts, Expr)
    end,
  {TC, TS} = elixir_translator:translate(CaseExpr, S),
  {TC, elixir_scope:mergec(S, TS)}.

build_cases([{'<-', Meta, [Left, Right]} | Rest], DoExpr) ->
  Other = {'other', Meta, ?MODULE},
  Clauses = [
    {'->', Meta, [[Left], build_cases(Rest, DoExpr)]},
    {'->', Meta, [[Other], Other]}
  ],
  {'case', Meta, [Right, [{do, Clauses}]]};
build_cases([], DoExpr) ->
  DoExpr;
build_cases([{_, Meta, _} = Expr | Rest], DoExpr) ->
  {'__block__', Meta, [Expr, build_cases(Rest, DoExpr)]}.

build_else(Meta, WithCases, RefVar, ElseClauses) ->
  Result = {'result', Meta, nil},
  MatchAllExist = has_match_all_clause(ElseClauses),
  Clauses = [
    {'->', Meta, [[{{'^', Meta, [RefVar]}, Result}], Result]}
    | ElseClauses
  ] ++ [build_raise(Meta) || not MatchAllExist],
  {'case', Meta, [WithCases, [{do, Clauses}]]}.

has_match_all_clause([{'->', _, [[Match], _]} | ElseClauses]) ->
  has_match_all_clause(Match) orelse has_match_all_clause(ElseClauses);
has_match_all_clause([{'->', _, _} | ElseClauses]) ->
  has_match_all_clause(ElseClauses);
has_match_all_clause({'=', _, Expressions}) ->
  lists:all(fun has_match_all_clause/1, Expressions);
has_match_all_clause({VarName, _, Context}) when is_atom(VarName) andalso is_atom(Context) ->
  true;
has_match_all_clause(_) ->
  false.

make_ref(RefVar, Meta) ->
  RefCall = {{'.', Meta, [erlang, make_ref]}, [], []},
  {'=', Meta, [RefVar, RefCall]}.

build_raise(Meta) ->
  Other = {'other', Meta, nil},
  {'->', Meta, [[Other], {{'.', Meta, [erlang, error]}, Meta, [{with_clause, Other}]}]}.
