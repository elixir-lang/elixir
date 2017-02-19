-module(elixir_with).
-export([expand/3]).
-include("elixir.hrl").

expand(Meta, Args, Env) ->
  {Exprs, Opts} =
    case elixir_utils:split_last(Args) of
      {_, LastArg} = SplitResult when is_list(LastArg) ->
        SplitResult;
      _ ->
        {Args, []}
    end,

  {DoExpr, OtherOpts} =
    case lists:keytake(do, 1, Opts) of
      {value, {do, DoValue}, RestOpts} ->
        {DoValue, RestOpts};
      false ->
        elixir_errors:compile_error(Meta, ?key(Env, file), "missing do keyword in with")
    end,

  ElseExpr =
    case lists:keytake(else, 1, OtherOpts) of
      {value, {else, ElseValue}, []} ->
        assert_clauses(Meta, ElseValue, Env),
        ElseValue;
      {value, _, [{Key, _} | _]} ->
        elixir_errors:compile_error(Meta, ?key(Env, file),
          "unexpected keyword ~ts in with", [Key]);
      false ->
        nil
    end,

  ResultCase =
    case ElseExpr of
      nil ->
        {MainCase, _} = build_main_case(Exprs, DoExpr, fun(Ret) -> Ret end, false),
        MainCase;
      _ ->
        Wrapper = fun(Ret) -> {error, Ret} end,
        case build_main_case(Exprs, {ok, DoExpr}, Wrapper, false) of
          {MainCase, false} ->
            Message =
              "\"else\" clauses will never match"
              " because all patterns in \"with\" will always match",
            elixir_errors:warn(?line(Meta), ?key(Env, file), Message),
            {{'.', Meta, [erlang, element]}, Meta, [MainCase, 2]};
          {MainCase, true} ->
            build_else_case(Meta, MainCase, ElseExpr, Wrapper)
        end
    end,
  elixir_expand:expand(ResultCase, Env).

%% Helpers

assert_clauses(_Meta, [], _Env) ->
  ok;
assert_clauses(Meta, [{'->', _, [_, _]} | Rest], Env) ->
  assert_clauses(Meta, Rest, Env);
assert_clauses(Meta, _Other, Env) ->
  Message = "expected -> clauses for else in with",
  elixir_errors:compile_error(Meta, ?key(Env, file), Message, []).

build_main_case([{'<-', Meta, [{Name, _, Ctx}, _] = Args} | Rest], DoExpr, Wrapper, HasMatch)
    when is_atom(Name) andalso is_atom(Ctx) ->
  build_main_case([{'=', Meta, Args} | Rest], DoExpr, Wrapper, HasMatch);
build_main_case([{'<-', Meta, [Left, Right]} | Rest], DoExpr, Wrapper, _HasMatch) ->
  {InnerCase, true} = build_main_case(Rest, DoExpr, Wrapper, true),
  Generated = ?generated(Meta),
  Other = {other, Generated, 'Elixir'},
  Clauses = [
    {'->', Generated, [[Left], InnerCase]},
    {'->', Generated, [[Other], Wrapper(Other)]}
  ],
  {{'case', [{export_vars, false} | Meta], [Right, [{do, Clauses}]]}, true};
build_main_case([Expr | Rest], DoExpr, Wrapper, HasMatch) ->
  {InnerCase, InnerHasMatch} = build_main_case(Rest, DoExpr, Wrapper, HasMatch),
  {{'__block__', [], [Expr, InnerCase]}, InnerHasMatch};
build_main_case([], DoExpr, _Wrapper, HasMatch) ->
  {DoExpr, HasMatch}.

build_else_case(Meta, MainCase, Clauses, Wrapper) ->
  Generated = ?generated(Meta),

  Return = {return, Generated, 'Elixir'},
  ReturnClause = {'->', Generated, [[{ok, Return}], Return]},

  Other = {other, Generated, 'Elixir'},
  RaiseError = {{'.', Generated, [erlang, error]}, Meta, [{with_clause, Other}]},
  RaiseErrorClause = {'->', Generated, [[Wrapper(Other)], RaiseError]},

  ClauseWrapper = fun(Clause) -> wrap_clause_pattern(Clause, Wrapper) end,
  ResultClauses = [ReturnClause] ++ lists:map(ClauseWrapper, Clauses) ++ [RaiseErrorClause],
  {'case', [{export_vars, false} | Meta], [MainCase, [{do, ResultClauses}]]}.

wrap_clause_pattern({'->', Meta, [[Left], Right]}, Wrapper) ->
  {'->', Meta, [[wrap_pattern(Left, Wrapper)], Right]}.

wrap_pattern({'when', Meta, [Left, Right]}, Wrapper) ->
  {'when', Meta, [Wrapper(Left), Right]};
wrap_pattern(Expr, Wrapper) ->
  Wrapper(Expr).
