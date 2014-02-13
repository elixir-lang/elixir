-module(elixir_for).
-export([expand/3, translate/3]).
-include("elixir.hrl").

%% Expansion

expand(Meta, Args, E) ->
  case elixir_utils:split_last(Args) of
    { Cases, [{do,Expr}] } ->
      { ECases, EC } = lists:mapfoldl(fun expand/2, E, Cases),
      { EExpr, _ }   = elixir_exp:expand(Expr, EC),
      { { for, Meta, ECases ++ [[{do,EExpr}]] }, E };
    _ ->
      elixir_errors:compile_error(Meta, E#elixir_env.file,
        "missing do keyword in for comprehension")
  end.

expand({'<-', Meta, [Left, Right]}, E) ->
  { ERight, ER } = elixir_exp:expand(Right, E),
  { ELeft, EL }  = elixir_exp_clauses:match(fun elixir_exp:expand/2, Left, E),
  { { '<-', Meta, [ELeft, ERight] }, elixir_env:mergev(EL, ER) };
expand({ '<<>>', Meta, Args } = X, E) when is_list(Args) ->
  case elixir_utils:split_last(Args) of
    { LeftStart, {'<-', OpMeta, [LeftEnd, Right] } } ->
      { ERight, ER } = elixir_exp:expand(Right, E),
      Left = { '<<>>', Meta, LeftStart ++ [LeftEnd] },
      { ELeft, EL }  = elixir_exp_clauses:match(fun elixir_exp:expand/2, Left, E),
      { { '<<>>', [], [ { '<-', OpMeta, [ELeft, ERight] }] }, elixir_env:mergev(EL, ER) };
    _ ->
      expand(X, E)
  end;
expand(X, E) ->
  elixir_exp:expand(X, E).

%% Translation

translate(_Meta, Args, S) ->
  { Cases, [{do,Expr}] } = elixir_utils:split_last(Args),
  translate(Cases, Expr, 1, S).

translate([{ '<-', Meta, [Left, Right] }|T], Expr, Counter, S) ->
  { TRight, SR } = elixir_translator:translate(Right, S),
  { TLeft, SL  } = elixir_clauses:match(fun elixir_translator:translate/2, Left, SR),

  { Var, _, SV } = elixir_scope:build_var('_', SL),
  { TExpr, SE }  = translate(T, Expr, Counter+1, SV),

  Line = ?line(Meta),
  TVar = {var, Line, Var},
  Body = {'case', -1, TRight, [
    {clause, -1,
      [TVar],
      [[?wrap_call(Line, erlang, is_list, [TVar])]],
      [list_fun(Line, TLeft, TVar, TExpr)]},
    {clause, -1,
      [TVar],
      [],
      [reduce_fun(Line, TLeft, TVar, TExpr)]}
  ]},

  { Body, SE };

translate([], Expr, _Counter, S) ->
  { TExpr, SE } = elixir_translator:translate(Expr, S),
  { TExpr, elixir_scope:mergef(S, SE) }.

list_fun(Line, TLeft, TRight, TExpr) ->
  For  = {var, Line, '@for'},
  Tail = {var, Line, '@tail'},

  Clauses0 =
    [{clause, Line,
      [{nil, Line}], [],
      [{nil, Line}]},
     {clause, Line,
      [Tail], [],
      [?wrap_call(Line, erlang, error, [badarg(Line, Tail)])]}],

  Clauses1 =
    case is_var(TLeft) of
      true ->
        Clauses0;
      false ->
        [{clause, Line,
          [{cons, Line, {var, Line, '_'}, Tail}], [],
          [{call, Line, For, [Tail]}]}|Clauses0]
    end,

  Clauses2 =
    [{clause, Line,
      [{cons, Line, TLeft, Tail}], [],
      [{cons, Line, TExpr, {call, Line, For, [Tail]}}]}|Clauses1],

  {call,Line,
    {named_fun, Line, element(3, For), Clauses2},
    [TRight]}.

reduce_fun(Line, TLeft, TRight, TExpr) ->
  Acc = {var, Line, '@acc'},

  Clauses0 =
    case is_var(TLeft) of
      true  -> [];
      false ->
        [{clause, Line,
          [{var, Line, '_'}, Acc], [],
          [cont(Line, Acc)]}]
    end,

  Clauses1 =
    [{clause, Line,
      [TLeft, Acc], [],
      [cont(Line, {cons, Line, TExpr, Acc})]}|Clauses0],

  ReduceArgs = [TRight, cont(Line, {nil, Line}), {'fun', Line, {clauses, Clauses1}}],
  ReduceTuple = ?wrap_call(Line, 'Elixir.Enumerable', reduce, ReduceArgs),
  ReduceList = ?wrap_call(Line, erlang, element, [{ integer, Line, 2 }, ReduceTuple]),
  ?wrap_call(Line, lists, reverse, [ReduceList]).

is_var({var, _, _}) -> true;
is_var(_) -> false.

cont(Line, Tail) ->
  {tuple, Line, [{atom, Line, cont}, Tail]}.

badarg(Line, Tail) ->
  {tuple, Line, [{atom, Line, badarg}, Tail]}.
