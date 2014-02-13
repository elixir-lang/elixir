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
      elixir_exp:expand(X, E)
  end;
expand(X, E) ->
  elixir_exp:expand(X, E).

%% Translation

translate(Meta, Args, S) ->
  { Cases, [{do,Expr}] } = elixir_utils:split_last(Args),
  translate_gen(Meta, Cases, Expr, S).

translate_gen(ForMeta, [{ '<-', Meta, [Left, Right] }|T], Expr, S) ->
  { TRight, SR } = elixir_translator:translate(Right, S),
  { TLeft, SL  } = elixir_clauses:match(fun elixir_translator:translate/2, Left, SR),

  { Var, _, S0 } = elixir_scope:build_var('_', SL),
  { TT, FF, S1 } = translate_filters(ForMeta, T, S0),
  { TExpr, S2 }  = translate_gen(ForMeta, TT, Expr, S1),

  Line = ?line(Meta),
  TVar = {var, Line, Var},
  Body = {'case', -1, TRight, [
    {clause, -1,
      [TVar],
      [[?wrap_call(Line, erlang, is_list, [TVar])]],
      [list_fun(Line, FF, TLeft, TVar, TExpr)]},
    {clause, -1,
      [TVar],
      [],
      [reduce_fun(Line, FF, TLeft, TVar, TExpr)]}
  ]},

  { Body, S2 };

translate_gen(_ForMeta, [], Expr, S) ->
  { TExpr, SE } = elixir_translator:translate(Expr, S),
  { TExpr, elixir_scope:mergef(S, SE) }.

translate_filters(Meta, Args, S) ->
  case collect_filters(Args, []) of
    { [], Rest } ->
      { Rest, fun(True, _False) -> True end, S };
    { Filters, Rest } ->
      Line = ?line(Meta),
      Join = fun(X, { TF, SF }) -> join_filter(Line, X, TF, SF) end,
      { TFilters, TS } = lists:mapfoldl(fun elixir_translator:translate/2, S, Filters),
      { RFilters, RS } = lists:foldl(Join, { {atom, Line, true}, TS }, TFilters),
      { Rest, fun(True, False) -> join_gen(Line, RFilters, True, False) end, RS }
  end.

collect_filters([{ '<-', _, [_, _] }|_] = T, Acc) ->
  { Acc, T };
collect_filters([H|T], Acc) ->
  collect_filters(T, [H|Acc]);
collect_filters([], Acc) ->
  { Acc, [] }.

join_filter(Line, Prev, Next, S) ->
  case elixir_utils:returns_boolean(Prev) of
    true ->
      { {op, Line, 'andalso', Prev, Next}, S };
    false ->
      { Name, _, TS } = elixir_scope:build_var('_', S),
      Var = {var, Line, Name},
      Any = {var, Line, '_'},

      Guard =
        {op, Line, 'orelse',
          {op, Line, '===', Var, {atom, Line, false}},
          {op, Line, '===', Var, {atom, Line, nil}}},

      { {'case', Line, Prev, [
        {clause, Line, [Var], [[Guard]], [{atom, Line, false}] },
        {clause, Line, [Any], [], [Next] }
      ] }, TS }
  end.

join_gen(Line, Clause, True, False) ->
  {'case', Line, Clause, [
    {clause, Line, [{atom, Line, true}], [], [True] },
    {clause, Line, [{atom, Line, false}], [], [False] }
  ] }.

%% Helpers

list_fun(Line, Cond, TLeft, TRight, TExpr) ->
  For  = {var, Line, '@for'},
  Tail = {var, Line, '@tail'},

  True  = {cons, Line, TExpr, {call, Line, For, [Tail]}},
  False = {call, Line, For, [Tail]},

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
          [False]}|Clauses0]
    end,

  Clauses2 =
    [{clause, Line,
      [{cons, Line, TLeft, Tail}], [],
      [Cond(True, False)]}|Clauses1],

  {call,Line,
    {named_fun, Line, element(3, For), Clauses2},
    [TRight]}.

reduce_fun(Line, Cond, TLeft, TRight, TExpr) ->
  Acc = {var, Line, '@acc'},

  True  = cont(Line, {cons, Line, TExpr, Acc}),
  False = cont(Line, Acc),

  Clauses0 =
    case is_var(TLeft) of
      true  -> [];
      false ->
        [{clause, Line,
          [{var, Line, '_'}, Acc], [],
          [False]}]
    end,

  Clauses1 =
    [{clause, Line,
      [TLeft, Acc], [],
      [Cond(True, False)]}|Clauses0],

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
