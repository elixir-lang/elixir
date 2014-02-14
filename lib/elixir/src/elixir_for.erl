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
  { TCases, SC } = translate_gen(Meta, Cases, [], S),
  { TExpr, SE } = elixir_translator:translate(Expr, SC),
  build_gen(TCases, TExpr, elixir_scope:mergef(S, SE)).

translate_gen(ForMeta, [{ '<-', Meta, [Left, Right] }|T], Acc, S) ->
  { TRight, SR } = elixir_translator:translate(Right, S),
  { TLeft, SL }  = elixir_clauses:match(fun elixir_translator:translate/2, Left, SR),
  { TT, { TFilters, SF } } = translate_filters(ForMeta, T, SL),
  TAcc = [{ enum, Meta, TLeft, TRight, TFilters }|Acc],
  translate_gen(ForMeta, TT, TAcc, SF);
translate_gen(_ForMeta, [], Acc, S) ->
  { lists:reverse(Acc), S }.

translate_filters(Meta, T, S) ->
  { Filters, Rest } = collect_filters(T, []),
  Line = ?line(Meta),
  Join = fun(X, { TF, SF }) -> join_filter(Line, X, TF, SF) end,
  { TFilters, TS } = lists:mapfoldl(fun elixir_translator:translate/2, S, Filters),
  { Rest, lists:foldl(Join, { {atom, Line, true}, TS }, TFilters) }.

collect_filters([{ '<-', _, [_, _] }|_] = T, Acc) ->
  { Acc, T };
collect_filters([H|T], Acc) ->
  collect_filters(T, [H|Acc]);
collect_filters([], Acc) ->
  { Acc, [] }.

build_gen([{ enum, Meta, Left, Right, Filters }|T], Expr, S) ->
  { Name, _, SV } = elixir_scope:build_var('_', S),
  { Inner, SI } = build_gen(T, Expr, SV),

  Line = ?line(Meta),
  Var  = {var, Line, Name},

  Body = {'case', -1, Right, [
    {clause, -1,
      [Var],
      [[?wrap_call(Line, erlang, is_list, [Var])]],
      [list_fun(Line, Filters, Left, Var, Inner)]},
    {clause, -1,
      [Var],
      [],
      [reduce_fun(Line, Filters, Left, Var, Inner)]}
  ]},

  { Body, SI };
build_gen([], Expr, S) ->
  { Expr, S }.

%% Helpers

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

join_gen(_Line, { atom, _, true }, True, _False) ->
  True;
join_gen(Line, Clause, True, False) ->
  {'case', Line, Clause, [
    {clause, Line, [{atom, Line, true}], [], [True] },
    {clause, Line, [{atom, Line, false}], [], [False] }
  ] }.

list_fun(Line, Cond, Left, Right, Expr) ->
  For  = {var, Line, '@for'},
  Tail = {var, Line, '@tail'},

  True  = {cons, Line, Expr, {call, Line, For, [Tail]}},
  False = {call, Line, For, [Tail]},

  Clauses0 =
    [{clause, Line,
      [{nil, Line}], [],
      [{nil, Line}]},
     {clause, Line,
      [Tail], [],
      [?wrap_call(Line, erlang, error, [badarg(Line, Tail)])]}],

  Clauses1 =
    case is_var(Left) of
      true ->
        Clauses0;
      false ->
        [{clause, Line,
          [{cons, Line, {var, Line, '_'}, Tail}], [],
          [False]}|Clauses0]
    end,

  Clauses2 =
    [{clause, Line,
      [{cons, Line, Left, Tail}], [],
      [join_gen(Line, Cond, True, False)]}|Clauses1],

  {call,Line,
    {named_fun, Line, element(3, For), Clauses2},
    [Right]}.

reduce_fun(Line, Cond, Left, Right, Expr) ->
  Acc = {var, Line, '@acc'},

  True  = cont(Line, {cons, Line, Expr, Acc}),
  False = cont(Line, Acc),

  Clauses0 =
    case is_var(Left) of
      true  -> [];
      false ->
        [{clause, Line,
          [{var, Line, '_'}, Acc], [],
          [False]}]
    end,

  Clauses1 =
    [{clause, Line,
      [Left, Acc], [],
      [join_gen(Line, Cond, True, False)]}|Clauses0],

  ReduceArgs = [Right, cont(Line, {nil, Line}), {'fun', Line, {clauses, Clauses1}}],
  ReduceTuple = ?wrap_call(Line, 'Elixir.Enumerable', reduce, ReduceArgs),
  ReduceList = ?wrap_call(Line, erlang, element, [{ integer, Line, 2 }, ReduceTuple]),
  ?wrap_call(Line, lists, reverse, [ReduceList]).

is_var({var, _, _}) -> true;
is_var(_) -> false.

cont(Line, Tail) ->
  {tuple, Line, [{atom, Line, cont}, Tail]}.

badarg(Line, Tail) ->
  {tuple, Line, [{atom, Line, badarg}, Tail]}.
