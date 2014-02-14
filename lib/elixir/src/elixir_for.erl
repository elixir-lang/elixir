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
  { AccName, _, SA } = elixir_scope:build_var('_', S),
  { VarName, _, SV } = elixir_scope:build_var('_', SA),

  Line = ?line(Meta),
  Acc  = { var, Line, AccName },
  Var  = { var, Line, VarName },

  { Cases, [{do,Expr}] } = elixir_utils:split_last(Args),
  { TCases, SC } = translate_gen(Meta, Cases, [], SV),
  { TExpr, SE } = elixir_translator:translate(Expr, SC),
  { build(TCases, TExpr, Var, Acc), elixir_scope:mergef(SV, SE) }.

translate_gen(ForMeta, [{ '<-', Meta, [Left, Right] }|T], Acc, S) ->
  { TRight, SR } = elixir_translator:translate(Right, S),
  { TLeft, SL } = elixir_clauses:match(fun elixir_translator:translate/2, Left, SR),
  { TT, { TFilters, SF } } = translate_filters(ForMeta, T, SL),
  TAcc = [{ enum, Meta, TLeft, TRight, TFilters }|Acc],
  translate_gen(ForMeta, TT, TAcc, SF);
translate_gen(_ForMeta, [], Acc, S) ->
  { lists:reverse(Acc), S };
translate_gen(ForMeta, _, _, S) ->
  elixir_errors:compile_error(ForMeta, S#elixir_scope.file,
    "for comprehensions must start with a generator").

translate_filters(Meta, T, S) ->
  { Filters, Rest } = collect_filters(T, []),
  { TFilters, TS } = lists:mapfoldl(fun elixir_translator:translate/2, S, Filters),

  Line = ?line(Meta),
  Join = fun(X, { TF, SF }) -> join_filter(Line, X, TF, SF) end,
  { Rest, lists:foldl(Join, { {atom, Line, true}, TS }, TFilters) }.

collect_filters([{ '<-', _, [_, _] }|_] = T, Acc) ->
  { Acc, T };
collect_filters([H|T], Acc) ->
  collect_filters(T, [H|Acc]);
collect_filters([], Acc) ->
  { Acc, [] }.

%% If all we have is one enum generator, we check if it is a list
%% for optimization otherwise fallback to the reduce generator.
build([{ enum, Meta, Left, Right, Filters }] = Orig, Expr, Var, Acc) ->
  Line = ?line(Meta),

  case Right of
    { cons, _, _, _ } ->
      build_comprehension(lc, Line, Orig, Expr);
    { Other, _, _ } when Other == tuple; Other == map ->
      build_reduce(Orig, Expr, Acc);
    _ ->
      Gens = [{ enum, Meta, Left, Var, Filters }],

      {'case', -1, Right, [
        {clause, -1,
          [Var],
          [[?wrap_call(Line, erlang, is_list, [Var])]],
          [build_comprehension(lc, Line, Gens, Expr)]},
        {clause, -1,
          [Var],
          [],
          [build_reduce(Gens, Expr, Acc)]}
      ]}
  end;

%% Common case
build(Clauses, Expr, _Var, Acc) ->
  build_reduce(Clauses, Expr, Acc).

build_reduce(Clauses, Expr, Acc) ->
  ?wrap_call(0, lists, reverse, [build_reduce_clause(Clauses, Expr, {nil, 0}, Acc)]).

%% Helpers

build_reduce_clause([{ enum, Meta, Left, Right, Filters }|T], Expr, Arg, Acc) ->
  Line  = ?line(Meta),
  Inner = build_reduce_clause(T, Expr, Acc, Acc),

  True  = cont(Line, Inner),
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
      [join_gen(Line, Filters, True, False)]}|Clauses0],

  Args  = [Right, cont(Line, Arg), {'fun', Line, {clauses, Clauses1}}],
  Tuple = ?wrap_call(Line, 'Elixir.Enumerable', reduce, Args),
  ?wrap_call(Line, erlang, element, [{ integer, Line, 2 }, Tuple]);
build_reduce_clause([], Expr, _Arg, Acc) ->
  {cons, 0, Expr, Acc}.

is_var({var, _, _}) -> true;
is_var(_) -> false.

cont(Line, Tail) ->
  {tuple, Line, [{atom, Line, cont}, Tail]}.

build_comprehension(Kind, Line, Clauses, Expr) ->
  {Kind, Line, Expr, build_comprehension_clause(Clauses)}.

build_comprehension_clause([{ Kind, Meta, Left, Right, Filters }|T]) ->
  Line = ?line(Meta),
  [{comprehension_clause(Kind), Line, Left, Right}] ++
    comprehension_filter(Line, Filters) ++
    build_comprehension_clause(T);
build_comprehension_clause([]) ->
  [].

comprehension_clause(enum) -> generate;
comprehension_clause(bit) -> b_generate.

comprehension_filter(_Line, { atom, _, true }) ->
  [];
comprehension_filter(_Line, { 'case', _, _, _ } = Filters) ->
  [Filters];
comprehension_filter(Line, Filters) ->
  [{match, Line, {var, Line, '_'}, Filters}].

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
          {op, Line, '==', Var, {atom, Line, false}},
          {op, Line, '==', Var, {atom, Line, nil}}},

      { {'case', Line, Prev, [
        {clause, Line, [Var], [[Guard]], [{atom, Line, false}] },
        {clause, Line, [Any], [], [Next] }
      ] }, TS }
  end.

join_gen(_Line, {atom, _, true}, True, _False) ->
  True;
join_gen(Line, Clause, True, False) ->
  {'case', Line, Clause, [
    {clause, Line, [{atom, Line, true}], [], [True] },
    {clause, Line, [{atom, Line, false}], [], [False] }
  ] }.
