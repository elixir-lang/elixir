-module(elixir_erl_for).
-export([translate/4]).
-include("elixir.hrl").

translate(Meta, Args, Return, S) ->
  {IntoAccName, _, SIA} = elixir_erl_var:build('_', S),
  {UniqAccName, _, SUA} = elixir_erl_var:build('_', SIA),
  {VarName, _, SV} = elixir_erl_var:build('_', SUA),

  Ann = ?ann(Meta),
  IntoAcc = {var, Ann, IntoAccName},
  UniqAcc = {var, Ann, UniqAccName},
  Var = {var, Ann, VarName},

  {Cases, [{do, Expr} | Opts]} = elixir_utils:split_last(Args),

  {TInto, SI} =
    case lists:keyfind(into, 1, Opts) of
      {into, Into} -> elixir_erl_pass:translate(Into, SV);
      false when Return -> {{nil, Ann}, SV};
      false -> {false, SV}
    end,

  {TUniq, SU} =
    case lists:keyfind(uniq, 1, Opts) of
      {uniq, Uniq} -> build_uniq(Uniq, Ann, SI);
      false -> build_uniq(false, Ann, SI)
    end,

  {TCases, SC} = translate_gen(Meta, Cases, [], SU),
  {TExpr, SE}  = elixir_erl_pass:translate(wrap_expr(Expr, TInto), SC),
  SF = elixir_erl_var:mergec(SI, SE),

  case comprehension_expr(TInto, TExpr) of
    {inline, TIntoExpr} ->
      {build_inline(Ann, TCases, TIntoExpr, TUniq, TInto, Var, IntoAcc, UniqAcc, SE), SF};
    {into, TIntoExpr} ->
      build_into(Ann, TCases, TIntoExpr, TUniq, TInto, Var, IntoAcc, UniqAcc, SF)
  end.

%% In case we have no return, we wrap the expression
%% in a block that returns nil.
wrap_expr(Expr, false) -> {'__block__', [], [Expr, nil]};
wrap_expr(Expr, _)  -> Expr.

build_uniq(false, Ann, S) ->
  {{atom, Ann, nil}, S};
build_uniq(true, Ann, S) ->
  {{map, Ann, []}, S};
build_uniq(Uniq, Ann, S) ->
  Generated = erl_anno:set_generated(true, Ann),
  {Filter, SF} = translate_filter(Uniq, S),
  {join_filter(Generated, Filter, {map, Ann, []}, {atom, Ann, nil}), SF}.

translate_gen(ForMeta, [{'<-', Meta, [Left, Right]} | T], Acc, S) ->
  {TLeft, TRight, TFilters, TT, TS} = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{enum, Meta, TLeft, TRight, TFilters} | Acc],
  translate_gen(ForMeta, TT, TAcc, TS);
translate_gen(ForMeta, [{'<<>>', _, [{'<-', Meta, [Left, Right]}]} | T], Acc, S) ->
  {TLeft, TRight, TFilters, TT, TS} = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{bin, Meta, TLeft, TRight, TFilters} | Acc],
  translate_gen(ForMeta, TT, TAcc, TS);
translate_gen(_ForMeta, [], Acc, S) ->
  {lists:reverse(Acc), S}.

translate_gen(_Meta, Left, Right, T, S) ->
  {TRight, SR} = elixir_erl_pass:translate(Right, S),
  {LeftArgs, LeftGuards} = elixir_utils:extract_guards(Left),
  {TLeft, SL} = elixir_erl_clauses:match(fun elixir_erl_pass:translate/2, LeftArgs,
                                         SR#elixir_erl{extra=pin_guard}),

  TLeftGuards = elixir_erl_clauses:guards(LeftGuards, [], SL),
  ExtraGuards = [{nil, X} || X <- SL#elixir_erl.extra_guards],
  SF = SL#elixir_erl{extra=S#elixir_erl.extra, extra_guards=[]},

  {TT, {TFilters, TS}} = translate_filters(T, SF),

  %% The list of guards is kept in reverse order
  Guards = TFilters ++ translate_guards(TLeftGuards) ++ ExtraGuards,
  {TLeft, TRight, Guards, TT, TS}.

translate_guards([]) ->
  [];
translate_guards([[Guards]]) ->
  [{nil, Guards}];
translate_guards([[Left], [Right] | Rest]) ->
  translate_guards([[{op, element(2, Left), 'orelse', Left, Right}] | Rest]).

translate_filters(T, S) ->
  {Filters, Rest} = collect_filters(T, []),
  {Rest, lists:mapfoldr(fun translate_filter/2, S, Filters)}.

translate_filter(Filter, S) ->
  {TFilter, TS} = elixir_erl_pass:translate(Filter, S),
  case elixir_utils:returns_boolean(Filter) of
    true ->
      {{nil, TFilter}, TS};
    false ->
      {Name, _, VS} = elixir_erl_var:build('_', TS),
      {{{var, 0, Name}, TFilter}, VS}
  end.

collect_filters([{'<-', _, [_, _]} | _] = T, Acc) ->
  {Acc, T};
collect_filters([{'<<>>', _, [{'<-', _, [_, _]}]} | _] = T, Acc) ->
  {Acc, T};
collect_filters([H | T], Acc) ->
  collect_filters(T, [H | Acc]);
collect_filters([], Acc) ->
  {Acc, []}.

build_inline(Ann, Clauses, Expr, Uniq, Into, _Var, IntoAcc, UniqAcc, S) ->
  case lists:all(fun(Clause) -> element(1, Clause) == bin end, Clauses) of
    true  -> build_comprehension(Ann, Clauses, Expr, Into);
    false -> build_reduce(Clauses, Expr, Uniq, Into, IntoAcc, UniqAcc, S)
  end.

build_into(Ann, Clauses, Expr, Uniq, {map, _, []} = Into, _Var, IntoAcc, UniqAcc, S) ->
  AccTuple = {tuple, Ann, [Into, Uniq]},
  {IntoExpr, SI} = build_into_map_expr(Expr, Ann, IntoAcc, UniqAcc, S),
  Map = build_reduce_clause(Clauses, IntoExpr, AccTuple, IntoAcc, UniqAcc, SI),
  {extract_into(Map, Ann), SI};

build_into(Ann, Clauses, Expr, Uniq, Into, Fun, IntoAcc, UniqAcc, S) ->
  {Kind, SK}   = build_var(Ann, S),
  {Reason, SR} = build_var(Ann, SK),
  {Stack, ST}  = build_var(Ann, SR),
  {Done, SD}   = build_var(Ann, ST),

  AccTuple = {tuple, Ann, [IntoAcc, UniqAcc]},
  {IntoExpr, SI} = build_into_expr(Expr, Ann, Fun, IntoAcc, UniqAcc, SD),

  MatchIntoExpr = {match, Ann,
    {tuple, Ann, [IntoAcc, Fun]},
    elixir_erl:remote(Ann, 'Elixir.Collectable', into, [Into])
  },
  MatchUniqExpr = {match, Ann, UniqAcc, Uniq},

  TryExpr =
    {'try', Ann,
      [build_reduce_clause(Clauses, IntoExpr, AccTuple, IntoAcc, UniqAcc, SI)],
      [{clause, Ann,
        [Done],
        [],
        [{call, Ann, Fun, [extract_into(Done, Ann), {atom, Ann, done}]}]}],
      [{clause, Ann,
        [{tuple, Ann, [Kind, Reason, {var, Ann, '_'}]}],
        [],
        [{match, Ann, Stack, elixir_erl:remote(Ann, erlang, get_stacktrace, [])},
         {call, Ann, Fun, [IntoAcc, {atom, Ann, halt}]},
         elixir_erl:remote(Ann, erlang, raise, [Kind, Reason, Stack])]}],
      []},

  {{block, Ann, [MatchIntoExpr, MatchUniqExpr, TryExpr]}, SI}.

%% Helpers

build_reduce(Clauses, Expr, _, false, IntoAcc, UniqAcc, S) ->
  Arg = {tuple, 0, [{nil, 0}, {atom, 0, nil}]},
  IgnoreExpr = {block, 0, [Expr, Arg]},
  build_reduce_clause(Clauses, IgnoreExpr, Arg, IntoAcc, UniqAcc, S);
build_reduce(Clauses, Expr, Uniq, {nil, Ann} = Into, IntoAcc, UniqAcc, S) ->
  Arg = {tuple, Ann, [Into, Uniq]},
  {ListExpr, SL} = build_reduce_expr(enum, Expr, Ann, IntoAcc, UniqAcc, S),
  List = build_reduce_clause(Clauses, ListExpr, Arg, IntoAcc, UniqAcc, SL),
  elixir_erl:remote(Ann, lists, reverse, [extract_into(List, Ann)]);
build_reduce(Clauses, Expr, Uniq, {bin, Ann, _} = Into, IntoAcc, UniqAcc, S) ->
  Arg = {tuple, Ann, [Into, Uniq]},
  {BinExpr, SB} = build_reduce_expr(bin, Expr, Ann, IntoAcc, UniqAcc, S),
  Bin = build_reduce_clause(Clauses, BinExpr, Arg, IntoAcc, UniqAcc, SB),
  elixir_erl:remote(Ann, erlang, iolist_to_binary, [extract_into(Bin, Ann)]).

extract_into(Expr, Ann) ->
  elixir_erl:remote(Ann, erlang, element, [{integer, Ann, 1}, Expr]).

build_into_map_expr(Expr, Ann, IntoAcc, UniqAcc, S) ->
  {Key, SK} = build_var(Ann, S),
  {Val, SV} = build_var(Ann, SK),
  MapExpr = fun (Value, Acc) ->
    {block, Ann, [
      {match, Ann, {tuple, Ann, [Key, Val]}, Value},
      {call, Ann, {remote, Ann, {atom, Ann, maps}, {atom, Ann, put}}, [Key, Val, Acc]}
    ]}
  end,
  build_reduce_expr(MapExpr, Expr, Ann, IntoAcc, UniqAcc, SV).

build_into_expr(Expr, Ann, Fun, IntoAcc, UniqAcc, S) ->
  IntoExpr = fun (Value, Acc) -> {call, Ann, Fun, [Acc, pair(Ann, cont, Value)]} end,
  build_reduce_expr(IntoExpr, Expr, Ann, IntoAcc, UniqAcc, S).

build_reduce_expr(enum, Expr, Ann, IntoAcc, UniqAcc, S) ->
  ListExpr = fun (Value, Acc) -> {cons, Ann, Value, Acc} end,
  build_reduce_expr(ListExpr, Expr, Ann, IntoAcc, UniqAcc, S);
build_reduce_expr(bin, Expr, Ann, IntoAcc, UniqAcc, S) ->
  BinExpr = fun (Value, Acc) -> {cons, Ann, Acc, Value} end,
  build_reduce_expr(BinExpr, Expr, Ann, IntoAcc, UniqAcc, S);

build_reduce_expr(ReduceExpr, Expr, Ann, IntoAcc, UniqAcc, S) ->
  {Value, SV} = build_var(Ann, S),
  AccTuple = {tuple, Ann, [IntoAcc, UniqAcc]},
  UniqExpr = elixir_erl:remote(Ann, maps, put, [Value, {atom, Ann, true}, UniqAcc]),

  Block = {block, Ann, [
    {match, Ann, Value, Expr},
    {'case', Ann, UniqAcc, [
      {clause, Ann, [{atom, Ann, nil}], [], [{tuple, Ann, [ReduceExpr(Value, IntoAcc), {atom, Ann, nil}]}]},
      {clause, Ann, [{map, Ann, [{map_field_exact, Ann, Value, {atom, Ann, true}}]}], [], [AccTuple]},
      {clause, Ann, [{map, Ann, []}], [], [{tuple, Ann, [ReduceExpr(Value, IntoAcc), UniqExpr]}]}
    ]}
  ]},
  {Block, SV}.

build_reduce_clause([{enum, Meta, Left, Right, Filters} | T], Expr, Arg, IntoAcc, UniqAcc, S) ->
  Ann = ?ann(Meta),
  AccTuple = {tuple, Ann, [IntoAcc, UniqAcc]},
  True = build_reduce_clause(T, Expr, AccTuple, IntoAcc, UniqAcc, S),
  False = AccTuple,
  Generated = erl_anno:set_generated(true, Ann),

  Clauses0 =
    case is_var(Left) of
      true  -> [];
      false ->
        [{clause, Generated,
          [{var, Ann, '_'}, AccTuple], [],
          [False]}]
    end,

  Clauses1 =
    [{clause, Ann,
      [Left, AccTuple], [],
      [join_filters(Generated, Filters, True, False)]} | Clauses0],

  Args  = [Right, Arg, {'fun', Ann, {clauses, Clauses1}}],
  elixir_erl:remote(Ann, 'Elixir.Enum', reduce, Args);

build_reduce_clause([{bin, Meta, Left, Right, Filters} | T], Expr, Arg, IntoAcc, UniqAcc, S) ->
  Ann = ?ann(Meta),
  Generated  = erl_anno:set_generated(true, Ann),
  {Tail, ST} = build_var(Ann, S),
  {Fun, SF}  = build_var(Ann, ST),
  AccTuple = {tuple, Ann, [IntoAcc, UniqAcc]},

  True  = build_reduce_clause(T, Expr, AccTuple, IntoAcc, UniqAcc, SF),
  False = AccTuple,

  {bin, _, Elements} = Left,

  BinMatch =
    {bin, Ann, Elements ++ [{bin_element, Ann, Tail, default, [bitstring]}]},
  NoVarMatch =
    {bin, Ann, no_var(Elements) ++ [{bin_element, Ann, Tail, default, [bitstring]}]},

  Clauses =
    [{clause, Ann,
      [BinMatch, AccTuple], [],
      [{call, Ann, Fun, [Tail, join_filters(Generated, Filters, True, False)]}]},
     {clause, Generated,
      [NoVarMatch, AccTuple], [],
      [{call, Ann, Fun, [Tail, False]}]},
     {clause, Generated,
      [{bin, Ann, []}, AccTuple], [],
      [AccTuple]},
     {clause, Generated,
      [Tail, {var, Ann, '_'}], [],
      [elixir_erl:remote(Ann, erlang, error, [pair(Ann, badarg, Tail)])]}],

  {call, Ann,
    {named_fun, Ann, element(3, Fun), Clauses},
    [Right, Arg]};

build_reduce_clause([], Expr, _Arg, _IntoAcc, _UniqAcc, _S) ->
  Expr.

is_var({var, _, _}) -> true;
is_var(_) -> false.

pair(Ann, Atom, Arg) ->
  {tuple, Ann, [{atom, Ann, Atom}, Arg]}.

build_var(Ann, S) ->
  {Name, _, ST} = elixir_erl_var:build('_', S),
  {{var, Ann, Name}, ST}.

no_var(Elements) ->
  [{bin_element, Ann, no_var_expr(Expr), Size, Types} ||
    {bin_element, Ann, Expr, Size, Types} <- Elements].
no_var_expr({var, Ann, _}) ->
  {var, Ann, '_'}.

build_comprehension(Ann, Clauses, Expr, false) ->
  {lc, Ann, Expr, comprehension_clause(Clauses)};
build_comprehension(Ann, Clauses, Expr, Into) ->
  {comprehension_kind(Into), Ann, Expr, comprehension_clause(Clauses)}.

comprehension_clause([{Kind, Meta, Left, Right, Filters} | T]) ->
  Ann = ?ann(Meta),
  [{comprehension_generator(Kind), Ann, Left, Right}] ++
    comprehension_filter(Ann, Filters) ++
    comprehension_clause(T);
comprehension_clause([]) ->
  [].

comprehension_kind({nil, _}) -> lc;
comprehension_kind({bin, _, []}) -> bc.

comprehension_generator(enum) -> generate;
comprehension_generator(bin) -> b_generate.

comprehension_expr({bin, _, []}, {bin, _, _} = Expr) ->
  {inline, Expr};
comprehension_expr({bin, Ann, []}, Expr) ->
  BinExpr = {bin, Ann, [{bin_element, Ann, Expr, default, [bitstring]}]},
  {inline, BinExpr};
comprehension_expr({nil, _}, Expr) ->
  {inline, Expr};
comprehension_expr(false, Expr) ->
  {inline, Expr};
comprehension_expr(_, Expr) ->
  {into, Expr}.

comprehension_filter(Ann, Filters) ->
  [join_filter(Ann, Filter, {atom, Ann, true}, {atom, Ann, false}) ||
   Filter <- lists:reverse(Filters)].

join_filters(_Ann, [], True, _False) ->
  True;
join_filters(Ann, [H | T], True, False) ->
  lists:foldl(fun(Filter, Acc) ->
    join_filter(Ann, Filter, Acc, False)
  end, join_filter(Ann, H, True, False), T).

join_filter(Ann, {nil, Filter}, True, False) ->
  {'case', Ann, Filter, [
    {clause, Ann, [{atom, Ann, true}], [], [True]},
    {clause, Ann, [{atom, Ann, false}], [], [False]}
  ]};
join_filter(Ann, {Var, Filter}, True, False) ->
  Guard =
    {op, Ann, 'orelse',
      {op, Ann, '==', Var, {atom, Ann, false}},
      {op, Ann, '==', Var, {atom, Ann, nil}}},

  {'case', Ann, Filter, [
    {clause, Ann, [Var], [[Guard]], [False]},
    {clause, Ann, [{var, Ann, '_'}], [], [True]}
  ]}.
