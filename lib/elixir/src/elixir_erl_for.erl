-module(elixir_erl_for).
-export([translate/3]).
-include("elixir.hrl").

translate(Meta, Args, S) ->
  {Cases, [{do, Expr} | Opts]} = elixir_utils:split_last(Args),

  case lists:keyfind(reduce, 1, Opts) of
    {reduce, Reduce} -> translate_reduce(Meta, Cases, Expr, Reduce, S);
    false -> translate_into(Meta, Cases, Expr, Opts, S)
  end.

translate_reduce(Meta, Cases, Expr, Reduce, S) ->
  Ann = ?ann(Meta),
  {TReduce, SR} = elixir_erl_pass:translate(Reduce, Ann, S),
  {TCases, SC} = translate_gen(Meta, Cases, [], SR),
  CaseExpr = {'case', Meta, [ok, [{do, Expr}]]},
  {TExpr, SE} = elixir_erl_pass:translate(CaseExpr, Ann, SC),

  InnerFun = fun
    ({'case', CaseAnn, _, CaseBlock}, InnerAcc) -> {'case', CaseAnn, InnerAcc, CaseBlock}
  end,

  build_reduce(Ann, TCases, InnerFun, TExpr, TReduce, false, SE).

translate_into(Meta, Cases, Expr, Opts, S) ->
  Ann = ?ann(Meta),

  {TInto, SI} =
    case lists:keyfind(into, 1, Opts) of
      {into, Into} -> elixir_erl_pass:translate(Into, Ann, S);
      false -> {false, S}
    end,

  TUniq = lists:keyfind(uniq, 1, Opts) == {uniq, true},

  {TCases, SC} = translate_gen(Meta, Cases, [], SI),
  {TExpr, SE}  = elixir_erl_pass:translate(wrap_expr_if_unused(Expr, TInto), Ann, SC),

  case inline_or_into(TInto) of
    inline -> build_inline(Ann, TCases, TExpr, TInto, TUniq, SE);
    into -> build_into(Ann, TCases, TExpr, TInto, TUniq, SE)
  end.

%% In case we have no return, we wrap the expression
%% in a block that returns nil.
wrap_expr_if_unused(Expr, false) -> {'__block__', [], [Expr, nil]};
wrap_expr_if_unused(Expr, _)  -> Expr.

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

translate_gen(Meta, Left, Right, T, S) ->
  Ann = ?ann(Meta),
  {TRight, SR} = elixir_erl_pass:translate(Right, Ann, S),
  {LeftArgs, LeftGuards} = elixir_utils:extract_guards(Left),
  {TLeft, SL} = elixir_erl_clauses:match(Ann, fun elixir_erl_pass:translate/3, LeftArgs,
                                         SR#elixir_erl{extra=pin_guard}),

  TLeftGuards = elixir_erl_clauses:guards(Ann, LeftGuards, [], SL),
  ExtraGuards = [{nil, X} || X <- SL#elixir_erl.extra_guards],
  {Filters, TT} = collect_filters(T, []),

  {TFilters, TS} =
    lists:mapfoldr(
      fun(F, SF) -> translate_filter(F, Ann, SF) end,
      SL#elixir_erl{extra=S#elixir_erl.extra, extra_guards=[]},
      Filters
    ),

  %% The list of guards is kept in reverse order
  Guards = TFilters ++ translate_guards(TLeftGuards) ++ ExtraGuards,
  {TLeft, TRight, Guards, TT, TS}.

translate_guards([]) ->
  [];
translate_guards([[Guards]]) ->
  [{nil, Guards}];
translate_guards([[Left], [Right] | Rest]) ->
  translate_guards([[{op, element(2, Left), 'orelse', Left, Right}] | Rest]).

translate_filter(Filter, Ann, S) ->
  {TFilter, TS} = elixir_erl_pass:translate(Filter, Ann, S),
  case elixir_utils:returns_boolean(Filter) of
    true ->
      {{nil, TFilter}, TS};
    false ->
      {Name, VS} = elixir_erl_var:build('_', TS),
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

build_inline(Ann, Clauses, Expr, Into, Uniq, S) ->
  case not Uniq and lists:all(fun(Clause) -> element(1, Clause) == bin end, Clauses) of
    true  -> {build_comprehension(Ann, Clauses, Expr, Into), S};
    false -> build_inline_each(Ann, Clauses, Expr, Into, Uniq, S)
  end.

build_inline_each(Ann, Clauses, Expr, false, Uniq, S) ->
  InnerFun = fun(InnerExpr, _InnerAcc) -> InnerExpr end,
  build_reduce(Ann, Clauses, InnerFun, Expr, {nil, Ann}, Uniq, S);
build_inline_each(Ann, [{enum, _, Left = {var, _, _}, Right, [] = _Filters}], Expr, {nil, _} = _Into, false, S) ->
  Clauses = [{clause, Ann, [Left], [], [Expr]}],
  Args = [Right, {'fun', Ann, {clauses, Clauses}}],
  {?remote(Ann, 'Elixir.Enum', map, Args), S};
build_inline_each(Ann, [{enum, _, Left = {var, _, _}, Right, [] = _Filters}], Expr, {map, _, []} = _Into, false, S) ->
  Clauses = [{clause, Ann, [Left], [], [Expr]}],
  Args = [Right, {'fun', Ann, {clauses, Clauses}}],
  List = ?remote(Ann, 'Elixir.Enum', map, Args),
  {?remote(Ann, maps, from_list, [List]), S};
build_inline_each(Ann, Clauses, Expr, {nil, _} = Into, Uniq, S) ->
  InnerFun = fun(InnerExpr, InnerAcc) -> {cons, Ann, InnerExpr, InnerAcc} end,
  {ReduceExpr, SR} = build_reduce(Ann, Clauses, InnerFun, Expr, Into, Uniq, S),
  {?remote(Ann, lists, reverse, [ReduceExpr]), SR};
build_inline_each(Ann, Clauses, Expr, {bin, _, []}, Uniq, S) ->
  {InnerValue, SV} = build_var(Ann, S),
  Generated = erl_anno:set_generated(true, Ann),

  InnerFun = fun(InnerExpr, InnerAcc) ->
    {'case', Ann, InnerExpr, [
      {clause, Generated,
       [InnerValue],
       [[?remote(Ann, erlang, is_bitstring, [InnerValue]),
         ?remote(Ann, erlang, is_list, [InnerAcc])]],
       [{cons, Generated, InnerAcc, InnerValue}]},
      {clause, Generated,
       [InnerValue],
       [],
       [?remote(Ann, erlang, error, [{tuple, Ann, [{atom, Ann, badarg}, InnerValue]}])]}
    ]}
  end,

  {ReduceExpr, SR} = build_reduce(Ann, Clauses, InnerFun, Expr, {nil, Ann}, Uniq, SV),
  {?remote(Ann, erlang, list_to_bitstring, [ReduceExpr]), SR}.

build_into(Ann, Clauses, Expr, {map, _, []}, Uniq, S) ->
  {ReduceExpr, SR} = build_inline_each(Ann, Clauses, Expr, {nil, Ann}, Uniq, S),
  {?remote(Ann, maps, from_list, [ReduceExpr]), SR};
build_into(Ann, Clauses, Expr, Into, Uniq, S) ->
  {Fun, SF}    = build_var(Ann, S),
  {Acc, SA}    = build_var(Ann, SF),
  {Kind, SK}   = build_var(Ann, SA),
  {Reason, SR} = build_var(Ann, SK),
  {Stack, ST}  = build_var(Ann, SR),
  {Done, SD}   = build_var(Ann, ST),

  InnerFun = fun(InnerExpr, InnerAcc) ->
    {call, Ann, Fun, [InnerAcc, pair(Ann, cont, InnerExpr)]}
  end,

  MatchExpr = {match, Ann,
    {tuple, Ann, [Acc, Fun]},
    ?remote(Ann, 'Elixir.Collectable', into, [Into])
  },

  {IntoReduceExpr, SN} = build_reduce(Ann, Clauses, InnerFun, Expr, Acc, Uniq, SD),

  TryExpr =
    {'try', Ann,
      [IntoReduceExpr],
      [{clause, Ann,
        [Done],
        [],
        [{call, Ann, Fun, [Done, {atom, Ann, done}]}]}],
      [stacktrace_clause(Ann, Fun, Acc, Kind, Reason, Stack)],
      []},

  {{block, Ann, [MatchExpr, TryExpr]}, SN}.

stacktrace_clause(Ann, Fun, Acc, Kind, Reason, Stack) ->
  {clause, Ann,
    [{tuple, Ann, [Kind, Reason, Stack]}],
    [],
    [{call, Ann, Fun, [Acc, {atom, Ann, halt}]},
     ?remote(Ann, erlang, raise, [Kind, Reason, Stack])]}.

%% Helpers

build_reduce(Ann, Clauses, InnerFun, Expr, Into, false, S) ->
  {Acc, SA} = build_var(Ann, S),
  {build_reduce_each(Clauses, InnerFun(Expr, Acc), Into, Acc, SA), SA};
build_reduce(Ann, Clauses, InnerFun, Expr, Into, true, S) ->
  %% Those variables are used only inside the anonymous function
  %% so we don't need to worry about returning the scope.
  {Acc, SA} = build_var(Ann, S),
  {Value, SV} = build_var(Ann, SA),
  {IntoAcc, SI} = build_var(Ann, SV),
  {UniqAcc, SU} = build_var(Ann, SI),

  NewInto = {tuple, Ann, [Into, {map, Ann, []}]},
  AccTuple = {tuple, Ann, [IntoAcc, UniqAcc]},
  PutUniqExpr = {map, Ann, UniqAcc, [{map_field_assoc, Ann, Value, {atom, Ann, true}}]},

  InnerExpr = {block, Ann, [
    {match, Ann, AccTuple, Acc},
    {match, Ann, Value, Expr},
    {'case', Ann, UniqAcc, [
      {clause, Ann, [{map, Ann, [{map_field_exact, Ann, Value, {atom, Ann, true}}]}], [], [AccTuple]},
      {clause, Ann, [{map, Ann, []}], [], [{tuple, Ann, [InnerFun(Value, IntoAcc), PutUniqExpr]}]}
    ]}
  ]},

  EnumReduceCall = build_reduce_each(Clauses, InnerExpr, NewInto, Acc, SU),
  {?remote(Ann, erlang, element, [{integer, Ann, 1}, EnumReduceCall]), SU}.

build_reduce_each([{enum, Meta, Left, Right, Filters} | T], Expr, Arg, Acc, S) ->
  Ann = ?ann(Meta),
  True = build_reduce_each(T, Expr, Acc, Acc, S),
  False = Acc,
  Generated = erl_anno:set_generated(true, Ann),

  Clauses0 =
    case is_var(Left) of
      true  -> [];
      false ->
        [{clause, Generated,
          [{var, Ann, '_'}, Acc], [],
          [False]}]
    end,

  Clauses1 =
    [{clause, Ann,
      [Left, Acc], [],
      [join_filters(Generated, Filters, True, False)]} | Clauses0],

  Args  = [Right, Arg, {'fun', Ann, {clauses, Clauses1}}],
  ?remote(Ann, 'Elixir.Enum', reduce, Args);

build_reduce_each([{bin, Meta, Left, Right, Filters} | T], Expr, Arg, Acc, S) ->
  Ann = ?ann(Meta),
  Generated  = erl_anno:set_generated(true, Ann),
  {Tail, ST} = build_var(Ann, S),
  {Fun, SF}  = build_var(Ann, ST),

  True = build_reduce_each(T, Expr, Acc, Acc, SF),
  False = Acc,
  {bin, _, Elements} = Left,
  TailElement = {bin_element, Ann, Tail, default, [bitstring]},

  Clauses =
    [{clause, Generated,
      [{bin, Ann, [TailElement]}, Acc], [],
      [Acc]},
     {clause, Generated,
      [Tail, {var, Ann, '_'}], [],
      [?remote(Ann, erlang, error, [pair(Ann, badarg, Tail)])]}],

  NoVarClauses =
    case no_var(Generated, Elements) of
      error ->
        Clauses;

      NoVarElements ->
        NoVarMatch = {bin, Ann, NoVarElements ++ [TailElement]},
        [{clause, Generated, [NoVarMatch, Acc], [], [{call, Ann, Fun, [Tail, False]}]} | Clauses]
    end,

  BinMatch = {bin, Ann, Elements ++ [TailElement]},
  VarClauses =
    [{clause, Ann,
     [BinMatch, Acc], [],
     [{call, Ann, Fun, [Tail, join_filters(Generated, Filters, True, False)]}]} | NoVarClauses],

  {call, Ann,
    {named_fun, Ann, element(3, Fun), VarClauses},
    [Right, Arg]};

build_reduce_each([], Expr, _Arg, _Acc, _S) ->
  Expr.

is_var({var, _, _}) -> true;
is_var(_) -> false.

pair(Ann, Atom, Arg) ->
  {tuple, Ann, [{atom, Ann, Atom}, Arg]}.

build_var(Ann, S) ->
  {Name, ST} = elixir_erl_var:build('_', S),
  {{var, Ann, Name}, ST}.

no_var(ParentAnn, Elements) ->
  try
    [{bin_element, Ann, NoVarExpr, no_var_size(Size), Types} ||
     {bin_element, Ann, Expr, Size, Types} <- Elements,
     NoVarExpr <- no_var_expr(ParentAnn, Expr)]
  catch
    unbound_size -> error
  end.

no_var_expr(Ann, {string, _, String}) -> [{var, Ann, '_'} || _ <- String];
no_var_expr(Ann, _) -> [{var, Ann, '_'}].
no_var_size({var, _, _}) -> throw(unbound_size);
no_var_size(Size) -> Size.

build_comprehension(Ann, Clauses, Expr, Into) ->
  {comprehension_kind(Into), Ann, Expr, comprehension_clause(Clauses)}.

comprehension_clause([{bin, Meta, Left, Right, Filters} | T]) ->
  Ann = ?ann(Meta),
  [{b_generate, Ann, Left, Right}] ++
    comprehension_filter(Ann, Filters) ++
    comprehension_clause(T);
comprehension_clause([]) ->
  [].

comprehension_kind(false) -> lc;
comprehension_kind({nil, _}) -> lc;
comprehension_kind({bin, _, []}) -> bc.

inline_or_into({bin, _, []}) -> inline;
inline_or_into({nil, _}) -> inline;
inline_or_into(false) -> inline;
inline_or_into(_) -> into.

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
  Guards = [[{op, Ann, 'orelse',
    {op, Ann, '==', Var, {atom, Ann, false}},
    {op, Ann, '==', Var, {atom, Ann, nil}}
  }]],

  {'case', Ann, Filter, [
    {clause, Ann, [Var], Guards, [False]},
    {clause, Ann, [{var, Ann, '_'}], [], [True]}
  ]}.
