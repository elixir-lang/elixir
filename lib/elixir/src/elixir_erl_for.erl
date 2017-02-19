-module(elixir_erl_for).
-export([translate/4]).
-include("elixir.hrl").

translate(Meta, Args, Return, S) ->
  {AccName, _, SA} = elixir_erl_var:build('_', S),
  {VarName, _, SV} = elixir_erl_var:build('_', SA),

  Ann = ?ann(Meta),
  Acc  = {var, Ann, AccName},
  Var  = {var, Ann, VarName},

  {Cases, [{do, Expr} | Opts]} = elixir_utils:split_last(Args),

  {TInto, SI} =
    case lists:keyfind(into, 1, Opts) of
      {into, Into} -> elixir_erl_pass:translate(Into, SV);
      false when Return -> {{nil, Ann}, SV};
      false -> {false, SV}
    end,

  {TCases, SC} = translate_gen(Meta, Cases, [], SI),
  {TExpr, SE}  = elixir_erl_pass:translate(wrap_expr(Expr, TInto), SC),
  SF = elixir_erl_var:mergec(SI, SE),

  case comprehension_expr(TInto, TExpr) of
    {inline, TIntoExpr} ->
      {build_inline(Ann, TCases, TIntoExpr, TInto, Var, Acc, SE), SF};
    {into, TIntoExpr} ->
      build_into(Ann, TCases, TIntoExpr, TInto, Var, Acc, SF)
  end.

%% In case we have no return, we wrap the expression
%% in a block that returns nil.
wrap_expr(Expr, false) -> {'__block__', [], [Expr, nil]};
wrap_expr(Expr, _)  -> Expr.

translate_gen(ForMeta, [{'<-', Meta, [Left, Right]} | T], Acc, S) ->
  {TLeft, TRight, TFilters, TT, TS} = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{enum, Meta, TLeft, TRight, TFilters} | Acc],
  translate_gen(ForMeta, TT, TAcc, TS);
translate_gen(ForMeta, [{'<<>>', _, [{'<-', Meta, [Left, Right]}]} | T], Acc, S) ->
  {TLeft, TRight, TFilters, TT, TS} = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{bin, Meta, TLeft, TRight, TFilters} | Acc],
  case elixir_bitstring:has_size(TLeft) of
    true  -> translate_gen(ForMeta, TT, TAcc, TS);
    false ->
      elixir_errors:compile_error(Meta, S#elixir_erl.file,
        "bitstring fields without size are not allowed in bitstring generators")
  end;
translate_gen(_ForMeta, [], Acc, S) ->
  {lists:reverse(Acc), S}.

translate_gen(_Meta, Left, Right, T, S) ->
  {TRight, SR} = elixir_erl_pass:translate(Right, S),
  {LeftArgs, LeftGuards} = elixir_utils:extract_guards(Left),
  {TLeft, SL} = elixir_erl_clauses:match(fun elixir_erl_pass:translate/2, LeftArgs,
                                     SR#elixir_erl{extra=pin_guard, extra_guards=[]}),

  TLeftGuards = elixir_erl_clauses:guards(LeftGuards, [], SL),
  ExtraGuards = [{nil, X} || X <- SL#elixir_erl.extra_guards],
  SF = SL#elixir_erl{extra=S#elixir_erl.extra, extra_guards=nil},

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

build_inline(Ann, Clauses, Expr, Into, _Var, Acc, S) ->
  case lists:all(fun(Clause) -> element(1, Clause) == bin end, Clauses) of
    true  -> build_comprehension(Ann, Clauses, Expr, Into);
    false -> build_reduce(Clauses, Expr, Into, Acc, S)
  end.

build_into(Ann, Clauses, Expr, {map, _, []} = Into, _Var, Acc, S) ->
  {Key, SK} = build_var(Ann, S),
  {Val, SV} = build_var(Ann, SK),
  MapExpr =
    {block, Ann, [
      {match, Ann, {tuple, Ann, [Key, Val]}, Expr},
      {call, Ann, {remote, Ann, {atom, Ann, maps}, {atom, Ann, put}}, [Key, Val, Acc]}
    ]},
  {build_reduce_clause(Clauses, MapExpr, Into, Acc, SV), SV};

build_into(Ann, Clauses, Expr, Into, Fun, Acc, S) ->
  {Kind, SK}   = build_var(Ann, S),
  {Reason, SR} = build_var(Ann, SK),
  {Stack, ST}  = build_var(Ann, SR),
  {Done, SD}   = build_var(Ann, ST),

  IntoExpr  = {call, Ann, Fun, [Acc, pair(Ann, cont, Expr)]},
  MatchExpr = {match, Ann,
    {tuple, Ann, [Acc, Fun]},
    elixir_erl:remote(Ann, 'Elixir.Collectable', into, [Into])
  },

  TryExpr =
    {'try', Ann,
      [build_reduce_clause(Clauses, IntoExpr, Acc, Acc, SD)],
      [{clause, Ann,
        [Done],
        [],
        [{call, Ann, Fun, [Done, {atom, Ann, done}]}]}],
      [{clause, Ann,
        [{tuple, Ann, [Kind, Reason, {var, Ann, '_'}]}],
        [],
        [{match, Ann, Stack, elixir_erl:remote(Ann, erlang, get_stacktrace, [])},
         {call, Ann, Fun, [Acc, {atom, Ann, halt}]},
         elixir_erl:remote(Ann, erlang, raise, [Kind, Reason, Stack])]}],
      []},

  {{block, Ann, [MatchExpr, TryExpr]}, SD}.

%% Helpers

build_reduce(Clauses, Expr, false, Acc, S) ->
  build_reduce_clause(Clauses, Expr, {nil, 0}, Acc, S);
build_reduce(Clauses, Expr, {nil, Ann} = Into, Acc, S) ->
  ListExpr = {cons, Ann, Expr, Acc},
  elixir_erl:remote(Ann, lists, reverse,
    [build_reduce_clause(Clauses, ListExpr, Into, Acc, S)]);
build_reduce(Clauses, Expr, {bin, _, _} = Into, Acc, S) ->
  {bin, Ann, Elements} = Expr,
  BinExpr = {bin, Ann, [{bin_element, Ann, Acc, default, [bitstring]} | Elements]},
  build_reduce_clause(Clauses, BinExpr, Into, Acc, S).

build_reduce_clause([{enum, Meta, Left, Right, Filters} | T], Expr, Arg, Acc, S) ->
  Generated = ?generated(Meta),
  Ann   = ?ann(Meta),
  True  = build_reduce_clause(T, Expr, Acc, Acc, S),
  False = Acc,

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
      [join_filters(Ann, Filters, True, False)]} | Clauses0],

  Args  = [Right, Arg, {'fun', Ann, {clauses, Clauses1}}],
  elixir_erl:remote(Ann, 'Elixir.Enum', reduce, Args);

build_reduce_clause([{bin, Meta, Left, Right, Filters} | T], Expr, Arg, Acc, S) ->
  Ann = ?ann(Meta),
  Generated = ?generated(Meta),
  {Tail, ST} = build_var(Ann, S),
  {Fun, SF}  = build_var(Ann, ST),

  True  = build_reduce_clause(T, Expr, Acc, Acc, SF),
  False = Acc,

  {bin, _, Elements} = Left,

  BinMatch =
    {bin, Ann, Elements ++ [{bin_element, Ann, Tail, default, [bitstring]}]},
  NoVarMatch =
    {bin, Ann, no_var(Elements) ++ [{bin_element, Ann, Tail, default, [bitstring]}]},

  Clauses =
    [{clause, Ann,
      [BinMatch, Acc], [],
      [{call, Ann, Fun, [Tail, join_filters(Ann, Filters, True, False)]}]},
     {clause, ?generated,
      [NoVarMatch, Acc], [],
      [{call, Ann, Fun, [Tail, False]}]},
     {clause, Generated,
      [{bin, Ann, []}, Acc], [],
      [Acc]},
     {clause, Generated,
      [Tail, {var, Ann, '_'}], [],
      [elixir_erl:remote(Ann, erlang, error, [pair(Ann, badarg, Tail)])]}],

  {call, Ann,
    {named_fun, Ann, element(3, Fun), Clauses},
    [Right, Arg]};

build_reduce_clause([], Expr, _Arg, _Acc, _S) ->
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
