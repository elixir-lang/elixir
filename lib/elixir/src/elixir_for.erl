-module(elixir_for).
-export([expand/3, translate/3]).
-include("elixir.hrl").

%% Expansion

expand(Meta, Args, E) ->
  { Cases, Block } =
    case elixir_utils:split_last(Args) of
      { OuterCases, OuterOpts } when is_list(OuterOpts) ->
        case elixir_utils:split_last(OuterCases) of
          { InnerCases, InnerOpts } when is_list(InnerOpts) ->
            { InnerCases, InnerOpts ++ OuterOpts };
          _ ->
            { OuterCases, OuterOpts }
        end;
      _ ->
        { Args, [] }
    end,

  { Expr, Opts } =
    case lists:keyfind(do, 1, Block) of
      { do, Do } -> { Do, lists:keydelete(do, 1, Block) };
      _ -> elixir_errors:compile_error(Meta, E#elixir_env.file,
            "missing do keyword in for comprehension")
    end,

  { EOpts, EO }  = elixir_exp:expand(Opts, E),
  { ECases, EC } = lists:mapfoldl(fun expand/2, EO, Cases),
  { EExpr, _ }   = elixir_exp:expand(Expr, EC),
  { { for, Meta, ECases ++ [[{do,EExpr}|EOpts]] }, E }.

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

translate(Meta, Args, #elixir_scope{return=Return} = RS) ->
  S = RS#elixir_scope{return=true},
  { AccName, _, SA } = elixir_scope:build_var('_', S),
  { VarName, _, SV } = elixir_scope:build_var('_', SA),

  Line = ?line(Meta),
  Acc  = { var, Line, AccName },
  Var  = { var, Line, VarName },

  { Cases, [{do,Expr}|Opts] } = elixir_utils:split_last(Args),

  { TInto, SI } =
    case lists:keyfind(into, 1, Opts) of
      { into, Into } ->
        %% TODO: Get rid of this code once we support into protocol
        case Res = elixir_translator:translate(Into, SV) of
          { {nil, _}, _ } -> Res;
          { {bin, _, []}, _ } -> Res;
          _ ->
            elixir_errors:compile_error(Meta, S#elixir_scope.file,
              ":into in comprehensions must be an empty list or an empty string at compile time, got: ~ts",
              ['Elixir.Macro':to_string(Into)])
        end;
      false when Return -> { { nil, Line }, SV };
      false -> { { atom, Line, nil }, SV }
    end,

  { TCases, SC } = translate_gen(Meta, Cases, [], SI),
  { TExpr, SE }  = elixir_translator:translate_block(Expr, Return, SC),

  { build(Line, TCases, comprehension_expr(TInto, TExpr), TInto, Return, Var, Acc, SE),
    elixir_scope:mergef(SI, SE) }.

translate_gen(ForMeta, [{ '<-', Meta, [Left, Right] }|T], Acc, S) ->
  { TLeft, TRight, TFilters, TT, TS } = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{ enum, Meta, TLeft, TRight, TFilters }|Acc],
  translate_gen(ForMeta, TT, TAcc, TS);
translate_gen(ForMeta, [{ '<<>>', _, [ { '<-', Meta, [Left, Right] } ] }|T], Acc, S) ->
  { TLeft, TRight, TFilters, TT, TS } = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{ bin, Meta, TLeft, TRight, TFilters }|Acc],
  case elixir_bitstring:has_size(TLeft) of
    true  -> translate_gen(ForMeta, TT, TAcc, TS);
    false ->
      elixir_errors:compile_error(Meta, S#elixir_scope.file,
        "bitstring fields without size are not allowed in bitstring generators")
  end;
translate_gen(_ForMeta, [], Acc, S) ->
  { lists:reverse(Acc), S };
translate_gen(ForMeta, _, _, S) ->
  elixir_errors:compile_error(ForMeta, S#elixir_scope.file,
    "for comprehensions must start with a generator").

translate_gen(_Meta, Left, Right, T, S) ->
  { TRight, SR } = elixir_translator:translate(Right, S),
  { TLeft, SL } = elixir_clauses:match(fun elixir_translator:translate/2, Left, SR),
  { TT, { TFilters, TS } } = translate_filters(T, SL),
  { TLeft, TRight, TFilters, TT, TS }.

translate_filters(T, S) ->
  { Filters, Rest } = collect_filters(T, []),
  { Rest, lists:mapfoldr(fun translate_filter/2, S, Filters) }.

translate_filter(Filter, S) ->
  { TFilter, TS } = elixir_translator:translate(Filter, S),
  case elixir_utils:returns_boolean(TFilter) of
    true ->
      { { nil, TFilter }, TS };
    false ->
      { Name, _, VS } = elixir_scope:build_var('_', TS),
      { { { var, 0, Name }, TFilter }, VS }
  end.

collect_filters([{ '<-', _, [_, _] }|_] = T, Acc) ->
  { Acc, T };
collect_filters([{ '<<>>', _, [{ '<-', _, [_, _] }] }|_] = T, Acc) ->
  { Acc, T };
collect_filters([H|T], Acc) ->
  collect_filters(T, [H|Acc]);
collect_filters([], Acc) ->
  { Acc, [] }.

%% If all we have is one enum generator, we check if it is a list
%% for optimization otherwise fallback to the reduce generator.
build(Line, [{ enum, Meta, Left, Right, Filters }] = Orig, { inline, Expr }, Into, Return, Var, Acc, S) ->
  case Right of
    { cons, _, _, _ } ->
      build_comprehension(Line, Orig, Expr, Into, Return);
    { Other, _, _ } when Other == tuple; Other == map ->
      build_reduce(Orig, Expr, Into, Acc, S);
    _ ->
      Clauses = [{ enum, Meta, Left, Var, Filters }],

      {'case', -1, Right, [
        {clause, -1,
          [Var],
          [[?wrap_call(Line, erlang, is_list, [Var])]],
          [build_comprehension(Line, Clauses, Expr, Into, Return)]},
        {clause, -1,
          [Var],
          [],
          [build_reduce(Clauses, Expr, Into, Acc, S)]}
      ]}
  end;

build(Line, Clauses, { Kind, Expr }, Into, Return, _Var, Acc, S) ->
  case (Kind == inline) andalso
       lists:all(fun(Clause) -> element(1, Clause) == bin end, Clauses) of
    true  -> build_comprehension(Line, Clauses, Expr, Into, Return);
    false -> build_reduce(Clauses, Expr, Into, Acc, S)
  end.

%% TODO: Extend this to support the into protocol
build_reduce(Clauses, Expr, {atom, _, nil}, Acc, S) ->
  build_reduce_clause(Clauses, Expr, {nil, 0}, Acc, S);
build_reduce(Clauses, Expr, Into, Acc, S)
    when (element(1, Into) == nil) orelse (element(1, Into) == cons) ->
  ListExpr = {cons, 0, Expr, Acc},
  ?wrap_call(0, lists, reverse,
    [build_reduce_clause(Clauses, ListExpr, Into, Acc, S)]);
build_reduce(Clauses, Expr, {bin, _, _} = Into, Acc, S) ->
  {bin, Line, Elements} = Expr,
  BinExpr = {bin, Line, [{bin_element, Line, Acc, default, [bitstring]}|Elements]},
  build_reduce_clause(Clauses, BinExpr, Into, Acc, S).

%% Helpers

build_reduce_clause([{ enum, Meta, Left, Right, Filters }|T], Expr, Arg, Acc, S) ->
  Line  = ?line(Meta),
  Inner = build_reduce_clause(T, Expr, Acc, Acc, S),

  True  = pair(Line, cont, Inner),
  False = pair(Line, cont, Acc),

  Clauses0 =
    case is_var(Left) of
      true  -> [];
      false ->
        [{clause, -1,
          [{var, Line, '_'}, Acc], [],
          [False]}]
    end,

  Clauses1 =
    [{clause, Line,
      [Left, Acc], [],
      [join_filters(Line, Filters, True, False)]}|Clauses0],

  Args  = [Right, pair(Line, cont, Arg), {'fun', Line, {clauses, Clauses1}}],
  Tuple = ?wrap_call(Line, 'Elixir.Enumerable', reduce, Args),

  %% Use -1 because in case of no returns we don't care about the result
  ?wrap_call(-1, erlang, element, [{integer, Line, 2}, Tuple]);

build_reduce_clause([{ bin, Meta, Left, Right, Filters }|T], Expr, Arg, Acc, S) ->
  { TailName, _, ST } = elixir_scope:build_var('_', S),
  { FunName, _, SF } = elixir_scope:build_var('_', ST),

  True  = build_reduce_clause(T, Expr, Acc, Acc, SF),
  False = Acc,

  Line = ?line(Meta),
  Fun  = {var, Line, FunName},
  Tail = {var, Line, TailName},

  {bin, _, Elements} = Left,

  BinMatch =
    {bin, Line, Elements ++ [{ bin_element, Line, Tail, default, [bitstring] }]},
  NoVarMatch =
    {bin, Line, no_var(Elements) ++ [{ bin_element, Line, Tail, default, [bitstring] }]},

  Clauses =
    [{clause, Line,
      [BinMatch, Acc], [],
      [{call, Line, Fun, [Tail, join_filters(Line, Filters, True, False)]}]},
     {clause, -1,
      [NoVarMatch, Acc], [],
      [{call, Line, Fun, [Tail, False]}]},
     {clause, -1,
      [{bin, Line, []}, Acc], [],
      [Acc]},
     {clause, -1,
      [Tail, {var, Line, '_'}], [],
      [?wrap_call(Line, erlang, error, [pair(Line, badarg, Tail)])]}],

  {call, Line,
    {named_fun, Line, element(3, Fun), Clauses},
    [Right, Arg]};

build_reduce_clause([], Expr, _Arg, _Acc, _S) ->
  Expr.

is_var({var, _, _}) -> true;
is_var(_) -> false.

pair(Line, Atom, Arg) ->
  {tuple, Line, [{atom, Line, Atom}, Arg]}.

no_var(Elements) ->
  [{bin_element, Line, no_var_expr(Expr), Size, Types} ||
    {bin_element, Line, Expr, Size, Types} <- Elements].
no_var_expr({ var, Line, _ }) ->
  {var, Line, '_'}.


%% TODO: Test this clause once we support into: nil
build_comprehension(Line, Clauses, Expr, {atom, _, nil}, true) ->
  {block, Line, [
    build_comprehension(Line, Clauses, Expr, {nil, Line}, true),
    {nil, Line}
  ]};
build_comprehension(Line, Clauses, Expr, {atom, _, nil}, false) ->
  build_comprehension(Line, Clauses, Expr, {nil, Line}, false);
build_comprehension(Line, Clauses, Expr, Into, _Return) ->
  {comprehension_kind(Into), Line, Expr, comprehension_clause(Clauses)}.

comprehension_clause([{ Kind, Meta, Left, Right, Filters }|T]) ->
  Line = ?line(Meta),
  [{comprehension_generator(Kind), Line, Left, Right}] ++
    comprehension_filter(Line, Filters) ++
    comprehension_clause(T);
comprehension_clause([]) ->
  [].

comprehension_kind({ nil, _ }) -> lc;
comprehension_kind({ bin, _, [] }) -> bc.

comprehension_generator(enum) -> generate;
comprehension_generator(bin) -> b_generate.

%% TODO: Extend this to support the into protocol
comprehension_expr({ bin, _, [] }, { bin, _, _ } = Expr) ->
  { inline, Expr };
comprehension_expr({ bin, Line, [] }, Expr) ->
  BinExpr = { bin, Line, [{ bin_element, Line, Expr, default, [bitstring] }] },
  { inline, BinExpr };
comprehension_expr({ nil, _ }, Expr) ->
  { inline, Expr };
comprehension_expr({ atom, _, nil }, Expr) ->
  { inline, Expr }.

comprehension_filter(Line, Filters) ->
  [join_filter(Line, Filter, { atom, Line, true }, { atom, Line, false }) ||
   Filter <- lists:reverse(Filters)].

join_filters(_Line, [], True, _False) ->
  True;
join_filters(Line, [H|T], True, False) ->
  lists:foldl(fun(Filter, Acc) ->
    join_filter(Line, Filter, Acc, False)
  end, join_filter(Line, H, True, False), T).

join_filter(Line, { nil, Filter }, True, False) ->
  {'case', Line, Filter, [
    {clause, Line, [{atom, Line, true}], [], [True] },
    {clause, Line, [{atom, Line, false}], [], [False] }
  ] };
join_filter(Line, { Var, Filter }, True, False) ->
  Guard =
    {op, Line, 'orelse',
      {op, Line, '==', Var, {atom, Line, false}},
      {op, Line, '==', Var, {atom, Line, nil}}},

  {'case', Line, Filter, [
    {clause, Line, [Var], [[Guard]], [False] },
    {clause, Line, [{var, Line, '_'}], [], [True] }
  ] }.
