%% Translate Elixir quoted expressions to Erlang Abstract Format.
-module(elixir_erl_pass).
-export([translate/3, translate_args/3, no_parens_remote/2, parens_map_field/2]).
-include("elixir.hrl").

%% =

translate({'=', Meta, [{'_', _, Atom}, Right]}, _Ann, S) when is_atom(Atom) ->
  Ann = ?ann(Meta),
  {TRight, SR} = translate(Right, Ann, S),
  {{match, Ann, {var, Ann, '_'}, TRight}, SR};

translate({'=', Meta, [Left, Right]}, _Ann, S) ->
  Ann = ?ann(Meta),
  {TRight, SR} = translate(Right, Ann, S),
  case elixir_erl_clauses:match(Ann, fun translate/3, Left, SR) of
    {TLeft, #elixir_erl{extra_guards=ExtraGuards, context=Context} = SL0}
        when ExtraGuards =/= [], Context =/= match ->
      SL1 = SL0#elixir_erl{extra_guards=[]},
      {ResultVarName, SL2} = elixir_erl_var:build('_', SL1),
      Match = {match, Ann, TLeft, TRight},
      Generated = erl_anno:set_generated(true, Ann),
      ResultVar = {var, Generated, ResultVarName},
      ResultMatch = {match, Generated, ResultVar, Match},
      True = {atom, Generated, true},
      Reason = {tuple, Generated, [{atom, Generated, badmatch}, ResultVar]},
      RaiseExpr = ?remote(Generated, erlang, error, [Reason]),
      GuardsExp = {'if', Generated, [
        {clause, Generated, [], [ExtraGuards], [ResultVar]},
        {clause, Generated, [], [[True]], [RaiseExpr]}
      ]},
      {{block, Generated, [ResultMatch, GuardsExp]}, SL2};

    {TLeft, SL} ->
      {{match, Ann, TLeft, TRight}, SL}
  end;

%% Containers

translate({'{}', Meta, Args}, _Ann, S) when is_list(Args) ->
  Ann = ?ann(Meta),
  {TArgs, SE} = translate_args(Args, Ann, S),
  {{tuple, Ann, TArgs}, SE};

translate({'%{}', Meta, Args}, _Ann, S) when is_list(Args) ->
  translate_map(?ann(Meta), Args, S);

translate({'%', Meta, [{'^', _, [{Name, _, Context}]} = Left, Right]}, _Ann, S) when is_atom(Name), is_atom(Context) ->
  translate_struct_var_name(?ann(Meta), Left, Right, S);

translate({'%', Meta, [{Name, _, Context} = Left, Right]}, _Ann, S) when is_atom(Name), is_atom(Context) ->
  translate_struct_var_name(?ann(Meta), Left, Right, S);

translate({'%', Meta, [Left, Right]}, _Ann, S) ->
  translate_struct(?ann(Meta), Left, Right, S);

translate({'<<>>', Meta, Args}, _Ann, S) when is_list(Args) ->
  translate_bitstring(Meta, Args, S);

%% Blocks

translate({'__block__', Meta, Args}, _Ann, S) when is_list(Args) ->
  Ann = ?ann(Meta),
  {TArgs, SA} = translate_block(Args, Ann, [], S),
  {{block, Ann, lists:reverse(TArgs)}, SA};

%% Compilation environment macros

translate({'__CALLER__', Meta, Atom}, _Ann, S) when is_atom(Atom) ->
  {{var, ?ann(Meta), '__CALLER__'}, S#elixir_erl{caller=true}};

translate({'__STACKTRACE__', Meta, Atom}, _Ann, S = #elixir_erl{stacktrace=Var}) when is_atom(Atom) ->
  {{var, ?ann(Meta), Var}, S};

translate({'super', Meta, Args}, _Ann, S) when is_list(Args) ->
  Ann = ?ann(Meta),

  %% In the expanded AST, super is used to invoke a function
  %% in the current module originated from a default clause
  %% or a super call.
  {TArgs, SA} = translate_args(Args, Ann, S),
  {super, {Kind, Name}} = lists:keyfind(super, 1, Meta),

  if
    Kind == defmacro; Kind == defmacrop ->
      MacroName = elixir_utils:macro_name(Name),
      {{call, Ann, {atom, Ann, MacroName}, [{var, Ann, '__CALLER__'} | TArgs]}, SA#elixir_erl{caller=true}};
    Kind == def; Kind == defp ->
      {{call, Ann, {atom, Ann, Name}, TArgs}, SA}
  end;

%% Functions

translate({'&', Meta, [{'/', _, [{{'.', _, [Remote, Fun]}, _, []}, Arity]}]}, _Ann, S)
    when is_atom(Fun), is_integer(Arity) ->
  Ann = ?ann(Meta),
  {TRemote, SR} = translate(Remote, Ann, S),
  TFun = {atom, Ann, Fun},
  TArity = {integer, Ann, Arity},
  {{'fun', Ann, {function, TRemote, TFun, TArity}}, SR};
translate({'&', Meta, [{'/', _, [{Fun, _, Atom}, Arity]}]}, Ann, S)
    when is_atom(Fun), is_atom(Atom), is_integer(Arity) ->
  case S of
    #elixir_erl{expand_captures=true} ->
      {Vars, SV} = lists:mapfoldl(fun(_, Acc) ->
        {Var, _, AccS} = elixir_erl_var:assign(Meta, Acc),
        {Var, AccS}
      end, S, tl(lists:seq(0, Arity))),
      translate({'fn', Meta, [{'->', Meta, [Vars, {Fun, Meta, Vars}]}]}, Ann, SV);

    #elixir_erl{expand_captures=false} ->
      {{'fun', ?ann(Meta), {function, Fun, Arity}}, S}
  end;

translate({fn, Meta, Clauses}, _Ann, S) ->
  Transformer = fun({'->', CMeta, [ArgsWithGuards, Expr]}, Acc) ->
    {Args, Guards} = elixir_utils:extract_splat_guards(ArgsWithGuards),
    elixir_erl_clauses:clause(?ann(CMeta), fun translate_fn_match/3, Args, Expr, Guards, Acc)
  end,
  {TClauses, NS} = lists:mapfoldl(Transformer, S, Clauses),
  {{'fun', ?ann(Meta), {clauses, TClauses}}, NS};

%% Cond

translate({'cond', CondMeta, [[{do, Clauses}]]}, Ann, S) ->
  [{'->', Meta, [[Condition], _]} = H | T] = lists:reverse(Clauses),

  FirstMeta =
    if
      is_atom(Condition), Condition /= false, Condition /= nil -> ?generated(Meta);
      true -> Meta
    end,

  Error = {{'.', Meta, [erlang, error]}, Meta, [cond_clause]},
  {Case, SC} = build_cond_clauses([H | T], Error, FirstMeta, S),
  translate(replace_case_meta(CondMeta, Case), Ann, SC);

%% Case

translate({'case', Meta, [Expr, Opts]}, _Ann, S) ->
  translate_case(Meta, Expr, Opts, S);

%% Try

translate({'try', Meta, [Opts]}, _Ann, S) ->
  Ann = ?ann(Meta),
  Do = proplists:get_value('do', Opts, nil),
  {TDo, SB} = translate(Do, Ann, S),

  Catch = [Tuple || {X, _} = Tuple <- Opts, X == 'rescue' orelse X == 'catch'],
  {TCatch, SC} = elixir_erl_try:clauses(Ann, Catch, SB),

  {TAfter, SA} = case lists:keyfind('after', 1, Opts) of
    {'after', After} ->
      {TBlock, SAExtracted} = translate(After, Ann, SC),
      {unblock(TBlock), SAExtracted};
    false ->
      {[], SC}
  end,

  Else = elixir_erl_clauses:get_clauses('else', Opts, match),
  {TElse, SE} = elixir_erl_clauses:clauses(Else, SA),
  {{'try', ?ann(Meta), unblock(TDo), TElse, TCatch, TAfter}, SE};

%% Receive

translate({'receive', Meta, [Opts]}, _Ann, S) ->
  Do = elixir_erl_clauses:get_clauses(do, Opts, match),

  case lists:keyfind('after', 1, Opts) of
    false ->
      {TClauses, SC} = elixir_erl_clauses:clauses(Do, S),
      {{'receive', ?ann(Meta), TClauses}, SC};
    _ ->
      After = elixir_erl_clauses:get_clauses('after', Opts, expr),
      {TClauses, SC} = elixir_erl_clauses:clauses(Do ++ After, S),
      {FClauses, TAfter} = elixir_utils:split_last(TClauses),
      {_, _, [FExpr], _, FAfter} = TAfter,
      {{'receive', ?ann(Meta), FClauses, FExpr, FAfter}, SC}
  end;

%% Comprehensions and with

translate({for, Meta, [_ | _] = Args}, _Ann, S) ->
  elixir_erl_for:translate(Meta, Args, S);

translate({with, Meta, [_ | _] = Args}, _Ann, S) ->
  Ann = ?ann(Meta),
  {Exprs, [{do, Do} | Opts]} = elixir_utils:split_last(Args),
  {ElseClause, MaybeFun, SE} = translate_with_else(Meta, Opts, S),
  {Case, SD} = translate_with_do(Exprs, Ann, Do, ElseClause, SE),

  case MaybeFun of
    nil -> {Case, SD};
    FunAssign -> {{block, Ann, [FunAssign, Case]}, SD}
  end;

%% Variables

translate({'^', _, [{Name, VarMeta, Kind}]}, _Ann, S) when is_atom(Name), is_atom(Kind) ->
  {Var, VS} = elixir_erl_var:translate(VarMeta, Name, Kind, S),

  case S#elixir_erl.extra of
    pin_guard ->
      {PinVarName, PS} = elixir_erl_var:build('_', VS),
      Ann = ?ann(?generated(VarMeta)),
      PinVar = {var, Ann, PinVarName},
      Guard = {op, Ann, '=:=', Var, PinVar},
      {PinVar, PS#elixir_erl{extra_guards=[Guard | PS#elixir_erl.extra_guards]}};
    _ ->
      {Var, VS}
  end;

translate({Name, Meta, Kind}, _Ann, S) when is_atom(Name), is_atom(Kind) ->
  elixir_erl_var:translate(Meta, Name, Kind, S);

%% Local calls

translate({Name, Meta, Args}, _Ann, S) when is_atom(Name), is_list(Meta), is_list(Args) ->
  Ann = ?ann(Meta),
  {TArgs, NS} = translate_args(Args, Ann, S),
  {{call, Ann, {atom, Ann, Name}, TArgs}, NS};

%% Remote calls

translate({{'.', _, [Left, Right]}, Meta, []}, _Ann, #elixir_erl{context=guard} = S)
    when is_tuple(Left), is_atom(Right), is_list(Meta) ->
  Ann = ?ann(Meta),
  {TLeft, SL}  = translate(Left, Ann, S),
  TRight = {atom, Ann, Right},
  {?remote(Ann, erlang, map_get, [TRight, TLeft]), SL};

translate({{'.', _, [Left, Right]}, Meta, []}, _Ann, S)
  when is_tuple(Left) orelse Left =:= nil orelse is_boolean(Left), is_atom(Right), is_list(Meta) ->
  Ann = ?ann(Meta),
  {TLeft, SL} = translate(Left, Ann, S),
  TRight = {atom, Ann, Right},

  Generated = erl_anno:set_generated(true, Ann),
  {InnerVar, SI} = elixir_erl_var:build('_', SL),
  TInnerVar = {var, Generated, InnerVar},
  {Var, SV} = elixir_erl_var:build('_', SI),
  TVar = {var, Generated, Var},

  case proplists:get_value(no_parens, Meta, false) of
    true ->
      TError = {tuple, Ann, [{atom, Ann, badkey}, TRight, TVar]},
      {{'case', Generated, TLeft, [
        {clause, Generated,
          [{map, Ann, [{map_field_exact, Ann, TRight, TVar}]}],
          [],
          [TVar]},
        {clause, Generated,
          [TVar],
          [],
          [{'case', Generated, ?remote(Generated, elixir_erl_pass, no_parens_remote, [TVar, TRight]), [
            {clause, Generated,
             [{tuple, Generated, [{atom, Generated, ok}, TInnerVar]}], [], [TInnerVar]},
            {clause, Generated,
             [{var, Generated, '_'}], [], [?remote(Ann, erlang, error, [TError])]}
          ]}]}
      ]}, SV};
    false ->
      {{'case', Generated, TLeft, [
        {clause, Generated,
          [{map, Ann, [{map_field_exact, Ann, TRight, TVar}]}],
          [],
          [?remote(Generated, elixir_erl_pass, parens_map_field, [TRight, TVar])]},
        {clause, Generated,
          [TVar],
          [],
          [{call, Generated, {remote, Generated, TVar, TRight}, []}]}
      ]}, SV}
    end;

translate({{'.', _, [Left, Right]}, Meta, Args}, _Ann, S)
    when (is_tuple(Left) orelse is_atom(Left)), is_atom(Right), is_list(Meta), is_list(Args) ->
  translate_remote(Left, Right, Meta, Args, S);

%% Anonymous function calls

translate({{'.', _, [Expr]}, Meta, Args}, _Ann, S) when is_list(Args) ->
  Ann = ?ann(Meta),
  {TExpr, SE} = translate(Expr, Ann, S),
  {TArgs, SA} = translate_args(Args, Ann, SE),
  {{call, Ann, TExpr, TArgs}, SA};

%% Literals

translate({Left, Right}, Ann, S) ->
  {TLeft, SL} = translate(Left, Ann, S),
  {TRight, SR} = translate(Right, Ann, SL),
  {{tuple, Ann, [TLeft, TRight]}, SR};

translate(List, Ann, S) when is_list(List) ->
  translate_list(List, Ann, [], S);

translate(Other, Ann, S) ->
  {elixir_erl:elixir_to_erl(Other, Ann), S}.

%% Helpers

translate_case(Meta, Expr, Opts, S) ->
  Ann = ?ann(Meta),
  Clauses = elixir_erl_clauses:get_clauses(do, Opts, match),
  {TExpr, SE} = translate(Expr, Ann, S),
  {TClauses, SC} = elixir_erl_clauses:clauses(Clauses, SE),
  {{'case', Ann, TExpr, TClauses}, SC}.

translate_list([{'|', _, [Left, Right]}], Ann, List, Acc) ->
  {TLeft, LAcc} = translate(Left, Ann, Acc),
  {TRight, TAcc} = translate(Right, Ann, LAcc),
  {build_list([TLeft | List], TRight, Ann), TAcc};
translate_list([H | T], Ann, List, Acc) ->
  {TH, TAcc} = translate(H, Ann, Acc),
  translate_list(T, Ann, [TH | List], TAcc);
translate_list([], Ann, List, Acc) ->
  {build_list(List, {nil, Ann}, Ann), Acc}.

build_list([H | T], Acc, Ann) ->
  build_list(T, {cons, Ann, H, Acc}, Ann);
build_list([], Acc, _Ann) ->
  Acc.

%% Pack a list of expressions from a block.
unblock({'block', _, Exprs}) -> Exprs;
unblock(Expr)                -> [Expr].

translate_fn_match(Arg, Ann, S) ->
  {TArg, TS} = translate_args(Arg, Ann, S#elixir_erl{extra=pin_guard}),
  {TArg, TS#elixir_erl{extra=S#elixir_erl.extra}}.

%% Translate args

translate_args(Args, Ann, S) ->
  lists:mapfoldl(fun
    (Arg, SA) when is_list(Arg) ->
      translate_list(Arg, Ann, [], SA);
    (Arg, SA) when is_tuple(Arg) ->
      translate(Arg, Ann, SA);
    (Arg, SA) ->
      {elixir_erl:elixir_to_erl(Arg, Ann), SA}
  end, S, Args).

%% Translate blocks

translate_block([], _Ann, Acc, S) ->
  {Acc, S};
translate_block([H], Ann, Acc, S) ->
  {TH, TS} = translate(H, Ann, S),
  translate_block([], Ann, [TH | Acc], TS);
translate_block([{'__block__', Meta, Args} | T], Ann, Acc, S) when is_list(Args) ->
  {TAcc, SA} = translate_block(Args, ?ann(Meta), Acc, S),
  translate_block(T, Ann, TAcc, SA);
translate_block([H | T], Ann, Acc, S) ->
  {TH, TS} = translate(H, Ann, S),
  translate_block(T, Ann, [TH | Acc], TS).

%% Cond

build_cond_clauses([{'->', NewMeta, [[Condition], Body]} | T], Acc, OldMeta, S) ->
  {NewCondition, Truthy, Other, ST} = build_truthy_clause(NewMeta, Condition, Body, S),
  Falsy = {'->', OldMeta, [[Other], Acc]},
  Case = {'case', NewMeta, [NewCondition, [{do, [Truthy, Falsy]}]]},
  build_cond_clauses(T, Case, NewMeta, ST);
build_cond_clauses([], Acc, _, S) ->
  {Acc, S}.

replace_case_meta(Meta, {'case', _, Args}) ->
  {'case', Meta, Args};
replace_case_meta(_Meta, Other) ->
  Other.

build_truthy_clause(Meta, Condition, Body, S) ->
  case returns_boolean(Condition, Body) of
    {NewCondition, NewBody} ->
      {NewCondition, {'->', Meta, [[true], NewBody]}, false, S};
    false ->
      {Var, _, SV} = elixir_erl_var:assign(Meta, S),
      Head = {'when', [], [Var,
        {{'.', [], [erlang, 'andalso']}, [], [
          {{'.', [], [erlang, '/=']}, [], [Var, nil]},
          {{'.', [], [erlang, '/=']}, [], [Var, false]}
        ]}
      ]},
      {Condition, {'->', Meta, [[Head], Body]}, {'_', [], nil}, SV}
  end.

%% In case a variable is defined to match in a condition
%% but a condition returns boolean, we can replace the
%% variable directly by the boolean result.
returns_boolean({'=', _, [{Var, _, Ctx}, Condition]}, {Var, _, Ctx}) when is_atom(Var), is_atom(Ctx) ->
  case elixir_utils:returns_boolean(Condition) of
    true  -> {Condition, true};
    false -> false
  end;

%% For all other cases, we check the condition but
%% return both condition and body untouched.
returns_boolean(Condition, Body) ->
  case elixir_utils:returns_boolean(Condition) of
    true  -> {Condition, Body};
    false -> false
  end.

%% with

translate_with_else(Meta, [], S) ->
  Ann = ?ann(Meta),
  {VarName, SC} = elixir_erl_var:build('_', S),
  Var = {var, Ann, VarName},
  Generated = erl_anno:set_generated(true, Ann),
  {{clause, Generated, [Var], [], [Var]}, nil, SC};
translate_with_else(Meta, [{'else', [{'->', _, [[{Var, VarMeta, Kind}], Clause]}]}], S) when is_atom(Var), is_atom(Kind) ->
  Ann = ?ann(Meta),
  {ElseVarErl, SV} = elixir_erl_var:translate(VarMeta, Var, Kind, S#elixir_erl{context=match}),
  {TranslatedClause, SC} = translate(Clause, Ann, SV#elixir_erl{context=nil}),
  Clauses = [{clause, Ann, [ElseVarErl], [], [TranslatedClause]}],
  with_else_closure(Meta, Clauses, SC);
translate_with_else(Meta, [{'else', Else}], S) ->
  Generated = ?generated(Meta),
  {RaiseVar, _, SV} = elixir_erl_var:assign(Generated, S),

  RaiseExpr = {{'.', Generated, [erlang, error]}, Generated, [{else_clause, RaiseVar}]},
  RaiseClause = {'->', Generated, [[RaiseVar], RaiseExpr]},

  Clauses = elixir_erl_clauses:get_clauses('else', [{'else', Else ++ [RaiseClause]}], match),
  {TranslatedClauses, SC} = elixir_erl_clauses:clauses(Clauses, SV#elixir_erl{extra=pin_guard}),
  with_else_closure(Generated, TranslatedClauses, SC#elixir_erl{extra=SV#elixir_erl.extra}).

with_else_closure(Meta, TranslatedClauses, S) ->
  Ann = ?ann(Meta),
  {_, FunErlVar, SC} = elixir_erl_var:assign(Meta, S),
  {_, ArgErlVar, SA} = elixir_erl_var:assign(Meta, SC),
  Generated = erl_anno:set_generated(true, Ann),
  FunAssign = {match, Ann, FunErlVar, {'fun', Generated, {clauses, TranslatedClauses}}},
  FunCall = {call, Ann, FunErlVar, [ArgErlVar]},
  {{clause, Generated, [ArgErlVar], [], [FunCall]}, FunAssign, SA}.

translate_with_do([{'<-', Meta, [{Var, _, Ctx} = Left, Expr]} | Rest], Ann, Do, Else, S) when is_atom(Var), is_atom(Ctx) ->
  translate_with_do([{'=', Meta, [Left, Expr]} | Rest], Ann, Do, Else, S);
translate_with_do([{'<-', Meta, [Left, Expr]} | Rest], _Ann, Do, Else, S) ->
  Ann = ?ann(Meta),
  {Args, Guards} = elixir_utils:extract_guards(Left),
  {TExpr, SR} = translate(Expr, Ann, S),
  {TArgs, SA} = elixir_erl_clauses:match(Ann, fun translate/3, Args, SR),
  TGuards = elixir_erl_clauses:guards(Ann, Guards, SA#elixir_erl.extra_guards, SA),
  {TBody, SB} = translate_with_do(Rest, Ann, Do, Else, SA#elixir_erl{extra_guards=[]}),
  Clause = {clause, Ann, [TArgs], TGuards, unblock(TBody)},
  {{'case', Ann, TExpr, [Clause, Else]}, SB};
translate_with_do([Expr | Rest], Ann, Do, Else, S) ->
  {TExpr, TS} = translate(Expr, Ann, S),
  {TRest, RS} = translate_with_do(Rest, Ann, Do, Else, TS),
  {{block, Ann, [TExpr | unblock(TRest)]}, RS};
translate_with_do([], Ann, Do, _Else, S) ->
  translate(Do, Ann, S).

%% Maps and structs

translate_struct_var_name(Ann, Name, Args, S0) ->
  {{map, MapAnn, TArgs0}, S1} = translate_struct(Ann, Name, Args, S0),
  {TArgs1, S2} = generate_struct_name_guard(TArgs0, [], S1),
  {{map, MapAnn, TArgs1}, S2}.

translate_struct(Ann, Name, {'%{}', _, [{'|', _, [Update, Assocs]}]}, S) ->
  Generated = erl_anno:set_generated(true, Ann),
  {VarName, VS} = elixir_erl_var:build('_', S),

  Var = {var, Ann, VarName},
  Map = {map, Ann, [{map_field_exact, Ann, {atom, Ann, '__struct__'}, {atom, Ann, Name}}]},

  Match = {match, Ann, Var, Map},
  Error = {tuple, Ann, [{atom, Ann, badstruct}, {atom, Ann, Name}, Var]},

  {TUpdate, TU} = translate(Update, Ann, VS),
  {TAssocs, TS} = translate_map(Ann, Assocs, {ok, Var}, TU),

  {{'case', Generated, TUpdate, [
    {clause, Ann, [Match], [], [TAssocs]},
    {clause, Generated, [Var], [], [?remote(Ann, erlang, error, [Error])]}
  ]}, TS};
translate_struct(Ann, Name, {'%{}', _, Assocs}, S) ->
  translate_map(Ann, [{'__struct__', Name}] ++ Assocs, none, S).

translate_map(Ann, [{'|', Meta, [Update, Assocs]}], S) ->
  {TUpdate, SU} = translate(Update, Ann, S),
  translate_map(?ann(Meta), Assocs, {ok, TUpdate}, SU);
translate_map(Ann, Assocs, S) ->
  translate_map(Ann, Assocs, none, S).

translate_map(Ann, Assocs, TUpdate, #elixir_erl{extra=Extra} = S) ->
  Op = translate_key_val_op(TUpdate, S),

  {TArgs, SA} = lists:mapfoldl(fun({Key, Value}, Acc0) ->
    {TKey, Acc1} = translate(Key, Ann, Acc0#elixir_erl{extra=map_key}),
    {TValue, Acc2} = translate(Value, Ann, Acc1#elixir_erl{extra=Extra}),
    {{Op, Ann, TKey, TValue}, Acc2}
  end, S, Assocs),

  build_map(Ann, TUpdate, TArgs, SA).

translate_key_val_op(_TUpdate, #elixir_erl{extra=map_key}) -> map_field_assoc;
translate_key_val_op(_TUpdate, #elixir_erl{context=match}) -> map_field_exact;
translate_key_val_op(none, _) -> map_field_assoc;
translate_key_val_op(_, _) -> map_field_exact.

build_map(Ann, {ok, TUpdate}, TArgs, SA) -> {{map, Ann, TUpdate, TArgs}, SA};
build_map(Ann, none, TArgs, SA) -> {{map, Ann, TArgs}, SA}.

%% Translate bitstrings

translate_bitstring(Meta, Args, S) ->
  build_bitstr(Args, ?ann(Meta), S, []).

build_bitstr([{'::', Meta, [H, V]} | T], Ann, S, Acc) ->
  {Size, Types} = extract_bit_info(V, Meta, S#elixir_erl{context=nil, extra=nil}),
  build_bitstr(T, Ann, S, Acc, H, Size, Types);
build_bitstr([], Ann, S, Acc) ->
  {{bin, Ann, lists:reverse(Acc)}, S}.

build_bitstr(T, Ann, S, Acc, H, default, Types) when is_binary(H) ->
  Element =
    case requires_utf_conversion(Types) of
      false ->
        %% See explanation in elixir_erl:elixir_to_erl/1 to
        %% know why we can simply convert the binary to a list.
        {bin_element, Ann, {string, 0, binary_to_list(H)}, default, default};
      true ->
        %% UTF types require conversion.
        {bin_element, Ann, {string, 0, elixir_utils:characters_to_list(H)}, default, Types}
    end,
  build_bitstr(T, Ann, S, [Element | Acc]);

build_bitstr(T, Ann, S, Acc, H, Size, Types) ->
  {Expr, NS} = translate(H, Ann, S),
  build_bitstr(T, Ann, NS, [{bin_element, Ann, Expr, Size, Types} | Acc]).

requires_utf_conversion([bitstring | _]) -> false;
requires_utf_conversion([binary | _]) -> false;
requires_utf_conversion(_) -> true.

extract_bit_info({'-', _, [L, {size, _, [Size]}]}, Meta, S) ->
  {extract_bit_size(Size, Meta, S), extract_bit_type(L, [])};
extract_bit_info({size, _, [Size]}, Meta, S) ->
  {extract_bit_size(Size, Meta, S), []};
extract_bit_info(L, _Meta, _S) ->
  {default, extract_bit_type(L, [])}.

extract_bit_size(Size, Meta, S) ->
  {TSize, _} = translate(Size, ?ann(Meta), S#elixir_erl{context=guard}),
  TSize.

extract_bit_type({'-', _, [L, R]}, Acc) ->
  extract_bit_type(L, extract_bit_type(R, Acc));
extract_bit_type({unit, _, [Arg]}, Acc) ->
  [{unit, Arg} | Acc];
extract_bit_type({Other, _, nil}, Acc) ->
  [Other | Acc].

%% Optimizations that are specific to Erlang and change
%% the format of the AST.

translate_remote('Elixir.String.Chars', to_string, Meta, [Arg], S) ->
  Ann = ?ann(Meta),
  {TArg, TS} = translate(Arg, Ann, S),
  {VarName, VS} = elixir_erl_var:build('_', TS),

  Generated = erl_anno:set_generated(true, Ann),
  Var = {var, Generated, VarName},
  Guard = ?remote(Generated, erlang, is_binary, [Var]),
  Slow = ?remote(Generated, 'Elixir.String.Chars', to_string, [Var]),
  Fast = Var,

  {{'case', Generated, TArg, [
    {clause, Generated, [Var], [[Guard]], [Fast]},
    {clause, Generated, [Var], [], [Slow]}
  ]}, VS};
translate_remote(maps, put, Meta, [Key, Value, Map], S) ->
  Ann = ?ann(Meta),

  case translate_args([Key, Value, Map], Ann, S) of
    {[TKey, TValue, {map, _, InnerMap, Pairs}], TS} ->
      {{map, Ann, InnerMap, Pairs ++ [{map_field_assoc, Ann, TKey, TValue}]}, TS};

    {[TKey, TValue, {map, _, Pairs}], TS} ->
      {{map, Ann, Pairs ++ [{map_field_assoc, Ann, TKey, TValue}]}, TS};

    {[TKey, TValue, TMap], TS} ->
      {{map, Ann, TMap, [{map_field_assoc, Ann, TKey, TValue}]}, TS}
  end;
translate_remote(maps, merge, Meta, [Map1, Map2], S) ->
  Ann = ?ann(Meta),

  case translate_args([Map1, Map2], Ann, S) of
    {[{map, _, Pairs1}, {map, _, Pairs2}], TS} ->
      {{map, Ann, Pairs1 ++ Pairs2}, TS};

    {[{map, _, InnerMap1, Pairs1}, {map, _, Pairs2}], TS} ->
      {{map, Ann, InnerMap1, Pairs1 ++ Pairs2}, TS};

    {[TMap1, {map, _, Pairs2}], TS} ->
      {{map, Ann, TMap1, Pairs2}, TS};

    {[TMap1, TMap2], TS} ->
      {{call, Ann, {remote, Ann, {atom, Ann, maps}, {atom, Ann, merge}}, [TMap1, TMap2]}, TS}
  end;
translate_remote(Left, Right, Meta, Args, S) ->
  Ann = ?ann(Meta),

  case rewrite_strategy(Left, Right, Args) of
    guard_op ->
      {TArgs, SA} = translate_args(Args, Ann, S),
      %% Rewrite Erlang function calls as operators so they
      %% work in guards, matches and so on.
      case TArgs of
        [TOne]       -> {{op, Ann, Right, TOne}, SA};
        [TOne, TTwo] -> {{op, Ann, Right, TOne, TTwo}, SA}
      end;
    {inline_pure, Result} ->
      translate(Result, Ann, S);
    {inline_args, NewArgs} ->
      {TLeft, SL} = translate(Left, Ann, S),
      {TArgs, SA} = translate_args(NewArgs, Ann, SL),
      TRight = {atom, Ann, Right},
      {{call, Ann, {remote, Ann, TLeft, TRight}, TArgs}, SA};
    none ->
      {TLeft, SL} = translate(Left, Ann, S),
      {TArgs, SA} = translate_args(Args, Ann, SL),
      TRight = {atom, Ann, Right},
      {{call, Ann, {remote, Ann, TLeft, TRight}, TArgs}, SA}
  end.

rewrite_strategy(erlang, Right, Args) ->
  Arity  = length(Args),
  case elixir_utils:guard_op(Right, Arity) of
    true -> guard_op;
    false -> none
  end;
rewrite_strategy(Left, shift, [Struct, Opts | RestArgs]) when
  Left == 'Elixir.Date';
  Left == 'Elixir.DateTime';
  Left == 'Elixir.NaiveDateTime';
  Left == 'Elixir.Time'
->
  case basic_type_arg(Opts) of
    true ->
      try
        {inline_args, [Struct, Left:'__duration__!'(Opts) | RestArgs]}
      catch _:_ ->
        % fail silently, will fail at runtime
        none
      end;
    false ->
      none
  end;
rewrite_strategy(Left, Right, Args) ->
  case inline_pure_function(Left, Right) andalso basic_type_arg(Args) of
    true ->
      try
        {inline_pure, apply(Left, Right, Args)}
      catch _:_ ->
        % fail silently, will fail at runtime
        none
      end;
    false ->
      none
  end.

inline_pure_function('Elixir.Duration', 'new!') -> true;
inline_pure_function('Elixir.MapSet', new) -> true;
inline_pure_function('Elixir.String', length) -> true;
inline_pure_function('Elixir.String', graphemes) -> true;
inline_pure_function('Elixir.String', codepoints) -> true;
inline_pure_function('Elixir.String', split) -> true;
inline_pure_function('Elixir.Kernel', to_timeout) -> true;
inline_pure_function('Elixir.URI', new) -> true;
inline_pure_function('Elixir.URI', 'new!') -> true;
inline_pure_function('Elixir.URI', parse) -> true;
inline_pure_function('Elixir.URI', encode_query) -> true;
inline_pure_function('Elixir.URI', encode_www_form) -> true;
inline_pure_function('Elixir.URI', decode) -> true;
inline_pure_function('Elixir.URI', decode_www_for) -> true;
inline_pure_function('Elixir.Version', parse) -> true;
inline_pure_function('Elixir.Version', 'parse!') -> true;
inline_pure_function('Elixir.Version', parse_requirement) -> true;
inline_pure_function('Elixir.Version', 'parse_requirement!') -> true;
inline_pure_function(_Left, _Right) -> false.

% we do not want to try and inline calls which might depend on protocols that might be overridden later
basic_type_arg(Term) when is_number(Term); is_atom(Term); is_binary(Term) -> true;
basic_type_arg(List) when is_list(List) -> lists:all(fun basic_type_arg/1, List);
basic_type_arg({Left, Right}) -> basic_type_arg(Left) and basic_type_arg(Right);
basic_type_arg(_) -> false.

generate_struct_name_guard([{map_field_exact, Ann, {atom, _, '__struct__'} = Key, Var} | Rest], Acc, S0) ->
  {ModuleVarName, S1} = elixir_erl_var:build('_', S0),
  Generated = erl_anno:set_generated(true, Ann),
  ModuleVar = {var, Generated, ModuleVarName},
  Match = {match, Generated, ModuleVar, Var},
  Guard = ?remote(Generated, erlang, is_atom, [ModuleVar]),
  S2 = S1#elixir_erl{extra_guards=[Guard | S1#elixir_erl.extra_guards]},
  {lists:reverse(Acc, [{map_field_exact, Ann, Key, Match} | Rest]), S2};
generate_struct_name_guard([Field | Rest], Acc, S) ->
  generate_struct_name_guard(Rest, [Field | Acc], S).

%% TODO: Make this a runtime error on Elixir v2.0
no_parens_remote(nil, _Fun) -> error;
no_parens_remote(false, _Fun) -> error;
no_parens_remote(true, _Fun) -> error;
no_parens_remote(Atom, Fun) when is_atom(Atom) ->
  Message = fun() ->
    io_lib:format(
      "using map.field notation (without parentheses) to invoke function ~ts.~ts() is deprecated, "
      "you must add parentheses instead: remote.function()",
      [elixir_aliases:inspect(Atom), Fun]
    )
  end,
  'Elixir.IO':warn_once(?MODULE, Message, 3),
  {ok, apply(Atom, Fun, [])};
no_parens_remote(_Other, _Fun) ->
  error.

parens_map_field(Key, Value) ->
  Message = fun() ->
    io_lib:format(
      "using module.function() notation (with parentheses) to fetch map field ~ts is deprecated, "
      "you must remove the parentheses: map.field",
      [elixir_aliases:inspect(Key)]
    )
  end,
  'Elixir.IO':warn_once(?MODULE, Message, 3),
  Value.
