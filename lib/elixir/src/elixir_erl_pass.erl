%% Translate Elixir quoted expressions to Erlang Abstract Format.
-module(elixir_erl_pass).
-export([translate/2, translate_arg/3, translate_args/2]).
-import(elixir_erl_var, [mergev/2, mergec/2]).
-include("elixir.hrl").

%% =

translate({'=', Meta, [{'_', _, Atom}, Right]}, S) when is_atom(Atom) ->
  {TRight, SR} = translate(Right, S),
  {{match, ?ann(Meta), {var, ?ann(Meta), '_'}, TRight}, SR};

translate({'=', Meta, [Left, Right]}, S) ->
  {TRight, SR} = translate(Right, S),
  case elixir_erl_clauses:match(fun translate/2, Left, SR) of
    {TLeft, #elixir_erl{extra_guards=ExtraGuards, context=Context} = SL0}
        when ExtraGuards =/= [], Context =/= match ->
      SL1 = SL0#elixir_erl{extra_guards=[]},
      {ResultVarName, _, SL2} = elixir_erl_var:build('_', SL1),
      Match = {match, ?ann(Meta), TLeft, TRight},
      Generated = ?ann(?generated(Meta)),
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
      {{match, ?ann(Meta), TLeft, TRight}, SL}
  end;

%% Containers

translate({'{}', Meta, Args}, S) when is_list(Args) ->
  {TArgs, SE} = translate_args(Args, S),
  {{tuple, ?ann(Meta), TArgs}, SE};

translate({'%{}', Meta, Args}, S) when is_list(Args) ->
  translate_map(Meta, Args, S);

translate({'%', Meta, [{'^', _, [{Name, _, Context}]} = Left, Right]}, S) when is_atom(Name), is_atom(Context) ->
  translate_struct_var_name(Meta, Left, Right, S);

translate({'%', Meta, [{Name, _, Context} = Left, Right]}, S) when is_atom(Name), is_atom(Context) ->
  translate_struct_var_name(Meta, Left, Right, S);

translate({'%', Meta, [Left, Right]}, S) ->
  translate_struct(Meta, Left, Right, S);

translate({'<<>>', Meta, Args}, S) when is_list(Args) ->
  translate_bitstring(Meta, Args, S);

%% Blocks

translate({'__block__', Meta, Args}, S) when is_list(Args) ->
  {TArgs, SA} = translate_block(Args, [], S),
  {{block, ?ann(Meta), TArgs}, SA};

%% Compilation environment macros

translate({'__CALLER__', Meta, Atom}, S) when is_atom(Atom) ->
  {{var, ?ann(Meta), '__CALLER__'}, S#elixir_erl{caller=true}};

translate({'__STACKTRACE__', Meta, Atom}, S = #elixir_erl{stacktrace={Var, _}}) when is_atom(Atom) ->
  {{var, ?ann(Meta), Var}, S#elixir_erl{stacktrace={Var, true}}};

translate({'super', Meta, Args}, S) ->
  %% In the expanded AST, super is used to invoke a function
  %% in the current module originated from a default clause
  %% or a super call.
  {TArgs, SA} = translate_args(Args, S),
  Ann = ?ann(Meta),
  {super, {Kind, Name}} = lists:keyfind(super, 1, Meta),

  if
    Kind == defmacro; Kind == defmacrop ->
      MacroName = elixir_utils:macro_name(Name),
      {{call, Ann, {atom, Ann, MacroName}, [{var, Ann, '__CALLER__'} | TArgs]}, SA#elixir_erl{caller=true}};
    Kind == def; Kind == defp ->
      {{call, Ann, {atom, Ann, Name}, TArgs}, SA}
  end;

%% Functions

translate({'&', Meta, [{'/', _, [{{'.', _, [Remote, Fun]}, _, []}, Arity]}]}, S)
    when is_atom(Fun), is_integer(Arity) ->
  {TRemote, SR} = translate(Remote, S),
  Ann = ?ann(Meta),
  TFun = {atom, Ann, Fun},
  TArity = {integer, Ann, Arity},
  {{'fun', Ann, {function, TRemote, TFun, TArity}}, SR};
translate({'&', Meta, [{'/', _, [{Fun, _, Atom}, Arity]}]}, S)
    when is_atom(Fun), is_atom(Atom), is_integer(Arity) ->
  {{'fun', ?ann(Meta), {function, Fun, Arity}}, S};

translate({fn, Meta, Clauses}, S) ->
  Transformer = fun({'->', CMeta, [ArgsWithGuards, Expr]}, Acc) ->
    {Args, Guards} = elixir_utils:extract_splat_guards(ArgsWithGuards),
    {TClause, TS} = elixir_erl_clauses:clause(CMeta, fun translate_fn_match/2,
                                              Args, Expr, Guards, Acc),
    {TClause, elixir_erl_var:mergec(S, TS)}
  end,
  {TClauses, NS} = lists:mapfoldl(Transformer, S, Clauses),
  {{'fun', ?ann(Meta), {clauses, TClauses}}, NS};

%% Cond

translate({'cond', CondMeta, [[{do, Clauses}]]}, S) ->
  [{'->', Meta, [[Condition], _]} = H | T] = lists:reverse(Clauses),

  FirstMeta =
    if
      is_atom(Condition), Condition /= false, Condition /= nil -> ?generated(Meta);
      true -> Meta
    end,

  Error = {{'.', Meta, [erlang, error]}, [], [cond_clause]},
  Case = build_cond_clauses([H | T], Error, FirstMeta),
  translate(replace_case_meta(CondMeta, Case), S);

%% Case

translate({'case', Meta, [Expr, Opts]}, S) ->
  translate_case(Meta, Expr, Opts, S);

%% Try

translate({'try', Meta, [Opts]}, S) ->
  Do = proplists:get_value('do', Opts, nil),
  {TDo, SB} = translate(Do, S),

  Catch = [Tuple || {X, _} = Tuple <- Opts, X == 'rescue' orelse X == 'catch'],
  {TCatch, SC} = elixir_erl_try:clauses(Meta, Catch, mergec(S, SB)),

  {TAfter, SA} = case lists:keyfind('after', 1, Opts) of
    {'after', After} ->
      {TBlock, SAExtracted} = translate(After, mergec(S, SC)),
      {unblock(TBlock), SAExtracted};
    false ->
      {[], mergec(S, SC)}
  end,

  Else = elixir_erl_clauses:get_clauses(else, Opts, match),
  {TElse, SE} = elixir_erl_clauses:clauses(Else, mergec(S, SA)),
  {{'try', ?ann(Meta), unblock(TDo), TElse, TCatch, TAfter}, mergec(S, SE)};

%% Receive

translate({'receive', Meta, [Opts]}, S) ->
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

translate({for, Meta, [_ | _] = Args}, S) ->
  elixir_erl_for:translate(Meta, Args, true, S);

translate({with, Meta, [_ | _] = Args}, S) ->
  {Exprs, [{do, Do} | Opts]} = elixir_utils:split_last(Args),
  {ElseClause, SE} = translate_with_else(Meta, Opts, S),
  {With, SD} = translate_with_do(Exprs, Do, ElseClause, elixir_erl_var:mergec(S, SE)),
  {With, elixir_erl_var:mergec(S, SD)};

%% Variables

translate({'^', Meta, [{Name, VarMeta, Kind}]}, #elixir_erl{context=match} = S) when is_atom(Name), is_atom(Kind) ->
  Tuple = {Name, elixir_utils:var_context(VarMeta, Kind)},
  {ok, {_Counter, Value}} = maps:find(Tuple, S#elixir_erl.backup_vars),

  PAnn = ?ann(?generated(Meta)),
  PVar = {var, PAnn, Value},

  case S#elixir_erl.extra of
    pin_guard ->
      {TVar, TS} = elixir_erl_var:translate(VarMeta, Name, elixir_utils:var_context(VarMeta, Kind), S),
      Guard = {op, PAnn, '=:=', PVar, TVar},
      {TVar, TS#elixir_erl{extra_guards=[Guard | TS#elixir_erl.extra_guards]}};
    _ ->
      {PVar, S}
  end;

translate({'_', Meta, Kind}, #elixir_erl{context=match} = S) when is_atom(Kind) ->
  {{var, ?ann(Meta), '_'}, S};

translate({Name, Meta, Kind}, S) when is_atom(Name), is_atom(Kind) ->
  elixir_erl_var:translate(Meta, Name, elixir_utils:var_context(Meta, Kind), S);

%% Local calls

translate({Name, Meta, Args}, S) when is_atom(Name), is_list(Meta), is_list(Args) ->
  Ann = ?ann(Meta),
  {TArgs, NS} = translate_args(Args, S),
  {{call, Ann, {atom, Ann, Name}, TArgs}, NS};

%% Remote calls

translate({{'.', _, [Left, Right]}, Meta, []}, S)
    when is_tuple(Left), is_atom(Right), is_list(Meta) ->
  {TLeft, SL}  = translate(Left, S),
  {Var, _, SV} = elixir_erl_var:build('_', SL),

  Ann = ?ann(Meta),
  Generated = erl_anno:set_generated(true, Ann),
  TRight = {atom, Ann, Right},
  TVar = {var, Ann, Var},
  TError = {tuple, Ann, [{atom, Ann, badkey}, TRight, TVar]},

  {{'case', Generated, TLeft, [
    {clause, Generated,
      [{map, Ann, [{map_field_exact, Ann, TRight, TVar}]}],
      [],
      [TVar]},
    {clause, Generated,
      [TVar],
      [[?remote(Generated, erlang, is_map, [TVar])]],
      [?remote(Ann, erlang, error, [TError])]},
    {clause, Generated,
      [TVar],
      [],
      [{call, Generated, {remote, Generated, TVar, TRight}, []}]}
  ]}, SV};

translate({{'.', _, [Left, Right]}, Meta, Args}, S)
    when (is_tuple(Left) orelse is_atom(Left)), is_atom(Right), is_list(Meta), is_list(Args) ->
  translate_remote(Left, Right, Meta, Args, S);

%% Anonymous function calls

translate({{'.', _, [Expr]}, Meta, Args}, S) when is_list(Args) ->
  {TExpr, SE} = translate(Expr, S),
  {TArgs, SA} = translate_args(Args, mergec(S, SE)),
  {{call, ?ann(Meta), TExpr, TArgs}, mergev(SE, SA)};

%% Literals

translate(List, S) when is_list(List) ->
  Fun = case S#elixir_erl.context of
    match -> fun translate/2;
    _     -> fun(X, Acc) -> translate_arg(X, Acc, S) end
  end,
  translate_list(List, Fun, S, []);

translate({Left, Right}, S) ->
  {TArgs, SE} = translate_args([Left, Right], S),
  {{tuple, 0, TArgs}, SE};

translate(Other, S) ->
  {elixir_erl:elixir_to_erl(Other), S}.

%% Helpers

translate_case(Meta, Expr, Opts, S) ->
  Clauses = elixir_erl_clauses:get_clauses(do, Opts, match),
  {TExpr, SE} = translate(Expr, S),
  {TClauses, SC} = elixir_erl_clauses:clauses(Clauses, SE),
  {{'case', ?ann(Meta), TExpr, TClauses}, SC}.

translate_list([{'|', _, [_, _]=Args}], Fun, Acc, List) ->
  {[TLeft, TRight], TAcc} = lists:mapfoldl(Fun, Acc, Args),
  {build_list([TLeft | List], TRight), TAcc};
translate_list([H | T], Fun, Acc, List) ->
  {TH, TAcc} = Fun(H, Acc),
  translate_list(T, Fun, TAcc, [TH | List]);
translate_list([], _Fun, Acc, List) ->
  {build_list(List, {nil, 0}), Acc}.

build_list([H | T], Acc) ->
  build_list(T, {cons, 0, H, Acc});
build_list([], Acc) ->
  Acc.

%% Pack a list of expressions from a block.
unblock({'block', _, Exprs}) -> Exprs;
unblock(Expr)                -> [Expr].

translate_fn_match(Arg, S) ->
  {TArg, TS} = translate_args(Arg, S#elixir_erl{extra=pin_guard}),
  {TArg, TS#elixir_erl{extra=S#elixir_erl.extra}}.

%% Translate args

translate_arg(Arg, Acc, S) when is_number(Arg); is_atom(Arg); is_binary(Arg); is_pid(Arg); is_function(Arg) ->
  {TArg, _} = translate(Arg, S),
  {TArg, Acc};
translate_arg(Arg, Acc, S) ->
  {TArg, TAcc} = translate(Arg, mergec(S, Acc)),
  {TArg, mergev(Acc, TAcc)}.

translate_args(Args, #elixir_erl{context=match} = S) ->
  lists:mapfoldl(fun translate/2, S, Args);

translate_args(Args, S) ->
  lists:mapfoldl(fun(X, Acc) -> translate_arg(X, Acc, S) end, S, Args).

%% Translate blocks

translate_block([], Acc, S) ->
  {lists:reverse(Acc), S};
translate_block([H], Acc, S) ->
  {TH, TS} = translate(H, S),
  translate_block([], [TH | Acc], TS);
translate_block([{'__block__', _Meta, Args} | T], Acc, S) when is_list(Args) ->
  translate_block(Args ++ T, Acc, S);
translate_block([{for, Meta, [_ | _] = Args} | T], Acc, S) ->
  {TH, TS} = elixir_erl_for:translate(Meta, Args, false, S),
  translate_block(T, [TH | Acc], TS);
translate_block([{'=', _, [{'_', _, Ctx}, {for, Meta, [_ | _] = Args}]} | T], Acc, S) when is_atom(Ctx) ->
  {TH, TS} = elixir_erl_for:translate(Meta, Args, false, S),
  translate_block(T, [TH | Acc], TS);
translate_block([H | T], Acc, S) ->
  {TH, TS} = translate(H, S),
  translate_block(T, [TH | Acc], TS).

%% Cond

build_cond_clauses([{'->', NewMeta, [[Condition], Body]} | T], Acc, OldMeta) ->
  {NewCondition, Truthy, Other} = build_truthy_clause(NewMeta, Condition, Body),
  Falsy = {'->', OldMeta, [[Other], Acc]},
  Case = {'case', NewMeta, [NewCondition, [{do, [Truthy, Falsy]}]]},
  build_cond_clauses(T, Case, NewMeta);
build_cond_clauses([], Acc, _) ->
  Acc.

replace_case_meta(Meta, {'case', _, Args}) ->
  {'case', Meta, Args};
replace_case_meta(_Meta, Other) ->
  Other.

build_truthy_clause(Meta, Condition, Body) ->
  case returns_boolean(Condition, Body) of
    {NewCondition, NewBody} ->
      {NewCondition, {'->', Meta, [[true], NewBody]}, false};
    false ->
      Var = {'cond', [], ?var_context},
      Head = {'when', [], [Var,
        {{'.', [], [erlang, 'andalso']}, [], [
          {{'.', [], [erlang, '/=']}, [], [Var, nil]},
          {{'.', [], [erlang, '/=']}, [], [Var, false]}
        ]}
      ]},
      {Condition, {'->', Meta, [[Head], Body]}, {'_', [], nil}}
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
  Ann = ?ann(?generated(Meta)),
  {VarName, _, SC} = elixir_erl_var:build('_', S),
  Var = {var, Ann, VarName},
  {{clause, Ann, [Var], [], [Var]}, SC};
translate_with_else(Meta, [{else, [{'->', _, [[{Var, VarMeta, Kind}], Clause]}]}], S) when is_atom(Var), is_atom(Kind) ->
  Ctx = elixir_utils:var_context(VarMeta, Kind),
  {ElseVarErl, SV} = elixir_erl_var:assign(Meta, Var, Ctx, S),
  {TranslatedClause, SC} = elixir_erl_pass:translate(Clause, SV),
  {{clause, Meta, [ElseVarErl], [], [TranslatedClause]}, SC};
translate_with_else(Meta, [{else, Else}], S) ->
  Generated = ?generated(Meta),
  ElseVarEx = {else, Generated, ?var_context},
  {ElseVarErl, SV} = elixir_erl_var:assign(Generated, else, ?var_context, S),

  RaiseVar = {catch_all, Generated, ?var_context},
  RaiseExpr = {{'.', Generated, [erlang, error]}, Generated, [{with_clause, RaiseVar}]},
  RaiseClause = {'->', Generated, [[RaiseVar], RaiseExpr]},

  GeneratedElse = [build_generated_clause(Generated, ElseClause) || ElseClause <- Else],

  Case = {'case', Generated, [ElseVarEx, [{do, GeneratedElse ++ [RaiseClause]}]]},
  {TranslatedCase, SC} = elixir_erl_pass:translate(Case, SV),
  {{clause, ?ann(Generated), [ElseVarErl], [], [TranslatedCase]}, SC}.

build_generated_clause(Generated, {'->', _, [Args, Clause]}) ->
  NewArgs = [build_generated_clause_arg(Generated, Arg) || Arg <- Args],
  {'->', Generated, [NewArgs, Clause]}.

build_generated_clause_arg(Generated, Arg) ->
  {Expr, Guards} = elixir_utils:extract_guards(Arg),
  NewGuards = [build_generated_guard(Generated, Guard) || Guard <- Guards],
  concat_guards(Generated, Expr, NewGuards).

build_generated_guard(Generated, {{'.', _, _} = Call, _, Args}) ->
  {Call, Generated, [build_generated_guard(Generated, Arg) || Arg <- Args]};
build_generated_guard(_, Expr) ->
  Expr.

concat_guards(_Meta, Expr, []) ->
  Expr;
concat_guards(Meta, Expr, [Guard | Tail]) ->
  {'when', Meta, [Expr, concat_guards(Meta, Guard, Tail)]}.

translate_with_do([{'<-', Meta, [Left, Expr]} | Rest], Do, Else, S) ->
  {Args, Guards} = elixir_utils:extract_guards(Left),
  {TExpr, SR} = elixir_erl_pass:translate(Expr, S),
  {TArgs, SA} = elixir_erl_clauses:match(fun elixir_erl_pass:translate/2, Args, SR),
  TGuards = elixir_erl_clauses:guards(Guards, [], SA),
  {TBody, SB} = translate_with_do(Rest, Do, Else, SA),

  Clause = {clause, ?ann(Meta), [TArgs], TGuards, unblock(TBody)},
  {{'case', ?ann(?generated(Meta)), TExpr, [Clause, Else]}, SB};
translate_with_do([Expr | Rest], Do, Else, S) ->
  {TExpr, TS} = elixir_erl_pass:translate(Expr, S),
  {TRest, RS} = translate_with_do(Rest, Do, Else, TS),
  {{block, 0, [TExpr | unblock(TRest)]}, RS};
translate_with_do([], Do, _Else, S) ->
  elixir_erl_pass:translate(Do, S).

%% Maps and structs

translate_map(Meta, [{'|', _Meta, [Update, Assocs]}], S) ->
  {TUpdate, US} = translate_arg(Update, S, S),
  translate_map(Meta, Assocs, {ok, TUpdate}, US);
translate_map(Meta, Assocs, S) ->
  translate_map(Meta, Assocs, none, S).

translate_struct_var_name(Meta, Name, Args, S0) ->
  {{map, MapAnn, TArgs0}, S1} = translate_struct(Meta, Name, Args, S0),
  {TArgs1, S2} = generate_struct_name_guard(TArgs0, [], S1),
  {{map, MapAnn, TArgs1}, S2}.

translate_struct(Meta, Name, {'%{}', _, [{'|', _, [Update, Assocs]}]}, S) ->
  Ann = ?ann(Meta),
  Generated = erl_anno:set_generated(true, Ann),
  {VarName, _, VS} = elixir_erl_var:build('_', S),

  Var = {var, Ann, VarName},
  Map = {map, Ann, [{map_field_exact, Ann, {atom, Ann, '__struct__'}, {atom, Ann, Name}}]},

  Match = {match, Ann, Var, Map},
  Error = {tuple, Ann, [{atom, Ann, badstruct}, {atom, Ann, Name}, Var]},

  {TUpdate, US} = translate_arg(Update, VS, VS),
  {TAssocs, TS} = translate_map(Meta, Assocs, {ok, Var}, US),

  {{'case', Generated, TUpdate, [
    {clause, Ann, [Match], [], [TAssocs]},
    {clause, Generated, [Var], [], [?remote(Ann, erlang, error, [Error])]}
  ]}, TS};
translate_struct(Meta, Name, {'%{}', _, Assocs}, S) ->
  translate_map(Meta, Assocs ++ [{'__struct__', Name}], none, S).

translate_map(Meta, Assocs, TUpdate, #elixir_erl{extra=Extra} = S) ->
  {Op, KeyFun, ValFun} = translate_key_val_op(TUpdate, S),
  Ann = ?ann(Meta),

  {TArgs, SA} = lists:mapfoldl(fun({Key, Value}, Acc0) ->
    {TKey, Acc1} = KeyFun(Key, Acc0),
    {TValue, Acc2} = ValFun(Value, Acc1#elixir_erl{extra=Extra}),
    {{Op, ?ann(Meta), TKey, TValue}, Acc2}
  end, S, Assocs),

  build_map(Ann, TUpdate, TArgs, SA).

translate_key_val_op(_TUpdate, #elixir_erl{extra=map_key}) ->
  {map_field_assoc,
    fun(X, Acc) -> translate(X, Acc#elixir_erl{extra=map_key}) end,
    fun translate/2};
translate_key_val_op(_TUpdate, #elixir_erl{context=match}) ->
  {map_field_exact,
    fun(X, Acc) -> translate(X, Acc#elixir_erl{extra=map_key}) end,
    fun translate/2};
translate_key_val_op(TUpdate, S) ->
  KS = S#elixir_erl{extra=map_key},
  Op = if TUpdate == none -> map_field_assoc; true -> map_field_exact end,
  {Op,
    fun(X, Acc) -> translate_arg(X, Acc, KS) end,
    fun(X, Acc) -> translate_arg(X, Acc, S) end}.

build_map(Ann, {ok, TUpdate}, TArgs, SA) -> {{map, Ann, TUpdate, TArgs}, SA};
build_map(Ann, none, TArgs, SA) -> {{map, Ann, TArgs}, SA}.

%% Translate bitstrings

translate_bitstring(Meta, Args, S) ->
  case S#elixir_erl.context of
    match -> build_bitstr(fun translate/2, Args, Meta, S, []);
    _ -> build_bitstr(fun(X, Acc) -> translate_arg(X, Acc, S) end, Args, Meta, S, [])
  end.

build_bitstr(Fun, [{'::', _, [H, V]} | T], Meta, S, Acc) ->
  {Size, Types} = extract_bit_info(V, S#elixir_erl{context=nil}),
  build_bitstr(Fun, T, Meta, S, Acc, H, Size, Types);
build_bitstr(_Fun, [], Meta, S, Acc) ->
  {{bin, ?ann(Meta), lists:reverse(Acc)}, S}.

build_bitstr(Fun, T, Meta, S, Acc, H, default, Types) when is_binary(H) ->
  Element =
    case requires_utf_conversion(Types) of
      false ->
        %% See explanation in elixir_erl:elixir_to_erl/1 to
        %% know why we can simply convert the binary to a list.
        {bin_element, ?ann(Meta), {string, 0, binary_to_list(H)}, default, default};
      true ->
        %% UTF types require conversion.
        {bin_element, ?ann(Meta), {string, 0, elixir_utils:characters_to_list(H)}, default, Types}
    end,
  build_bitstr(Fun, T, Meta, S, [Element | Acc]);

build_bitstr(Fun, T, Meta, S, Acc, H, Size, Types) ->
  {Expr, NS} = Fun(H, S),
  build_bitstr(Fun, T, Meta, NS, [{bin_element, ?ann(Meta), Expr, Size, Types} | Acc]).

requires_utf_conversion([bitstring | _]) -> false;
requires_utf_conversion([binary | _]) -> false;
requires_utf_conversion(_) -> true.

extract_bit_info({'-', _, [L, {size, _, [Size]}]}, S) ->
  {extract_bit_size(Size, S), extract_bit_type(L, [])};
extract_bit_info({size, _, [Size]}, S) ->
  {extract_bit_size(Size, S), []};
extract_bit_info(L, _S) ->
  {default, extract_bit_type(L, [])}.

extract_bit_size(Size, S) ->
  {TSize, _} = translate(Size, S),
  TSize.

extract_bit_type({'-', _, [L, R]}, Acc) ->
  extract_bit_type(L, extract_bit_type(R, Acc));
extract_bit_type({unit, _, [Arg]}, Acc) ->
  [{unit, Arg} | Acc];
extract_bit_type({Other, _, []}, Acc) ->
  [Other | Acc].

%% Optimizations that are specific to Erlang and change
%% the format of the AST.

translate_remote('Elixir.Access' = Mod, get, Meta, [Container, Value], S) ->
  Ann = ?ann(Meta),
  {TArgs, SA} = translate_args([Container, Value, nil], S),
  {?remote(Ann, Mod, get, TArgs), SA};
translate_remote('Elixir.String.Chars', to_string, Meta, [Arg], S) ->
  case is_always_string(Arg) of
    true ->
      translate(Arg, S);
    false ->
      {TArg, TS} = translate(Arg, S),
      {VarName, _, VS} = elixir_erl_var:build(rewrite, TS),

      Generated = ?ann(?generated(Meta)),
      Var = {var, Generated, VarName},
      Guard = ?remote(Generated, erlang, is_binary, [Var]),
      Slow = ?remote(Generated, 'Elixir.String.Chars', to_string, [Var]),
      Fast = Var,

      {{'case', Generated, TArg, [
        {clause, Generated, [Var], [[Guard]], [Fast]},
        {clause, Generated, [Var], [], [Slow]}
      ]}, VS}
  end;
translate_remote(maps, put, Meta, [Key, Value, Map], S) ->
  Ann = ?ann(Meta),
  {TExpr, ES} =
    case translate_args([Key, Value, Map], S) of
      {[TKey, TValue, {map, _, InnerMap, Pairs}], TS} ->
        {{map, Ann, InnerMap, Pairs ++ [{map_field_assoc, Ann, TKey, TValue}]}, TS};

      {[TKey, TValue, {map, _, Pairs}], TS} ->
        {{map, Ann, Pairs ++ [{map_field_assoc, Ann, TKey, TValue}]}, TS};

      {[TKey, TValue, TMap], TS} ->
        {{map, Ann, TMap, [{map_field_assoc, Ann, TKey, TValue}]}, TS}
    end,
  {TExpr, mergev(S, ES)};
translate_remote(maps, merge, Meta, [Map1, Map2], S) ->
  Ann = ?ann(Meta),
  {TExpr, ES} =
    case translate_args([Map1, Map2], S) of
      {[{map, _, Pairs1}, {map, _, Pairs2}], TS} ->
        {{map, Ann, Pairs1 ++ Pairs2}, TS};

      {[{map, _, InnerMap1, Pairs1}, {map, _, Pairs2}], TS} ->
        {{map, Ann, InnerMap1, Pairs1 ++ Pairs2}, TS};

      {[TMap1, {map, _, Pairs2}], TS} ->
        {{map, Ann, TMap1, Pairs2}, TS};

      {[TMap1, TMap2], TS} ->
        {{call, Ann, {remote, Ann, {atom, Ann, maps}, {atom, Ann, merge}}, [TMap1, TMap2]}, TS}
    end,
  {TExpr, mergev(S, ES)};
translate_remote(Left, Right, Meta, Args, S) ->
  {TLeft, SL} = translate(Left, S),
  {TArgs, SA} = translate_args(Args, mergec(S, SL)),

  Ann    = ?ann(Meta),
  Arity  = length(Args),
  TRight = {atom, Ann, Right},
  SC = mergev(SL, SA),

  %% Rewrite Erlang function calls as operators so they
  %% work in guards, matches and so on.
  case (Left == erlang) andalso elixir_utils:guard_op(Right, Arity) of
    true ->
      case TArgs of
        [TOne]       -> {{op, Ann, Right, TOne}, SC};
        [TOne, TTwo] -> {{op, Ann, Right, TOne, TTwo}, SC}
      end;
    false ->
      {{call, Ann, {remote, Ann, TLeft, TRight}, TArgs}, SC}
  end.

is_always_string({{'.', _, [Module, Function]}, _, Args}) ->
  is_always_string(Module, Function, length(Args));
%% Binary literals were already excluded in earlier passes.
is_always_string(_Ast) ->
  false.

is_always_string('Elixir.Enum', join, _) -> true;
is_always_string('Elixir.Enum', map_join, _) -> true;
is_always_string('Elixir.Kernel', inspect, _) -> true;
is_always_string('Elixir.Macro', to_string, _) -> true;
is_always_string('Elixir.String.Chars', to_string, _) -> true;
is_always_string('Elixir.Path', join, _) -> true;
is_always_string(_Module, _Function, _Args) -> false.

generate_struct_name_guard([{map_field_exact, Ann, {atom, _, '__struct__'} = Key, Var} | Rest], Acc, S0) ->
  {ModuleVarName, _, S1} = elixir_erl_var:build('_', S0),
  Generated = erl_anno:set_generated(true, Ann),
  ModuleVar = {var, Generated, ModuleVarName},
  Match = {match, Generated, ModuleVar, Var},
  Guard = ?remote(Generated, erlang, is_atom, [ModuleVar]),
  S2 = S1#elixir_erl{extra_guards=[Guard | S1#elixir_erl.extra_guards]},
  {lists:reverse(Acc, [{map_field_exact, Ann, Key, Match} | Rest]), S2};
generate_struct_name_guard([Field | Rest], Acc, S) ->
  generate_struct_name_guard(Rest, [Field | Acc], S).
