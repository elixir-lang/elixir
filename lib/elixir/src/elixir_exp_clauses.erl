%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_exp_clauses).
-export([match/3, clause/5, def/2, head/2,
         'case'/3, 'receive'/3, 'try'/3, 'cond'/3]).
-import(elixir_errors, [compile_error/3, compile_error/4]).
-include("elixir.hrl").

match(Fun, Expr, #{context := Context} = E) ->
  {EExpr, EE} = Fun(Expr, E#{context := match}),
  {EExpr, EE#{context := Context}}.

def({Meta, Args, Guards, Body}, E) ->
  {EArgs, EA}   = elixir_expand:expand(Args, E#{context := match}),
  {EGuards, EG} = guard(Guards, EA#{context := guard}),
  {EBody, _}    = elixir_expand:expand(Body, EG#{context := ?key(E, context)}),
  {Meta, EArgs, EGuards, EBody}.

clause(Meta, Kind, Fun, {'->', ClauseMeta, [_, _]} = Clause, E) when is_function(Fun, 3) ->
  clause(Meta, Kind, fun(X, Acc) -> Fun(ClauseMeta, X, Acc) end, Clause, E);
clause(_Meta, _Kind, Fun, {'->', Meta, [Left, Right]}, #{export_vars := ExportVars} = E) ->
  {ELeft, EL}  = Fun(Left, E),
  {ERight, ER} = elixir_expand:expand(Right, EL#{export_vars := ExportVars}),
  {{'->', Meta, [ELeft, ERight]}, ER};
clause(Meta, Kind, _Fun, _, E) ->
  compile_error(Meta, ?key(E, file), "expected -> clauses in ~ts", [Kind]).

head([{'when', Meta, [_, _ | _] = All}], E) ->
  {Args, Guard} = elixir_utils:split_last(All),
  {EArgs, EA}   = match(fun elixir_expand:expand_args/2, Args, E),
  {EGuard, EG}  = guard(Guard, EA#{context := guard}),
  {[{'when', Meta, EArgs ++ [EGuard]}], EG#{context := ?key(E, context)}};
head(Args, E) ->
  match(fun elixir_expand:expand_args/2, Args, E).

guard({'when', Meta, [Left, Right]}, E) ->
  {ELeft, EL}  = guard(Left, E),
  {ERight, ER} = guard(Right, EL),
  {{'when', Meta, [ELeft, ERight]}, ER};
guard(Other, E) ->
  elixir_expand:expand(Other, E).

%% Case

'case'(Meta, [], E) ->
  compile_error(Meta, ?key(E, file), "missing do keyword in case");
'case'(Meta, KV, E) when not is_list(KV) ->
  compile_error(Meta, ?key(E, file), "invalid arguments for case");
'case'(Meta, KV, E) ->
  ok = assert_at_most_once('do', KV, 0, fun(Kind) ->
    compile_error(Meta, ?key(E, file), "duplicated ~ts clauses given for case", [Kind])
  end),
  EE = E#{export_vars := []},
  {EClauses, EVars} = lists:mapfoldl(fun(X, Acc) -> do_case(Meta, X, Acc, EE) end, [], KV),
  {EClauses, elixir_env:mergev(EVars, E)}.

do_case(Meta, {'do', _} = Do, Acc, E) ->
  Fun = expand_one(Meta, 'case', 'do', fun head/2),
  expand_with_export(Meta, 'case', Fun, Do, Acc, E);
do_case(Meta, {Key, _}, _Acc, E) ->
  compile_error(Meta, ?key(E, file), "unexpected keyword ~ts in case", [Key]).

%% Cond

'cond'(Meta, [], E) ->
  compile_error(Meta, ?key(E, file), "missing do keyword in cond");
'cond'(Meta, KV, E) when not is_list(KV) ->
  compile_error(Meta, ?key(E, file), "invalid arguments for cond");
'cond'(Meta, KV, E) ->
  ok = assert_at_most_once('do', KV, 0, fun(Kind) ->
    compile_error(Meta, ?key(E, file), "duplicated ~ts clauses given for cond", [Kind])
  end),
  EE = E#{export_vars := []},
  {EClauses, EVars} = lists:mapfoldl(fun(X, Acc) -> do_cond(Meta, X, Acc, EE) end, [], KV),
  {EClauses, elixir_env:mergev(EVars, E)}.

do_cond(Meta, {'do', _} = Do, Acc, E) ->
  Fun = expand_one(Meta, 'cond', 'do', fun elixir_expand:expand_args/2),
  expand_with_export(Meta, 'cond', Fun, Do, Acc, E);
do_cond(Meta, {Key, _}, _Acc, E) ->
  compile_error(Meta, ?key(E, file), "unexpected keyword ~ts in cond", [Key]).

%% Receive

'receive'(Meta, [], E) ->
  compile_error(Meta, ?key(E, file), "missing do or after keyword in receive");
'receive'(Meta, KV, E) when not is_list(KV) ->
  compile_error(Meta, ?key(E, file), "invalid arguments for receive");
'receive'(Meta, KV, E) ->
  RaiseError = fun(Kind) ->
    compile_error(Meta, ?key(E, file), "duplicated ~ts clauses given for receive", [Kind])
  end,
  ok = assert_at_most_once('do', KV, 0, RaiseError),
  ok = assert_at_most_once('after', KV, 0, RaiseError),
  EE = E#{export_vars := []},
  {EClauses, EVars} = lists:mapfoldl(fun(X, Acc) -> do_receive(Meta, X, Acc, EE) end, [], KV),
  {EClauses, elixir_env:mergev(EVars, E)}.

do_receive(_Meta, {'do', nil} = Do, Acc, _E) ->
  {Do, Acc};
do_receive(Meta, {'do', _} = Do, Acc, E) ->
  Fun = expand_one(Meta, 'receive', 'do', fun head/2),
  expand_with_export(Meta, 'receive', Fun, Do, Acc, E);
do_receive(Meta, {'after', [_]} = After, Acc, E) ->
  Fun = expand_one(Meta, 'receive', 'after', fun elixir_expand:expand_args/2),
  expand_with_export(Meta, 'receive', Fun, After, Acc, E);
do_receive(Meta, {'after', _}, _Acc, E) ->
  compile_error(Meta, ?key(E, file), "expected a single -> clause for after in receive");
do_receive(Meta, {Key, _}, _Acc, E) ->
  compile_error(Meta, ?key(E, file), "unexpected keyword ~ts in receive", [Key]).

%% Try

'try'(Meta, [], E) ->
  compile_error(Meta, ?key(E, file), "missing do keyword in try");
'try'(Meta, [{do, _}], E) ->
  compile_error(Meta, ?key(E, file), "missing catch/rescue/after/else keyword in try");
'try'(Meta, KV, E) when not is_list(KV) ->
  compile_error(Meta, ?key(E, file), "invalid arguments for try");
'try'(Meta, KV, E) ->
  RaiseError = fun(Kind) ->
    compile_error(Meta, ?key(E, file), "duplicated ~ts clauses given for try", [Kind])
  end,
  ok = assert_at_most_once('do', KV, 0, RaiseError),
  ok = assert_at_most_once('rescue', KV, 0, RaiseError),
  ok = assert_at_most_once('catch', KV, 0, RaiseError),
  ok = assert_at_most_once('else', KV, 0, RaiseError),
  ok = assert_at_most_once('after', KV, 0, RaiseError),
  {lists:map(fun(X) -> do_try(Meta, X, E) end, KV), E}.

do_try(_Meta, {'do', Expr}, E) ->
  {EExpr, _} = elixir_expand:expand(Expr, E),
  {'do', EExpr};
do_try(_Meta, {'after', Expr}, E) ->
  {EExpr, _} = elixir_expand:expand(Expr, E),
  {'after', EExpr};
do_try(Meta, {'else', _} = Else, E) ->
  Fun = expand_one(Meta, 'try', 'else', fun head/2),
  expand_without_export(Meta, 'try', Fun, Else, E);
do_try(Meta, {'catch', _} = Catch, E) ->
  expand_without_export(Meta, 'try', fun expand_catch/3, Catch, E);
do_try(Meta, {'rescue', _} = Rescue, E) ->
  expand_without_export(Meta, 'try', fun expand_rescue/3, Rescue, E);
do_try(Meta, {Key, _}, E) ->
  compile_error(Meta, ?key(E, file), "unexpected keyword ~ts in try", [Key]).

expand_catch(_Meta, [_] = Args, E) ->
  head(Args, E);
expand_catch(_Meta, [_, _] = Args, E) ->
  head(Args, E);
expand_catch(Meta, _, E) ->
  compile_error(Meta, ?key(E, file), "expected one or two args for catch clauses (->) in try").

expand_rescue(Meta, [Arg], E) ->
  case expand_rescue(Arg, E) of
    {EArg, EA} ->
      {[EArg], EA};
    false ->
      compile_error(Meta, ?key(E, file), "invalid rescue clause. The clause should "
        "match on an alias, a variable or be in the \"var in [alias]\" format")
  end;
expand_rescue(Meta, _, E) ->
  compile_error(Meta, ?key(E, file), "expected one arg for rescue clauses (->) in try").

%% rescue var
expand_rescue({Name, _, Atom} = Var, E) when is_atom(Name), is_atom(Atom) ->
  match(fun elixir_expand:expand/2, Var, E);

%% rescue var in [Exprs]
expand_rescue({in, Meta, [Left, Right]}, E) ->
  {ELeft, EL}  = match(fun elixir_expand:expand/2, Left, E),
  {ERight, ER} = elixir_expand:expand(Right, EL),

  case ELeft of
    {Name, _, Atom} when is_atom(Name), is_atom(Atom) ->
      case normalize_rescue(ERight) of
        false -> false;
        Other -> {{in, Meta, [ELeft, Other]}, ER}
      end;
    _ ->
      false
  end;

%% rescue Error => _ in [Error]
expand_rescue(Arg, E) ->
  expand_rescue({in, [], [{'_', [], ?key(E, module)}, Arg]}, E).

normalize_rescue({'_', _, Atom} = N) when is_atom(Atom) -> N;
normalize_rescue(Atom) when is_atom(Atom) -> [Atom];
normalize_rescue(Other) ->
  is_list(Other) andalso lists:all(fun is_atom/1, Other) andalso Other.

%% Expansion helpers

%% Returns a function that expands arguments
%% considering we have at maximum one entry.
expand_one(Meta, Kind, Key, Fun) ->
  fun
    ([_] = Args, E) ->
      Fun(Args, E);
    (_, E) ->
      compile_error(Meta, ?key(E, file),
        "expected one arg for ~ts clauses (->) in ~ts", [Key, Kind])
  end.

%% Expands all -> pairs in a given key keeping the overall vars.
expand_with_export(Meta, Kind, Fun, {Key, Clauses}, Acc, E) when is_list(Clauses) ->
  Transformer = fun(Clause, Vars) ->
    {EClause, EC} = clause(Meta, Kind, Fun, Clause, E),
    {EClause, elixir_env:merge_vars(Vars, ?key(EC, export_vars))}
  end,
  {EClauses, EVars} = lists:mapfoldl(Transformer, Acc, Clauses),
  {{Key, EClauses}, EVars};
expand_with_export(Meta, Kind, _Fun, {Key, _}, _Acc, E) ->
  compile_error(Meta, ?key(E, file), "expected -> clauses for ~ts in ~ts", [Key, Kind]).

%% Expands all -> pairs in a given key but do not keep the overall vars.
expand_without_export(Meta, Kind, Fun, {Key, Clauses}, E) when is_list(Clauses) ->
  Transformer = fun(Clause) ->
    {EClause, _} = clause(Meta, Kind, Fun, Clause, E),
    EClause
  end,
  {Key, lists:map(Transformer, Clauses)};
expand_without_export(Meta, Kind, _Fun, {Key, _}, E) ->
  compile_error(Meta, ?key(E, file), "expected -> clauses for ~ts in ~ts", [Key, Kind]).

assert_at_most_once(_Kind, [], _Count, _Fun) -> ok;
assert_at_most_once(Kind, [{Kind, _} | _], 1, ErrorFun) ->
  ErrorFun(Kind);
assert_at_most_once(Kind, [{Kind, _} | Rest], Count, Fun) ->
  assert_at_most_once(Kind, Rest, Count + 1, Fun);
assert_at_most_once(Kind, [_ | Rest], Count, Fun) ->
  assert_at_most_once(Kind, Rest, Count, Fun).
