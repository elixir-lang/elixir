%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([match/3, clause/5, def/2, head/2,
         'case'/3, 'receive'/3, 'try'/3, 'cond'/3,
         format_error/1]).
-import(elixir_errors, [form_error/4]).
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
  form_error(Meta, ?key(E, file), ?MODULE, {bad_or_missing_clauses, Kind}).

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
  form_error(Meta, ?key(E, file), ?MODULE, {missing_do, 'case'});
'case'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_args, 'case'});
'case'(Meta, Opts, E) ->
  ok = assert_at_most_once('do', Opts, 0, fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'case', Key})
  end),
  EE = E#{export_vars := []},
  {EClauses, EVars} = lists:mapfoldl(fun(X, Acc) -> expand_case(Meta, X, Acc, EE) end, [], Opts),
  {EClauses, elixir_env:mergev(EVars, E)}.

expand_case(Meta, {'do', _} = Do, Acc, E) ->
  Fun = expand_one(Meta, 'case', 'do', fun head/2),
  expand_with_export(Meta, 'case', Fun, Do, Acc, E);
expand_case(Meta, {Key, _}, _Acc, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_keyword, 'case', Key}).

%% Cond

'cond'(Meta, [], E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {missing_do, 'cond'});
'cond'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_args, 'cond'});
'cond'(Meta, Opts, E) ->
  ok = assert_at_most_once('do', Opts, 0, fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'cond', Key})
  end),
  EE = E#{export_vars := []},
  {EClauses, EVars} = lists:mapfoldl(fun(X, Acc) -> expand_cond(Meta, X, Acc, EE) end, [], Opts),
  {EClauses, elixir_env:mergev(EVars, E)}.

expand_cond(Meta, {'do', _} = Do, Acc, E) ->
  Fun = expand_one(Meta, 'cond', 'do', fun elixir_expand:expand_args/2),
  expand_with_export(Meta, 'cond', Fun, Do, Acc, E);
expand_cond(Meta, {Key, _}, _Acc, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_keyword, 'cond', Key}).

%% Receive

'receive'(Meta, [], E) ->
  form_error(Meta, ?key(E, file), ?MODULE, missing_do_or_after_in_receive);
'receive'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_args, 'receive'});
'receive'(Meta, Opts, E) ->
  RaiseError = fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'receive', Key})
  end,
  ok = assert_at_most_once('do', Opts, 0, RaiseError),
  ok = assert_at_most_once('after', Opts, 0, RaiseError),
  EE = E#{export_vars := []},
  {EClauses, EVars} = lists:mapfoldl(fun(X, Acc) -> expand_receive(Meta, X, Acc, EE) end, [], Opts),
  {EClauses, elixir_env:mergev(EVars, E)}.

expand_receive(_Meta, {'do', nil} = Do, Acc, _E) ->
  {Do, Acc};
expand_receive(Meta, {'do', _} = Do, Acc, E) ->
  Fun = expand_one(Meta, 'receive', 'do', fun head/2),
  expand_with_export(Meta, 'receive', Fun, Do, Acc, E);
expand_receive(Meta, {'after', [_]} = After, Acc, E) ->
  Fun = expand_one(Meta, 'receive', 'after', fun elixir_expand:expand_args/2),
  expand_with_export(Meta, 'receive', Fun, After, Acc, E);
expand_receive(Meta, {'after', _}, _Acc, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, multiple_after_clauses_in_receive);
expand_receive(Meta, {Key, _}, _Acc, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_keyword, 'receive', Key}).

%% Try

'try'(Meta, [], E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {missing_do, 'try'});
'try'(Meta, [{do, _}], E) ->
  form_error(Meta, ?key(E, file), ?MODULE, missing_keyword_in_try);
'try'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_args, 'try'});
'try'(Meta, Opts, E) ->
  RaiseError = fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'try', Key})
  end,
  ok = assert_at_most_once('do', Opts, 0, RaiseError),
  ok = assert_at_most_once('rescue', Opts, 0, RaiseError),
  ok = assert_at_most_once('catch', Opts, 0, RaiseError),
  ok = assert_at_most_once('else', Opts, 0, RaiseError),
  ok = assert_at_most_once('after', Opts, 0, RaiseError),
  {lists:map(fun(X) -> expand_try(Meta, X, E) end, Opts), E}.

expand_try(_Meta, {'do', Expr}, E) ->
  {EExpr, _} = elixir_expand:expand(Expr, E),
  {'do', EExpr};
expand_try(_Meta, {'after', Expr}, E) ->
  {EExpr, _} = elixir_expand:expand(Expr, E),
  {'after', EExpr};
expand_try(Meta, {'else', _} = Else, E) ->
  Fun = expand_one(Meta, 'try', 'else', fun head/2),
  expand_without_export(Meta, 'try', Fun, Else, E);
expand_try(Meta, {'catch', _} = Catch, E) ->
  expand_without_export(Meta, 'try', fun expand_catch/3, Catch, E);
expand_try(Meta, {'rescue', _} = Rescue, E) ->
  expand_without_export(Meta, 'try', fun expand_rescue/3, Rescue, E);
expand_try(Meta, {Key, _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_keyword, 'try', Key}).

expand_catch(_Meta, [_] = Args, E) ->
  head(Args, E);
expand_catch(_Meta, [_, _] = Args, E) ->
  head(Args, E);
expand_catch(Meta, _, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {wrong_number_of_args_for_clause, "one or two args", 'try', 'catch'}).

expand_rescue(Meta, [Arg], E) ->
  case expand_rescue(Arg, E) of
    {EArg, EA} ->
      {[EArg], EA};
    false ->
      form_error(Meta, ?key(E, file), ?MODULE, invalid_rescue_clause)
  end;
expand_rescue(Meta, _, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {wrong_number_of_args_for_clause, "one arg", 'try', 'rescue'}).

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
      form_error(Meta, ?key(E, file), ?MODULE, {wrong_number_of_args_for_clause, "one arg", Kind, Key})
  end.

%% Expands all -> pairs in a given key keeping the overall vars.
expand_with_export(Meta, Kind, Fun, {Key, Clauses}, Acc, E) when is_list(Clauses) ->
  Transformer = fun(Clause, Vars) ->
    {EClause, EC} = clause(Meta, {Kind, Key}, Fun, Clause, E),
    {EClause, elixir_env:merge_vars(Vars, ?key(EC, export_vars))}
  end,
  {EClauses, EVars} = lists:mapfoldl(Transformer, Acc, Clauses),
  {{Key, EClauses}, EVars};
expand_with_export(Meta, Kind, _Fun, {Key, _}, _Acc, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {bad_or_missing_clauses, {Kind, Key}}).

%% Expands all -> pairs in a given key but do not keep the overall vars.
expand_without_export(Meta, Kind, Fun, {Key, Clauses}, E) when is_list(Clauses) ->
  Transformer = fun(Clause) ->
    {EClause, _} = clause(Meta, {Kind, Key}, Fun, Clause, E),
    EClause
  end,
  {Key, lists:map(Transformer, Clauses)};
expand_without_export(Meta, Kind, _Fun, {Key, _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {bad_or_missing_clauses, {Kind, Key}}).

assert_at_most_once(_Kind, [], _Count, _Fun) -> ok;
assert_at_most_once(Kind, [{Kind, _} | _], 1, ErrorFun) ->
  ErrorFun(Kind);
assert_at_most_once(Kind, [{Kind, _} | Rest], Count, Fun) ->
  assert_at_most_once(Kind, Rest, Count + 1, Fun);
assert_at_most_once(Kind, [_ | Rest], Count, Fun) ->
  assert_at_most_once(Kind, Rest, Count, Fun).

format_error({bad_or_missing_clauses, {Kind, Key}}) ->
  io_lib:format("expected -> clauses for ~ts in ~ts", [Key, Kind]);
format_error({bad_or_missing_clauses, Kind}) ->
  io_lib:format("expected -> clauses in ~ts", [Kind]);
format_error({missing_do, Kind}) ->
  io_lib:format("missing do keyword in ~ts", [Kind]);
format_error({invalid_args, Kind}) ->
  io_lib:format("invalid arguments for ~ts", [Kind]);
format_error({duplicated_clauses, Kind, Key}) ->
  io_lib:format("duplicated ~ts clauses given for ~ts", [Key, Kind]);
format_error({unexpected_keyword, Kind, Keyword}) ->
  io_lib:format("unexpected keyword ~ts in ~ts", [Keyword, Kind]);
format_error({wrong_number_of_args_for_clause, Expected, Kind, Key}) ->
  io_lib:format("expected ~ts for ~ts clauses (->) in ~ts", [Expected, Key, Kind]);
format_error(multiple_after_clauses_in_receive) ->
  "expected a single -> clause for after in receive";
format_error(missing_do_or_after_in_receive) ->
  "missing do or after keyword in receive";
format_error(missing_keyword_in_try) ->
  "missing catch/rescue/after/else keyword in try";
format_error(invalid_rescue_clause) ->
  "invalid rescue clause. The clause should match on an alias, a variable "
    "or be in the \"var in [alias]\" format".
