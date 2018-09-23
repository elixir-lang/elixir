%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([match/3, clause/5, def/2, head/2,
         'case'/3, 'receive'/3, 'try'/3, 'cond'/3, with/3,
         format_error/1]).
-import(elixir_errors, [form_error/4, form_warn/4]).
-include("elixir.hrl").

match(Fun, Expr, #{context := match} = E) ->
  Fun(Expr, E);
match(Fun, Expr, #{context := Context, prematch_vars := Prematch, current_vars := Current} = E) ->
  {EExpr, EE} = Fun(Expr, E#{context := match, prematch_vars := Current}),
  {EExpr, EE#{context := Context, prematch_vars := Prematch}}.

def({Meta, Args, Guards, Body}, E) ->
  {EArgs, EA}   = elixir_expand:expand(Args, E#{context := match, prematch_vars := #{}}),
  {EGuards, EG} = guard(Guards, EA#{context := guard, prematch_vars := warn}),
  {EBody, EB}   = elixir_expand:expand(Body, EG#{context := nil}),
  elixir_env:check_unused_vars(EB),
  {Meta, EArgs, EGuards, EBody}.

clause(Meta, Kind, Fun, {'->', ClauseMeta, [_, _]} = Clause, E) when is_function(Fun, 3) ->
  clause(Meta, Kind, fun(X, Acc) -> Fun(ClauseMeta, X, Acc) end, Clause, E);
clause(_Meta, _Kind, Fun, {'->', Meta, [Left, Right]}, E) ->
  {ELeft, EL}  = Fun(Left, E),
  {ERight, ER} = elixir_expand:expand(Right, EL),
  {{'->', Meta, [ELeft, ERight]}, ER};
clause(Meta, Kind, _Fun, _, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {bad_or_missing_clauses, Kind}).

head([{'when', Meta, [_ | _] = All}], E) ->
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
guard(Guard, E) ->
  {EGuard, EG} = elixir_expand:expand(Guard, E),
  warn_zero_length_guard(EGuard, EG),
  {EGuard, EG}.

warn_zero_length_guard({{'.', _, [erlang, Op]}, Meta,
                        [{{'.', _, [erlang, length]}, _, [Arg]}, 0]}, E) when Op == '=='; Op == '>' ->
  Warn =
    case Op of
      '==' -> {zero_list_length_in_guard, Arg};
      '>' -> {positive_list_length_in_guard, Arg}
    end,
  form_warn(Meta, ?key(E, file), ?MODULE, Warn);
warn_zero_length_guard({Op, _, [L, R]}, E) when Op == 'or'; Op == 'and' ->
  warn_zero_length_guard(L, E),
  warn_zero_length_guard(R, E);
warn_zero_length_guard(_, _) ->
  ok.

%% Case

'case'(Meta, [], E) ->
  form_error(Meta, ?key(E, file), elixir_expand, {missing_option, 'case', [do]});
'case'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), elixir_expand, {invalid_args, 'case'});
'case'(Meta, Opts, E) ->
  ok = assert_at_most_once('do', Opts, 0, fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'case', Key})
  end),
  lists:mapfoldl(fun(X, Acc) -> expand_case(Meta, X, Acc) end, E, Opts).

expand_case(Meta, {'do', _} = Do, E) ->
  Fun = expand_one(Meta, 'case', 'do', fun head/2),
  expand_clauses(Meta, 'case', Fun, Do, E);
expand_case(Meta, {Key, _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_option, 'case', Key}).

%% Cond

'cond'(Meta, [], E) ->
  form_error(Meta, ?key(E, file), elixir_expand, {missing_option, 'cond', [do]});
'cond'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), elixir_expand, {invalid_args, 'cond'});
'cond'(Meta, Opts, E) ->
  ok = assert_at_most_once('do', Opts, 0, fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'cond', Key})
  end),
  lists:mapfoldl(fun(X, Acc) -> expand_cond(Meta, X, Acc) end, E, Opts).

expand_cond(Meta, {'do', _} = Do, E) ->
  Fun = expand_one(Meta, 'cond', 'do', fun elixir_expand:expand_args/2),
  expand_clauses(Meta, 'cond', Fun, Do, E);
expand_cond(Meta, {Key, _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_option, 'cond', Key}).

%% Receive

'receive'(Meta, [], E) ->
  form_error(Meta, ?key(E, file), elixir_expand, {missing_option, 'receive', [do, 'after']});
'receive'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), elixir_expand, {invalid_args, 'receive'});
'receive'(Meta, Opts, E) ->
  RaiseError = fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'receive', Key})
  end,
  ok = assert_at_most_once('do', Opts, 0, RaiseError),
  ok = assert_at_most_once('after', Opts, 0, RaiseError),
  lists:mapfoldl(fun(X, Acc) -> expand_receive(Meta, X, Acc) end, E, Opts).

expand_receive(_Meta, {'do', {'__block__', _, []}} = Do, E) ->
  {Do, E};
expand_receive(Meta, {'do', _} = Do, E) ->
  Fun = expand_one(Meta, 'receive', 'do', fun head/2),
  expand_clauses(Meta, 'receive', Fun, Do, E);
expand_receive(Meta, {'after', [_]} = After, E) ->
  Fun = expand_one(Meta, 'receive', 'after', fun elixir_expand:expand_args/2),
  expand_clauses(Meta, 'receive', Fun, After, E);
expand_receive(Meta, {'after', _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, multiple_after_clauses_in_receive);
expand_receive(Meta, {Key, _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_option, 'receive', Key}).

%% With

with(Meta, Args, E) ->
  {Exprs, Opts0} =
    case elixir_utils:split_last(Args) of
      {_, LastArg} = SplitResult when is_list(LastArg) ->
        SplitResult;
      _ ->
        {Args, []}
    end,

  {EExprs, {E1, HasMatch}} = lists:mapfoldl(fun expand_with/2, {E, false}, Exprs),
  {EDo, Opts1, E2} = expand_with_do(Meta, Opts0, E, E1),
  {EOpts, Opts2, E3} = expand_with_else(Meta, Opts1, E2, HasMatch),

  case Opts2 of
    [{Key, _} | _] ->
      form_error(Meta, ?key(E, file), elixir_clauses, {unexpected_option, with, Key});
    [] ->
      ok
  end,

  {{with, Meta, EExprs ++ [[{do, EDo} | EOpts]]}, E3}.

expand_with({'<-', Meta, [{Name, _, Ctx}, _] = Args}, Acc) when is_atom(Name), is_atom(Ctx) ->
  expand_with({'=', Meta, Args}, Acc);
expand_with({'<-', Meta, [Left, Right]}, {E, _HasMatch}) ->
  {ERight, ER} = elixir_expand:expand(Right, E),
  {[ELeft], EL}  = head([Left], E),
  {{'<-', Meta, [ELeft, ERight]}, {elixir_env:mergev(EL, ER), true}};
expand_with(Expr, {E, HasMatch}) ->
  {EExpr, EE} = elixir_expand:expand(Expr, E),
  {EExpr, {EE, HasMatch}}.

expand_with_do(Meta, Opts, E, Acc) ->
  case lists:keytake(do, 1, Opts) of
    {value, {do, Expr}, RestOpts} ->
      {EExpr, EAcc} = elixir_expand:expand(Expr, Acc),
      {EExpr, RestOpts, elixir_env:merge_and_check_unused_vars(E, EAcc)};
    false ->
      form_error(Meta, ?key(E, file), elixir_expand, {missing_option, 'with', [do]})
  end.

expand_with_else(Meta, Opts, E, HasMatch) ->
  case lists:keytake(else, 1, Opts) of
    {value, Pair, RestOpts} ->
      if
        HasMatch -> ok;
        true -> form_warn(Meta, ?key(E, file), ?MODULE, unmatchable_else_in_with)
      end,
      Fun = expand_one(Meta, 'with', 'else', fun head/2),
      {EPair, EE} = expand_clauses(Meta, 'with', Fun, Pair, E),
      {[EPair], RestOpts, EE};
    false ->
      {[], Opts, E}
  end.

%% Try

'try'(Meta, [], E) ->
  form_error(Meta, ?key(E, file), elixir_expand, {missing_option, 'try', [do]});
'try'(Meta, [{do, _}], E) ->
  form_error(Meta, ?key(E, file), elixir_expand, {missing_option, 'try', ['catch', 'rescue', 'after', 'else']});
'try'(Meta, [{do, _}, {else, _}], E) ->
  Kind =
    case lists:keyfind(origin, 1, Meta) of
      {origin, Origin} -> Origin;
      false -> 'try'
    end,
  form_error(Meta, ?key(E, file), ?MODULE, {try_with_only_else_clause, Kind});
'try'(Meta, Opts, E) when not is_list(Opts) ->
  form_error(Meta, ?key(E, file), elixir_expand, {invalid_args, 'try'});
'try'(Meta, Opts, E) ->
  RaiseError = fun(Key) ->
    form_error(Meta, ?key(E, file), ?MODULE, {duplicated_clauses, 'try', Key})
  end,
  ok = assert_at_most_once('do', Opts, 0, RaiseError),
  ok = assert_at_most_once('rescue', Opts, 0, RaiseError),
  ok = assert_at_most_once('catch', Opts, 0, RaiseError),
  ok = assert_at_most_once('else', Opts, 0, RaiseError),
  ok = assert_at_most_once('after', Opts, 0, RaiseError),
  ok = warn_catch_before_rescue(Opts, Meta, E, false),
  lists:mapfoldl(fun(X, Acc) -> expand_try(Meta, X, Acc) end, E, Opts).

expand_try(_Meta, {'do', Expr}, E) ->
  {EExpr, EE} = elixir_expand:expand(Expr, E),
  {{'do', EExpr}, elixir_env:merge_and_check_unused_vars(E, EE)};
expand_try(_Meta, {'after', Expr}, E) ->
  {EExpr, EE} = elixir_expand:expand(Expr, E),
  {{'after', EExpr}, elixir_env:merge_and_check_unused_vars(E, EE)};
expand_try(Meta, {'else', _} = Else, E) ->
  Fun = expand_one(Meta, 'try', 'else', fun head/2),
  expand_clauses(Meta, 'try', Fun, Else, E);
expand_try(Meta, {'catch', _} = Catch, E) ->
  expand_clauses_with_stacktrace(Meta, fun expand_catch/3, Catch, E);
expand_try(Meta, {'rescue', _} = Rescue, E) ->
  expand_clauses_with_stacktrace(Meta, fun expand_rescue/3, Rescue, E);
expand_try(Meta, {Key, _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {unexpected_option, 'try', Key}).

expand_clauses_with_stacktrace(Meta, Fun, Clauses, E) ->
  OldContextualVars = ?key(E, contextual_vars),
  ES = E#{contextual_vars := ['__STACKTRACE__' | OldContextualVars]},
  {Ret, EE} = expand_clauses(Meta, 'try', Fun, Clauses, ES),
  {Ret, EE#{contextual_vars := OldContextualVars}}.

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

%% rescue var in _ => rescue var
expand_rescue({in, _, [{Name, _, VarContext} = Var, {'_', _, UnderscoreContext}]}, E)
    when is_atom(Name), is_atom(VarContext), is_atom(UnderscoreContext) ->
  expand_rescue(Var, E);

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

%% Expands all -> pairs in a given key but do not keep the overall vars.
expand_clauses(Meta, Kind, Fun, Clauses, E) ->
  NewKind =
    case lists:keyfind(origin, 1, Meta) of
      {origin, Origin} -> Origin;
      _ -> Kind
    end,
  expand_clauses_origin(Meta, NewKind, Fun, Clauses, E).

expand_clauses_origin(Meta, Kind, Fun, {Key, Clauses}, E) when is_list(Clauses) ->
  Transformer = fun(Clause, Acc) ->
    {EClause, EAcc} = clause(Meta, {Kind, Key}, Fun, Clause, Acc),
    {EClause, elixir_env:merge_and_check_unused_vars(Acc, EAcc)}
  end,
  {Values, EE} = lists:mapfoldl(Transformer, E, Clauses),
  {{Key, Values}, EE};
expand_clauses_origin(Meta, Kind, _Fun, {Key, _}, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {bad_or_missing_clauses, {Kind, Key}}).

assert_at_most_once(_Kind, [], _Count, _Fun) -> ok;
assert_at_most_once(Kind, [{Kind, _} | _], 1, ErrorFun) ->
  ErrorFun(Kind);
assert_at_most_once(Kind, [{Kind, _} | Rest], Count, Fun) ->
  assert_at_most_once(Kind, Rest, Count + 1, Fun);
assert_at_most_once(Kind, [_ | Rest], Count, Fun) ->
  assert_at_most_once(Kind, Rest, Count, Fun).

warn_catch_before_rescue([], _, _, _) ->
  ok;
warn_catch_before_rescue([{'rescue', _} | _], Meta, E, true) ->
  form_warn(Meta, ?key(E, file), ?MODULE, catch_before_rescue);
warn_catch_before_rescue([{'catch', _} | Rest], Meta, E, _) ->
  warn_catch_before_rescue(Rest, Meta, E, true);
warn_catch_before_rescue([_ | Rest], Meta, E, Found) ->
  warn_catch_before_rescue(Rest, Meta, E, Found).

format_error({bad_or_missing_clauses, {Kind, Key}}) ->
  io_lib:format("expected -> clauses for :~ts in \"~ts\"", [Key, Kind]);
format_error({bad_or_missing_clauses, Kind}) ->
  io_lib:format("expected -> clauses in \"~ts\"", [Kind]);

format_error({duplicated_clauses, Kind, Key}) ->
  io_lib:format("duplicated :~ts clauses given for \"~ts\"", [Key, Kind]);

format_error({unexpected_option, Kind, Option}) ->
  io_lib:format("unexpected option ~ts in \"~ts\"", ['Elixir.Macro':to_string(Option), Kind]);

format_error({wrong_number_of_args_for_clause, Expected, Kind, Key}) ->
  io_lib:format("expected ~ts for :~ts clauses (->) in \"~ts\"", [Expected, Key, Kind]);

format_error(multiple_after_clauses_in_receive) ->
  "expected a single -> clause for :after in \"receive\"";

format_error(invalid_rescue_clause) ->
  "invalid \"rescue\" clause. The clause should match on an alias, a variable "
    "or be in the \"var in [alias]\" format";

format_error(catch_before_rescue) ->
  "\"catch\" should always come after \"rescue\" in try";

format_error({try_with_only_else_clause, Kind}) ->
  io_lib:format("\"else\" can't be used as the only clause in \"~ts\" since it's equivalent to not "
                "having the \"try\" in the first place", [Kind]);

format_error(unmatchable_else_in_with) ->
  "\"else\" clauses will never match because all patterns in \"with\" will always match";

format_error({zero_list_length_in_guard, ListArg}) ->
  Arg = 'Elixir.Macro':to_string(ListArg),
  io_lib:format("do not use \"length(~ts) == 0\" to check if a list is empty since length "
                "always traverses the whole list. Prefer to pattern match on an empty list or "
                "use \"~ts == []\" as a guard", [Arg, Arg]);

format_error({positive_list_length_in_guard, ListArg}) ->
  Arg = 'Elixir.Macro':to_string(ListArg),
  io_lib:format("do not use \"length(~ts) > 0\" to check if a list is not empty since length "
                "always traverses the whole list. Prefer to pattern match on a non-empty list, "
                "such as [_ | _], or use \"~ts != []\" as a guard", [Arg, Arg]).
