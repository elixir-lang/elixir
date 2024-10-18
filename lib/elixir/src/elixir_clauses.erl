%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([parallel_match/4, match/6, clause/6, def/3, head/4,
         'case'/4, 'receive'/4, 'try'/4, 'cond'/4, with/4,
         format_error/1]).
-import(elixir_errors, [file_error/4, file_warn/4]).
-include("elixir.hrl").

%% Deal with parallel matches and loops in variables

parallel_match(Meta, Expr, S, #{context := match} = E) ->
  #elixir_ex{vars={_Read, Write}} = S,
  Matches = unpack_match(Expr, Meta, []),

  {[{_, EHead} | ETail], EWrites, SM, EM} =
    lists:foldl(fun({EMeta, Match}, {AccMatches, AccWrites, SI, EI}) ->
      #elixir_ex{vars={Read, _Write}} = SI,
      {EMatch, SM, EM} = elixir_expand:expand(Match, SI#elixir_ex{vars={Read, #{}}}, EI),
      #elixir_ex{vars={_, EWrite}} = SM,
      {[{EMeta, EMatch} | AccMatches], [EWrite | AccWrites], SM, EM}
    end, {[], [], S, E}, Matches),

  EMatch =
    lists:foldl(fun({EMeta, EMatch}, Acc) ->
      {'=', EMeta, [EMatch, Acc]}
    end, EHead, ETail),

  #elixir_ex{vars={VRead, _}, prematch={PRead, Cycles, PInfo}} = SM,
  {PCycles, PWrites} = store_cycles(EWrites, Cycles, #{}),
  VWrite = (Write /= false) andalso elixir_env:merge_vars(Write, PWrites),
  {EMatch, SM#elixir_ex{vars={VRead, VWrite}, prematch={PRead, PCycles, PInfo}}, EM}.

unpack_match({'=', Meta, [Left, Right]}, _Meta, Acc) ->
  unpack_match(Left, Meta, unpack_match(Right, Meta, Acc));
unpack_match(Node, Meta, Acc) ->
  [{Meta, Node} | Acc].

store_cycles([Write | Writes], {Cycles, SkipList}, Acc) ->
  %% Compute the variables this parallel pattern depends on
  DependsOn = lists:foldl(fun maps:merge/2, Acc, Writes),

  %% For each variable on a sibling, we store it inside the graph (Cycles).
  %% The graph will by definition have at least one degree cycles. We need
  %% to find variables which depend on each other more than once (tagged as
  %% error below) and also all second-degree (or later) cycles. In other
  %% words, take this code:
  %%
  %%     {x = y, x = {:ok, y}} = expr()
  %%
  %% The first parallel match will say we store the following cycle:
  %%
  %%     #{{x,nil} => #{{y,nil} => 1}, {y,nil} => #{{x,nil} => 0}}
  %%
  %% That's why one degree cycles are allowed. However, once we go
  %% over the next parallel pattern, we will have:
  %%
  %%     #{{x,nil} => #{{y,nil} => error}, {y,nil} => #{{x,nil} => error}}
  %%
  AccCycles =
    maps:fold(fun(Pair, _, AccCycles) ->
      maps:update_with(Pair, fun(Current) ->
        maps:merge_with(fun(_, _, _) -> error end, Current, DependsOn)
      end, DependsOn, AccCycles)
    end, Cycles, Write),

  %% The SkipList keeps variables that are seen as defined together by other
  %% nodes. Those must be skipped on the graph traversal, as they will always
  %% contain cycles between them. For example:
  %%
  %%     {{a} = b} = c = expr()
  %%
  %% In the example above, c sees "a" and "b" as defined together and therefore
  %% one should not point to the other when looking for cycles.
  AccSkipList =
    case map_size(DependsOn) > 1 of
      true -> [DependsOn | SkipList];
      false -> SkipList
    end,

  store_cycles(Writes, {AccCycles, AccSkipList}, maps:merge(Acc, Write));
store_cycles([], Cycles, Acc) ->
  {Cycles, Acc}.

validate_cycles({Cycles, SkipList}, Meta, Expr, E) ->
  maps:fold(fun(Current, _DependsOn, Seen) ->
    recur_cycles(Cycles, Current, root, Seen, SkipList, Meta, Expr, E)
  end, #{}, Cycles).

recur_cycles(Cycles, Current, Source, Seen, SkipList, Meta, Expr, E) ->
  case is_map_key(Current, Seen) of
    true ->
      Seen;

    false ->
      case maps:get(Current, Cycles) of
        #{Current := _} ->
          file_error(Meta, E, ?MODULE, {recursive, [Current], Expr});

        DependsOn ->
          maps:fold(fun
            (Key, error, _See) ->
              file_error(Meta, E, ?MODULE, {recursive, [Current, Key], Expr});

            %% Never go back to the node that we came from (as we can always one hop).
            (Key, _, AccSeen) when Key =:= Source ->
              AccSeen;

            (Key, _, AccSeen) ->
              Fun = fun
                (#{Current := _, Key := _}) -> true;
                (#{}) -> false
              end,

              case lists:any(Fun, SkipList) of
                true ->
                  AccSeen;

                false when is_map_key(Key, Seen) ->
                  file_error(Meta, E, ?MODULE, {recursive, [Current | maps:keys(Seen)], Expr});

                false ->
                  recur_cycles(Cycles, Key, Current, AccSeen, SkipList, Meta, Expr, E)
              end
          end, maps:put(Current, true, Seen), DependsOn)
      end
  end.

%% Match

match(Fun, Meta, Expr, AfterS, BeforeS, #{context := nil} = E) ->
  #elixir_ex{vars=Current, unused={_, Counter} = Unused} = AfterS,
  #elixir_ex{vars={Read, _}, prematch=Prematch} = BeforeS,

  CallS = BeforeS#elixir_ex{
    prematch={Read, {#{}, []}, Counter},
    unused=Unused,
    vars=Current
  },

  CallE = E#{context := match},
  {EExpr, SE, EE} = Fun(Expr, CallS, CallE),

  #elixir_ex{
    vars=NewCurrent,
    unused=NewUnused,
    prematch={_, Cycles, _}
  } = SE,

  validate_cycles(Cycles, Meta, {match, Expr}, E),

  EndS = AfterS#elixir_ex{
    prematch=Prematch,
    unused=NewUnused,
    vars=NewCurrent
  },

  EndE = EE#{context := ?key(E, context)},
  {EExpr, EndS, EndE}.

def({Meta, Args, Guards, Body}, S, E) ->
  {EArgs, SA, EA} = elixir_expand:expand_args(Args, S#elixir_ex{prematch={#{}, {#{}, []}, 0}}, E#{context := match}),
  #elixir_ex{prematch={_, Cycles, _}} = SA,
  validate_cycles(Cycles, Meta, {?key(E, function), Args}, E),
  {EGuards, SG, EG} = guard(Guards, SA#elixir_ex{prematch=none}, EA#{context := guard}),
  {EBody, SB, EB} = elixir_expand:expand(Body, SG, EG#{context := nil}),
  elixir_env:check_unused_vars(SB, EB),
  {Meta, EArgs, EGuards, EBody}.

clause(_Meta, _Kind, Fun, {'->', Meta, [Left, Right]}, S, E) ->
  {ELeft, SL, EL}  = case is_function(Fun, 4) of
    true -> Fun(Meta, Left, S, E);
    false -> Fun(Left, S, E)
  end,
  {ERight, SR, ER} = elixir_expand:expand(Right, SL, EL),
  {{'->', Meta, [ELeft, ERight]}, SR, ER};
clause(Meta, Kind, _Fun, _, _, E) ->
  file_error(Meta, E, ?MODULE, {bad_or_missing_clauses, Kind}).

head(Meta, [{'when', WhenMeta, [_ | _] = All}], S, E) ->
  {Args, Guard} = elixir_utils:split_last(All),
  {EArgs, SA, EA} = match(fun elixir_expand:expand_args/3, Meta, Args, S, S, E),
  {EGuard, SG, EG} = guard(Guard, SA, EA#{context := guard}),
  {[{'when', WhenMeta, EArgs ++ [EGuard]}], SG, EG#{context := nil}};
head(Meta, Args, S, E) ->
  match(fun elixir_expand:expand_args/3, Meta, Args, S, S, E).

guard({'when', Meta, [Left, Right]}, S, E) ->
  {ELeft, SL, EL}  = guard(Left, S, E),
  {ERight, SR, ER} = guard(Right, SL, EL),
  {{'when', Meta, [ELeft, ERight]}, SR, ER};
guard(Guard, S, E) ->
  {EGuard, SG, EG} = elixir_expand:expand(Guard, S, E),
  warn_zero_length_guard(EGuard, EG),
  {EGuard, SG, EG}.

warn_zero_length_guard({{'.', _, [erlang, Op]}, Meta,
                        [{{'.', _, [erlang, length]}, _, [Arg]}, 0]}, E) when Op == '=='; Op == '>' ->
  Warn =
    case Op of
      '==' -> {zero_list_length_in_guard, Arg};
      '>' -> {positive_list_length_in_guard, Arg}
    end,
  file_warn(Meta, ?key(E, file), ?MODULE, Warn);
warn_zero_length_guard({Op, _, [L, R]}, E) when Op == 'or'; Op == 'and' ->
  warn_zero_length_guard(L, E),
  warn_zero_length_guard(R, E);
warn_zero_length_guard(_, _) ->
  ok.

%% Case

'case'(Meta, [], _S, E) ->
  file_error(Meta, E, elixir_expand, {missing_option, 'case', [do]});
'case'(Meta, Opts, _S, E) when not is_list(Opts) ->
  file_error(Meta, E, elixir_expand, {invalid_args, 'case'});
'case'(Meta, Opts, S, E) ->
  ok = assert_at_most_once('do', Opts, 0, fun(Key) ->
    file_error(Meta, E, ?MODULE, {duplicated_clauses, 'case', Key})
  end),
  {Case, SA} = lists:mapfoldl(fun(X, SA) -> expand_case(Meta, X, SA, E) end, S, Opts),
  {Case, SA, E}.

expand_case(Meta, {'do', _} = Do, S, E) ->
  Fun = expand_head('case', 'do'),
  expand_clauses(Meta, 'case', Fun, Do, S, E);
expand_case(Meta, {Key, _}, _S, E) ->
  file_error(Meta, E, ?MODULE, {unexpected_option, 'case', Key}).

%% Cond

'cond'(Meta, [], _S, E) ->
  file_error(Meta, E, elixir_expand, {missing_option, 'cond', [do]});
'cond'(Meta, Opts, _S, E) when not is_list(Opts) ->
  file_error(Meta, E, elixir_expand, {invalid_args, 'cond'});
'cond'(Meta, Opts, S, E) ->
  ok = assert_at_most_once('do', Opts, 0, fun(Key) ->
    file_error(Meta, E, ?MODULE, {duplicated_clauses, 'cond', Key})
  end),
  {Cond, SA} = lists:mapfoldl(fun(X, SA) -> expand_cond(Meta, X, SA, E) end, S, Opts),
  {Cond, SA, E}.

expand_cond(Meta, {'do', _} = Do, S, E) ->
  Fun = expand_one(Meta, 'cond', 'do', fun elixir_expand:expand_args/3),
  expand_clauses(Meta, 'cond', Fun, Do, S, E);
expand_cond(Meta, {Key, _}, _S, E) ->
  file_error(Meta, E, ?MODULE, {unexpected_option, 'cond', Key}).

%% Receive

'receive'(Meta, [], _S, E) ->
  file_error(Meta, E, elixir_expand, {missing_option, 'receive', [do, 'after']});
'receive'(Meta, Opts, _S, E) when not is_list(Opts) ->
  file_error(Meta, E, elixir_expand, {invalid_args, 'receive'});
'receive'(Meta, Opts, S, E) ->
  RaiseError = fun(Key) ->
    file_error(Meta, E, ?MODULE, {duplicated_clauses, 'receive', Key})
  end,
  ok = assert_at_most_once('do', Opts, 0, RaiseError),
  ok = assert_at_most_once('after', Opts, 0, RaiseError),
  {Receive, SA} = lists:mapfoldl(fun(X, SA) -> expand_receive(Meta, X, SA, E) end, S, Opts),
  {Receive, SA, E}.

expand_receive(_Meta, {'do', {'__block__', _, []}} = Do, S, _E) ->
  {Do, S};
expand_receive(Meta, {'do', _} = Do, S, E) ->
  Fun = expand_head('receive', 'do'),
  expand_clauses(Meta, 'receive', Fun, Do, S, E);
expand_receive(Meta, {'after', [_]} = After, S, E) ->
  Fun = expand_one(Meta, 'receive', 'after', fun elixir_expand:expand_args/3),
  expand_clauses(Meta, 'receive', Fun, After, S, E);
expand_receive(Meta, {'after', _}, _S, E) ->
  file_error(Meta, E, ?MODULE, multiple_after_clauses_in_receive);
expand_receive(Meta, {Key, _}, _S, E) ->
  file_error(Meta, E, ?MODULE, {unexpected_option, 'receive', Key}).

%% With

with(Meta, Args, S, E) ->
  {Exprs, Opts0} = elixir_utils:split_opts(Args),
  S0 = elixir_env:reset_unused_vars(S),
  {EExprs, {S1, E1, HasMatch}} = lists:mapfoldl(fun expand_with/2, {S0, E, false}, Exprs),
  {EDo, Opts1, S2} = expand_with_do(Meta, Opts0, S, S1, E1),
  {EOpts, Opts2, S3} = expand_with_else(Meta, Opts1, S2, E, HasMatch),

  case Opts2 of
    [{Key, _} | _] ->
      file_error(Meta, E, elixir_clauses, {unexpected_option, with, Key});
    [] ->
      ok
  end,

  {{with, Meta, EExprs ++ [[{do, EDo} | EOpts]]}, S3, E}.

expand_with({'<-', Meta, [Left, Right]}, {S, E, HasMatch}) ->
  {ERight, SR, ER} = elixir_expand:expand(Right, S, E),
  SM = elixir_env:reset_read(SR, S),
  {[ELeft], SL, EL} = head(Meta, [Left], SM, ER),
  NewHasMatch =
    case ELeft of
      {Var, _, Ctx} when is_atom(Var), is_atom(Ctx) -> HasMatch;
      _ -> true
    end,
  {{'<-', Meta, [ELeft, ERight]}, {SL, EL, NewHasMatch}};
expand_with(Expr, {S, E, HasMatch}) ->
  {EExpr, SE, EE} = elixir_expand:expand(Expr, S, E),
  {EExpr, {SE, EE, HasMatch}}.

expand_with_do(Meta, Opts, S, Acc, E) ->
  case lists:keytake(do, 1, Opts) of
    {value, {do, Expr}, RestOpts} ->
      {EExpr, SAcc, EAcc} = elixir_expand:expand(Expr, Acc, E),
      {EExpr, RestOpts, elixir_env:merge_and_check_unused_vars(SAcc, S, EAcc)};
    false ->
      file_error(Meta, E, elixir_expand, {missing_option, 'with', [do]})
  end.

expand_with_else(Meta, Opts, S, E, HasMatch) ->
  case lists:keytake('else', 1, Opts) of
    {value, Pair, RestOpts} ->
      if
        HasMatch -> ok;
        true -> file_warn(Meta, ?key(E, file), ?MODULE, unmatchable_else_in_with)
      end,
      Fun = expand_head('with', 'else'),
      {EPair, SE} = expand_clauses(Meta, 'with', Fun, Pair, S, E),
      {[EPair], RestOpts, SE};
    false ->
      {[], Opts, S}
  end.

%% Try

'try'(Meta, [], _S, E) ->
  file_error(Meta, E, elixir_expand, {missing_option, 'try', [do]});
'try'(Meta, [{do, _}], _S, E) ->
  file_error(Meta, E, elixir_expand, {missing_option, 'try', ['catch', 'rescue', 'after']});
'try'(Meta, Opts, _S, E) when not is_list(Opts) ->
  file_error(Meta, E, elixir_expand, {invalid_args, 'try'});
'try'(Meta, Opts, S, E) ->
  % TODO: Make this an error on v2.0
  case Opts of
    [{do, _}, {'else', _}] ->
      file_warn(Meta, ?key(E, file), ?MODULE, {try_with_only_else_clause, origin(Meta, 'try')});
    _ ->
      ok
  end,
  RaiseError = fun(Key) ->
    file_error(Meta, E, ?MODULE, {duplicated_clauses, 'try', Key})
  end,
  ok = assert_at_most_once('do', Opts, 0, RaiseError),
  ok = assert_at_most_once('rescue', Opts, 0, RaiseError),
  ok = assert_at_most_once('catch', Opts, 0, RaiseError),
  ok = assert_at_most_once('else', Opts, 0, RaiseError),
  ok = assert_at_most_once('after', Opts, 0, RaiseError),
  ok = warn_catch_before_rescue(Opts, Meta, E, false),
  {Try, SA} = lists:mapfoldl(fun(X, SA) -> expand_try(Meta, X, SA, E) end, S, Opts),
  {Try, SA, E}.

expand_try(_Meta, {'do', Expr}, S, E) ->
  {EExpr, SE, EE} = elixir_expand:expand(Expr, elixir_env:reset_unused_vars(S), E),
  {{'do', EExpr}, elixir_env:merge_and_check_unused_vars(SE, S, EE)};
expand_try(_Meta, {'after', Expr}, S, E) ->
  {EExpr, SE, EE} = elixir_expand:expand(Expr, elixir_env:reset_unused_vars(S), E),
  {{'after', EExpr}, elixir_env:merge_and_check_unused_vars(SE, S, EE)};
expand_try(Meta, {'else', _} = Else, S, E) ->
  Fun = expand_head('try', 'else'),
  expand_clauses(Meta, 'try', Fun, Else, S, E);
expand_try(Meta, {'catch', _} = Catch, S, E) ->
  expand_clauses_with_stacktrace(Meta, fun expand_catch/4, Catch, S, E);
expand_try(Meta, {'rescue', _} = Rescue, S, E) ->
  expand_clauses_with_stacktrace(Meta, fun expand_rescue/4, Rescue, S, E);
expand_try(Meta, {Key, _}, _S, E) ->
  file_error(Meta, E, ?MODULE, {unexpected_option, 'try', Key}).

expand_clauses_with_stacktrace(Meta, Fun, Clauses, S, E) ->
  OldStacktrace = S#elixir_ex.stacktrace,
  SS = S#elixir_ex{stacktrace=true},
  {Ret, SE} = expand_clauses(Meta, 'try', Fun, Clauses, SS, E),
  {Ret, SE#elixir_ex{stacktrace=OldStacktrace}}.

expand_catch(Meta, [{'when', _, [_, _, _, _ | _]}], _, E) ->
  Error = {wrong_number_of_args_for_clause, "one or two args", origin(Meta, 'try'), 'catch'},
  file_error(Meta, E, ?MODULE, Error);
expand_catch(Meta, [_] = Args, S, E) ->
  head(Meta, Args, S, E);
expand_catch(Meta, [_, _] = Args, S, E) ->
  head(Meta, Args, S, E);
expand_catch(Meta, _, _, E) ->
  Error = {wrong_number_of_args_for_clause, "one or two args", origin(Meta, 'try'), 'catch'},
  file_error(Meta, E, ?MODULE, Error).

expand_rescue(Meta, [Arg], S, E) ->
  case expand_rescue(Arg, S, E) of
    {EArg, SA, EA} ->
      {[EArg], SA, EA};
    false ->
      file_error(Meta, E, ?MODULE, {invalid_rescue_clause, Arg})
  end;
expand_rescue(Meta, _, _, E) ->
  Error = {wrong_number_of_args_for_clause, "one argument", origin(Meta, 'try'), 'rescue'},
  file_error(Meta, E, ?MODULE, Error).

%% rescue var
expand_rescue({Name, Meta, Atom} = Var, S, E) when is_atom(Name), is_atom(Atom) ->
  match(fun elixir_expand:expand/3, Meta, Var, S, S, E);

%% rescue Alias => _ in [Alias]
expand_rescue({'__aliases__', _, [_ | _]} = Alias, S, E) ->
  expand_rescue({in, [], [{'_', [], ?key(E, module)}, Alias]}, S, E);

%% rescue var in _
expand_rescue({in, _, [{Name, Meta, VarContext} = Var, {'_', _, UnderscoreContext}]}, S, E)
    when is_atom(Name), is_atom(VarContext), is_atom(UnderscoreContext) ->
  match(fun elixir_expand:expand/3, Meta, Var, S, S, E);

%% rescue var in (list() or atom())
expand_rescue({in, Meta, [Left, Right]}, S, E) ->
  {ELeft, SL, EL}  = match(fun elixir_expand:expand/3, Meta, Left, S, S, E),
  {ERight, SR, ER} = elixir_expand:expand(Right, SL, EL),

  case ELeft of
    {Name, _, Atom} when is_atom(Name), is_atom(Atom) ->
      case normalize_rescue(ERight) of
        false -> false;
        Other -> {{in, Meta, [ELeft, Other]}, SR, ER}
      end;
    _ ->
      false
  end;

%% rescue expr() => rescue expanded_expr()
expand_rescue({_, Meta, _} = Arg, S, E) ->
  case 'Elixir.Macro':expand_once(Arg, E#{line := ?line(Meta)}) of
    Arg -> false;
    NewArg -> expand_rescue(NewArg, S, E)
  end;

%% rescue list() or atom() => _ in (list() or atom())
expand_rescue(Arg, S, E) ->
  expand_rescue({in, [], [{'_', [], ?key(E, module)}, Arg]}, S, E).

normalize_rescue(Atom) when is_atom(Atom) ->
  [Atom];
normalize_rescue(Other) ->
  is_list(Other) andalso lists:all(fun is_atom/1, Other) andalso Other.

%% Expansion helpers

expand_head(Kind, Key) ->
  fun
    (Meta, [{'when', _, [_, _, _ | _]}], _, E) ->
      file_error(Meta, E, ?MODULE, {wrong_number_of_args_for_clause, "one argument", Kind, Key});
    (Meta, [_] = Args, S, E) ->
      head(Meta, Args, S, E);
    (Meta, _, _, E) ->
      file_error(Meta, E, ?MODULE, {wrong_number_of_args_for_clause, "one argument", Kind, Key})
  end.

%% Returns a function that expands arguments
%% considering we have at maximum one entry.
expand_one(Meta, Kind, Key, Fun) ->
  fun
    ([_] = Args, S, E) ->
      Fun(Args, S, E);
    (_, _, E) ->
      file_error(Meta, E, ?MODULE, {wrong_number_of_args_for_clause, "one argument", Kind, Key})
  end.

%% Expands all -> pairs in a given key but do not keep the overall vars.
expand_clauses(Meta, Kind, Fun, Clauses, S, E) ->
  NewKind = origin(Meta, Kind),
  expand_clauses_origin(Meta, NewKind, Fun, Clauses, S, E).

expand_clauses_origin(Meta, Kind, Fun, {Key, [_ | _] = Clauses}, S, E) ->
  Transformer = fun(Clause, SA) ->
    {EClause, SAcc, EAcc} =
      clause(Meta, {Kind, Key}, Fun, Clause, elixir_env:reset_unused_vars(SA), E),

    {EClause, elixir_env:merge_and_check_unused_vars(SAcc, SA, EAcc)}
  end,
  {Values, SE} = lists:mapfoldl(Transformer, S, Clauses),
  {{Key, Values}, SE};
expand_clauses_origin(Meta, Kind, _Fun, {Key, _}, _, E) ->
  file_error(Meta, E, ?MODULE, {bad_or_missing_clauses, {Kind, Key}}).

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
  file_warn(Meta, ?key(E, file), ?MODULE, {catch_before_rescue, origin(Meta, 'try')});
warn_catch_before_rescue([{'catch', _} | Rest], Meta, E, _) ->
  warn_catch_before_rescue(Rest, Meta, E, true);
warn_catch_before_rescue([_ | Rest], Meta, E, Found) ->
  warn_catch_before_rescue(Rest, Meta, E, Found).

origin(Meta, Default) ->
  case lists:keyfind(origin, 1, Meta) of
    {origin, Origin} -> Origin;
    false -> Default
  end.

format_error({recursive, Vars, TypeExpr}) ->
  Code =
    case TypeExpr of
      {match, Expr} -> 'Elixir.Macro':to_string(Expr);
      {{Name, _Arity}, Args} -> 'Elixir.Macro':to_string({Name, [], Args})
    end,

  Message =
    case lists:map(fun({Name, Context}) -> elixir_utils:var_info(Name, Context) end, lists:sort(Vars)) of
      [Var] ->
        io_lib:format("the variable ~ts is defined in function of itself", [Var]);
      [Var1, Var2] ->
        io_lib:format("the variable ~ts is defined recursively in function of ~ts", [Var1, Var2]);
      [Head | Tail] ->
        List = lists:foldl(fun(X, Acc) -> [Acc, $,, $\s, X] end, Head, Tail),
        io_lib:format("the following variables form a cycle: ~ts", [List])
    end,

  io_lib:format(
    "recursive variable definition in patterns:~n~n    ~ts~n~n~ts",
    [Code, Message]
  );

format_error({bad_or_missing_clauses, {Kind, Key}}) ->
  io_lib:format("expected -> clauses for :~ts in \"~ts\"", [Key, Kind]);
format_error({bad_or_missing_clauses, Kind}) ->
  io_lib:format("expected -> clauses in \"~ts\"", [Kind]);

format_error({duplicated_clauses, Kind, Key}) ->
  io_lib:format("duplicate :~ts clauses given for \"~ts\"", [Key, Kind]);

format_error({unexpected_option, Kind, Option}) ->
  io_lib:format("unexpected option ~ts in \"~ts\"", ['Elixir.Macro':to_string(Option), Kind]);

format_error({wrong_number_of_args_for_clause, Expected, Kind, Key}) ->
  io_lib:format("expected ~ts for :~ts clauses (->) in \"~ts\"", [Expected, Key, Kind]);

format_error(multiple_after_clauses_in_receive) ->
  "expected a single -> clause for :after in \"receive\"";

format_error({invalid_rescue_clause, Arg}) ->
  io_lib:format(
    "invalid \"rescue\" clause. The clause should match on an alias, a variable "
    "or be in the \"var in [alias]\" format. Got: ~ts",
    ['Elixir.Macro':to_string(Arg)]
  );

format_error({catch_before_rescue, Origin}) ->
  io_lib:format("\"catch\" should always come after \"rescue\" in ~ts", [Origin]);

format_error({try_with_only_else_clause, Origin}) ->
  io_lib:format("\"else\" shouldn't be used as the only clause in \"~ts\", use \"case\" instead",
                [Origin]);

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
