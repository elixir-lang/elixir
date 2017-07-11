%% Convenience functions used to manipulate scope and its variables.
-module(elixir_erl_var).
-export([translate/4, build/2, assign/4,
  load_binding/2, dump_binding/2,
  mergev/2, mergec/2, merge_vars/2, merge_opt_vars/2,
  warn_unsafe_var/4, format_error/1
]).
-include("elixir.hrl").

%% VAR HANDLING

translate(Meta, Name, Kind, S) when is_atom(Kind); is_integer(Kind) ->
  Tuple = {Name, Kind},

  {Current, Safe} =
    case maps:find(Tuple, S#elixir_erl.vars) of
      {ok, {VarC, _, VarS}} -> {VarC, VarS};
      error -> {nil, true}
    end,

  case S#elixir_erl.context of
    match ->
      Previous =
        case maps:find(Tuple, S#elixir_erl.backup_vars) of
          {ok, {BackupVarC, _, _}} -> BackupVarC;
          error -> nil
        end,

      if
        Current /= nil, Current /= Previous ->
          {{var, ?ann(Meta), Current}, S};
        true ->
          assign(Meta, Name, Kind, S)
      end;
    _  when Current /= nil ->
      warn_unsafe_var(Meta, S#elixir_erl.file, Name, Safe),
      {{var, ?ann(Meta), Current}, S}
  end.

assign(Meta, Name, Kind, S) ->
  Tuple = {Name, Kind},

  %% We attempt to give vars a nice name because we
  %% still use the unused vars warnings from erl_lint.
  %%
  %% Once we move the warning to Elixir compiler, we
  %% can name vars as _@COUNTER.
  {NewVar, Counter, NS} =
    if
      Kind /= nil ->
        build('_', S);
      true ->
        build(Name, S)
    end,

  FS = NS#elixir_erl{
    vars=maps:put(Tuple, {NewVar, Counter, true}, S#elixir_erl.vars),
    export_vars=case S#elixir_erl.export_vars of
      nil -> nil;
      EV  -> maps:put(Tuple, {NewVar, Counter, true}, EV)
    end
  },

  {{var, ?ann(Meta), NewVar}, FS}.

build(Key, #elixir_erl{counter=Counter} = S) ->
  Cnt =
    case maps:find(Key, Counter) of
      {ok, Val} -> Val + 1;
      error -> 1
    end,
  {list_to_atom(atom_to_list(Key) ++ "@" ++ integer_to_list(Cnt)),
   Cnt,
   S#elixir_erl{counter=maps:put(Key, Cnt, Counter)}}.

warn_unsafe_var(Meta, File, Name, Safe) ->
  case (not Safe) andalso (lists:keyfind(generated, 1, Meta) /= {generated, true}) of
    true ->
      elixir_errors:form_warn(Meta, File, ?MODULE, {unsafe_var, Name});
    false ->
      ok
  end.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on
%% the second with their variables merged.

mergev(S1, S2) ->
  S2#elixir_erl{
    vars=merge_vars(S1#elixir_erl.vars, S2#elixir_erl.vars),
    export_vars=merge_opt_vars(S1#elixir_erl.export_vars, S2#elixir_erl.export_vars)
 }.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.

mergec(S1, S2) ->
  S1#elixir_erl{
    counter=S2#elixir_erl.counter,
    caller=S2#elixir_erl.caller
 }.

%% Mergers.

merge_vars(V, V) -> V;
merge_vars(V1, V2) ->
  merge_maps(fun var_merger/3, V1, V2).

merge_opt_vars(nil, _C2) -> nil;
merge_opt_vars(_C1, nil) -> nil;
merge_opt_vars(C, C)     -> C;
merge_opt_vars(C1, C2)   ->
  merge_maps(fun var_merger/3, C1, C2).

var_merger(_Var, {_, V1, _} = K1, {_, V2, _}) when V1 > V2 -> K1;
var_merger(_Var, _K1, K2) -> K2.

merge_maps(Fun, Map1, Map2) ->
  maps:fold(fun(K, V2, Acc) ->
    V =
      case maps:find(K, Acc) of
        {ok, V1} -> Fun(K, V1, V2);
        error -> V2
      end,
    maps:put(K, V, Acc)
  end, Map1, Map2).

%% BINDINGS

load_binding(Binding, Scope) ->
  {NewBinding, NewKeys, NewVars, NewCounter} = load_binding(Binding, [], [], #{}, 0),
  {NewBinding, NewKeys, Scope#elixir_erl{
    vars=NewVars,
    counter=#{'_' => NewCounter}
 }}.

load_binding([{Key, Value} | T], Binding, Keys, Vars, Counter) ->
  Actual = case Key of
    {_Name, _Kind} -> Key;
    Name when is_atom(Name) -> {Name, nil}
  end,
  InternalName = list_to_atom("_@" ++ integer_to_list(Counter)),
  load_binding(T,
    orddict:store(InternalName, Value, Binding),
    ordsets:add_element(Actual, Keys),
    maps:put(Actual, {InternalName, 0, true}, Vars), Counter + 1);
load_binding([], Binding, Keys, Vars, Counter) ->
  {Binding, Keys, Vars, Counter}.

dump_binding(Binding, #elixir_erl{vars=Vars}) ->
  maps:fold(fun
    ({Var, Kind} = Key, {InternalName, _, _}, Acc) when is_atom(Kind) ->
      Actual = case Kind of
        nil -> Var;
        _   -> Key
      end,

      Value = case orddict:find(InternalName, Binding) of
        {ok, V} -> V;
        error -> nil
      end,

      orddict:store(Actual, Value, Acc);
    (_, _, Acc) ->
      Acc
  end, [], Vars).

%% Errors

format_error({unsafe_var, Name}) ->
  io_lib:format("the variable \"~ts\" is unsafe as it has been set inside "
                "one of: case, cond, receive, if, and, or, &&, ||. "
                "Please explicitly return the variable value instead. For example:\n\n"
                "    case integer do\n"
                "      1 -> atom = :one\n"
                "      2 -> atom = :two\n"
                "    end\n\n"
                "should be written as\n\n"
                "    atom =\n"
                "      case integer do\n"
                "        1 -> :one\n"
                "        2 -> :two\n"
                "      end\n\n"
                "Unsafe variable found at:", [Name]).
