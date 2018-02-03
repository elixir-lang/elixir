%% Convenience functions used to manipulate scope and its variables.
-module(elixir_erl_var).
-export([translate/4, build/2, assign/4,
  load_binding/2, dump_binding/2, mergev/2, mergec/2
]).
-include("elixir.hrl").

%% VAR HANDLING

translate(Meta, Name, Kind, S) when is_atom(Kind); is_integer(Kind) ->
  Tuple = {Name, Kind},

  Current =
    case maps:find(Tuple, S#elixir_erl.vars) of
      {ok, {_, VarC}} -> VarC;
      error -> nil
    end,

  case S#elixir_erl.context of
    match ->
      Previous =
        case maps:find(Tuple, S#elixir_erl.backup_vars) of
          {ok, {_, BackupVarC}} -> BackupVarC;
          error -> nil
        end,

      if
        Current /= nil, Current /= Previous ->
          {{var, ?ann(Meta), Current}, S};
        true ->
          assign(Meta, Name, Kind, S)
      end;
    _  when Current /= nil ->
      {{var, ?ann(Meta), Current}, S}
  end.

assign(Meta, Name, Kind, S) ->
  Tuple = {Name, Kind},

  {NewVar, Counter, NS} =
    if
      Kind /= nil ->
        build('_', S);
      true ->
        build(Name, S)
    end,

  FS = NS#elixir_erl{vars=maps:put(Tuple, {Counter, NewVar}, S#elixir_erl.vars)},
  {{var, ?ann(Meta), NewVar}, FS}.

build(Key, #elixir_erl{counter=Counter} = S) ->
  Cnt =
    case maps:find(Key, Counter) of
      {ok, Val} -> Val + 1;
      error -> 1
    end,
  {list_to_atom([$_ | atom_to_list(Key)] ++ "@" ++ integer_to_list(Cnt)),
   Cnt,
   S#elixir_erl{counter=maps:put(Key, Cnt, Counter)}}.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on
%% the second with their variables merged.

mergev(S1, S2) ->
  S2#elixir_erl{vars=merge_vars(S1#elixir_erl.vars, S2#elixir_erl.vars)}.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.

mergec(S1, S2) ->
  S1#elixir_erl{
    counter=S2#elixir_erl.counter,
    caller=S2#elixir_erl.caller,
    stacktrace=S2#elixir_erl.stacktrace
  }.

merge_vars(V, V) -> V;
merge_vars(V1, V2) ->
  maps:fold(fun(K, M2, Acc) ->
    case Acc of
      #{K := M1} when M1 >= M2 -> Acc;
      _ -> maps:put(K, M2, Acc)
    end
  end, V1, V2).

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
    maps:put(Actual, {0, InternalName}, Vars), Counter + 1);
load_binding([], Binding, Keys, Vars, Counter) ->
  {Binding, Keys, Vars, Counter}.

dump_binding(Binding, #elixir_erl{vars=Vars}) ->
  maps:fold(fun
    ({Var, Kind} = Key, {_, InternalName}, Acc) when is_atom(Kind) ->
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
