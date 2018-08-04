%% Convenience functions used to manipulate scope and its variables.
-module(elixir_erl_var).
-export([translate/4, build/2, assign/4,
  load_binding/2, dump_binding/2, mergev/2, mergec/2
]).
-include("elixir.hrl").

%% VAR HANDLING

translate(Meta, Name, Kind, S) ->
  Tuple = {Name, Kind},

  Current =
    case S#elixir_erl.vars of
      #{Tuple := {_, VarC}} -> VarC;
      _ -> nil
    end,

  if
    S#elixir_erl.context =:= match ->
      Previous =
        case S#elixir_erl.backup_vars of
          #{Tuple := {_, BackupVarC}} -> BackupVarC;
          _ -> nil
        end,

      if
        Current =/= nil, Current =/= Previous ->
          {{var, ?ann(Meta), Current}, S};
        true ->
          assign(Meta, Name, Kind, S)
      end;
    Current =/= nil ->
      {{var, ?ann(Meta), Current}, S}
  end.

assign(Meta, Name, Kind, S) ->
  Tuple = {Name, Kind},

  {NewVar, Counter, NS} =
    if
      Kind /= nil -> build('_', S);
      true -> build(Name, S)
    end,

  FS = NS#elixir_erl{vars=(S#elixir_erl.vars)#{Tuple => {Counter, NewVar}}},
  {{var, ?ann(Meta), NewVar}, FS}.

build(Key, #elixir_erl{counter=Counter} = S) ->
  Cnt =
    case Counter of
      #{Key := Val} -> Val + 1;
      _ -> 1
    end,
  {list_to_atom("_" ++ atom_to_list(Key) ++ "@" ++ integer_to_list(Cnt)),
   Cnt,
   S#elixir_erl{counter=Counter#{Key => Cnt}}}.

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on
%% the second with their variables merged.

mergev(#elixir_erl{vars=V1}, #elixir_erl{vars=V2} = S2) ->
  if
    V1 =/= V2 -> S2#elixir_erl{vars=merge_vars(V1, V2)};
    true -> S2
  end.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.

mergec(S1, S2) ->
  S1#elixir_erl{
    counter=S2#elixir_erl.counter,
    caller=S2#elixir_erl.caller,
    stacktrace=S2#elixir_erl.stacktrace
  }.

merge_vars(V1, V2) ->
  maps:fold(fun(K, M2, Acc) ->
    case Acc of
      #{K := M1} when M1 >= M2 -> Acc;
      _ -> Acc#{K => M2}
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
