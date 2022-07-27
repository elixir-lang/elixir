%% Convenience functions used to manipulate scope and its variables.
-module(elixir_erl_var).
-export([
  translate/4, assign/2, build/2,
  load_binding/1, dump_binding/3,
  from_env/1, from_env/2
]).
-include("elixir.hrl").

%% VAR HANDLING

translate(Meta, '_', _Kind, S) ->
  {{var, ?ann(Meta), '_'}, S};

translate(Meta, Name, Kind, #elixir_erl{var_names=VarNames} = S) ->
  {version, Version} = lists:keyfind(version, 1, Meta),

  case VarNames of
    #{Version :=  ErlName} -> {{var, ?ann(Meta), ErlName}, S};
    #{} when Kind /= nil -> assign(Meta, '_', Version, S);
    #{} -> assign(Meta, Name, Version, S)
  end.

assign(Meta, #elixir_erl{var_names=VarNames} = S) ->
  Version = -(map_size(VarNames)+1),
  ExVar = {var, [{version, Version} | Meta], ?var_context},
  {ErlVar, SV} = assign(Meta, '_', Version, S),
  {ExVar, ErlVar, SV}.

assign(Meta, Name, Version, #elixir_erl{var_names=VarNames} = S) ->
  {NewVar, NS} = build(Name, S),
  NewVarNames = VarNames#{Version => NewVar},
  {{var, ?ann(Meta), NewVar}, NS#elixir_erl{var_names=NewVarNames}}.

build(Key, #elixir_erl{counter=Counter} = S) ->
  Count =
    case Counter of
      #{Key := Val} -> Val + 1;
      _ -> 1
    end,
  {build_name(Key, Count),
   S#elixir_erl{counter=Counter#{Key => Count}}}.

build_name('_', Count) -> list_to_atom("_@" ++ integer_to_list(Count));
build_name(Name, Count) -> list_to_atom("_" ++ atom_to_list(Name) ++ "@" ++ integer_to_list(Count)).

%% BINDINGS

from_env(#{versioned_vars := Read} = Env) ->
  VarsList = to_erl_vars(maps:values(Read), 0),
  {VarsList, from_env(Env, maps:from_list(VarsList))}.

from_env(#{context := Context}, VarsMap) ->
  #elixir_erl{
    context=Context,
    var_names=VarsMap,
    counter=#{'_' => map_size(VarsMap)}
  }.

to_erl_vars([Version | Versions], Counter) ->
  [{Version, to_erl_var(Counter)} | to_erl_vars(Versions, Counter + 1)];
to_erl_vars([], _Counter) ->
  [].

to_erl_var(Counter) ->
  list_to_atom("_@" ++ integer_to_list(Counter)).

load_binding(Binding) ->
  load_binding(Binding, #{}, [], [], 0).

load_binding([Binding | NextBindings], ExVars, ErlVars, Normalized, Counter) ->
  {Pair, Value} = load_pair(Binding),

  case ExVars of
    #{Pair := VarCounter} ->
      ErlVar = to_erl_var(VarCounter),
      load_binding(NextBindings, ExVars, ErlVars, [{ErlVar, Value} | Normalized], Counter);

    #{} ->
      ErlVar = to_erl_var(Counter),

      load_binding(
        NextBindings,
        ExVars#{Pair => Counter},
        [{Counter, ErlVar} | ErlVars],
        [{ErlVar, Value} | Normalized],
        Counter + 1
      )
  end;
load_binding([], ExVars, ErlVars, Normalized, _Counter) ->
  %% TODO: Remove me once we require Erlang/OTP 24+
  %% Also revisit dump_binding below and remove the vars field for simplicity.
  Mod =
    case erlang:system_info(otp_release) >= "24" of
      true -> maps;
      false -> orddict
    end,

  {ExVars, maps:from_list(ErlVars), Mod:from_list(lists:reverse(Normalized))}.

load_pair({Key, Value}) when is_atom(Key) -> {{Key, nil}, Value};
load_pair({Pair, Value}) -> {Pair, Value}.

dump_binding(Binding, #elixir_ex{vars={ExVars, _}}, #elixir_erl{var_names=ErlVars}) ->
  maps:fold(fun
    ({Var, Kind} = Pair, Version, Acc) when is_atom(Kind) ->
      Key = case Kind of
        nil -> Var;
        _ -> Pair
      end,

      ErlName = maps:get(Version, ErlVars),
      Value = find_binding(ErlName, Binding),
      [{Key, Value} | Acc];

    (_, _, Acc) ->
      Acc
  end, [], ExVars).

find_binding(ErlName, Binding = #{}) ->
  case Binding of
    #{ErlName := V} -> V;
    _ -> nil
  end;
find_binding(ErlName, Binding) ->
  case orddict:find(ErlName, Binding) of
    {ok, V} -> V;
    error -> nil
  end.
