% Holds implementation for most Module::Behavior methods.
-module(elixir_module_behavior).
-export([is_module/1, mixins/1, data/1, slate_bind/2, bind/3,
  get_ivar/2, set_ivar/3, set_ivars/2, remove_ivar/2, update_ivar/3]).
-include("elixir.hrl").

% Introspection

is_module(#elixir_module__{}) -> true;
is_module(_) -> false.

mixins(#elixir_module__{data=Data}) when is_atom(Data) ->
  try
    ets:lookup_element(Data, mixins, 2)
  catch
    error:badarg -> []
  end;

mixins(#elixir_module__{name=Name}) ->
  Name:'__mixins__'([]);

mixins(Native) -> 
  Name = elixir_dispatch:builtin_mixin(Native),
  Name:'__mixins__'([]).

data(#elixir_slate__{data=Data}) ->
  Data;

data(#elixir_module__{data=Data}) when not is_atom(Data) ->
  Data;

data(#elixir_module__{data=Data}) ->
  try
    ets:lookup_element(Data, data, 2)
  catch
    error:badarg -> orddict:new()
  end;

data(Native) ->
  orddict:new(). % Native types has no data.

%% ivars

get_ivar(Self, Name) when is_atom(Name) ->
  elixir_helpers:orddict_find(Name, data(Self));

get_ivar(Self, Name) ->
  elixir_errors:error({bad_ivar, Name}).

set_ivar(Self, Name, Value) when is_atom(Name) ->
  set_ivar_dict(Self, Name, set_ivar, fun(Dict) -> orddict:store(Name, Value, Dict) end).

set_ivars(Self, Value) ->
  assert_dict_with_atoms(Value),
  set_ivar_dict(Self, elixir, set_ivars, fun(Dict) -> elixir_helpers:orddict_merge(Dict, element(2, Value)) end).

update_ivar(Self, Name, Function) ->
  set_ivar_dict(Self, Name, update_ivar, fun(Dict) -> orddict:update(Name, Function, nil, Dict) end).

remove_ivar(Self, Name) ->
  set_ivar_dict(Self, Name, remove_ivar, fun(Dict) -> orddict:erase(Name, Dict) end).

set_ivar_dict(_, Name, _, _) when not is_atom(Name) ->
  elixir_errors:error({bad_ivar, Name});

set_ivar_dict(#elixir_slate__{data=Dict} = Self, Name, _, Function) ->
  Self#elixir_slate__{data=Function(Dict)};

set_ivar_dict(#elixir_module__{data=Dict} = Self, Name, _, Function) when not is_atom(Dict) ->
  Self#elixir_module__{data=Function(Dict)};

set_ivar_dict(#elixir_module__{data=Data} = Self, Name, _, Function) ->
  Dict = ets:lookup_element(Data, data, 2),
  Object = Self#elixir_module__{data=Function(Dict)},
  ets:insert(Data, { data, Object#elixir_module__.data }),
  Object;

set_ivar_dict(Self, _, Method, _) ->
  builtin_not_allowed(Self, Method).

assert_dict_with_atoms(#elixir_orddict__{struct=Dict} = Object) ->
  case lists:all(fun is_atom/1, orddict:fetch_keys(Dict)) of
    true  -> Dict;
    false ->
      elixir_errors:error({bad_ivars, Object})
  end;

assert_dict_with_atoms(Data) ->
  elixir_errors:error({bad_ivars, Data}).

% Binding

slate_bind(Right, Args) ->
  check_module(Right),
  Module = Right#elixir_module__.name,
  Bound = #elixir_slate__{module=Module},
  bind_callback(Module, Bound, Args).

bind(#elixir_slate__{} = Left, Right, Args) ->
  check_module(Right),
  Module = Right#elixir_module__.name,
  Bound = Left#elixir_slate__{module=Module},
  bind_callback(Module, Bound, Args);

bind(Left, Right, Args) ->
  builtin_not_allowed(Left, '__bind__').

bind_callback(Module, Bound, Args) ->
  Final = apply(Module, '__bound__', [Bound|Args]),
  assert_same(Module, Final),
  Final.

assert_same(Module,   #elixir_slate__{module=Module}) -> [];
assert_same(Expected, Actual) -> elixir_errors:error({bad_binding, {?ELIXIR_EX_MODULE(Expected),Actual}}).

bad_binding_error(Expected, Actual) ->
  FinalExpected = ?ELIXIR_EX_MODULE(Expected),
  FinalActual   = ?ELIXIR_EX_MODULE(Actual),
  elixir_errors:error({bad_binding, {FinalExpected,FinalActual}}).

check_module(#elixir_module__{}) -> [];
check_module(Else) -> elixir_errors:error({not_a_module, Else}).

% Helpers

builtin_not_allowed(Builtin, Reason) ->
  elixir_errors:error({builtin_not_allowed, {Reason, Builtin}}).
