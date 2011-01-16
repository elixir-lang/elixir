% Holds all methods required to bootstrap the object model.
% These methods are overwritten by their Elixir version later in Object::Methods.
-module(elixir_object_methods).
-export([mixin/2, proto/2]).
-include("elixir.hrl").

mixin(Self, Value) -> prepend_as(Self, mixins, Value).
proto(Self, Value) -> prepend_as(Self, protos, Value).

prepend_as(Self, Kind, Value) -> 
  Table = Self#elixir_object.data,
  [{_, Data}] = ets:lookup(Table, Kind),
  ets:insert(Table, {Kind, [Value|Data]}).