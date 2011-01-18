% Holds all methods required to bootstrap the object model.
% These methods are overwritten by their Elixir version later in Object::Methods.
-module(elixir_object_methods).
-export([mixin/2, proto/2, mixins/1, protos/1, ancestors/1, dispatch_chain/1]).
-include("elixir.hrl").

mixin(Self, Value) when is_list(Value) -> [mixin(Self, Item) || Item <- Value];
mixin(Self, Value) -> prepend_as(Self, mixins, Value).
proto(Self, Value) when is_list(Value) -> [proto(Self, Item) || Item <- Value];
proto(Self, Value) -> prepend_as(Self, protos, Value).

mixins(Self) -> Self#elixir_object.mixins.
protos(Self) -> Self#elixir_object.protos.

% TODO Only allow modules to be proto/mixed in.
prepend_as(#elixir_object{data={def, Table}}, Kind, Value) -> 
  [{_, Data}] = ets:lookup(Table, Kind),
  ets:insert(Table, {Kind, lists:append(Value#elixir_object.protos, Data)}).

% Returns all the methods used when dispatching the object.
% It contains all mixins from the parents and the current object protos.
dispatch_chain(#elixir_object{mixins=Mixins} = Self) ->
  lists:append(Mixins, dispatch_chain(r_ancestors(Self), []));

% Account for native types where usually just the parent name is given.
dispatch_chain(Name) ->
  dispatch_chain(r_ancestors(Name), []).

dispatch_chain([], Acc) ->
  Acc;

dispatch_chain([H|T], Acc) ->
  Protos = proplists:get_value(protos, H:module_info(attributes)),
  dispatch_chain(T, lists:append(Protos, Acc)).

% Returns all ancestors for the given object.
ancestors(Self) ->
  lists:reverse(r_ancestors(Self)).

% Returns the ancestors chain considering only parents, but in reverse order.
r_ancestors(#elixir_object{parent=Parent}) ->
  r_ancestors(Parent, []);

% Account for native types where usually just the parent name is given.
r_ancestors(Name) ->
  r_ancestors(Name, []).

r_ancestors([], Acc) ->
  Acc;

r_ancestors(Name, Acc) ->
  case proplists:get_value(parent, Name:module_info(attributes)) of
    []   -> Parent = [];
    Else -> Parent = hd(Else)
  end,
  r_ancestors(Parent, [Name|Acc]).