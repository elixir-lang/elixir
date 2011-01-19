% Holds all methods required to bootstrap the object model.
% These methods are overwritten by their Elixir version later in Object::Methods.
-module(elixir_object_methods).
-export([mixin/2, proto/2, mixins/1, protos/1, ancestors/1, dispatch_chain/1]).
-include("elixir.hrl").

mixin(Self, Value) when is_list(Value) -> [mixin(Self, Item) || Item <- Value];
mixin(Self, Value) -> prepend_as(Self, mixins, Value).
proto(Self, Value) when is_list(Value) -> [proto(Self, Item) || Item <- Value];
proto(Self, Value) -> prepend_as(Self, protos, Value).

mixins(Self) -> object_mixins(Self).
protos(Self) -> object_protos(Self).

% TODO Only allow modules to be proto/mixed in.
prepend_as(#elixir_object{data={def, Table}}, Kind, Value) -> 
  [{_, Data}] = ets:lookup(Table, Kind),
  ets:insert(Table, {Kind, lists:append(Value#elixir_object.protos, Data)}).

% Returns all the methods used when dispatching the object.
% It contains all mixins from the parents and the current object protos.
dispatch_chain(Object) ->
  lists:append(object_mixins(Object), dispatch_chain(r_ancestors(Object), [])).

dispatch_chain([], Acc) ->
  Acc;

dispatch_chain([H|T], Acc) ->
  dispatch_chain(T, lists:append(?ELIXIR_MOD_PROTOS(H), Acc)).

% Returns all ancestors for the given object.
ancestors(Self) ->
  lists:reverse(r_ancestors(Self)).

% Returns the ancestors chain considering only parents, but in reverse order.
r_ancestors(Object) ->
  r_ancestors(object_parent(Object), []).

r_ancestors([], Acc) ->
  Acc;

r_ancestors(Name, Acc) ->
  r_ancestors(?ELIXIR_MOD_PARENT(Name), [Name|Acc]).

% Methods that get values from objects taking into account native types.

object_parent(#elixir_object{parent=Parent}) ->
  Parent;

object_parent(Native) when is_integer(Native) ->
  'Integer';

object_parent(Native) when is_float(Native) ->
  'Float'.

object_mixins(#elixir_object{mixins=Mixins}) ->
  Mixins;

object_mixins(Native) ->
  []. % Native types has all mixins from parents.

object_protos(#elixir_object{protos=Protos}) ->
  Protos;

object_protos(Native) ->
  []. % Native types has no protos.