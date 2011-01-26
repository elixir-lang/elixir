% Holds all methods required to bootstrap the object model.
% These methods are overwritten by their Elixir version later in Object::Methods.
-module(elixir_object_methods).
-export([mixin/2, proto/2, new/2, name/1, parent/1, mixins/1, protos/1, get_ivar/2, ancestors/1]).
-include("elixir.hrl").

% EXTERNAL API

% TODO Disable .new call on native types
new(#elixir_object{name=Name, protos=Protos} = Self, Args) ->
  Parent = case Name of
    [] -> Self;
    _  -> Name
  end,
  Object = #elixir_object{name=[], parent=Parent, mixins=Protos, protos=[], data=[]},
  Data = elixir_dispatch:dispatch(Object, constructor, Args),
  Object#elixir_object{data=Data}.

mixin(Self, Value) when is_list(Value) -> [mixin(Self, Item) || Item <- Value];
mixin(Self, Value) -> prepend_as(Self, mixins, Value).
proto(Self, Value) when is_list(Value) -> [proto(Self, Item) || Item <- Value];
proto(Self, Value) -> prepend_as(Self, protos, Value).

name(Self)   -> object_name(Self).
parent(Self) -> object_parent(Self).
mixins(Self) ->
  reverse_append(object_mixins(Self), traverse_chain(r_ancestors(Self), [])).
protos(Self) ->
  reverse_append(object_protos(Self), traverse_chain(r_ancestors(Self), [])).

get_ivar(#elixir_object{data=Data}, Name) -> 
  case dict:find(Name, Data) of
    { ok, Value } -> Value;
    error -> []
  end;

get_ivar(Self, Name) -> % Native types do not have instance variables.
  [].

% Returns all ancestors for the given object.
ancestors(Self) ->
  lists:reverse(r_ancestors(Self)).

% INTERNAL API

% TODO Only allow modules to be proto/mixed in.
% TODO Handle native types
prepend_as(#elixir_object{} = Self, Kind, Value) -> 
  Name         = Self#elixir_object.name,
  {def, Table} = Self#elixir_object.data,
  [{_, Data}]  = ets:lookup(Table, Kind),
  List         = Value#elixir_object.protos,

  % If we are adding prototypes and the current name is
  % in the list of protos, this means we are adding a
  % proto to a module and we need to ensure all added modules
  % will come after the module name in the proto list.
  case lists:member(Name, Data) of
    true ->
      { Before, After } = lists:splitwith(fun(X) -> X /= Name end, Data),
      Final = umerge(List, lists:delete(Name, After)),
      Updated = umerge(Before, umerge([Name], Final));
    _ ->
      Updated = umerge(List, Data)
  end,

  ets:insert(Table, {Kind, Updated}).

% Merge two lists taking into account uniqueness. Opposite to
% lists:umerge2, does not require lists to be merged.

umerge(List, Data) ->
  umerge2(lists:reverse(List), Data).
  
umerge2([], Data) ->
  Data;

umerge2([H|T], Data) ->
  case lists:member(H, Data) of
    true  -> New = Data;
    false -> New = [H|Data]
  end,
  umerge(T, New).

% Returns the ancestors chain considering only parents, but in reverse order.
r_ancestors(Object) ->
  r_ancestors(object_parent(Object), []).

r_ancestors([], Acc) ->
  Acc;

r_ancestors(Name, Acc) ->
  r_ancestors(abstract_parent(Name), [Name|Acc]).

% Methods that get values from objects. Argument can either be an
% #elixir_object or an erlang native type.

object_name(#elixir_object{name=Name}) ->
  Name;

object_name(Native) ->
  []. % Native types are instances and has no name.

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

% Method that get values from parents. Argument can either be an atom
% or an #elixir_object.

abstract_parent(#elixir_object{parent=Parent}) ->
  Parent;

abstract_parent(Name) ->
  case proplists:get_value(parent, Name:module_info(attributes)) of
    []   -> [];
    Else -> hd(Else)
  end.

abstract_mixins(#elixir_object{mixins=Mixins}) ->
  Mixins;

abstract_mixins(Name) ->
  proplists:get_value(mixins, Name:module_info(attributes)).

abstract_protos(#elixir_object{protos=Protos}) ->
  Protos;

abstract_protos(Name) ->
  proplists:get_value(protos, Name:module_info(attributes)).

% Methods that traverses the ancestors chain and append.

traverse_chain([], Acc) ->
  Acc;

traverse_chain([H|T], Acc) ->
  traverse_chain(T, reverse_append(abstract_protos(H), Acc)).

reverse_append(List, Acc) ->
  r_append(lists:reverse(List), Acc).

r_append([], Acc) ->
  Acc;

r_append([H|T], Acc) ->
  r_append(T, [H|Acc]).