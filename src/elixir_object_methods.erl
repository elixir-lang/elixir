% Holds all runtime methods required to bootstrap the object model.
% These methods are overwritten by their Elixir version later in Object::Methods.
-module(elixir_object_methods).
-export([mixin/2, proto/2, new/2, name/1, parent/1, mixins/1, protos/1, data/1,
  get_ivar/2, set_ivar/3, ancestors/1, function_catch/1,
  object_parent/1, object_mixins/1, object_protos/1, object_data/1,
  abstract_parent/1, abstract_mixins/1, abstract_protos/1, abstract_data/1]).
-include("elixir.hrl").

% EXTERNAL API

new(#elixir_object__{name=Name, protos=Protos} = Self, Args) ->
  Parent = case Name of
    [] -> Self;
    _  -> Name
  end,
  Object = #elixir_object__{name=[], parent=Parent, mixins=Protos, protos=[], data=[]},
  Data = elixir_dispatch:dispatch(true, Object, constructor, Args),
  Dict = assert_dict_with_atoms(Data),
  Object#elixir_object__{data=Dict};

new(Else, Args) -> builtinnotallowed(Else, new).

mixin(Self, Value) when is_list(Value) -> [mixin(Self, Item) || Item <- Value];
mixin(Self, Value) -> prepend_as(Self, object_mixins(Self), mixin, Value).
proto(Self, Value) when is_list(Value) -> [proto(Self, Item) || Item <- Value];
proto(Self, Value) -> prepend_as(Self, object_protos(Self), proto, Value).

% Reflections

name(Self)      -> object_name(Self).
parent(Self)    -> object_parent(Self).
mixins(Self)    -> apply_chain(object_mixins(Self), traverse_chain(r_ancestors(Self), [])).
protos(Self)    -> apply_chain(object_protos(Self), traverse_chain(r_ancestors(Self), [])).
data(Self)      -> object_data(Self).
ancestors(Self) -> lists:reverse(r_ancestors(Self)).

% Methods available to all objects

function_catch(Function) ->
  catch Function().

%% PROTECTED API

get_ivar(Self, Name) when not is_atom(Name) ->
  elixir_errors:raise(badarg, "instance variable name needs to be an atom, got ~ts", [inspect(Name)]);

get_ivar(#elixir_object__{data=Data}, Name) when is_atom(Data) ->
  Dict = ets:lookup_element(Data, data, 2),
  get_ivar_dict(Name, Dict);

get_ivar(#elixir_object__{data=Dict}, Name) ->
  get_ivar_dict(Name, Dict);

get_ivar(Self, Name) -> % Native types do not have instance variables.
  [].

set_ivar(Self, Name, Value) when not is_atom(Name) ->
  elixir_errors:raise(badarg, "instance variable name needs to be an atom, got ~ts", [inspect(Name)]);

set_ivar(#elixir_object__{data=Data} = Self, Name, Value) when is_atom(Data) ->
  Dict = ets:lookup_element(Data, data, 2),
  Object = set_ivar_dict(Self, Name, Value, Dict),
  ets:insert(Data, { data, Object#elixir_object__.data }),
  Object;

set_ivar(#elixir_object__{data=Dict} = Self, Name, Value) ->
  set_ivar_dict(Self, Name, Value, Dict);

set_ivar(Self, Name, Value) ->
  builtinnotallowed(Self, set_ivar).

% HELPERS

get_ivar_dict(Name, Data) ->
  case orddict:find(Name, Data) of
    { ok, Value } -> Value;
    error -> []
  end.

set_ivar_dict(Self, Name, Value, Dict) ->
  Self#elixir_object__{data=orddict:store(Name, Value, Dict)}.

assert_dict_with_atoms(#elixir_orddict__{struct=Dict} = Object) ->
  case lists:all(fun is_atom/1, orddict:fetch_keys(Dict)) of
    true  -> Dict;
    false ->
      elixir_errors:raise(badarg, "constructor needs to return a OrderedDict with all keys as symbols, got ~ts", [inspect(Object)])
  end;

assert_dict_with_atoms(Data) ->
  elixir_errors:raise(badarg, "constructor needs to return a OrderedDict, got ~ts", [inspect(Data)]).

inspect(Object) ->
  element(2, elixir_dispatch:dispatch(false, Object, inspect, [])).

% Helper that prepends a mixin or a proto to the object chain.
prepend_as(#elixir_object__{name=Name} = Self, Chain, Kind, Value) ->
  check_module(Value, Kind),
  List = object_protos(Value),

  % If we are adding prototypes and the current name is
  % in the list of protos, this means we are adding a
  % proto to a module and we need to ensure all added modules
  % will come after the module name in the proto list.
  case lists:member(Name, Chain) of
    true ->
      { Before, After } = lists:splitwith(fun(X) -> X /= Name end, Chain),
      Final = umerge(List, lists:delete(Name, After)),
      Updated = umerge(Before, umerge([Name], Final));
    _ ->
      Updated = umerge(List, Chain)
  end,

  Object = update_object_chain(Self, Kind, Updated),

  % Invoke the appropriate hook.
  elixir_dispatch:dispatch(true, Value, ?ELIXIR_ATOM_CONCAT(["__added_as_", atom_to_list(Kind), "__"]), [Object]);

% Raise an error if mixin or proto is called on builtin.
prepend_as(Else, Chain, Kind, Value) -> builtinnotallowed(Else, Kind).

% Update the given object chain. Sometimes it means we need to update
% the table, sometimes update a record.
update_object_chain(#elixir_object__{data=Data} = Self, Kind, Chain) when is_atom(Data) ->
  TableKind = ?ELIXIR_ATOM_CONCAT([Kind, s]),
  ets:insert(Data, {TableKind, Chain}),
  Self;

update_object_chain(Self, mixin, Chain) ->
  Self#elixir_object__{mixins=Chain};

update_object_chain(Self, proto, Chain) ->
  Self#elixir_object__{protos=Chain}.

% Check if it is a module and raises an error if not.
check_module(#elixir_object__{parent='Module'}, Kind) -> [];
check_module(Else, Kind) -> elixir_errors:error({notamodule, {Else, Kind}}).

% Raise builtinnotallowed error with the given reason:
builtinnotallowed(Builtin, Reason) ->
  elixir_errors:error({builtinnotallowed, {Builtin, Reason}}).

% Returns the ancestors chain considering only parents, but in reverse order.

r_ancestors(Object) ->
  r_ancestors(object_parent(Object), []).

r_ancestors([], Acc) ->
  Acc;

r_ancestors(Name, Acc) ->
  r_ancestors(abstract_parent(Name), [Name|Acc]).

% Methods that get values from objects. Argument can either be an
% #elixir_object__ or an erlang native type.

object_name(#elixir_object__{name=Name}) ->
  Name;

object_name(Native) ->
  []. % Native types are instances and has no name.

object_parent(#elixir_object__{parent=Parent}) ->
  Parent;

object_parent(Native) when is_integer(Native) ->
  'Integer';

object_parent(Native) when is_float(Native) ->
  'Float';

object_parent(Native) when is_atom(Native) ->
  'Atom';

object_parent(Native) when is_list(Native) ->
  'List';

object_parent(Native) when is_bitstring(Native) ->
  'BitString';

object_parent(#elixir_orddict__{}) ->
  'OrderedDict';

object_parent(#elixir_string__{}) ->
  'String';

object_parent(Native) when is_tuple(Native) ->
  'Tuple';

object_parent(Native) when is_pid(Native) ->
  'Process'.

object_mixins(#elixir_object__{data=Data}) when is_atom(Data) ->
  ets:lookup_element(Data, mixins, 2);

object_mixins(#elixir_object__{mixins=Mixins}) ->
  Mixins;

object_mixins(Native) ->
  []. % Native types has all mixins from parents.

object_protos(#elixir_object__{data=Data}) when is_atom(Data) ->
  ets:lookup_element(Data, protos, 2);

object_protos(#elixir_object__{protos=Protos}) ->
  Protos;

object_protos(Native) ->
  []. % Native types has no protos.

object_data(#elixir_object__{data=Data}) when is_atom(Data) ->
  ets:lookup_element(Data, data, 2);

object_data(#elixir_object__{data=Data}) ->
  Data;

object_data(Native) ->
  orddict:new(). % Native types has no protos.

% Method that get values from parents. Argument can either be an atom
% or an #elixir_object__.

abstract_parent(#elixir_object__{parent=Parent}) ->
  Parent;

abstract_parent(Name) ->
  case proplists:get_value(parent, elixir_constants:lookup(Name, attributes)) of
    []   -> [];
    Else -> hd(Else)
  end.

abstract_mixins(#elixir_object__{mixins=Mixins}) ->
  Mixins;

abstract_mixins(Name) ->
  proplists:get_value(mixins, elixir_constants:lookup(Name, attributes)).

abstract_protos(#elixir_object__{protos=Protos}) ->
  Protos;

abstract_protos(Name) ->
  proplists:get_value(protos, elixir_constants:lookup(Name, attributes)).

abstract_data(#elixir_object__{data=Data}) ->
  Data;

abstract_data(Name) ->
  hd(proplists:get_value(data, elixir_constants:lookup(Name, attributes))).

% Merge two lists taking into account uniqueness. Opposite to
% lists:umerge2, does not require lists to be sorted.

umerge(List, Data) ->
  umerge2(lists:reverse(List), Data).

umerge2([], Data) ->
  Data;

umerge2([H|T], Data) ->
  case lists:member(H, Data) of
    true  -> New = Data;
    false -> New = [H|Data]
  end,
  umerge2(T, New).

% Methods that traverses the ancestors chain and append.

traverse_chain([], Acc) ->
  Acc;

traverse_chain([H|T], Acc) ->
  traverse_chain(T, apply_chain(abstract_protos(H), Acc)).

apply_chain(List, Acc) ->
  umerge(List, Acc).