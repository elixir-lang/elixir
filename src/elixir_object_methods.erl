% Holds all runtime methods required to bootstrap the object model.
% These methods are overwritten by their Elixir version later in Object::Methods.
-module(elixir_object_methods).
-export([mixin/2, proto/2, new/2, name/1, parent/1, parent_name/1, mixins/1, protos/1, data/1, builtin_mixin/1,
  get_ivar/2, set_ivar/3, set_ivars/2, update_ivar/3, update_ivar/4, ancestors/1, function_catch/1]).
-include("elixir.hrl").

% EXTERNAL API

% TODO: Rewrite this as remove_method once we add it
new(#elixir_object__{parent='Module'} = Self, Args) ->
  elixir_dispatch:dispatch(Self, method_missing, [new, Args]);

new(#elixir_object__{name=Name, protos=Protos} = Self, Args) ->
  Parent = case Name of
    [] -> Self;
    _  -> Name
  end,
  Object = #elixir_object__{name=[], parent=Parent, mixins=Protos, protos=[], data=[]},
  NewObject = elixir_dispatch:dispatch(Object, initialize, Args),
  assert_same_object(Object, NewObject),
  NewObject;

new(Else, Args) -> builtinnotallowed(Else, new).

mixin(Self, Value) when is_list(Value) -> [mixin(Self, Item) || Item <- Value];
mixin(Self, Value) -> prepend_as(Self, object_mixins(Self), mixin, Value).

% TODO: Rewrite this as remove_method once we add it
proto(#elixir_object__{parent='Module'} = Self, Value) ->
  elixir_dispatch:dispatch(Self, method_missing, [proto, [Value]]);
proto(Self, Value) when is_list(Value) -> [proto(Self, Item) || Item <- Value];
proto(Self, Value) -> prepend_as(Self, object_protos(Self), proto, Value).

% Reflections

name(Self)      -> object_name(Self).
data(Self)      -> object_data(Self).
ancestors(Self) -> lists:reverse(r_ancestors(Self)).

mixins(#elixir_object__{parent='Module'} = Self) -> object_mixins(Self) ++ ['Module::Methods', 'Object::Methods'];
mixins(#elixir_object__{} = Self)                -> apply_chain(object_mixins(Self), traverse_chain(r_ancestors(Self), []));
mixins(Self)                                     -> object_mixins(Self) ++ ['Object::Methods'].

protos(#elixir_object__{} = Self)                -> apply_chain(object_protos(Self), traverse_chain(r_ancestors(Self), []));
protos(Self) -> mixins(Self).

parent(Self) ->
  case object_parent(Self) of
    Object when is_atom(Object) -> elixir_constants:lookup(Object);
    Object -> Object
  end.

parent_name(Self) ->
  case object_parent(Self) of
    Object when is_atom(Object) -> Object;
    _ -> []
  end.

% Methods available to all objects

function_catch(Function) ->
  catch Function().

%% PROTECTED API

get_ivar(Self, Name) when not is_atom(Name) ->
  elixir_errors:error({badivar, Name});

get_ivar(#elixir_object__{} = Self, Name) ->
  get_ivar_dict(Name, object_data(Self));

get_ivar(Self, Name) -> % Native types do not have instance variables.
  [].

set_ivar(Self, Name, Value) ->
  set_ivar_dict(Self, Name, set_ivar, fun(Dict) -> orddict:store(Name, Value, Dict) end).

set_ivars(Self, Value) ->
  assert_dict_with_atoms(Value),
  set_ivar_dict(Self, elixir, set_ivars, fun(Dict) -> orddict:merge(fun(_, V1, V2) -> V2 end, Dict, element(2, Value)) end).

update_ivar(Self, Name, Function) ->
  set_ivar_dict(Self, Name, update_ivar, fun(Dict) -> orddict:update(Name, Function, Dict) end).

update_ivar(Self, Name, Initial, Function) ->
  set_ivar_dict(Self, Name, update_ivar, fun(Dict) -> orddict:update(Name, Function, Initial, Dict) end).

% HELPERS

get_ivar_dict(Name, Data) ->
  case orddict:find(Name, Data) of
    { ok, Value } -> Value;
    error -> []
  end.

set_ivar_dict(_, Name, _, _) when not is_atom(Name) ->
  elixir_errors:error({badivar, Name});

set_ivar_dict(#elixir_object__{data=Data} = Self, Name, _, Function) when is_atom(Data) ->
  Dict = ets:lookup_element(Data, data, 2),
  Object = Self#elixir_object__{data=Function(Dict)},
  ets:insert(Data, { data, Object#elixir_object__.data }),
  Object;

set_ivar_dict(#elixir_object__{data=Dict} = Self, Name, _, Function) ->
  Self#elixir_object__{data=Function(Dict)};

set_ivar_dict(Self, _, Method, _) ->
  builtinnotallowed(Self, Method).

assert_dict_with_atoms(#elixir_orddict__{struct=Dict} = Object) ->
  case lists:all(fun is_atom/1, orddict:fetch_keys(Dict)) of
    true  -> Dict;
    false ->
      elixir_errors:error({badivars, Object})
  end;

assert_dict_with_atoms(Data) ->
  elixir_errors:error({badivars, Data}).

assert_same_object(#elixir_object__{parent=Parent}, #elixir_object__{parent=Parent}) -> true;
assert_same_object(_, Else) -> elixir_errors:error({badinitialize, Else}).

% Helper that prepends a mixin or a proto to the object chain.
prepend_as(#elixir_object__{} = Self, Chain, Kind, Value) ->
  check_module(Value, Kind),
  List = object_mixins(Value),
  Object = update_object_chain(Self, Kind, umerge(List, Chain)),

  % Invoke the appropriate hook.
  elixir_dispatch:dispatch(Value, ?ELIXIR_ATOM_CONCAT(["__added_as_", atom_to_list(Kind), "__"]), [Object]);

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

r_ancestors(#elixir_object__{parent='Object'})                -> ['Object'];
r_ancestors(#elixir_object__{parent=[]})                      -> [];
r_ancestors(#elixir_object__{parent=Else}) when is_atom(Else) -> ['Object', Else];
r_ancestors(#elixir_object__{} = Object)                      -> r_ancestors(object_parent(Object), []);
r_ancestors(Else)                                             -> ['Object', object_parent(Else)].

r_ancestors([], Acc)   -> Acc;
r_ancestors(Name, Acc) -> r_ancestors(abstract_parent(Name), [Name|Acc]).

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

object_parent(Native) when is_function(Native) ->
  'Function';

object_parent(Native) when is_pid(Native) ->
  'Process';

object_parent(Native) when is_reference(Native) ->
  'Reference';

object_parent(Native) when is_port(Native) ->
  'Port'.

object_mixins(#elixir_object__{data=Data}) when is_atom(Data) ->
  try
    ets:lookup_element(Data, mixins, 2)
  catch
    error:badarg -> []
  end;

object_mixins(#elixir_object__{name=[], mixins=Mixins} = Object) when is_atom(Mixins) ->
  abstract_protos(object_parent(Object));

object_mixins(#elixir_object__{name=Name, mixins=Mixins}) when is_atom(Mixins) ->
  abstract_mixins(Name);

object_mixins(#elixir_object__{mixins=Mixins}) ->
  Mixins;

object_mixins(Native) ->
  abstract_protos(object_parent(Native)).

object_protos(#elixir_object__{data=Data}) when is_atom(Data) ->
  try
    ets:lookup_element(Data, protos, 2)
  catch
    error:badarg -> []
  end;

object_protos(#elixir_object__{name=Name, protos=Protos}) when is_atom(Protos) ->
  abstract_protos(Name);

object_protos(#elixir_object__{protos=Protos}) ->
  Protos;

object_protos(Native) ->
  []. % Native types has no protos.

object_data(#elixir_object__{data=Data}) when is_atom(Data) ->
  try
    ets:lookup_element(Data, data, 2)
  catch
    error:badarg -> orddict:new()
  end;

object_data(#elixir_object__{data=Data}) ->
  Data;

object_data(Native) ->
  orddict:new(). % Native types has no data.

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

% Builtin mixins

builtin_mixin(Native) when is_integer(Native) ->
  'Integer::Proto';

builtin_mixin(Native) when is_float(Native) ->
  'Float::Proto';

builtin_mixin(Native) when is_atom(Native) ->
  'Atom::Proto';

builtin_mixin(Native) when is_list(Native) ->
  'List::Proto';

builtin_mixin(Native) when is_bitstring(Native) ->
  'BitString::Proto';

builtin_mixin(#elixir_orddict__{}) ->
  'OrderedDict::Proto';

builtin_mixin(#elixir_string__{}) ->
  'String::Proto';

builtin_mixin(Native) when is_tuple(Native) ->
  'Tuple::Proto';

builtin_mixin(Native) when is_function(Native) ->
  'Function::Proto';

builtin_mixin(Native) when is_pid(Native) ->
  'Process::Proto';

builtin_mixin(Native) when is_reference(Native) ->
  'Reference::Proto';

builtin_mixin(Native) when is_port(Native) ->
  'Port::Proto'.

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
  List ++ Acc.