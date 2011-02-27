% Holds all runtime methods required to bootstrap modules.
% These methods are overwritten by their Elixir version later in Module::Methods.
-module(elixir_module_methods).
-export([get_visibility/1, set_visibility/2, alias_local/5, define_attribute/3, behavior/1, copy_attributes_fun/1]).
-include("elixir.hrl").

set_visibility(#elixir_object__{name=Name, data=Data}, Visibility) when is_atom(Data) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:insert(MethodTable, { visibility, Visibility });

set_visibility(Self, Visibility) ->
  elixir_errors:raise(badarg, "cannot change visibility of defined object").

get_visibility(#elixir_object__{name=Name, data=Data}) when is_atom(Data) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:lookup_element(MethodTable, visibility, 2);

get_visibility(Self) ->
  [].

alias_local(#elixir_object__{name=Name, data=Data} = Self, Filename, Old, New, ElixirArity) when is_atom(Data) ->
  Arity = ElixirArity + 1,
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  case ets:lookup(MethodTable, { Old, Arity }) of
    [{{Old, Arity}, Line, Clauses}] ->
      elixir_methods:store_wrapped_method(Name, Filename, {function, Line, New, Arity, Clauses});
    [] ->
      elixir_errors:error({nolocalmethod, {Self, Old, Arity}})
  end;

alias_local(_, _, _, _, _) ->
  elixir_errors:raise(badarg, "cannot alias local method outside object definition scope").

define_attribute(#elixir_object__{data=Data, parent=Parent}, Key, Value) when is_atom(Data) ->
  case Parent of
    'Module' -> ets:insert(Data, { Key, Value });
    _ ->
      Current = ets:lookup_element(Data, module, 2),
      ets:insert(Data, { module, [{ Key, Value }|Current] })
  end;

define_attribute(_, _, _) ->
  elixir_errors:raise(badarg, "cannot add attribute to an already defined module").

copy_attributes_fun(Data) ->
  fun(Object) ->
    Copier = fun({Key,Value}) -> define_attribute(Object, Key, Value) end,
    lists:foreach(Copier, Data)
  end.

behavior(#elixir_object__{data=Data, parent='Module'}) when is_atom(Data) ->
  case ets:lookup(Data, behavior) of
    [{behavior,Behavior}] -> Behavior;
    _ -> []
  end;

behavior(#elixir_object__{data=Data}) when is_atom(Data) ->
  case ets:lookup(Data, module) of
    [{module,Attributes}] ->
      case proplists:get_value(behavior, Attributes) of
        undefined -> [];
        Else -> Else
      end;
    _ -> []
  end;

behavior(#elixir_object__{name=[]}) ->
  [];

behavior(#elixir_object__{name=Name} = Object) ->
  case module_behavior(Name) of
    elixir_callbacks -> module_behavior(elixir_callbacks:callback_name(Object));
    _ -> []
  end.

module_behavior(Name) ->
  case proplists:get_value(behavior, Name:module_info(attributes)) of
    undefined -> [];
    Else -> hd(Else)
  end.