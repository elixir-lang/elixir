% Holds all runtime methods required to bootstrap modules.
% These methods are overwritten by their Elixir version later in Module::Methods.
-module(elixir_module_methods).
-export([get_visibility/1, set_visibility/2, alias_local/5, define_attribute/3, copy_attributes_fun/1, module_eval/4]).
-include("elixir.hrl").

set_visibility(#elixir_object__{name=Name, data=Data}, Visibility) when is_atom(Data) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:insert(MethodTable, { visibility, Visibility });

set_visibility(Self, Visibility) ->
  elixir_errors:error({moduledefined, { Self, set_visibility }}).

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
      elixir_def_method:store_wrapped_method(Name, Filename, {function, Line, New, Arity, Clauses});
    [] ->
      elixir_errors:error({nolocalmethod, {Self, Old, Arity}})
  end;

alias_local(Self, _, _, _, _) ->
  elixir_errors:error({moduledefined, { Self, alias_local }}).

define_attribute(#elixir_object__{data=Data, parent=Parent}, Key, Value) when is_atom(Data) ->
  case Parent of
    'Module' -> ets:insert(Data, { Key, Value });
    _ ->
      Current = ets:lookup_element(Data, module, 2),
      ets:insert(Data, { module, [{ Key, Value }|Current] })
  end;

define_attribute(Self, _, _) ->
  elixir_errors:error({moduledefined, { Self, define_attribute }}).

copy_attributes_fun(Data) ->
  fun(Object) ->
    Copier = fun({Key,Value}) -> define_attribute(Object, Key, Value) end,
    lists:foreach(Copier, Data)
  end.

module_eval(#elixir_object__{name=Name, data=Data} = Self, String, Filename, Line) when is_atom(Data) ->
  Scope = #elixir_scope{scope={object_kind(Self), Name}},
  elixir:eval(String, [{self,Self}], Filename, Line, Scope).

object_kind(#elixir_object__{parent='Module'}) -> module;
object_kind(_) -> object.