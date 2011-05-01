% Holds all methods specific to modules, like visibility, alias_local and friends.
% This works on the same structures as elixir_def_methods, but elixir_def_methods
% are rather an internal module while most of the methods defined here are exposed
% in Module::Methods.
-module(elixir_module_methods).
-export([get_visibility/1, set_visibility/2, alias_local/5, define_erlang_method/6,
  define_attribute/3, copy_attributes_fun/1, module_eval/4]).
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
      elixir_def_method:store_wrapped_method(Name, Filename, {function, Line, New, Arity, Clauses}, []);
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
  elixir:eval(to_char_list(String), [{self,Self}], to_char_list(Filename), Line, Scope);

module_eval(Self, _, _, _) ->
  elixir_errors:error({moduledefined, { Self, module_eval }}).

define_erlang_method(#elixir_object__{name=Name, data=Data}, Filename, Line, Method, Arity, Clauses) when is_atom(Data)->
  TClauses = lists:map(fun expand_clauses/1, Clauses),
  elixir_def_method:store_wrapped_method(Name, to_char_list(Filename), {function, Line, Method, Arity + 1, TClauses}, []);

define_erlang_method(Self, _, _, _, _, _) ->
  elixir_errors:error({moduledefined, { Self, define_erlang_method }}).

object_kind(#elixir_object__{parent='Module'}) -> module;
object_kind(_) -> object.

expand_clauses({ clause, Line, Args, Guards, Exprs }) ->
  { clause, Line, [{var, Line, self}|Args], Guards, Exprs }.

to_char_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_char_list(List) when is_list(List) -> List.