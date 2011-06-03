% Holds implementation for most Module::Using methods.
-module(elixir_module_methods).
-export([mixin/2, get_visibility/1, set_visibility/2, alias_local/5, define_erlang_method/6, module_eval/4]).
-include("elixir.hrl").

% Mixins

mixin(Self, Value) when is_list(Value) -> [mixin(Self, Item) || Item <- Value];
mixin(Self, Value) -> 
  check_module(Value),
  NewMixins = elixir_object_methods:mixins(Value),
  CurrentMixins = elixir_object_methods:mixins(Self),
  update_mixins_chain(Self, umerge(NewMixins, CurrentMixins)),
  elixir_dispatch:dispatch(Value, '__added_as_mixin__', [Self]).

update_mixins_chain(#elixir_object__{data=Data} = Self, Chain) when is_atom(Data) ->
  ets:insert(Data, {mixins, Chain}).

check_module(#elixir_object__{parent='Module'}) -> [];
check_module(Else) -> elixir_errors:error({not_a_module, Else}).

% Visibility

set_visibility(#elixir_object__{name=Name, data=Data}, Visibility) when is_atom(Data) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:insert(MethodTable, { visibility, Visibility });

set_visibility(Self, Visibility) ->
  elixir_errors:error({moduledefined, { set_visibility, Self }}).

get_visibility(#elixir_object__{name=Name, data=Data}) when is_atom(Data) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:lookup_element(MethodTable, visibility, 2);

get_visibility(Self) ->
  [].

% alias_local

alias_local(#elixir_object__{name=Name, data=Data} = Self, Filename, Old, New, ElixirArity) when is_atom(Data) ->
  Arity = ElixirArity + 1,
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  case ets:lookup(MethodTable, { Old, Arity }) of
    [{{Old, Arity}, Line, Clauses}] ->
      elixir_def_method:store_wrapped_method(Self, Name, Filename, {function, Line, New, Arity, Clauses}, []);
    [] ->
      elixir_errors:error({nolocalmethod, {Old, Arity, Self}})
  end;

alias_local(Self, _, _, _, _) ->
  elixir_errors:error({moduledefined, { alias_local, Self }}).

% module_eval

module_eval(#elixir_object__{name=Name, data=Data} = Self, String, Filename, Line) when is_atom(Data) ->
  Scope = #elixir_scope{scope={object_kind(Self), Name}},
  elixir:eval(to_char_list(String), [{self,Self}], to_char_list(Filename), Line, Scope);

module_eval(Self, _, _, _) ->
  elixir_errors:error({moduledefined, { module_eval, Self }}).

object_kind(#elixir_object__{parent='Module'}) -> module;
object_kind(_) -> object.

% define_erlang_methods

define_erlang_method(#elixir_object__{name=Name, data=Data} = Self, Filename, Line, Method, Arity, Clauses) when is_atom(Data)->
  TClauses = lists:map(fun expand_clauses/1, Clauses),
  elixir_def_method:store_wrapped_method(Self, Name, to_char_list(Filename), {function, Line, Method, Arity + 1, TClauses}, []);

define_erlang_method(Self, _, _, _, _, _) ->
  elixir_errors:error({moduledefined, { define_erlang_method, Self }}).

expand_clauses({ clause, Line, Args, Guards, Exprs }) ->
  { clause, Line, [{var, Line, self}|Args], Guards, Exprs }.

to_char_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_char_list(List) when is_list(List) -> List.

% HELPERS

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