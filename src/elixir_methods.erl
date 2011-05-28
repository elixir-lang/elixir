% Holds introspection for methods.
% To check how methods are defined internally, check elixir_def_method.
-module(elixir_methods).
-export([assert_behavior/2, abstract_methods/1, proto_methods/1, mixin_methods/1]).
-include("elixir.hrl").
-import(lists, [umerge/2, sort/1]).

% Public in Elixir

mixin_methods(#elixir_object__{mixins=Mixin}) when is_atom(Mixin) ->
  convert_methods(Mixin:module_info(exports));

mixin_methods(#elixir_object__{} = Self) ->
  calculate_methods(Self, fun abstract_methods/1, elixir_object_methods:mixins(Self), []);

mixin_methods(Self) ->
  Mixin = elixir_object_methods:builtin_mixin(Self),
  convert_methods(Mixin:module_info(exports)).

proto_methods(#elixir_object__{protos=Proto}) when is_atom(Proto) ->
  convert_methods(Proto:module_info(exports));

proto_methods(#elixir_object__{} = Self) ->
  calculate_methods(Self, fun abstract_methods/1, elixir_object_methods:protos(Self), []);

proto_methods(Self) ->
  mixin_methods(Self).

% Public in Erlang

assert_behavior(Module, Object) when is_atom(Module) -> 
  assert_behavior(Module:module_info(exports) -- defaults(), Object);

assert_behavior(Exports, Object) -> 
  Methods = mixin_methods(Object),
  lists:foreach(fun({Name, Arity}) ->
    case lists:member({Name, Arity-1}, Methods) of
      true -> [];
      false -> elixir_errors:error({nocallback, {Name, Arity-1, Object}})
    end
  end, Exports).

abstract_methods(Name) when is_atom(Name) ->
  convert_methods(elixir_constants:lookup(Name, exports)).

% Helpers

defaults() ->
  [{module_info,0},{module_info,1},{'__function_exported__',2}].

convert_methods(Target) ->
  lists:map(fun convert_method/1, Target -- defaults()).

convert_method({Name, Arity}) -> { Name, Arity - 1 }.

calculate_methods(_Self, Fun, List, Acc) ->
  calculate_methods(Fun, List, Acc).

calculate_methods(Fun, [H|T], Acc) ->
  calculate_methods(Fun, T, umerge(Acc, sort(Fun(H))));

calculate_methods(Fun, [], Acc) ->
  Acc.