% Holds introspection for methods.
% To check how methods are defined internally, check elixir_def_method.
-module(elixir_methods).
-export([assert_behavior/2, unpack_default_clauses/3, abstract_methods/1, abstract_public_methods/1,
  abstract_protected_methods/1, proto_methods/1, public_proto_methods/1]).
-include("elixir.hrl").
-import(lists, [umerge/2, sort/1]).

% Public in Elixir

proto_methods(Self) ->
  calculate_methods(Self, fun abstract_methods/1, elixir_object_methods:protos(Self), []).

public_proto_methods(Self) ->
  calculate_methods(Self, fun abstract_public_methods/1, elixir_object_methods:protos(Self), []).

% Public in Erlang

% Methods that unpack default args from clauses
unpack_default_clauses(Name, [H|T], Acc) ->
  { NewArgs, NewAcc } = unpack_default_args(Name, element(3, H), [], Acc),
  unpack_default_clauses(Name, T, [setelement(3, H, NewArgs)|NewAcc]);

unpack_default_clauses(_Name, [], Acc) ->
  lists:reverse(Acc).

assert_behavior(Module, Object) when is_atom(Module) -> 
  assert_behavior(Module:module_info(exports) -- [{module_info,0},{module_info,1}], Object);

assert_behavior(Exports, Object) -> 
  Methods = proto_methods(Object),
  lists:foreach(fun({Name, Arity}) ->
    case lists:member({Name, Arity-1}, Methods) of
      true -> [];
      false -> elixir_errors:error({nocallback, {Object, Name, Arity-1}})
    end
  end, Exports).

abstract_methods(#elixir_object__{}) ->
  [];

abstract_methods(Name) ->
  Converter = fun({Name, Arity}) -> {Name, Arity - 1} end,
  lists:map(Converter, elixir_constants:lookup(Name, functions) -- [{module_info,0},{module_info,1}]).

abstract_public_methods(#elixir_object__{}) ->
  [];

abstract_public_methods(Name) ->
  abstract_methods(Name) -- abstract_protected_methods(Name).

abstract_protected_methods(#elixir_object__{}) ->
  [];

abstract_protected_methods(Name) ->
  proplists:get_value(protected, elixir_constants:lookup(Name, attributes)).

% Helpers

% If we are defining a module, we need to remove itself from the given
% List as the module was not defined in Erlang system yet.
calculate_methods(#elixir_object__{name=Name,parent=Parent,data=Data}, Fun, List, Acc) when is_atom(Data), Parent == 'Module' ->
  calculate_methods(Fun, lists:delete(Name, List), Acc);

calculate_methods(_Self, Fun, List, Acc) ->
  calculate_methods(Fun, List, Acc).

calculate_methods(Fun, [], Acc) ->
  Acc;

calculate_methods(Fun, [H|T], Acc) ->
  calculate_methods(Fun, T, umerge(Acc, sort(Fun(H)))).

% Build an args list
build_arg(0, _Line, Acc) -> Acc;

build_arg(Counter, Line, Acc) ->
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["X", Counter]) },
  build_arg(Counter - 1, Line, [Var|Acc]).

% Unpack default args from clauses
unpack_default_args(Name, [{default_arg, Line, Expr, Default}|T], Acc, Clauses) ->
  Args = build_arg(length(Acc), Line, []),
  Clause = { clause, Line, Args, [], [
    { call, Line, {atom, Line, Name}, Args ++ [Default] }
  ]},
  unpack_default_args(Name, T, [Expr|Acc], [Clause|Clauses]);

unpack_default_args(Name, [H|T], Acc, Clauses) ->
  unpack_default_args(Name, T, [H|Acc], Clauses);

unpack_default_args(_Name, [], Acc, Clauses) ->
  { lists:reverse(Acc), Clauses }.
