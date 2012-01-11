%% Convenience functions used to manipulate variables
%% and the variables escope.
-module(elixir_variables).
-export([
  build_erl/2, build_ex/2,
  umergev/2, umergec/2,
  serialize_scope/1, deserialize_scope/1]).
-include("elixir.hrl").

build_erl(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["X", Counter]) },
  { Var, NS }.

build_ex(Line, #elixir_scope{counter=Counter} = S) ->
  NS = S#elixir_scope{counter=Counter+1},
  Var = { ?ELIXIR_ATOM_CONCAT(["X", Counter]), Line, nil },
  { Var, NS }.

% Provides a tuple with only the scope information we want to serialize.

serialize_scope(S) ->
  elixir_tree_helpers:abstract_syntax(
    { S#elixir_scope.filename, S#elixir_scope.imports,
      S#elixir_scope.refer, S#elixir_scope.scheduled }
  ).

% Fill in the scope with the variables serialization set in serialize_scope.

deserialize_scope({ Filename, Imports, Refer, Scheduled }) ->
  #elixir_scope{
    filename=Filename,
    imports=Imports,
    refer=Refer,
    scheduled=Scheduled
  }.

% Receives two scopes and return a new scope based on the second
% with their variables merged.

umergev(S1, S2) ->
  V1 = S1#elixir_scope.vars,
  V2 = S2#elixir_scope.vars,
  C1 = S1#elixir_scope.clause_vars,
  C2 = S2#elixir_scope.clause_vars,
  S2#elixir_scope{
    vars=dict:merge(fun var_merger/3, V1, V2),
    clause_vars=dict:merge(fun var_merger/3, C1, C2)
  }.

% Receives two scopes and return a new scope based on the first
% with the counter values from the first one.

umergec(S1, S2) ->
  S1#elixir_scope{counter=S2#elixir_scope.counter}.

% Merge variables trying to find the most recently created.

var_merger(Var, Var, K2) -> K2;
var_merger(Var, K1, Var) -> K1;
var_merger(_Var, K1, K2) ->
  V1 = list_to_integer(tl(atom_to_list(K1))),
  V2 = list_to_integer(tl(atom_to_list(K2))),
  if V1 > V2 -> K1;
     true -> K2
  end.