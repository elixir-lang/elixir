-module(elixir_constants).
-export([boot/0, lookup/1, store/2]).
-include("elixir.hrl").

% Boot constants by setting ETS table.
boot() ->
  ets:new(ex_constants, [ordered_set, public, named_table]).

% Lookup a constant with the given name in the ETS table. Raises
% an error if the constant does not exist.
lookup(Name) ->
  case ets:lookup(ex_constants, Name) of
    []  -> erlang:error("No constant " ++ atom_to_list(Name) ++ " defined");
    _   -> ets:delete(ex_constants, Name)
  end.

% Store a given constant in the lookup table. Raises an error
% if the constant was already stored.
store(Name, Value) ->
  case raw_lookup(Name) of
    [] -> ets:insert(ex_constants, {Name, Value});
    _  -> erlang:error("Constant " ++ atom_to_list(Name) ++ " is already defined")
  end.

raw_lookup(Name) ->
  case ets:lookup(ex_constants, Name) of
    []   -> [];
    Else -> hd(Else)
  end.
  