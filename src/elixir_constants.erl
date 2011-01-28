% Handle constants in Elixir. Constants are compiled to erlang
% modules and consequently are not stored in any table.
-module(elixir_constants).
-export([lookup/1, lookup_attributes/1]).
-include("elixir.hrl").

% Lookup a constant with the given name in the ETS table. Raises
% an error if the constant does not exist.
lookup(Name) ->
  case code:ensure_loaded(Name) of
    {module, Name} -> elixir_object:build(Name);
    _ -> elixir_errors:raise(badarg, "no constant ~p defined", [Name])
  end.

lookup_attributes(Name) ->
  case code:ensure_loaded(Name) of
    {module, Name} -> Name:module_info(attributes);
    _ -> elixir_errors:raise(badarg, "no constant ~p defined", [Name])
  end.