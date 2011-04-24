% Handle constants in Elixir. Constants are compiled to erlang
% modules and consequently are not stored in any table.
-module(elixir_constants).
-export([lookup/1, lookup/2]).
-include("elixir.hrl").

lookup(RawName) ->
  Name = ?ELIXIR_ERL_MODULE(RawName),
  case code:ensure_loaded(Name) of
    {module, Name} -> elixir_object:build(Name);
    _ -> elixir_errors:error({noconstant, RawName})
  end.

lookup(RawName, Something) ->
  Name = ?ELIXIR_ERL_MODULE(RawName),
  case code:ensure_loaded(Name) of
    {module, Name} -> Name:module_info(Something);
    _ -> elixir_errors:error({noconstant, RawName})
  end.
