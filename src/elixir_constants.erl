% Handle constants in Elixir. Constants are compiled to erlang
% modules and consequently are not stored in any table.
-module(elixir_constants).
-export([lookup/1, lookup/2]).
-include("elixir.hrl").

lookup(RawName) ->
  Name = ?ELIXIR_ERL_MODULE(RawName),
  try
    Name:'__module__'([])
  catch
    error:undef -> elixir_errors:error({no_module, RawName})
  end.

lookup(RawName, Something) ->
  Name = ?ELIXIR_ERL_MODULE(RawName),
  try
    Name:module_info(Something)
  catch
    error:undef -> elixir_errors:error({no_module, RawName})
  end.
