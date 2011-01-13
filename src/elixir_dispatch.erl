-module(elixir_dispatch).
-export([dispatch/3]).
-include("elixir.hrl").

dispatch(Integer, Name, Args) when is_integer(Integer) ->
  apply('@Integer', Name, Args).