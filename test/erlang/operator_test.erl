-module(operator_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% Interpolations

booleans_test() ->
  {true, []} = elixir:eval("true"),
  {false, []} = elixir:eval("false").