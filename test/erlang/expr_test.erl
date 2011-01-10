-module(expr_test).
-include_lib("eunit/include/eunit.hrl").

assignment_test() ->
  ?assertEqual({1, [{a, 1}]}, elixir:eval("a = 1")).
