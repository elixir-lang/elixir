-module(arithmetic_test).
-include_lib("eunit/include/eunit.hrl").

basic_sum_test() ->
  ?assertEqual(3, elixir:eval("1+2")),
  ?assertEqual(6, elixir:eval("1+2+3")),
  ?assertEqual(6, elixir:eval("1 + 2 + 3")).
