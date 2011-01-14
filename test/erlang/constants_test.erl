-module(constants_test).
-include_lib("eunit/include/eunit.hrl").

constant_is_defined_and_value_is_retrieved_test() -> 
  F = fun() ->
    ?assertEqual({6,[]}, elixir:eval("Foo = 1 + 2; Foo + 3"))
  end,
  test_helper:run_and_remove(F, ['Foo']).