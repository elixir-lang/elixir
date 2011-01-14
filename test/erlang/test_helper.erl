-module(test_helper).
-export([test/0]).

test() ->
  elixir:boot(),
  eunit:test({inorder, [
    arithmetic_test,
    function_test,
    module_test,
    variables_test
  ]}).