-module(constants_test).
-include_lib("eunit/include/eunit.hrl").

constant_is_defined_and_value_is_retrieved_test() -> 
  F = fun() ->
    ?assertEqual({6,[]}, elixir:eval("const Foo = 1 + 2; Foo + 3"))
  end,
  test_helper:run_and_remove(F, ['Foo']).

constant_is_defined_and_value_is_retrieved_multiline_test() -> 
  F = fun() ->
    ?assertEqual({6,[]}, elixir:eval("const Foo =\n1 + 2; Foo + 3"))
  end,
  test_helper:run_and_remove(F, ['Foo']).

cannot_store_already_defined_constants_test() ->
  F = fun() ->
    ?assertError({badarg, "Constant 'Foo' is already defined" },
      elixir:eval("const Foo = 1 + 2; const Foo = 3"))
  end,
  test_helper:run_and_remove(F, ['Foo']).

cannot_lookup_not_stored_constants_test() ->
  ?assertError({badarg, "No constant 'Foo' defined" }, elixir:eval("Foo")).