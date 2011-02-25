-module(callbacks_test).
-include_lib("eunit/include/eunit.hrl").

define_behavior_test() ->
  F = fun() ->
    {bar,[]} = elixir:eval("module Foo; define_attribute('behavior, 'bar); __behavior__; end"),
    {bar,[]} = elixir:eval("Foo.__behavior__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'ex_callbacks_Foo']).

behavior_defines_a_callback_module_test() ->
  F = fun() ->
    elixir:eval("module CallbackSpecificModule; define_attribute('behavior, 'bar); end"),
    Name = 'ex_callbacks_CallbackSpecificModule',
    {module, Name} = code:ensure_loaded(Name)
  end,
  test_helper:run_and_remove(F, ['CallbackSpecificModule', 'ex_callbacks_CallbackSpecificModule']).

dispatching_to_callback_dispatch_to_ex_module_test() ->
  F = fun() ->
    elixir:eval("module Foo; define_attribute('behavior, 'bar); callbacks; def foo; 1; end; def bar(x, y); x - y - self.foo - foo; end; end"),
    1 = 'ex_callbacks_Foo':foo(),
    1 = 'ex_callbacks_Foo':bar(5, 2),
    {1,[]} = elixir:eval("Foo.__send__('foo)"),
    ?assertError({protectedmethod,_}, elixir:eval("Foo.foo"))
  end,
  test_helper:run_and_remove(F, ['Foo', 'ex_callbacks_Foo']).