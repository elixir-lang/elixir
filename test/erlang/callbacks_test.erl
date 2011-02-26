-module(callbacks_test).
-include_lib("eunit/include/eunit.hrl").

module_define_behavior_test() ->
  F = fun() ->
    {bar,[]} = elixir:eval("module Foo; define_module_attribute('behavior, 'bar); __behavior__; end"),
    {bar,[]} = elixir:eval("Foo.__behavior__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'ex_callbacks_Foo']).

module_behavior_defines_a_callback_module_test() ->
  F = fun() ->
    elixir:eval("module CallbackSpecificModule; define_module_attribute('behavior, 'bar); end"),
    Name = 'ex_callbacks_CallbackSpecificModule',
    {module, Name} = code:ensure_loaded(Name),
    {Name,[]} = elixir:eval("CallbackSpecificModule.__send__ '__callbacks_module__")
  end,
  test_helper:run_and_remove(F, ['CallbackSpecificModule', 'ex_callbacks_CallbackSpecificModule']).

module_dispatching_to_callback_dispatch_to_ex_module_test() ->
  F = fun() ->
    elixir:eval("module Foo; define_module_attribute('behavior, 'bar); callbacks; def foo; 1; end; def bar(x, y); x - y - self.foo - foo; end; end"),
    1 = 'ex_callbacks_Foo':foo(),
    1 = 'ex_callbacks_Foo':bar(5, 2),
    {1,[]} = elixir:eval("Foo.__send__('foo)"),
    ?assertError({protectedmethod,_}, elixir:eval("Foo.foo"))
  end,
  test_helper:run_and_remove(F, ['Foo', 'ex_callbacks_Foo']).

object_define_behavior_test() ->
  F = fun() ->
    {bar,[]} = elixir:eval("object Foo; def foo;1;end; define_module_attribute('behavior, 'bar); __behavior__; end"),
    {bar,[]} = elixir:eval("Foo::Proto.__behavior__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto', 'ex_callbacks_Foo::Proto']).

object_behavior_defines_a_callback_module_test() ->
  F = fun() ->
    elixir:eval("object CallbackSpecificObject; def foo; 1; end; define_module_attribute('behavior, 'bar); end"),
    Name = 'ex_callbacks_CallbackSpecificObject::Proto',
    {module, Name} = code:ensure_loaded(Name),
    {Name,[]} = elixir:eval("CallbackSpecificObject::Proto.__send__ '__callbacks_module__")
  end,
  test_helper:run_and_remove(F, ['CallbackSpecificObject', 'CallbackSpecificObject::Proto', 'ex_callbacks_CallbackSpecificObject::Proto']).

object_dispatching_to_callback_dispatch_to_ex_module_test() ->
  F = fun() ->
    elixir:eval("object Foo; define_module_attribute('behavior, 'bar); callbacks; def foo; 1; end; def bar(x, y); x - y - self.foo - foo; end; end"),
    1 = 'ex_callbacks_Foo::Proto':foo(),
    1 = 'ex_callbacks_Foo::Proto':bar(5, 2),
    {1,[]} = elixir:eval("Foo::Proto.__send__('foo)"),
    ?assertError({protectedmethod,_}, elixir:eval("Foo::Proto.foo"))
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto', 'ex_callbacks_Foo::Proto']).