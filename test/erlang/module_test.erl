-module(module_test).
-include_lib("eunit/include/eunit.hrl").

module_body_is_executable_test() -> 
  F = fun() ->
    ?assertError({unbound_var, a}, elixir:eval("module Foo; a; end")),
    elixir:eval("module Bar; 1 + 2; end")
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_compiler_precedence_test() -> 
  ?assertError({badmatch, _}, elixir:eval("1 + module Foo; end")).

modules_are_converted_into_erlang_modules_test() ->
  F = fun() ->
    elixir:eval("module Bar; 1 + 2; end"),
    {file, "nofile"} = code:is_loaded('Bar')
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_preceeded_by_other_expressions_test() ->
  F = fun() ->
    elixir:eval("1 + 2\nmodule Bar; 1 + 2; end"),
    {file, "nofile"} = code:is_loaded('Bar')
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_method_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; end"),
    ?assertEqual(3, 'Bar':foo(self))
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_several_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; def bar(); 2 + 3; end; end"),
    ?assertEqual(5, 'Bar':bar(self))
  end,
  test_helper:run_and_remove(F, ['Bar']).

nested_modules_with_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar; module Baz; def foo(); 1 + 2; end; end; end"),
    ?assertEqual(3, 'Bar::Baz':foo(self))
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Baz']).

nested_module_name_with_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar::Baz; def foo(); 1 + 2; end; end"),
    ?assertEqual(3, 'Bar::Baz':foo(self))
  end,
  test_helper:run_and_remove(F, ['Bar::Baz']).

method_invocation_in_module_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; end"),
    ?assertEqual({3,[]}, elixir:eval("Bar.foo"))
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_in_module_with_self_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); self.bar(1) + 3; end; def bar(x); x + 2; end; end"),
    ?assertEqual({6,[]}, elixir:eval("Bar.foo"))
  end,
  test_helper:run_and_remove(F, ['Bar']).
