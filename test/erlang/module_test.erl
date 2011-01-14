-module(module_test).
-include_lib("eunit/include/eunit.hrl").

%% Module functions

module_body_is_executable_test() -> 
  F = fun() ->
    ?assertError({unbound_var, a}, elixir:eval("module Foo; a; end")),
    elixir:eval("module Bar; 1 + 2; end")
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_compiler_precedence_test() -> 
  ?assertError({badmatch, _}, elixir:eval("1 + module Foo; end")).

module_are_converted_into_erlang_modules_test() ->
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

module_with_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; end"),
    ?assertEqual(3, 'Bar':foo(self))
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