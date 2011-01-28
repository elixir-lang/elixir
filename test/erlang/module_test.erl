-module(module_test).
-include_lib("eunit/include/eunit.hrl").

module_body_is_executable_test() -> 
  F = fun() ->
    ?assertError({nomethod, _}, elixir:eval("module Foo; a; end")),
    elixir:eval("module Bar; 1 + 2; end")
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_compiler_precedence_test() -> 
  ?assertError({badsyntax, _}, elixir:eval("1 + module Foo; end")).

modules_are_converted_into_erlang_modules_test() ->
  F = fun() ->
    elixir:eval("module Bar; 1 + 2; end"),
    {file, "nofile"} = code:is_loaded('Bar')
  end,
  test_helper:run_and_remove(F, ['Bar']).

blank_modules_are_converted_into_erlang_modules_test() ->
  F = fun() ->
    elixir:eval("module Bar; end"),
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
    3 = 'Bar':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_empty_method_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); end; end"),
    [] = 'Bar':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_several_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; def bar(); 2 + 3; end; end"),
    5 = 'Bar':bar(self)
  end,
  test_helper:run_and_remove(F, ['Bar']).

nested_modules_with_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar; module Baz; def foo(); 1 + 2; end; end; end"),
    3 = 'Bar::Baz':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Baz']).

nested_module_name_with_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar::Baz; def foo(); 1 + 2; end; end"),
    3 = 'Bar::Baz':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar::Baz']).

% Method invocation

method_invocation_in_module_test() ->
  F = fun() ->
    elixir:eval("module Bar; self.mixin self; def foo(); 1 + 2; end; end"),
    {3,[]} = elixir:eval("Bar.foo")
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_in_module_with_eol_test() ->
  F = fun() ->
    elixir:eval("module Bar; self.mixin self; def foo(); 1 + 2; end; end"),
    {3,[]} = elixir:eval("Bar.\nfoo")
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_in_module_with_self_test() ->
  F = fun() ->
    elixir:eval("module Bar; self.mixin self; def foo(); self.bar(1) + 3; end; def bar(x); x + 2; end; end"),
    {6,[]} = elixir:eval("Bar.foo")
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_in_module_with_self_without_parens_args_test() ->
  F = fun() ->
    elixir:eval("module Bar\n def foo(x)\n x + 1\n end\n  end"),
    {7,[]} = elixir:eval("Bar.foo 2 * 3")
  end,
  test_helper:run_and_remove(F, ['Bar']).

% Local calls

local_call_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; foo; end\nend"),
    {1,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_with_args_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo(x);x + 1;end\ndef bar; foo 2; end\nend"),
    {3,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_with_method_call_arg_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo(x);x + 1;end\ndef bar; foo baz; end\ndef baz; 2; end\nend"),
    {3,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_with_method_call_args_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo(x,y);x + y;end\ndef bar; foo baz, baz; end\ndef baz; 2; end\nend"),
    {4,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_gives_higher_preference_to_variables_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; foo = 3; foo; end\nend"),
    {3,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_gives_higher_preference_to_function_calls_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; foo = -> 3; foo(); end\nend"),
    {3,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_inside_a_function_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; baz = -> foo; baz(); end\nend"),
    {1,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_gives_higher_preference_to_function_calls_unless_no_function_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; foo(); end\nend"),
    {1,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_does_not_look_at_outer_modules_test() ->
  F = fun() ->
    ?assertError({undefined_local_method,"nofile:4: undefined local method foo/1"},
      elixir:eval("module Foo; def foo; 1; end; end\nmodule Bar\nmixin Foo\ndef bar; foo(); end\nend"))
  end,
  test_helper:run_and_remove(F, ['Foo','Bar']).

% Module lookup
% cannot_store_already_defined_constants_test() ->
%   F = fun() ->
%     ?assertError({badarg, "Constant 'Foo' is already defined" },
%       elixir:eval("const Foo = 1 + 2; const Foo = 3"))
%   end,
%   test_helper:run_and_remove(F, ['Foo']).

cannot_lookup_not_stored_constants_test() ->
  ?assertError({badarg, "no constant 'FooBarBaz' defined" }, elixir:eval("FooBarBaz")).