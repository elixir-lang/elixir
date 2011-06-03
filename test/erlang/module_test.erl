-module(module_test).
-include_lib("eunit/include/eunit.hrl").

module_body_is_executable_test() ->
  F = fun() ->
    ?assertError({nomethod, _}, elixir:eval("module Foo; a; end")),
    elixir:eval("module Bar; 1 + 2; end")
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_compiler_precedence_and_values_test() ->
  F = fun() ->
    {nil,[{foo,nil}]} = elixir:eval("foo = module Foo; end")
  end,
  test_helper:run_and_remove(F, ['Foo']).

invalid_scope_for_module_test() ->
  ?assertError({badsyntax, {1,"nofile","invalid scope for module",[]}},
    elixir:eval("module Foo; def foo; module Bar; end; end; end")).

invalid_scope_for_method_test() ->
  ?assertError({badsyntax, {1,"nofile","invalid scope for method",[]}},
    elixir:eval("def foo; end")),
  ?assertError({badsyntax, {1,"nofile","invalid scope for method",[]}},
    elixir:eval("module Foo; def foo; def bar; end; end; end")).

modules_are_converted_into_erlang_modules_test() ->
  F = fun() ->
    elixir:eval("module Bar; 1 + 2; end"),
    {file, "nofile"} = code:is_loaded('exBar')
  end,
  test_helper:run_and_remove(F, ['Bar']).

blank_modules_are_converted_into_erlang_modules_test() ->
  F = fun() ->
    elixir:eval("module Bar; end"),
    {file, "nofile"} = code:is_loaded('exBar')
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_preceeded_by_other_expressions_test() ->
  F = fun() ->
    elixir:eval("1 + 2\nmodule Bar; 1 + 2; end"),
    {file, "nofile"} = code:is_loaded('exBar')
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_method_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; end"),
    3 = 'exBar':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_method_without_parens_signature_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo x, y; x + y; end; end"),
    3 = 'exBar':foo(self, 1, 2)
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_empty_method_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); end; end"),
    nil = 'exBar':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_with_several_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; def bar(); 2 + 3; end; end"),
    5 = 'exBar':bar(self)
  end,
  test_helper:run_and_remove(F, ['Bar']).

nested_modules_with_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar; module Baz; def foo(); 1 + 2; end; end; end"),
    3 = 'exBar::Baz':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Baz']).

nested_module_name_with_methods_test() ->
  F = fun() ->
    elixir:eval("module Bar::Baz; def foo(); 1 + 2; end; end"),
    3 = 'exBar::Baz':foo(self)
  end,
  test_helper:run_and_remove(F, ['Bar::Baz']).

objectdefined_error_test() ->
  F = fun() ->
    elixir:eval("module Foo; def foo(); 1 + 2; end; end", [], "specialfile", 10),
    ?assertError({objectdefined, {'Foo',<<"specialfile">>,10}}, elixir:eval("module Foo; end"))
  end,
  test_helper:run_and_remove(F, ['Foo']).

% Method invocation

method_invocation_in_module_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; end"),
    {3,[]} = elixir:eval("Bar.foo")
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_in_module_with_eol_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); 1 + 2; end; end"),
    {3,[]} = elixir:eval("Bar.\nfoo")
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_in_module_with_self_test() ->
  F = fun() ->
    elixir:eval("module Bar; def foo(); self.bar(1) + 3; end; def bar(x); x + 2; end; end"),
    {6,[]} = elixir:eval("Bar.foo")
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_in_module_with_self_without_parens_args_test() ->
  F = fun() ->
    elixir:eval("module Bar\n def foo(x)\n x + 1\n end\n  end"),
    {7,[]} = elixir:eval("Bar.foo 2 * 3")
  end,
  test_helper:run_and_remove(F, ['Bar']).

method_invocation_from_mixin_test() ->
  F = fun() ->
    elixir:eval("module Foo;def foo;7;end;end\nmodule Bar;mixin Foo;end"),
    {7,[]} = elixir:eval("Bar.foo")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar']).

cannot_defined_underscore_as_method_test() ->
  ?assertError({badsyntax, _}, elixir:eval("module Bar; def _(); 1 + 2; end; end")).

invalid_method_definition_test() ->
  ?assertError({badsyntax, {1, "nofile", "invalid scope for method", []}}, elixir:eval("def bar; end")).

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

local_call_does_not_conflict_with_fun_calls() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; foo = -> 3; foo(); end\nend"),
    {1,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_inside_a_function_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; baz = -> foo; baz.(); end\nend"),
    {1,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_gives_higher_preference_to_function_calls_unless_no_function_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo;1;end\ndef bar; foo(); end\nend"),
    {1,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_precedence_in_arrays_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo(a,b);[a*2,b];end\ndef bar; [foo 2, 6]; end\nend"),
    {[[4,6]],[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

local_call_not_existing() ->
  F = fun() ->
    ?assertError({badform,{1, "nofile", erl_lint, {undefined_function, _}}},
      elixir:eval("module Foo; def foo; bar(); end; end"))
  end,
  test_helper:run_and_remove(F, ['Foo']).

local_call_does_not_look_at_outer_modules_test() ->
  F = fun() ->
      {1, []} = elixir:eval("module Foo; def foo; 1; end; end\nmodule Bar\nmixin Foo\ndef bar; foo(); end\nend\nBar.bar")
  end,
  test_helper:run_and_remove(F, ['Foo','Bar']).

cannot_lookup_not_stored_constants_test() ->
  ?assertError({noconstant, 'FooBarBaz' }, elixir:eval("FooBarBaz")).

reserved_modules_test() ->
  ?assertError({reservedmodulename, 'Example::Mixin'}, elixir:eval("module Example::Mixin; end")),
  ?assertError({reservedmodulename, 'Example::Proto'}, elixir:eval("module Example::Proto; end")).

%% Super

super_call_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef send(method);super(method)+1;end\ndef bar; 3; end\nend"),
    {4,[]} = elixir:eval("Bar.send 'bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

super_with_method_missing_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef bar;super + 1;end\ndef method_missing('bar, []); 3; end\nend"),
    {4,[]} = elixir:eval("Bar.bar")
  end,
  test_helper:run_and_remove(F, ['Bar']).

invalid_super_call_test() ->
  ?assertError({badsyntax, {1, "nofile", "invalid scope for super", []}}, elixir:eval("super")).

%% Callbacks

added_as_mixin_callback_test() ->
  F = fun() ->
    elixir:eval("module Foo; def __added_as_mixin__(base); base.set_ivar('foo, 2); end; end"),
    {2,[]} = elixir:eval("object Bar; mixin Foo; @foo; end")
  end,
  test_helper:run_and_remove(F, ['Foo','Bar']).

%% Module Methods

can_retrieve_visibility_test() ->
  F = fun() ->
    {private,[]} = elixir:eval("module Foo; private; __visibility__; end")
  end,
  test_helper:run_and_remove(F, ['Foo']).

can_retrieve_mixins_without_duplication_test() ->
  F = fun() ->
    {['Module::Methods', 'Object::Methods'],[]} = elixir:eval("module Foo; __mixins__; end"),
    {['Foo', 'Module::Methods', 'Object::Methods'],[]} = elixir:eval("Foo.__mixins__")
  end,
  test_helper:run_and_remove(F, ['Foo']).