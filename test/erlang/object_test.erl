% Holds all bootstraping assertions.
-module(object_test).
-include_lib("eunit/include/eunit.hrl").

object_name_test() ->
  {'Object', []} = elixir:eval("Object.__name__").

object_parent_test() ->
  {[], []} = elixir:eval("Object.__parent__").

object_mixins_test() ->
  {['Object::Methods'], []} = elixir:eval("Object.__mixins__").

object_protos_test() ->
  {['Object::Methods'], []} = elixir:eval("Object.__protos__").

object_ancestors_test() ->
  {[], []} = elixir:eval("Object.__ancestors__").

default_self_is_object_test() ->
  {'Object', []} = elixir:eval("__name__").

integer_name_test() ->
  {'Integer', []} = elixir:eval("Integer.__name__").

integer_parent_test() ->
  {'Object', []} = elixir:eval("Integer.__parent__").

integer_mixins_test() ->
  {['Integer::Mixin', 'Object::Methods'], []} = elixir:eval("Integer.__mixins__").

integer_protos_test() ->
  {['Integer::Proto', 'Numeric', 'Object::Methods'], []} = elixir:eval("Integer.__protos__").

integer_ancestors_test() ->
  {['Object'], []} = elixir:eval("Integer.__ancestors__").

integer_instance_name_test() ->
  {[], []} = elixir:eval("1.__name__").

integer_instance_parent_test() ->
  {'Integer', []} = elixir:eval("1.__parent__").

integer_instance_mixins_test() ->
  {['Integer::Proto', 'Numeric', 'Object::Methods'], []} = elixir:eval("1.__mixins__").

integer_instance_protos_test() ->
  {['Integer::Proto', 'Numeric', 'Object::Methods'], []} = elixir:eval("1.__protos__").

integer_instance_ancestors_test() ->
  {['Integer', 'Object'], []} = elixir:eval("1.__ancestors__").

%% Implicit methods
implicit_methods_are_compiled_to_proto_module_test() ->
  F = fun() ->
    elixir:eval("object Bar\ndef foo;1;end\nend"),
    {1,[]} = elixir:eval("Bar.new.foo"),
    {['Bar::Proto', 'Object::Methods'],[]} = elixir:eval("Bar.__protos__")
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).

%% Initialization and Ivars

hash_given_on_initialization_is_used_as_ivars_test() ->
  F = fun() ->
    elixir:eval("object Bar\ndef constructor;{'a: 1, 'b: 2};end\ndef a; @a; end\ndef b; @b; end\ndef c; @c; end\nend"),
    {1,[]}  = elixir:eval("Bar.new.a"),
    {2,[]}  = elixir:eval("Bar.new.b"),
    {[],[]} = elixir:eval("Bar.new.c")
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).

arguments_given_to_new_is_passed_to_constructors_test() ->
  F = fun() ->
    elixir:eval("object Bar\ndef constructor(x, y);{'a: x, 'b: y};end\ndef a; @a + 2; end\ndef b; @b; end\ndef c; @c; end\nend"),
    {3,[]}  = elixir:eval("Bar.new(1,2).a"),
    {2,[]}  = elixir:eval("Bar.new(1,2).b"),
    {[],[]} = elixir:eval("Bar.new(1,2).c")
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).

%% Others

do_not_mixin_modules_twice_test() ->
  F = fun() ->
    {['Bar', 'Object::Methods'], []} = elixir:eval("module Bar; mixin self; mixin self; end\nBar.__mixins__")
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_protos_come_later_than_self_by_default_test() ->
  F = fun() ->
    {['Bar', 'Foo', 'Object::Methods'], []} = elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nBar.__protos__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar']).

add_a_mixin_protos_to_dispatch_chain_test() ->
  F = fun() ->
    {['Baz', 'Bar', 'Foo', 'Object::Methods'], []} =
      elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nmodule Baz; mixin Bar; end\nBaz.__mixins__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz']).

% TODO We need inheritance to handle this one
% do_not_add_protos_twice_to_dispatch_chain_test() ->
%   F = fun() ->
%     {['Bar', 'Foo', 'Object::Methods'], []} =
%       elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nmodule Baz; mixin Bar; end\nBaz.dispatch_chain")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz']).
%
