% Holds all bootstraping assertions.
-module(object_test).
-include("elixir.hrl").
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

%% Inheritance

% inheritance_test() ->
%   F = fun() ->
%     elixir:eval("object Foo\nmodule Mixin; def foo; 'mixin; end; end\ndef foo; 'proto ;end\nend\n"
%       "object Bar < Foo\nmodule Mixin; def bar; 'mixin; end; end\ndef bar; 'proto; end\nend"),
%     {mixin,[]} = elixir:eval("Bar.foo"),
%     {proto,[]} = elixir:eval("Bar.new.foo"),
%     {mixin,[]} = elixir:eval("Bar.bar"),
%     {proto,[]} = elixir:eval("Bar.new.bar"),
%     {['Bar::Mixin', 'Foo::Mixin', 'Object::Methods'],[]} = elixir:eval("Bar.__mixins__"),
%     {['Bar::Proto', 'Foo::Proto', 'Object::Methods'],[]} = elixir:eval("Bar.__protos__")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Mixin', 'Foo::Proto', 'Bar', 'Bar::Mixin', 'Bar::Proto']).
% 
% cannot_inherit_from_a_module_test() ->
%   F = fun() ->
%     ?assertError({badarg, "cannot inherit from module Foo"}, elixir:eval("module Foo; end\nobject Bar < Foo; end"))
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Bar']).
% 
% ivars_inheritance_test() ->
%   F = fun() ->
%     elixir:eval("object Foo; set_ivar('foo, 'bar); end"),
%     {bar, []} = elixir:eval("object Bar < Foo; get_ivar('foo); end")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Bar']).

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

invalid_hash_on_construction_test() ->
  F = fun() ->
    elixir:eval("object Bar\ndef constructor;{1: 2};end\nend"),
    Error = "constructor needs to return a OrderedDict with all keys as symbols, got {1: 2}",
    ?assertError({badarg, Error}, elixir:eval("Bar.new"))
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).

not_a_hash_on_construction_test() ->
  F = fun() ->
    elixir:eval("object Bar\ndef constructor;'a;end\nend"),
    Error = "constructor needs to return a OrderedDict, got 'a",
    ?assertError({badarg, Error}, elixir:eval("Bar.new"))
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).

%% Others

do_not_mixin_modules_twice_test() ->
  F = fun() ->
    {['Bar', 'Module::Methods', 'Object::Methods'], []} = elixir:eval("module Bar; mixin self; mixin self; end\nBar.__mixins__")
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_protos_come_later_than_self_by_default_test() ->
  F = fun() ->
    {['Bar', 'Foo', 'Module::Methods', 'Object::Methods'], []} = elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nBar.__protos__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar']).

add_a_mixin_protos_to_dispatch_chain_test() ->
  F = fun() ->
    {['Baz', 'Bar', 'Foo', 'Module::Methods', 'Object::Methods'], []} =
      elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nmodule Baz; mixin Bar; end\nBaz.__mixins__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz']).

do_not_add_protos_twice_to_dispatch_chain_test() ->
  F = fun() ->
    {['Baz', 'Bar', 'Foo', 'Module::Methods', 'Object::Methods'], []} =
      elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nmodule Baz; proto Bar; proto Foo; end\nBaz.__protos__")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz']).

adds_module_methods_to_mixins_inside_the_class_test() ->
  F = fun() ->
    {['Bar', 'Foo', 'Module::Methods', 'Object::Methods'], []} =
      elixir:eval("module Foo; end\nmodule Bar; end\nobject Baz; mixin Foo; mixin Bar; __mixins__; end")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz']).

%% Ivars test

ivars_at_the_mixin_level_test() ->
  F = fun() ->
    {[], []} = elixir:eval("module Foo; get_ivar('bar); end"),
    {bar, []} = elixir:eval("module Bar; set_ivar('foo, 'bar); get_ivar('foo); end")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar']).

ivars_at_the_proto_level_test() ->
  F = fun() ->
    elixir:eval("object Foo; def get; self.get_ivar('foo); end; def set; self.set_ivar('foo, 'bar); end; end"),
    {[], []} = elixir:eval("Foo.new.get"),
    {bar, _} = elixir:eval("Foo.new.set.get")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

%% Visibility

can_retrieve_visibility_test() ->
  F = fun() ->
    {private,[]} = elixir:eval("object Foo; private; __visibility__; end")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

private_methods_cannot_be_invoked_test() ->
  F = fun() ->
    elixir:eval("object Foo; private; def foo; 1; end; end"),
    ?assertError({nomethod, "No method foo/0 in mixins [Foo::Proto, Object::Methods]"}, elixir:eval("Foo.new.foo"))
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

protected_methods_can_be_invoked_in_their_own_scope_test() ->
  F = fun() ->
    elixir:eval("object Foo; def foo(obj); obj.baz; end; def bar; self.baz; end; protected; def baz; 1; end; end"),
    {1,[]} = elixir:eval("Foo.new.bar"),
    {1,[]} = elixir:eval("Foo.new.foo(Foo.new)"),
    ?assertError({protectedmethod, "Cannot invoke protected method baz/0 in mixin Foo::Proto"}, elixir:eval("Foo.new.baz"))
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

public_proto_methods_test() ->
  F = fun() ->
    elixir:eval("object Foo; def foo; end; private; def bar; end; end"),
    {true, _}  = elixir:eval("Foo.__public_proto_methods__.member?({'foo,0})"),
    {true, _}  = elixir:eval("Foo.__public_proto_methods__.member?({'new,1})"),
    {false, _} = elixir:eval("Foo.__public_proto_methods__.member?({'module_info,0})")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

send_method_test() ->
  F = fun() ->
    elixir:eval("object Foo; def foo; 1; end; def foo(x); x * 2; end; end"),
    {1, _}  = elixir:eval("Foo.new.send('foo)"),
    {2, _}  = elixir:eval("Foo.new.send('foo, [1])")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

%% alias_local

alias_local_test() ->
  F = fun() ->
    elixir:eval("object Foo; def bar; 1; end; alias_local 'bar, 'baz, 0; end"),
    {1,[]} = elixir:eval("Foo.new.bar"),
    {1,[]} = elixir:eval("Foo.new.baz")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

alias_local_keeps_visibility_test() ->
  F = fun() ->
    elixir:eval("object Foo; def foo; baz; end; private; def bar; 1; end; alias_local 'bar, 'baz, 0; end"),
    {1,[]} = elixir:eval("Foo.new.foo"),
    ?assertError({nomethod, _}, elixir:eval("Foo.new.baz"))
  end,
  test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).

%% Catch test

no_error_catch_test() ->
  {2, []} = elixir:eval("Object.catch -> 1 + 1").

error_catch_test() ->
  {{'EXIT',{{badmatch,2},[]}}, []} = elixir:eval("Object.catch -> 1 = 2"),
  {{'EXIT',{function_clause,[{'Object::Methods','catch',1}]}}, []} = elixir:eval("Object.catch -> [].map(1)").

%% __LINE__ and __FILE__

line_underscore_test() ->
  {2, _} = elixir:eval("\na = __LINE__\na").

file_underscore_test() ->
  {"nofile", _} = elixir:eval("__FILE__.to_char_list"),
  {"another.ex", _} = elixir:eval("__FILE__.to_char_list", [], "another.ex").

%% to_s and inspect

eval_string(Expr) ->
  { String, Binding } = elixir:eval(Expr),
  { test_helper:unpack_string(String), Binding }.

to_s_test() ->
  F = fun() ->
    elixir:eval("object Bar\ndef constructor(x);{'a: x};end\nend"),
    {<<"Bar">>,[]}  = eval_string("Bar.to_s"),
    {<<"<Bar {'a: 1}>">>,[]}  = eval_string("Bar.new(1).to_s")
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).

inspect_test() ->
  F = fun() ->
    elixir:eval("object Bar\ndef constructor(x);{'a: x};end\nend"),
    {<<"Bar">>,[]}  = eval_string("Bar.inspect"),
    {<<"<Bar {'a: 1}>">>,[]}  = eval_string("Bar.new(1).inspect")
  end,
  test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
