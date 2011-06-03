% Holds all bootstraping assertions.
-module(object_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% object_name_test() ->
%   {'Object', []} = elixir:eval("Object.__name__").
% 
% object_parent_test() ->
%   {nil, []} = elixir:eval("Object.__parent__").
% 
% object_mixins_test() ->
%   {['Object::Methods'], []} = elixir:eval("Object.__mixins__").
% 
% object_protos_test() ->
%   {['Object::Methods'], []} = elixir:eval("Object.__protos__").
% 
% object_ancestors_test() ->
%   {[], []} = elixir:eval("Object.__ancestors__").
% 
% default_self_is_object_test() ->
%   {'Object', []} = elixir:eval("__name__").
% 
% integer_name_test() ->
%   {'Integer', []} = elixir:eval("Integer.__name__").
% 
% integer_parent_test() ->
%   {'Object', []} = elixir:eval("Integer.__parent_name__").
% 
% integer_mixins_test() ->
%   {['Integer::Mixin', 'Object::Methods'], []} = elixir:eval("Integer.__mixins__").
% 
% integer_protos_test() ->
%   {['Integer::Proto', 'Numeric', 'Object::Methods'], []} = elixir:eval("Integer.__protos__").
% 
% integer_ancestors_test() ->
%   {['Object'], []} = elixir:eval("Integer.__ancestors__").
% 
% integer_instance_name_test() ->
%   {nil, []} = elixir:eval("1.__name__").
% 
% integer_instance_parent_test() ->
%   {'Integer', []} = elixir:eval("1.__parent_name__").
% 
% integer_instance_mixins_test() ->
%   {['Integer::Proto', 'Numeric', 'Object::Methods'], []} = elixir:eval("1.__mixins__").
% 
% integer_instance_protos_test() ->
%   {['Integer::Proto', 'Numeric', 'Object::Methods'], []} = elixir:eval("1.__protos__").
% 
% integer_instance_ancestors_test() ->
%   {['Integer', 'Object'], []} = elixir:eval("1.__ancestors__").
% 
% %% Implicit methods
% 
% implicit_methods_are_compiled_to_proto_module_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef foo;1;end\nend"),
%     {1,[]} = elixir:eval("Bar.new.foo"),
%     {['Bar::Proto', 'Object::Methods'],[]} = elixir:eval("Bar.__protos__")
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
% 
% %% Initialization and Ivars
% 
% hash_given_on_initialization_is_used_as_ivars_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef initialize;@('a: 1, 'b: 2);end\ndef a; @a; end\ndef b; @b; end\ndef c; @c; end\nend"),
%     {1,[]}  = elixir:eval("Bar.new.a"),
%     {2,[]}  = elixir:eval("Bar.new.b"),
%     {nil,[]} = elixir:eval("Bar.new.c")
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
% 
% arguments_given_to_new_is_passed_to_constructors_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef initialize(x, y);@('a: x, 'b: y);end\ndef a; @a + 2; end\ndef b; @b; end\ndef c; @c; end\nend"),
%     {3,[]}  = elixir:eval("Bar.new(1,2).a"),
%     {2,[]}  = elixir:eval("Bar.new(1,2).b"),
%     {nil,[]} = elixir:eval("Bar.new(1,2).c")
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
% 
% invalid_hash_on_construction_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef initialize;@(1: 2);end\nend"),
%     ?assertError({badivars, {elixir_orddict__, [{1,2}]}}, elixir:eval("Bar.new"))
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
% 
% not_a_hash_on_construction_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef initialize;@('a);end\nend"),
%     ?assertError({badivars, a}, elixir:eval("Bar.new"))
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
% 
% not_an_object_on_initialize_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef initialize;1;end\nend"),
%     ?assertError({badinitialize, 1}, elixir:eval("Bar.new"))
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
% 
% %% Others
% 
% do_not_mixin_modules_twice_test() ->
%   F = fun() ->
%     {['Bar', 'Module::Methods', 'Object::Methods'], []} = elixir:eval("module Bar; mixin self; mixin self; end\nBar.__mixins__")
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% add_mixins_to_dispatch_chain_test() ->
%   F = fun() ->
%     {['Baz', 'Bar', 'Foo', 'Module::Methods', 'Object::Methods'], []} =
%       elixir:eval("module Foo; end\nmodule Bar; mixin Foo; end\nmodule Baz; mixin Bar; end\nBaz.__mixins__")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz']).
% 
% do_not_add_mixins_twice_to_dispatch_chain_test() ->
%   F = fun() ->
%     {['Baz::Mixin', 'Bar', 'Foo', 'Object::Methods'], []} =
%       elixir:eval("module Foo; end\nmodule Bar; mixin Foo; end\nobject Baz; mixin Bar; mixin Foo; end\nBaz.__mixins__")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz', 'Baz::Mixin']).
% 
% adds_module_methods_to_mixins_inside_the_class_test() ->
%   F = fun() ->
%     {['Bar', 'Foo', 'Module::Methods', 'Object::Methods'], []} =
%       elixir:eval("module Foo; end\nmodule Bar; end\nobject Baz; mixin Foo; mixin Bar; __mixins__; end")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Bar', 'Baz', 'Baz::Mixin']).
% 
% %% Ivars test
% 
% ivars_at_the_mixin_level_test() ->
%   F = fun() ->
%     {nil, []} = elixir:eval("module Foo; get_ivar('bar); end"),
%     {bar, []} = elixir:eval("module Bar; set_ivar('foo, 'bar); get_ivar('foo); end")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Bar']).
% 
% ivars_at_the_proto_level_test() ->
%   F = fun() ->
%     elixir:eval("object Foo; def get; self.get_ivar('foo); end; def set; self.set_ivar('foo, 'bar); end; end"),
%     {nil, []} = elixir:eval("Foo.new.get"),
%     {bar, _} = elixir:eval("Foo.new.set.get")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% %% Visibility
% 
% can_retrieve_visibility_test() ->
%   F = fun() ->
%     {private,[]} = elixir:eval("object Foo; private; __visibility__; end")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% private_methods_cannot_be_invoked_test() ->
%   F = fun() ->
%     elixir:eval("object Foo; private; def foo; 1; end; end"),
%     ?assertError({nomethod,{foo,0,_}}, elixir:eval("Foo.new.foo"))
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% proto_methods_test() ->
%   F = fun() ->
%     elixir:eval("object Foo; def foo; end; private; def bar; end; end"),
%     {true, _}  = elixir:eval("Foo.__proto_methods__.member?({'foo,0})"),
%     {true, _}  = elixir:eval("Foo.__proto_methods__.member?({'new,1})"),
%     {false, _} = elixir:eval("Foo.__proto_methods__.member?({'module_info,0})")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% send_public_method_test() ->
%   F = fun() ->
%     elixir:eval("object Foo; def foo; 1; end; def foo(x); x * 2; end; end"),
%     {1, _}  = elixir:eval("Foo.new.send('foo)"),
%     {2, _}  = elixir:eval("Foo.new.send('foo, [1])")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% % send_protected_method_test() ->
% %   F = fun() ->
% %     elixir:eval("object Foo; def bar; self.send('baz); end; protected; def baz; 3; end; end"),
% %     {3, _}  = elixir:eval("Foo.new.send('bar)"),
% %     {3, _}  = elixir:eval("Foo.new.send('baz)")
% %   end,
% %   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% %% alias_local
% 
% alias_local_test() ->
%   F = fun() ->
%     elixir:eval("object Foo; def bar; 1; end; alias_local 'bar, 'baz, 0; end"),
%     {1,[]} = elixir:eval("Foo.new.bar"),
%     {1,[]} = elixir:eval("Foo.new.baz")
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% alias_local_keeps_visibility_test() ->
%   F = fun() ->
%     elixir:eval("object Foo; def foo; baz; end; private; def bar; 1; end; alias_local 'bar, 'baz, 0; end"),
%     {1,[]} = elixir:eval("Foo.new.foo"),
%     ?assertError({nomethod, _}, elixir:eval("Foo.new.baz"))
%   end,
%   test_helper:run_and_remove(F, ['Foo', 'Foo::Proto']).
% 
% %% __LINE__ and __FILE__
% 
% line_underscore_test() ->
%   {2, _} = elixir:eval("\na = __LINE__\na").
% 
% file_underscore_test() ->
%   {"nofile", _} = elixir:eval("__FILE__.to_char_list"),
%   {"another.ex", _} = elixir:eval("__FILE__.to_char_list", [], "another.ex").
% 
% %% to_s and inspect
% 
% eval_string(Expr) ->
%   { String, Binding } = elixir:eval(Expr),
%   { test_helper:unpack_string(String), Binding }.
% 
% to_s_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef initialize(x);@('a: x);end\nend"),
%     {<<"Bar">>,[]}  = eval_string("Bar.to_s"),
%     {<<"<Bar {'a: 1}>">>,[]}  = eval_string("Bar.new(1).to_s")
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
% 
% inspect_test() ->
%   F = fun() ->
%     elixir:eval("object Bar\ndef initialize(x);@('a, x);end\nend"),
%     {<<"Bar">>,[]}  = eval_string("Bar.inspect"),
%     {<<"<Bar {'a: 1}>">>,[]}  = eval_string("Bar.new(1).inspect")
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Bar::Proto']).
