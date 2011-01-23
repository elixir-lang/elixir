% Holds all bootstraping assertions.
-module(object_model_test).
-include_lib("eunit/include/eunit.hrl").

object_mixins_test() ->
  {['Object::Methods'], []} = elixir:eval("Object.mixins").

object_protos_test() ->
  {['Object::Methods'], []} = elixir:eval("Object.protos").

object_ancestors_test() ->
  {[], []} = elixir:eval("Object.ancestors").

object_dispatch_chain_test() ->
  {['Object::Methods'], []} = elixir:eval("Object.dispatch_chain").

integer_mixins_test() ->
  {['Integer::Mixin'], []} = elixir:eval("Integer.mixins").

integer_protos_test() ->
  {['Integer::Proto', 'Numeric'], []} = elixir:eval("Integer.protos").

integer_ancestors_test() ->
  {['Object'], []} = elixir:eval("Integer.ancestors").

integer_dispatch_chain_test() ->
  {['Integer::Mixin', 'Object::Methods'], []} = elixir:eval("Integer.dispatch_chain").

integer_instance_mixins_test() ->
  {[], []} = elixir:eval("1.mixins").

integer_instance_protos_test() ->
  {[], []} = elixir:eval("1.protos").

integer_instance_ancestors_test() ->
  {['Integer', 'Object'], []} = elixir:eval("1.ancestors").

integer_instance_dispatch_chain_test() ->
  {['Integer::Proto', 'Numeric', 'Object::Methods'], []} = elixir:eval("1.dispatch_chain").

%% Others

do_not_mixin_modules_twice_test() ->
  F = fun() ->
    {['Bar'], []} = elixir:eval("module Bar; mixin self; mixin self; end\nBar.mixins")
  end,
  test_helper:run_and_remove(F, ['Bar']).

module_protos_come_later_than_self_by_default_test() ->
  F = fun() ->
    {['Bar', 'Foo'], []} = elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nBar.protos")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar']).

add_a_mixin_protos_to_dispatch_chain_test() ->
  F = fun() ->
    {['Bar', 'Foo', 'Object::Methods'], []} =
      elixir:eval("module Foo; end\nmodule Bar; proto Foo; end\nmodule Baz; mixin Bar; end\nBaz.dispatch_chain")
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
