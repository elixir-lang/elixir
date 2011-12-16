-module(namespace_test).
-include_lib("eunit/include/eunit.hrl").

namespace_definition_test() ->
  F = fun() ->
    elixir:eval("ns Foo::Bar::Baz")
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

single_ref_test() ->
  { '::Foo', _ } = elixir:eval("Foo").

nested_ref_test() ->
  { '::Foo::Bar::Baz', _ } = elixir:eval("Foo::Bar::Baz").