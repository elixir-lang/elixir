-module(namespace_test).
-include_lib("eunit/include/eunit.hrl").

namespace_definition_test() ->
  F = fun() ->
    elixir:eval("ns Foo::Bar::Baz")
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

namespace_method_test() ->
  F = fun() ->
    elixir:eval("ns Foo::Bar::Baz\ndef sum: [a, b] do\na + b\nend"),
    3 = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

namespace_macro_test() ->
  F = fun() ->
    elixir:eval("ns Foo::Bar::Baz\ndefmacro sum: [a, b] do\nquote(a + b)\nend"),
    {'+',3,[{a,3,false},{b,3,false}]} = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

single_ref_test() ->
  { '::Foo', _ } = elixir:eval("Foo").

nested_ref_test() ->
  { '::Foo::Bar::Baz', _ } = elixir:eval("Foo::Bar::Baz").