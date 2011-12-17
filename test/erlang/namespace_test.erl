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

namespace_quote_unquote_test() ->
  F = fun() ->
    elixir:eval("ns Foo::Bar::Baz\ndefmacro sum: [a, b], do: quote(unquote(a) + unquote(b))"),
    {'+',2,[1,2]} = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

namespace_operator_macro_test() ->
  F = fun() ->
    elixir:eval("ns Foo::Bar::Baz\ndefmacro +: [a, b], do: quote(unquote(a) - unquote(b))"),
    {'-',2,[1,2]} = '::Foo::Bar::Baz':'+'(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).
  
macro_call_test() ->
  {1,[]} = elixir:eval("if(true, do: 1, else: 2)"),
  {2,[]} = elixir:eval("Elixir::Macros.unless(true, do: 1, else: 2)").

single_ref_test() ->
  { '::Foo', _ } = elixir:eval("Foo").

nested_ref_test() ->
  { '::Foo::Bar::Baz', _ } = elixir:eval("Foo::Bar::Baz").