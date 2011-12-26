-module(record_test).
-include_lib("eunit/include/eunit.hrl").

record_reader_test() ->
  F = fun() ->
    elixir:eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { 1, _ } = elixir:eval("{ Foo, 1, 2, 3 }.a"),
    { 2, _ } = elixir:eval("{ Foo, 1, 2, 3 }.b"),
    { 3, _ } = elixir:eval("{ Foo, 1, 2, 3 }.c")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

record_setter_test() ->
  F = fun() ->
    elixir:eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { { '::Foo', 10, 2, 3 }, _ } = elixir:eval("{ Foo, 1, 2, 3 }.a(10)")
  end,
  test_helper:run_and_remove(F, ['::Foo']).
