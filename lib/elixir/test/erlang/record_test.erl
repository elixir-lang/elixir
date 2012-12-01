-module(record_test).
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, elixir_transform}).

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

record_reader_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { 1, _ } = eval("{ Foo, 1, 2, 3 }.a"),
    { 2, _ } = eval("{ Foo, 1, 2, 3 }.b"),
    { 3, _ } = eval("{ Foo, 1, 2, 3 }.c")
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo']).

record_setter_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { { 'Elixir.Foo', 10, 2, 3 }, _ } = eval("{ Foo, 1, 2, 3 }.a(10)")
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo']).

record_new_defaults_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { { 'Elixir.Foo', 1, 2, 3 }, _ } = eval("Foo.new"),
    { 1, _ } = eval("Foo.new.a")
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo']).

record_new_selective_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { { 'Elixir.Foo', 1, 20, 3 }, _ } = eval("Foo.new b: 20"),
    { 20, _ } = eval("Foo.new(b: 20).b")
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo']).
