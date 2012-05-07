-module(record_test).
-include_lib("eunit/include/eunit.hrl").

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
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

record_setter_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { { '__MAIN__.Foo', 10, 2, 3 }, _ } = eval("{ Foo, 1, 2, 3 }.a(10)")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

record_new_defaults_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { { '__MAIN__.Foo', 1, 2, 3 }, _ } = eval("Foo.new"),
    { 1, _ } = eval("Foo.new.a")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

record_new_selective_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: 2, c: 3"),
    { { '__MAIN__.Foo', 1, 20, 3 }, _ } = eval("Foo.new b: 20"),
    { 20, _ } = eval("Foo.new(b: 20).b")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

record_prepend_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 1, b: [3], c: 3"),
    { { '__MAIN__.Foo', 1, [1,2,3], 3 }, _ } = eval("Foo.new.prepend_b [1,2]")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

record_merge_test() ->
  F = fun() ->
    eval("defrecord Foo, a: [foo: :bar]"),
    { { '__MAIN__.Foo', [{a,1},{foo,baz}] }, _ } = eval("Foo.new.merge_a  [foo: :baz, a: 1]")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

record_increment_test() ->
  F = fun() ->
    eval("defrecord Foo, a: 0"),
    { { '__MAIN__.Foo', 1 }, _ } = eval("Foo.new.increment_a"),
    { { '__MAIN__.Foo', 10 }, _ } = eval("Foo.new.increment_a 10"),
    { { '__MAIN__.Foo', -2 }, _ } = eval("Foo.new.increment_a -2")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).