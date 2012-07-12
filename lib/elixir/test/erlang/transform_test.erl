-module(transform_test).
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, elixir_transform}).

tranform_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo.Bar.Baz do\ndef sum(a, b) do\na + b\nend\nend", []),
    3 = 'Elixir.Foo.Bar.Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo.Bar.Baz']).