-module(module_test).
-include_lib("eunit/include/eunit.hrl").

definition_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo::Bar::Baz, do: nil")
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

function_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo::Bar::Baz do\ndef sum(a, b) do\na + b\nend\nend"),
    3 = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

dynamic_function_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo::Bar::Baz do\ndef :sum.(a, b) do\na + b\nend\nend"),
    3 = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

quote_unquote_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo::Bar::Baz do\ndefmacro sum(a, b), do: quote { unquote(a) + unquote(b) }\nend"),
    {'+',0,[1,2]} = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

quote_unquote_splicing_test() ->
  { { '{}', 0, [1,2,3,4,5] }, _ } = elixir:eval("x = [2,3,4]\nquote { { 1, unquote_splicing(x), 5} }").

operator_macro_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo::Bar::Baz do\ndefmacro :+.(a, b), do: quote { unquote(a) - unquote(b) }\nend"),
    {'-',0,[1,2]} = '::Foo::Bar::Baz':'+'(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

curly_call_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo do\ndef ok(x), do: x\nend"),
    {[{do,nil}],_} = elixir:eval("Foo.ok { }"),
    {[{do,1}],_} = elixir:eval("Foo.ok { 1 }"),
    {[{do,3}],_} = elixir:eval("Foo.ok { 1\n2\n3 }"),
    {{1,2},_} = elixir:eval("Foo.ok { 1, 2 }"),
    {[{do,1}],_} = elixir:eval("Foo.ok() { \n1 }"),
    {[{do,3}],_} = elixir:eval("Foo.ok() { \n1\n2\n3 }"),
    {{},_} = elixir:eval("Foo.ok({ })"),
    {{1},_} = elixir:eval("Foo.ok({ 1 })"),
    {{1,2},_} = elixir:eval("Foo.ok({ 1, 2 })")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

def_shortcut_test() ->
  F = fun() ->
    {1,[]} = elixir:eval("defmodule Foo do\ndef version, do: 1\nend\nFoo.version")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

macro_test() ->
  F = fun() ->
    {'::Foo',[]} = elixir:eval("defmodule Foo do\ndef version, do: __MODULE__\nend\nFoo.version"),
    {nil,[]} = elixir:eval("__MODULE__")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

macro_line_test() ->
  F = fun() ->
    ?assertMatch({2, []}, elixir:eval("defmodule Foo do\ndef line, do: __LINE__\nend\nFoo.line")),
    ?assertMatch({1, []}, elixir:eval("__LINE__"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

macro_file_test() ->
  F = fun() ->
    ?assertMatch({<<"nofile">>, []}, elixir:eval("defmodule Foo do\ndef line, do: __FILE__\nend\nFoo.line")),
    ?assertMatch({<<"nofile">>, []}, elixir:eval("__FILE__"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

private_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo do\ndefp version, do: __MODULE__\nend"),
    ?assertError(undef, elixir:eval("Foo.version"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

def_default_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo do\ndef version(x // 1), do: x\nend"),
    ?assertEqual({1, []}, elixir:eval("Foo.version")),
    ?assertEqual({2, []}, elixir:eval("Foo.version(2)"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

def_left_default_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo do\ndef version(x // 1, y), do: x + y\nend"),
    ?assertEqual({4, []}, elixir:eval("Foo.version(3)")),
    ?assertEqual({5, []}, elixir:eval("Foo.version(2, 3)"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

def_with_guard_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo do\ndef v(x) when x < 10, do: true\ndef v(x) when x >= 10, do: false\nend"),
    {true,_} = elixir:eval("Foo.v(0)"),
    {false,_} = elixir:eval("Foo.v(20)")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

do_end_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo do\ndef a, do: 1\ndefmodule Bar do\ndef b, do: 2\nend\ndef c, do: 3\nend"),
    {1,_} = elixir:eval("Foo.a"),
    {2,_} = elixir:eval("Bar.b"),
    {3,_} = elixir:eval("Foo.c")
  end,
  test_helper:run_and_remove(F, ['::Foo', '::Bar']).

single_ref_test() ->
  { '::Foo', _ } = elixir:eval("Foo"),
  { '::Foo', _ } = elixir:eval("::Foo").

nested_ref_test() ->
  { '::Foo::Bar::Baz', _ } = elixir:eval("Foo::Bar::Baz").

dynamic_defmodule_test() ->
  F = fun() ->
    elixir:eval("defmodule Foo do\ndef a(name) do\ndefmodule name, do: (def x, do: 1)\nend\nend"),
    {_,_} = elixir:eval("Foo.a(Bar)"),
    {1,_} = elixir:eval("Bar.x")
  end,
  test_helper:run_and_remove(F, ['::Foo', '::Bar']).

dynamic_ref_test() ->
  { '::Foo::Bar::Baz', _ } = elixir:eval("x = Foo\ny = Bar\nz = :\"Baz\"\nx::y::z").

dynamic_ref_precedence_test() ->
  F = fun() ->
    elixir:eval("defmodule A::Foo, do: def(l, do: A::Foo); defmodule A::Bar, do: def(l(x), do: A::Bar)"),
    {'::A::Foo::B',[]} = elixir:eval("A::Foo.l :: B"),
    {'::A::Foo::B',[]} = elixir:eval("A::Foo.l::B"),
    {'::A::Bar',[]} = elixir:eval("A::Bar.l ::B")
  end,
  test_helper:run_and_remove(F, ['::Foo', '::Bar']).