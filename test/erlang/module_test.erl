-module(module_test).
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

definition_test() ->
  F = fun() ->
    eval("defmodule Foo.Bar.Baz, do: nil")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo.Bar.Baz']).

function_test() ->
  F = fun() ->
    eval("defmodule Foo.Bar.Baz do\ndef sum(a, b) do\na + b\nend\nend"),
    3 = '__MAIN__.Foo.Bar.Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo.Bar.Baz']).

dynamic_function_test() ->
  F = fun() ->
    eval("defmodule Foo.Bar.Baz do\ndef :sum.(a, b) do\na + b\nend\nend"),
    3 = '__MAIN__.Foo.Bar.Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo.Bar.Baz']).

quote_unquote_test() ->
  F = fun() ->
    eval("defmodule Foo.Bar.Baz do\ndefmacro sum(a, b), do: quote(do: unquote(a) + unquote(b))\nend"),
    {'+',0,[1,2]} = '__MAIN__.Foo.Bar.Baz':'MACRO-sum'(1, 2)
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo.Bar.Baz']).

quote_unquote_splicing_test() ->
  { { '{}', 0, [1,2,3,4,5] }, _ } = eval("x = [2,3,4]\nquote do: { 1, unquote_splicing(x), 5}").

operator_macro_test() ->
  F = fun() ->
    eval("defmodule Foo.Bar.Baz do\ndefmacro :+.(a, b), do: quote(do: unquote(a) - unquote(b))\nend"),
    {'-',0,[1,2]} = '__MAIN__.Foo.Bar.Baz':'MACRO-+'(1, 2)
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo.Bar.Baz']).

def_shortcut_test() ->
  F = fun() ->
    {1,[]} = eval("defmodule Foo do\ndef version, do: 1\nend\nFoo.version")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

macro_test() ->
  F = fun() ->
    {'__MAIN__.Foo',[]} = eval("defmodule Foo do\ndef version, do: __MODULE__\nend\nFoo.version"),
    {nil,[]} = eval("__MODULE__")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

macro_line_test() ->
  F = fun() ->
    ?assertMatch({2, []}, eval("defmodule Foo do\ndef line, do: __ENV__.line\nend\nFoo.line")),
    ?assertMatch({1, []}, eval("__ENV__.line"))
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

macro_file_test() ->
  F = fun() ->
    ?assertMatch({<<"nofile">>, []}, eval("defmodule Foo do\ndef line, do: __FILE__\nend\nFoo.line")),
    ?assertMatch({<<"nofile">>, []}, eval("__FILE__"))
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

private_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndefp version, do: __MODULE__\nend"),
    ?assertError(undef, eval("Foo.version"))
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

def_default_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef version(x // 1), do: x\nend"),
    ?assertEqual({1, []}, eval("Foo.version")),
    ?assertEqual({2, []}, eval("Foo.version(2)"))
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

def_left_default_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef version(x // 1, y), do: x + y\nend"),
    ?assertEqual({4, []}, eval("Foo.version(3)")),
    ?assertEqual({5, []}, eval("Foo.version(2, 3)"))
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

def_with_guard_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef v(x) when x < 10, do: true\ndef v(x) when x >= 10, do: false\nend"),
    {true,_} = eval("Foo.v(0)"),
    {false,_} = eval("Foo.v(20)")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo']).

do_end_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef a, do: 1\ndefmodule Bar do\ndef b, do: 2\nend\ndef c, do: 3\nend"),
    {1,_} = eval("Foo.a"),
    {2,_} = eval("Foo.Bar.b"),
    {3,_} = eval("Foo.c")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo', '__MAIN__.Foo.Bar']).

nesting_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndefmodule __MAIN__.Bar do\ndef b, do: 2\nend\nend"),
    {2,_} = eval("Bar.b")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo', '__MAIN__.Bar']).

dot_ref_test() ->
  { '__MAIN__.Foo.Bar.Baz', _ } = eval("Foo.Bar.Baz").

dot_dyn_ref_test() ->
  { '__MAIN__.Foo.Bar.Baz', _ } = eval("a = Foo.Bar; a.Baz").

single_ref_test() ->
  { '__MAIN__.Foo', _ } = eval("Foo"),
  { '__MAIN__.Foo', _ } = eval("__MAIN__.Foo").

nested_ref_test() ->
  { '__MAIN__.Foo.Bar.Baz', _ } = eval("Foo.Bar.Baz").

dynamic_defmodule_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef a(name) do\ndefmodule name, do: (def x, do: 1)\nend\nend"),
    {_,_} = eval("Foo.a(Bar)"),
    {1,_} = eval("Bar.x")
  end,
  test_helper:run_and_remove(F, ['__MAIN__.Foo', '__MAIN__.Bar']).