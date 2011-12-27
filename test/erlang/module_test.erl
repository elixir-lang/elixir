-module(module_test).
-include_lib("eunit/include/eunit.hrl").

definition_test() ->
  F = fun() ->
    elixir:eval("module Foo::Bar::Baz")
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

function_test() ->
  F = fun() ->
    elixir:eval("module Foo::Bar::Baz\ndef sum(a, b) do\na + b\nend"),
    3 = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

dynamic_function_test() ->
  F = fun() ->
    elixir:eval("module Foo::Bar::Baz\ndef :sum.(a, b) do\na + b\nend"),
    3 = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

quote_unquote_test() ->
  F = fun() ->
    elixir:eval("module Foo::Bar::Baz\ndefmacro sum(a, b), do: quote { unquote(a) + unquote(b) }"),
    {'+',0,[1,2]} = '::Foo::Bar::Baz':sum(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

quote_unquote_splice_test() ->
  { { '{}', 0, [1,2,3,4,5] }, _ } = elixir:eval("x = [2,3,4]\nquote { { 1, unquote_splice(x), 5} }").

operator_macro_test() ->
  F = fun() ->
    elixir:eval("module Foo::Bar::Baz\ndefmacro +(a, b), do: quote { unquote(a) - unquote(b) }"),
    {'-',0,[1,2]} = '::Foo::Bar::Baz':'+'(1, 2)
  end,
  test_helper:run_and_remove(F, ['::Foo::Bar::Baz']).

curly_call_test() ->
  F = fun() ->
    elixir:eval("module Foo\ndef ok(x), do: x"),
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

def_shortcut_and_endns_test() ->
  F = fun() ->
    {1,[]} = elixir:eval("module Foo\ndef version, do: 1\nendmodule\nFoo.version")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

macro_test() ->
  F = fun() ->
    {'::Foo',[]} = elixir:eval("module Foo\ndef version, do: __MODULE__\nendmodule\nFoo.version"),
    {nil,[]} = elixir:eval("__MODULE__")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

macro_line_test() ->
  F = fun() ->
    ?assertMatch({2, []}, elixir:eval("module Foo\ndef line, do: __LINE__\nendmodule\nFoo.line")),
    ?assertMatch({1, []}, elixir:eval("__LINE__"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

macro_file_test() ->
  F = fun() ->
    ?assertMatch({<<"nofile">>, []}, elixir:eval("module Foo\ndef line, do: __FILE__\nendmodule\nFoo.line")),
    ?assertMatch({<<"nofile">>, []}, elixir:eval("__FILE__"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

private_test() ->
  F = fun() ->
    elixir:eval("module Foo\n private\ndef version, do: __MODULE__\n"),
    ?assertError(undef, elixir:eval("Foo.version"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

public_test() ->
  F = fun() ->
    elixir:eval("module Foo\nprivate\npublic\ndef version, do: __MODULE__\n"),
    ?assertEqual({'::Foo', []}, elixir:eval("Foo.version"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

def_default_test() ->
  F = fun() ->
    elixir:eval("module Foo\ndef version(x // 1), do: x\n"),
    ?assertEqual({1, []}, elixir:eval("Foo.version")),
    ?assertEqual({2, []}, elixir:eval("Foo.version(2)"))
  end,
  test_helper:run_and_remove(F, ['::Foo']).

def_with_guard_test() ->
  F = fun() ->
    elixir:eval("module Foo\ndef v(x) when x < 10, do: true\ndef v(x) when x >= 10, do: false\n"),
    {true,_} = elixir:eval("Foo.v(0)"),
    {false,_} = elixir:eval("Foo.v(20)")
  end,
  test_helper:run_and_remove(F, ['::Foo']).

do_end_test() ->
  F = fun() ->
    elixir:eval("module Foo\ndef a, do: 1\nmodule Bar do\ndef b, do: 2\nend\ndef c, do: 3"),
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

dynamic_ref_test() ->
  { '::Foo::Bar::Baz', _ } = elixir:eval("x = Foo\ny = Bar\nz = :\"Baz\"\nx::y::z"),
  { '::Foo::Bar::Baz', _ } = elixir:eval("x = Foo\ny = Bar\nz = :\"Baz\"\n::(x, y, z)").

dynamic_ref_precedence_test() ->
  F = fun() ->
    elixir:eval("module A::Foo; def l, do: A::Foo; module A::Bar; def l(x), do: A::Bar;"),
    {'::A::Foo::B',[]} = elixir:eval("A::Foo.l :: B"),
    {'::A::Foo::B',[]} = elixir:eval("A::Foo.l::B"),
    {'::A::Bar',[]} = elixir:eval("A::Bar.l ::B")
  end,
  test_helper:run_and_remove(F, ['::Foo', '::Bar']).