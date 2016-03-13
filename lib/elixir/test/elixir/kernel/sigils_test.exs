Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.SigilsTest do
  use ExUnit.Case, async: true

  test "sigil s" do
    assert ~s(foo) == "foo"
    assert ~s(f#{:o}o) == "foo"
    assert ~s(f\no) == "f\no"
  end

  test "sigil s with heredoc" do
    assert "  foo\n\n" == ~s"""
      f#{:o}o\n
    """
  end

  test "sigil S" do
    assert ~S(foo) == "foo"
    assert ~S[foo] == "foo"
    assert ~S{foo} == "foo"
    assert ~S'foo' == "foo"
    assert ~S"foo" == "foo"
    assert ~S<foo> == "foo"
    assert ~S/foo/ == "foo"
    assert ~S|foo| == "foo"
    assert ~S(f#{o}o) == "f\#{o}o"
    assert ~S(f\#{o}o) == "f\\\#{o}o"
    assert ~S(f\no) == "f\\no"
    assert ~S(foo\)) == "foo)"
    assert ~S[foo\]] == "foo]"
    assert ~S(foo\
bar) == "foo\\\nbar"
  end

  test "sigil S with heredoc" do
    assert "  f\#{o}o\\n\n" == ~S"""
      f#{o}o\n
    """
  end

  test "sigil s/S expand to binary when possible" do
    assert Macro.expand(quote(do: ~s(foo)), __ENV__) == "foo"
    assert Macro.expand(quote(do: ~S(foo)), __ENV__) == "foo"
  end

  test "sigil c" do
    assert ~c(foo) == 'foo'
    assert ~c(f#{:o}o) == 'foo'
    assert ~c(f\no) == 'f\no'
  end

  test "sigil C" do
    assert ~C(foo) == 'foo'
    assert ~C[foo] == 'foo'
    assert ~C{foo} == 'foo'
    assert ~C'foo' == 'foo'
    assert ~C"foo" == 'foo'
    assert ~C|foo| == 'foo'
    assert ~C(f#{o}o) == 'f\#{o}o'
    assert ~C(f\no) == 'f\\no'
  end

  test "sigil w" do
    assert ~w() == []
    assert ~w(foo bar baz) == ["foo", "bar", "baz"]
    assert ~w(foo #{:bar} baz) == ["foo", "bar", "baz"]

    assert ~w(foo\ #{:bar}) == ["foo", "bar"]
    assert ~w(foo\ bar) == ["foo", "bar"]

    assert ~w(
      foo
      bar
      baz
    ) == ["foo", "bar", "baz"]

    assert ~w(foo bar baz)s == ["foo", "bar", "baz"]
    assert ~w(foo bar baz)a == [:foo, :bar, :baz]
    assert ~w(foo bar baz)c == ['foo', 'bar', 'baz']

    bad_modifier = quote(do: ~w(foo bar baz)x)
    assert %ArgumentError{} = catch_error(Code.eval_quoted(bad_modifier))

    assert ~w(Foo Bar)a == [:"Foo", :"Bar"]
    assert ~w(Foo.#{Bar}.Baz)a == [:"Foo.Elixir.Bar.Baz"]
    assert ~w(Foo.Bar)s == ["Foo.Bar"]
    assert ~w(Foo.#{Bar})c == ['Foo.Elixir.Bar']

    # Ensure it is fully expanded at compile time
    assert Macro.expand(quote(do: ~w(a b c)a), __ENV__) == [:a, :b, :c]
  end

  test "sigil W" do
    assert ~W() == []
    assert ~W(foo #{bar} baz) == ["foo", "\#{bar}", "baz"]

    assert ~W(foo\ bar) == ["foo\\", "bar"]

    assert ~W(
      foo
      bar
      baz
    ) == ["foo", "bar", "baz"]

    assert ~W(foo bar baz)s == ["foo", "bar", "baz"]
    assert ~W(foo bar baz)a == [:foo, :bar, :baz]
    assert ~W(foo bar baz)c == ['foo', 'bar', 'baz']

    bad_modifier = quote do: ~W(foo bar baz)x
    assert %ArgumentError{} = catch_error(Code.eval_quoted(bad_modifier))

    assert ~W(Foo #{Bar})a == [:"Foo", :"\#{Bar}"]
    assert ~W(Foo.Bar.Baz)a == [:"Foo.Bar.Baz"]
  end

  test "sigils matching" do
    assert ~s(f\(oo) == "f(oo"
    assert ~s(fo\)o) == "fo)o"
    assert ~s(f\(o\)o) == "f(o)o"
    assert ~s(f[oo) == "f[oo"
    assert ~s(fo]o) == "fo]o"
  end
end
