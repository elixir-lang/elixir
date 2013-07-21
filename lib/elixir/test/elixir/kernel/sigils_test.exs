Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.SigilsTest do
  use ExUnit.Case, async: true

  test :sigil_b do
    assert %b(foo) == "foo"
    assert %b(f#{:o}o) == "foo"
    assert %b(f\no) == "f\no"
  end

  test :sigil_b_with_heredoc do
    assert "  foo\n\n" == %b"""
      f#{:o}o\n
    """
  end

  test :sigil_B do
    assert %B(foo) == "foo"
    assert %B[foo] == "foo"
    assert %B{foo} == "foo"
    assert %B'foo' == "foo"
    assert %B"foo" == "foo"
    assert %B|foo| == "foo"
    assert %B(f#{o}o) == "f\#{o}o"
    assert %B(f\no) == "f\\no"
  end

  test :sigil_B_with_heredoc do
    assert "  f\#{o}o\\n\n" == %B"""
      f#{o}o\n
    """
  end

  test :sigil_c do
    assert %c(foo) == 'foo'
    assert %c(f#{:o}o) == 'foo'
    assert %c(f\no) == 'f\no'
  end

  test :sigil_C do
    assert %C(foo) == 'foo'
    assert %C[foo] == 'foo'
    assert %C{foo} == 'foo'
    assert %C'foo' == 'foo'
    assert %C"foo" == 'foo'
    assert %C|foo| == 'foo'
    assert %C(f#{o}o) == 'f\#{o}o'
    assert %C(f\no) == 'f\\no'
  end

  test :sigil_w do
    assert %w() == []
    assert %w(foo bar baz) == ["foo", "bar", "baz"]
    assert %w(foo #{:bar} baz) == ["foo", "bar", "baz"]

    assert %w(
      foo
      bar
      baz
    ) == ["foo", "bar", "baz"]

    assert %w(foo bar baz)b == ["foo", "bar", "baz"]
    assert %w(foo bar baz)a == [:foo, :bar, :baz]
    assert %w(foo bar baz)c == ['foo', 'bar', 'baz']

    bad_modifier = quote do: %w(foo bar baz)x
    assert ArgumentError[] = catch_error(Code.eval_quoted(bad_modifier))

    assert %w(Foo Bar)a == [:"Foo", :"Bar"]
    assert %w(Foo.#{Bar}.Baz)a == [:"Foo.Elixir.Bar.Baz"]
    assert %w(Foo.Bar)b == ["Foo.Bar"]
    assert %w(Foo.#{Bar})c == ['Foo.Elixir.Bar']

    # Ensure it is fully expanded at compile time
    assert Macro.expand(quote(do: %w(a b c)a), __ENV__) == [:a, :b, :c]
  end

  test :sigil_W do
    assert %W(foo #{bar} baz) == ["foo", "\#{bar}", "baz"]

    assert %W(
      foo
      bar
      baz
    ) == ["foo", "bar", "baz"]

    assert %W(foo bar baz)b == ["foo", "bar", "baz"]
    assert %W(foo bar baz)a == [:foo, :bar, :baz]
    assert %W(foo bar baz)c == ['foo', 'bar', 'baz']

    bad_modifier = quote do: %W(foo bar baz)x
    assert ArgumentError[] = catch_error(Code.eval_quoted(bad_modifier))

    assert %W(Foo #{Bar})a == [:"Foo", :"\#{Bar}"]
    assert %W(Foo.Bar.Baz)a == [:"Foo.Bar.Baz"]
  end
end
