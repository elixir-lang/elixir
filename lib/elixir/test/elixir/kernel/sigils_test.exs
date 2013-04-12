Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.SigilsTest do
  use ExUnit.Case, async: true

  test :__b__ do
    assert %b(foo) == "foo"
    assert %b(f#{:o}o) == "foo"
    assert %b(f\no) == "f\no"
  end

  test :__b__with_heredoc do
    assert "  foo\n\n" == %b"""
      f#{:o}o\n
    """
  end

  test :__B__ do
    assert %B(foo) == "foo"
    assert %B[foo] == "foo"
    assert %B{foo} == "foo"
    assert %B'foo' == "foo"
    assert %B"foo" == "foo"
    assert %B|foo| == "foo"
    assert %B(f#{o}o) == "f\#{o}o"
    assert %B(f\no) == "f\\no"
  end

  test :__B__with_heredoc do
    assert "  f\#{o}o\\n\n" == %B"""
      f#{o}o\n
    """
  end

  test :__c__ do
    assert %c(foo) == 'foo'
    assert %c(f#{:o}o) == 'foo'
    assert %c(f\no) == 'f\no'
  end

  test :__C__ do
    assert %C(foo) == 'foo'
    assert %C[foo] == 'foo'
    assert %C{foo} == 'foo'
    assert %C'foo' == 'foo'
    assert %C"foo" == 'foo'
    assert %C|foo| == 'foo'
    assert %C(f#{o}o) == 'f\#{o}o'
    assert %C(f\no) == 'f\\no'
  end

  test :__w__ do
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
  end

  test :__W__ do
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
  end
end
