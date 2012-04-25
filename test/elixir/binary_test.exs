Code.require_file "../test_helper", __FILE__

defmodule BinaryTest do
  use ExUnit.Case

  test :heredoc do
    assert_equal 7, __LINE__
    assert_equal "foo\nbar\n", """
foo
bar
"""

    assert_equal 13, __LINE__
    assert_equal "foo\nbar \"\"\"\n", """
foo
bar """
"""
  end

  test :heredoc_with_extra do
    assert_equal 21, __LINE__
    assert_equal "foo\nbar\nbar\n", """ <> "bar\n"
foo
bar
"""
  end

  test :aligned_heredoc do
    assert_equal "foo\nbar\nbar\n", """ <> "bar\n"
    foo
    bar
    """
  end

  test :utf8 do
    assert_equal 13, size(" ゆんゆん")
  end

  test :utf8_char do
    assert_equal 12422, ?ゆ
    assert_equal 12422, ?\ゆ
  end

  test :string_concatenation_as_match do
    "foo" <> x = "foobar"
    assert_equal "bar", x
  end

  test :__B__ do
    assert_equal "foo", %B(foo)
    assert_equal "foo", %B[foo]
    assert_equal "foo", %B{foo}
    assert_equal "foo", %B'foo'
    assert_equal "foo", %B"foo"
    assert_equal "foo", %B|foo|
    assert_equal "f\#{o}o", %B(f#{o}o)
    assert_equal "f\\no", %B(f\no)
  end

  test :__b__ do
    assert_equal "foo", %b(foo)
    assert_equal "foo", %b(f#{:o}o)
    assert_equal "f\no", %b(f\no)
  end

  test :__B__with_heredoc do
    assert_equal "  f\#{o}o\\n\n", %B"""
      f#{o}o\n
    """
  end

  test :__b__with_heredoc do
    assert_equal "  foo\n\n", %b"""
      f#{:o}o\n
    """
  end

  test :octals do
    assert_equal "S", "\123"
    assert_equal "\n8", "\128"
    assert_equal <<1, ?8>>, "\18"
  end

  test :match do
    assert is_match?("ab", ?a)
    assert not is_match?("cd", ?a)
  end

  defp is_match?(<<char, _|binary>>, char) do
    true
  end

  defp is_match?(_, _) do
    false
  end
end