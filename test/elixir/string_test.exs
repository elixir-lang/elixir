Code.require_file "../test_helper", __FILE__

defmodule StringTest do
  use ExUnit.Case

  test :double_quoted_heredoc do
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

  test :single_quoted_heredoc do
    assert_equal 21, __LINE__
    assert_equal 'foo\nbar\n', '''
foo
bar
'''

    assert_equal 27, __LINE__
    assert_equal 'foo\nbar \'\'\'\n', '''
foo
bar '''
'''
  end

  test :double_quoted_heredoc_with_extra do
    assert_equal 35, __LINE__
    assert_equal "foo\nbar\nbar\n", """ <> "bar\n"
foo
bar
"""
  end

  test :double_quoted_aligned_heredoc do
    assert_equal "foo\nbar\nbar\n", """ <> "bar\n"
    foo
    bar
    """
  end

  test :string_concatenation_as_match do
    "foo" <> x = "foobar"
    assert_equal "bar", x
  end

  test :utf8 do
    13 = size(" ゆんゆん")
  end

  test :__B__ do
    "foo" = %B(foo)
    "f\#{o}o" = %B(f#{o}o)
    "f\\no" = %B(f\no)
  end

  test :__b__ do
    "foo" = %b(foo)
    "foo" = %b(f#{:o}o)
    "f\no" = %b(f\no)
  end

  test :__B__with_heredoc do
    "  f\#{o}o\\n\n" = %B"""
      f#{o}o\n
    """
  end

  test :__b__with_heredoc do
    "  foo\n\n" = %b"""
      f#{:o}o\n
    """
  end
end