Code.require_file "../test_helper", __FILE__

defmodule StringTest do
  use ExUnit::Case

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

  test :string_join do
    assert_equal "Hello, World!", String.join(["Hello,", "World!"], " ")
    assert_equal "a,b,  c, d, e", String.join(["a", "b", "  c", " d", " e"], ",")
  end

  test :string_join_empty_list do
    assert_equal "", String.join([], " ")
  end

  test :string_join_newline do
    assert_equal "1\n2\n3\n4", String.join(["1", "2", "3", "4"], "\n")
  end

  test :string_join_long_separator do
    assert_equal "1asdf2asdf3asdf4", String.join(["1", "2", "3", "4"], "asdf")
  end
end
