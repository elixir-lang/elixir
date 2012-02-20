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

  test :string_concatenation_as_match do
    "foo" <> x = "foobar"
    assert_equal "bar", x
  end
end
