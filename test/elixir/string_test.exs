Code.require_file "../test_helper", __FILE__

defmodule StringTest do
  use ExUnit::Case

  test :heredoc do
    assert_equal "foo\nbar\n", """
foo
bar
"""

    assert_equal "foo\nbar \"\"\"\n", """
foo
bar """
"""
  end

  test :string_concatenation_as_match do
    "foo" <> x = "foobar"
    assert_equal "bar", x
  end
end
