Code.require_file "../test_helper", __FILE__

defmodule StringTest do
  use ExUnit::Case

  def test_string_concatenation_as_match do
    "foo" <> x = "foobar"
    assert_equal "bar", x
  end
end
