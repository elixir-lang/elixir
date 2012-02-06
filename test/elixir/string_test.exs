Code.require_file "../test_helper", __FILE__

defmodule StringTest do
  use ExUnit::Case

  def test_string_concatenation do
    "foo" <> x = "foobar"
    "bar" = x
  end
end