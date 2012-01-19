Code.require_file "../test_helper", __FILE__

defmodule StringTest do
  use ExUnit::Case

  def test_implicit_string_concat do
    "foobar" = "foo" "bar"
    "foobar" = "foo" "#{:bar}"
    "foobar" = "foo" \
      "#{:bar}"
  end
end