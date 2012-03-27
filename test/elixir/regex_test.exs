Code.require_file "../test_helper", __FILE__

defmodule RegexTest do
  use ExUnit.Case

  test :match? do
    assert Regex.match?(%r(foo), "foo")
    assert !Regex.match?(%r(foo), "FOO")
    assert Regex.match?(%r(foo)i, "FOO")
  end
end
