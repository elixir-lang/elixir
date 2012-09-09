Code.require_file "../test_helper.exs", __FILE__

defmodule StringTest do
  use ExUnit.Case, async: true

  test :split do
    assert String.split("a,b,c", ",") == ["a", "b,c"]
    assert String.split("a,b,c", ",", global: true) == ["a", "b", "c"]
    assert String.split("foo bar") == ["foo", "bar"]
    assert String.split("1,2 3,4", [" ", ","]) == ["1", "2 3,4"]
    assert String.split("1,2 3,4", [" ", ","], global: true) == ["1", "2", "3", "4"]
    assert String.split("a,b", ".") == ["a,b"]
  end

  test :split_with_regex do
    assert String.split("a,b", %r{,}) == ["a", "b"]
    assert String.split("a,b,c", %r{,}) == ["a", "b,c"]
    assert String.split("a,b,c", %r{,}, global: true) == ["a", "b", "c"]
    assert String.split("a,b", %r{\.}) == ["a,b"]
  end
end
