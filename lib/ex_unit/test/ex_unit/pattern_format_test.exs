Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternFormatTest do
  use ExUnit.Case

  alias ExUnit.{Pattern, PatternDiff, PatternFormat}

  test "simple pattern match" do
    simple =
      quote do
        :a
      end

    pattern = ExUnit.Pattern.new(simple, [], [])

    expected = [eq: ":a"]

    diff = PatternDiff.cmp(pattern, :a)
    actual = PatternFormat.format(diff)

    assert actual == expected

    expected = [del: ":a", ins: ":b"]

    diff = PatternDiff.cmp(pattern, :b)
    actual = PatternFormat.format(diff)

    assert actual == expected
  end

  # pin

  test "pin" do
    simple = quote do
      ^a
    end
    pattern = ExUnit.Pattern.new(simple, [a: 1], [])

    expected = [equiv: {"^a", "1"}]

    actual = PatternDiff.cmp(pattern, 1)

    assert actual == expected


    actual = PatternDiff.cmp(pattern, 2)
    expected  = [del: "^a", ins: "2"]
    assert actual == expected
  end

  # variable

  # tuple

  # list

  # keyword list

  # map

end
