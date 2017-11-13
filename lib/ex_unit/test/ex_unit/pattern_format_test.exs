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

    actual = pattern
      |> PatternDiff.cmp(1)
      |> PatternFormat.format()

    assert actual == expected


    actual = pattern
      |> PatternDiff.cmp(2)
      |> PatternFormat.format()

    expected  = [del: "^a", ins: "2"]
    assert actual == expected
  end

  test "variable" do
    simple = quote do
      a
    end
    pattern = ExUnit.Pattern.new(simple, [], a: :ex_unit_unbound_var)

    expected = [
      equiv: {"a", "1"}
    ]

    actual = pattern
    |> PatternDiff.cmp(1)
    |> PatternFormat.format()

    assert actual == expected

    pattern = ExUnit.Pattern.new(simple, [], a: 2)

    expected = [
      del: "a",
      ins: "1"
    ]

    actual = pattern
    |> PatternDiff.cmp(1)
    |> PatternFormat.format()

    assert actual == expected
  end

  test "one element tuple" do
    simple = quote do
      {:a}
    end
    pattern = ExUnit.Pattern.new(simple, [], [])

    expected = [
      eq: "{",
      eq: ":a",
      eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp({:a})
    |> PatternFormat.format()

    assert actual == expected

    expected = [
      eq: "{",
      del: ":a",
      ins: ":b",
      eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp({:b})
    |> PatternFormat.format()

    assert actual == expected
  end

  test "two element tuple" do
    simple = quote do
      {:a, :b}
    end
    pattern = ExUnit.Pattern.new(simple, [], [])

    expected = [
      eq: "{",
      eq: ":a",
      eq: ", ",
      eq: ":b",
      eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp({:a, :b})
    |> PatternFormat.format()

    assert actual == expected

    expected = [
      eq: "{",
      eq: ":a",
      eq: ", ",
      del: ":b",
      ins: ":a",
      eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp({:a, :a})
    |> PatternFormat.format()

    assert actual == expected
  end

  test "three element tuple" do
    simple = quote do
      {:a, :b, :c}
    end
    pattern = ExUnit.Pattern.new(simple, [], [])

    expected = [
      eq: "{",
      eq: ":a",
      eq: ", ",
      eq: ":b",
      eq: ", ",
      eq: ":c",
      eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp({:a, :b, :c})
    |> PatternFormat.format()

    assert actual == expected

    expected = [
      eq: "{",
      eq: ":a",
      eq: ", ",
      del: ":b",
      ins: ":a",
      eq: ", ",
      eq: ":c",
      eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp({:a, :a, :c})
    |> PatternFormat.format()

    assert actual == expected
  end

  test "tuple with variables" do
    simple = quote do
      {a, b}
    end

    pattern = ExUnit.Pattern.new(simple, [], [a: :ex_unit_unbound_var, b: :ex_unit_unbound_var])

    expected = [
      eq: "{",
      equiv: {"a", "1"},
      eq: ", ",
      equiv: {"b", "2"},
      eq: "}"
    ]
    actual = pattern
    |> PatternDiff.cmp({1, 2})
    |> PatternFormat.format()

    assert actual == expected

    simple = quote do
      {a, a}
    end

    pattern = ExUnit.Pattern.new(simple, [], [a: :ex_unit_unbound_var])

    expected = [
      eq: "{",
      equiv: {"a", "1"},
      eq: ", ",
      del: "a",
      ins: "2",
      eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp({1, 2})
    |> PatternFormat.format()

    assert actual == expected
  end

  test "single element list" do
    simple = quote do
      [1]
    end

    pattern = ExUnit.Pattern.new(simple, [], [])

    expected = [
      eq: "[",
      eq: "1",
      eq: "]"
    ]
    actual = pattern
    |> PatternDiff.cmp([1])
    |> PatternFormat.format()

    assert actual == expected

    expected = [
      eq: "[",
      del: "1",
      ins: "2",
      eq: "]"
    ]
    actual = pattern
    |> PatternDiff.cmp([2])
    |> PatternFormat.format()

    assert actual == expected
  end

  # keyword list
  test "keyword list" do
    simple = quote do
      [a: 1, b: 2]
    end

    pattern = ExUnit.Pattern.new(simple, [], [])


    expected = [
      eq: "[",
      eq: "a: ",
      eq: "1",
      eq: ", ",
      eq: "b: ",
      eq: "2",
      eq: "]"
    ]
    actual = pattern
    |> PatternDiff.cmp([a: 1, b: 2])
    |> PatternFormat.format()

    assert actual == expected
  end

  # map

end
