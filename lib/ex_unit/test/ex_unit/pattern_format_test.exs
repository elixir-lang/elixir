Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternFormatTest do
  use ExUnit.Case

  alias ExUnit.{Pattern, PatternDiff, PatternFormat}

  test "simple pattern match" do
    simple =
      quote do
        :a
      end

    pattern = Pattern.new(simple, [], [])

    expected = [eq: ":a"]

    diff = PatternDiff.cmp(pattern, :a)
    actual = PatternFormat.format(diff)

    assert actual == expected

    expected = [del: ":a", ins: ":b"]

    diff = PatternDiff.cmp(pattern, :b)
    actual = PatternFormat.format(diff)

    assert actual == expected
  end

  test "pin" do
    simple = quote do
      ^a
    end
    pattern = Pattern.new(simple, [a: 1], [])

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
    pattern = Pattern.new(simple, [], a: :ex_unit_unbound_var)

    expected = [
      equiv: {"a", "1"}
    ]

    actual = pattern
    |> PatternDiff.cmp(1)
    |> PatternFormat.format()

    assert actual == expected

    pattern = Pattern.new(simple, [], a: 2)

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
    pattern = Pattern.new(simple, [], [])

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
    pattern = Pattern.new(simple, [], [])

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
    pattern = Pattern.new(simple, [], [])

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

  test "mis-matched tuple" do
    simple = quote do
      {1, 2, 3}
    end

    pattern = Pattern.new(simple, [], [])

    actual = pattern
    |> PatternDiff.cmp({1, 2})
    |> PatternFormat.format()

    expected = [
      eq: "{",
      eq: "1",
      eq: ", ",
      eq: "2",
      del: ", ",
      del: "3",
      eq: "}"
    ]

    assert actual == expected

    actual = pattern
    |> PatternDiff.cmp({1, 2, 3, 4})
    |> PatternFormat.format()

    expected = [
      eq: "{",
      eq: "1",
      eq: ", ",
      eq: "2",
      eq: ", ",
      eq: "3",
      ins: ", ",
      ins: "4",
      eq: "}"
    ]

    assert actual == expected
  end

  test "tuple with variables" do
    simple = quote do
      {a, b}
    end

    pattern = Pattern.new(simple, [], [a: :ex_unit_unbound_var, b: :ex_unit_unbound_var])

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

    pattern = Pattern.new(simple, [], [a: :ex_unit_unbound_var])

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

  test "tuple with pins" do
    simple = quote do
      {^a, ^b}
    end

    pattern = Pattern.new(simple, [a: 1, b: 2], [])

    expected = [
      eq: "{",
      equiv: {"^a", "1"},
      eq: ", ",
      equiv: {"^b", "2"},
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

    pattern = Pattern.new(simple, [], [])

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

  test "keyword list" do
    simple = quote do
      [a: 1, b: 2]
    end

    pattern = Pattern.new(simple, [], [])

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

  test "not a keyword list" do
    simple = quote do
      [{:a, 1}, {2, 2}] 
    end

    pattern = Pattern.new(simple, [], [])


    expected = [
      eq: "[",
      eq: "{",
      eq: ":a",
      eq: ", ",
      eq: "1",
      eq: "}",
      eq: ", ",
      eq: "{",
      eq: "2",
      eq: ", ",
      eq: "2",
      eq: "}",
      eq: "]"
    ]
    actual = pattern
    |> PatternDiff.cmp([{:a, 1}, {2, 2}])
    |> PatternFormat.format()

    assert actual == expected

    expected = [
      eq: "[",
      eq: "{",
      eq: ":a",
      eq: ", ",
      eq: "1",
      eq: "}",
      eq: ", ",
      eq: "{",
      del: "2",
      ins: ":b",
      eq: ", ",
      eq: "2",
      eq: "}",
      eq: "]"
    ]
    actual = pattern
    |> PatternDiff.cmp([a: 1, b: 2])
    |> PatternFormat.format()

    assert actual == expected
  end

  test "map with one key" do
    simple = quote do
      %{a: 1}
    end

    pattern = Pattern.new(simple, [], [])

    expected = [
      eq: "%{",
      eq: "a: ",
      eq: "1",
      eq: "}",
    ]
    actual = pattern
    |> PatternDiff.cmp(%{a: 1})
    |> PatternFormat.format()

    assert actual == expected

    expected = [
      eq: "%{",
      eq: "a: ",
      del: "1",
      ins: "2",
      eq: "}",
    ]

    actual = pattern
    |> PatternDiff.cmp(%{a: 2})
    |> PatternFormat.format()

    assert actual == expected
  end

  test "map with string key" do
    simple = quote do
      %{"a" => 1}
    end

    pattern = Pattern.new(simple, [], [])

    expected = [
      eq: "%{",
      eq: ~s("a" => ),
      eq: "1",
      eq: "}",
    ]
    actual = pattern
    |> PatternDiff.cmp(%{"a" => 1})
    |> PatternFormat.format()

    assert actual == expected
  end

  test "map with extra keys" do
    simple = quote do
      %{a: 1, b: 2}
    end
    pattern = Pattern.new(simple, [], [])
    actual = pattern
    |> PatternDiff.cmp(%{a: 1})
    |> PatternFormat.format()

    expected = [
      eq: "%{",
      eq: "a: ",
      eq: "1",
      del: ", ",
      del: "b: ",
      del: "2",
      eq: "}"
    ]

    assert actual == expected

    actual = pattern
    |> PatternDiff.cmp(%{a: 1, b: 2, c: 3})
    |> PatternFormat.format()

    expected = [
      eq: "%{",
      eq: "a: ",
      eq: "1",
      eq: ", ",
      eq: "b: ",
      eq: "2",
      ins: ", ",
      ins: "c: ",
      ins: "3",
      eq: "}"
    ]

    assert actual == expected
  end

  test "empty map with map" do
    simple = quote do
      %{}
    end

    pattern = Pattern.new(simple, [], [])

    actual = pattern
    |> PatternDiff.cmp(%{a: 1})
    |> PatternFormat.format()

    expected = [eq: "%{", ins: "a: ", ins: "1", eq: "}"]
    assert actual == expected

    simple = quote do
      %{a: 1}
    end

    pattern = Pattern.new(simple, [], [])

    actual = pattern
    |> PatternDiff.cmp(%{})
    |> PatternFormat.format()

    expected = [eq: "%{", del: "a: ", del: "1", eq: "}"]
    assert actual == expected
  end

  test "map with pinned key" do
    simple = quote do
      %{:a => 1, ^b => 2, :c => 3}
    end
    pattern = Pattern.new(simple, [b: :b], [])
    expected = [eq: "%{", eq: ":a => ", eq: "1", eq: ", ", eq: ":c => ", eq: "3", eq: ", ", equiv: {"^b => ", ":b => "}, del: "2", ins: "3", eq: "}"
    ]

    actual = pattern
    |> PatternDiff.cmp(%{a: 1, b: 3, c: 3})
    |> PatternFormat.format()

    assert actual == expected
  end

  test "map with map key" do
    simple = quote do
      %{ %{a: 1} => %{b: 1} }
    end

    pattern = Pattern.new(simple, [], [])

    actual = pattern
    |> PatternDiff.cmp(%{ %{a: 1} => %{b: 2} })
    |> PatternFormat.format()

    expected = [eq: "%{", del: "%{a: 1} => ", del: "%{b: 1}", ins: "%{a: 1} => ", ins: "%{b: 2}", eq: "}"]
    assert actual == expected
  end

  test "list decomposition" do
    simple = quote do
      [2 | tail]
    end
    pattern = Pattern.new(simple, [], [])

    actual = pattern
    |> PatternDiff.cmp([1, 2, 3])
    |> PatternFormat.format()

    expected = [eq: "[", del: "2", ins: "1", eq: " | ", del: "tail", ins: "[2, 3]", eq: "]"]
    assert actual == expected
  end
end


