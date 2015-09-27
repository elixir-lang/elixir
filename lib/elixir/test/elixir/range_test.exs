Code.require_file "test_helper.exs", __DIR__

defmodule RangeTest do
  use ExUnit.Case, async: true

  test "precedence" do
    assert Enum.to_list(1..3+2) == [1, 2, 3, 4, 5]
    assert 1..3 |> Enum.to_list == [1, 2, 3]
  end

  test "op" do
    assert (1..3).first == 1
    assert (1..3).last  == 3
  end

  test "range?" do
    assert Range.range?(1..3)
    refute Range.range?(0)
  end

  test "enum" do
    refute Enum.empty?(1..1)

    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
    refute Enum.member?(1..3, 4)
    refute Enum.member?(3..1, 0)
    refute Enum.member?(3..1, 4)

    assert Enum.count(1..3) == 3
    assert Enum.count(3..1) == 3

    assert Enum.map(1..3, &(&1 * 2)) == [2, 4, 6]
    assert Enum.map(3..1, &(&1 * 2)) == [6, 4, 2]
  end

  test "inspect" do
    assert inspect(1..3) == "1..3"
    assert inspect(3..1) == "3..1"
  end

  test "assert ranges" do
    assert 0..42  == 0..42
    assert +0..42 == 0..42
    assert -0..42 == 0..42
    assert 1..42  == 1..42
    assert +1..42 == 1..42
    assert -1..42 == -1..42

    defmodule A, do: def a, do:  1
    assert (A.a)..42
  end

  test "raise error on invalid values" do
    invalid_values = [
      # atoms
      :atom, nil, false, true,
      # bitstrings
      "", "bitstring",
      # floats
      0.0, +0.0, -0.0,
      0.99, +0.99, -0.99,
      1.0e3, +1.0e3, -1.0e3,
      # tuples
      {}, {1}, {1, 2}, {1, 2, 3},
      # lists
      [], [1], [1, 2, 3],
      # maps
      %{}, %{a: 1},
      # charlists
      '', 'a', 'abc',
      # ranges
      1..1, 2..10,
    ]

    ranges = for val <- invalid_values do
      [{val, 42}, {42, val}, {val, val}]
    end

    require Logger

    for range_set <- ranges, {first, last} <- range_set do
      message = "ranges (first .. last) expect both sides to be integers, " <>
        "got: #{Macro.to_string({:.., [], [first, last]})}"
      assert_raise ArgumentError, message, fn ->
        # Logger.debug inspect({first, last})
        Macro.to_string(quote do: unquote(first)) <> ".." <>
        Macro.to_string(quote do: unquote(last))
        |> Code.eval_string
      end
    end
  end

  test "raise on special cases" do
    assert_raise ArgumentError,
      "ranges (first .. last) expect both sides to be integers, " <>
      "got: #{Macro.to_string({:.., [], [1, 2..3]})}",
      fn ->
        Code.eval_string("1..2..3")
    end

    assert_raise ArgumentError, fn ->
      Code.eval_string("42 .. File")
    end

    assert_raise ArgumentError, fn ->
      Code.eval_string("42 .. &File.cwd/0")
    end

    assert_raise ArgumentError, fn ->
      Code.eval_string("42 .. &(&1)")
    end

    assert_raise ArgumentError, fn ->
      Code.eval_string("42 .. fn -> true end")
    end
  end
end
