Code.require_file("test_helper.exs", __DIR__)

defmodule RangeTest do
  use ExUnit.Case, async: true

  doctest Range

  defp reverse(first..last) do
    last..first
  end

  defp assert_disjoint(r1, r2) do
    disjoint_assertions(r1, r2, true)
  end

  defp assert_overlap(r1, r2) do
    disjoint_assertions(r1, r2, false)
  end

  defp disjoint_assertions(r1, r2, expected) do
    # The caller should choose pairs of representative ranges, and we take care
    # here of commuting them.
    Enum.each([[r1, r2], [r2, r1]], fn [a, b] ->
      assert Range.disjoint?(a, b) == expected
      assert Range.disjoint?(reverse(a), b) == expected
      assert Range.disjoint?(a, reverse(b)) == expected
      assert Range.disjoint?(reverse(a), reverse(b)) == expected
    end)
  end

  test "precedence" do
    assert Enum.to_list(1..(3 + 2)) == [1, 2, 3, 4, 5]
    assert 1..3 |> Enum.to_list() == [1, 2, 3]
  end

  test "op" do
    assert (1..3).first == 1
    assert (1..3).last == 3
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

  test "integer only" do
    first = 1.0
    last = 3.0
    message = "ranges (first..last) expect both sides to be integers, got: 1.0..3.0"

    assert_raise ArgumentError, message, fn ->
      Enum.map(first..last, &(&1 * 2))
    end

    first = []
    last = []
    message = "ranges (first..last) expect both sides to be integers, got: []..[]"

    assert_raise ArgumentError, message, fn ->
      first..last
      Enum.map(first..last, & &1)
    end
  end

  describe "disjoint?" do
    test "returns true for disjoint ranges" do
      assert_disjoint(1..5, 6..9)
      assert_disjoint(-3..1, 2..3)
      assert_disjoint(-7..-5, -3..-1)

      assert Range.disjoint?(1..1, 2..2) == true
      assert Range.disjoint?(2..2, 1..1) == true
    end

    test "returns false for ranges with common endpoints" do
      assert_overlap(1..5, 5..9)
      assert_overlap(-1..0, 0..1)
      assert_overlap(-7..-5, -5..-1)
    end

    test "returns false for ranges that overlap" do
      assert_overlap(1..5, 3..7)
      assert_overlap(-3..1, -1..3)
      assert_overlap(-7..-5, -5..-1)

      assert Range.disjoint?(1..1, 1..1) == false
    end
  end
end
