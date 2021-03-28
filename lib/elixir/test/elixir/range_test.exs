Code.require_file("test_helper.exs", __DIR__)

defmodule RangeTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  doctest Range

  defp reverse(first..last) do
    last..first
  end

  @doc false
  def stepless(first..last//_step) do
    %{__struct__: Range, first: first, last: last}
  end

  defmacrop capture_err(fun_string) do
    quote bind_quoted: [fun_string: fun_string, env: Macro.escape(__CALLER__)] do
      capture_io(:stderr, fn ->
        Code.eval_string(fun_string, [], env)
      end)
    end
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

  test "new" do
    assert Range.new(1, 3) == 1..3//1
    assert Range.new(3, 1) == 3..1//-1
    assert Range.new(1, 3, 2) == 1..3//2
    assert Range.new(3, 1, -2) == 3..1//-2
  end

  test "op" do
    assert (1..3).first == 1
    assert (1..3).last == 3
    assert (1..3).step == 1
    assert (3..1).step == -1
    assert (1..3//2).step == 2
  end

  test "inspect" do
    assert inspect(1..3) == "1..3"
    assert inspect(3..1) == "3..1//-1"
  end

  test "limits are integer only" do
    first = 1.0
    last = 3.0
    message = "ranges (first..last) expect both sides to be integers, got: 1.0..3.0"
    assert_raise ArgumentError, message, fn -> first..last end

    first = []
    last = []
    message = "ranges (first..last) expect both sides to be integers, got: []..[]"
    assert_raise ArgumentError, message, fn -> first..last end
  end

  test "step is a non-zero integer" do
    step = 1.0
    message = ~r"the step to be an integer different than zero"
    assert_raise ArgumentError, message, fn -> 1..3//step end

    step = 0
    message = ~r"the step to be an integer different than zero"
    assert_raise ArgumentError, message, fn -> 1..3//step end
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

  describe "Old stepless format" do
    test "Range functions" do
      assert_disjoint(stepless(1..5), stepless(6..9))

      # skip warning in deprecated functions
      capture_err(~S"""
        Range.range?(1..5)
        Range.range?(__MODULE__.stepless(1..5))

        range = 1..5//2
        bad_range = %{range | step: 0}
        Range.range?(bad_range)
      """)
    end

    test "Inspect protocol" do
      assert inspect(stepless(1..5)) == "1..5"
    end

    test "Enum module" do
      old_range = stepless(1..3)

      assert Enum.min_max(old_range)
      assert Enum.count(old_range)
      assert Enum.member?(old_range, 3)
      assert Enum.reduce(old_range, [], fn x, acc -> [x | acc] end)

      assert Enum.slice(old_range, 5..10)
      assert Enum.slice(old_range, old_range)
      assert Enum.slice(5..10, old_range)
      assert Enum.slice(old_range, 5..10)

      assert Enum.slice(old_range, 5, 10)

      assert Enum.sum(old_range)
      assert Enum.max(old_range)
      assert Enum.max_by(old_range, fn x -> :math.pow(-2, x) end)
      assert Enum.min(old_range)
      assert Enum.min_by(old_range, fn x -> :math.pow(-2, x) end)
    end
  end
end
