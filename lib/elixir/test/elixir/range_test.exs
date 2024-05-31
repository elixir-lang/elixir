Code.require_file("test_helper.exs", __DIR__)

defmodule RangeTest do
  use ExUnit.Case, async: true

  doctest Range

  defp reverse(first..last//step) do
    last..first//-step
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
    assert Range.new(1, 3, 2) == 1..3//2
    assert Range.new(3, 1, -2) == 3..1//-2

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             assert Range.new(3, 1) == 3..1//-1
           end) =~ "has a default step of -1"
  end

  test "fields" do
    assert (1..3).first == 1
    assert (1..3).last == 3
    assert (1..3).step == 1
    assert (3..1//-1).step == -1
    assert (1..3//2).step == 2
  end

  test "inspect" do
    assert inspect(1..3) == "1..3"
    assert inspect(1..3//2) == "1..3//2"

    assert inspect(3..1//-1) == "3..1//-1"
    assert inspect(3..1//1) == "3..1//1"
  end

  test "shift" do
    assert Range.shift(0..10//2, 2) == 4..14//2
    assert Range.shift(10..0//-2, 2) == 6..-4//-2
    assert Range.shift(10..0//-2, -2) == 14..4//-2
  end

  test "in guard equality" do
    case {1, 1..1} do
      {n, range} when range == n..n//1 -> true
    end
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
    message = ~r"the step to be a non-zero integer"
    assert_raise ArgumentError, message, fn -> 1..3//step end

    step = 0
    message = ~r"the step to be a non-zero integer"
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

  describe "split" do
    @times 10

    test "increasing ranges" do
      for _ <- 1..@times do
        left = Enum.random(-10..10)
        right = Enum.random(-10..10)
        input = min(left, right)..max(left, right)//Enum.random(1..3)

        for split <- -3..3 do
          {left, right} = Range.split(input, split)
          assert input.first == left.first
          assert input.last == right.last
          assert input.step == left.step
          assert input.step == right.step

          assert Range.size(input) == Range.size(left) + Range.size(right),
                 "size mismatch: Range.split(#{inspect(input)}, #{split})"
        end
      end
    end

    test "decreasing ranges" do
      for _ <- 1..@times do
        left = Enum.random(-10..10)
        right = Enum.random(-10..10)
        input = max(left, right)..min(left, right)//-Enum.random(1..3)

        for split <- -3..3 do
          {left, right} = Range.split(input, split)
          assert input.first == left.first
          assert input.last == right.last
          assert input.step == left.step
          assert input.step == right.step

          assert Range.size(input) == Range.size(left) + Range.size(right),
                 "size mismatch: Range.split(#{inspect(input)}, #{split})"
        end
      end
    end

    test "empty increasing ranges" do
      for _ <- 1..@times,
          left = Enum.random(-10..10),
          right = Enum.random(-10..10),
          left != right do
        input = min(left, right)..max(left, right)//-Enum.random(1..3)

        for split <- -3..3 do
          {left, right} = Range.split(input, split)
          assert input.first == left.first
          assert input.last == right.last
          assert input.step == left.step
          assert input.step == right.step

          assert Range.size(input) == Range.size(left) + Range.size(right),
                 "size mismatch: Range.split(#{inspect(input)}, #{split})"
        end
      end
    end

    test "empty decreasing ranges" do
      for _ <- 1..@times,
          left = Enum.random(-10..10),
          right = Enum.random(-10..10),
          left != right do
        input = max(left, right)..min(left, right)//Enum.random(1..3)

        for split <- -3..3 do
          {left, right} = Range.split(input, split)
          assert input.first == left.first
          assert input.last == right.last
          assert input.step == left.step
          assert input.step == right.step

          assert Range.size(input) == Range.size(left) + Range.size(right),
                 "size mismatch: Range.split(#{inspect(input)}, #{split})"
        end
      end
    end
  end

  describe "old ranges" do
    test "inspect" do
      asc = %{__struct__: Range, first: 1, last: 3}
      desc = %{__struct__: Range, first: 3, last: 1}

      assert inspect(asc) == "1..3"
      assert inspect(desc) == "3..1//-1"
    end

    test "enum" do
      asc = %{__struct__: Range, first: 1, last: 3}
      desc = %{__struct__: Range, first: 3, last: 1}

      assert Enum.to_list(asc) == [1, 2, 3]
      assert Enum.member?(asc, 2)
      assert Enum.count(asc) == 3
      assert Enum.drop(asc, 1) == [2, 3]
      assert Enum.slice([1, 2, 3, 4, 5, 6], asc) == [2, 3, 4]
      # testing private Enum.aggregate
      assert Enum.max(asc) == 3
      assert Enum.sum(asc) == 6
      assert Enum.min_max(asc) == {1, 3}
      assert Enum.reduce(asc, 0, fn a, b -> a + b end) == 6

      assert Enum.to_list(desc) == [3, 2, 1]
      assert Enum.member?(desc, 2)
      assert Enum.count(desc) == 3
      assert Enum.drop(desc, 1) == [2, 1]

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert Enum.slice([1, 2, 3, 4, 5, 6], desc) == []
             end) =~ "negative steps are not supported in Enum.slice/2, pass 3..1//1 instead"

      # testing private Enum.aggregate
      assert Enum.max(desc) == 3
      assert Enum.sum(desc) == 6
      assert Enum.min_max(desc) == {1, 3}
      assert Enum.reduce(desc, 0, fn a, b -> a + b end) == 6
    end

    test "string" do
      asc = %{__struct__: Range, first: 1, last: 3}
      desc = %{__struct__: Range, first: 3, last: 1}

      assert String.slice("elixir", asc) == "lix"

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert String.slice("elixir", desc) == ""
             end) =~ "negative steps are not supported in String.slice/2, pass 3..1//1 instead"
    end
  end
end
