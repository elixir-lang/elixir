Code.require_file "test_helper.exs", __DIR__

defmodule RangeTest do
  use ExUnit.Case, async: true

  test :precedence do
    assert Enum.to_list(1..3+2) == [1, 2, 3, 4, 5]
    assert 1..3 |> Enum.to_list == [1, 2, 3]
  end

  test :first do
    assert Range.new(first: 1, last: 3).first == 1
  end

  test :last do
    assert Range.new(first: 1, last: 3).last == 3
  end

  test :op do
    assert (1..3).first == 1
    assert (1..3).last  == 3
  end

  test :in do
    refute 0 in 1..3, "in range assertion"
    assert 1 in 1..3, "in range assertion"
    assert 2 in 1..3, "in range assertion"
    assert 3 in 1..3, "in range assertion"
    refute 4 in 1..3, "in range assertion"
    assert -3 in -1..-3, "in range assertion"
  end

  test :is_range do
    assert is_range(1..3)
    refute is_range(not_range)
  end

  test :enum do
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

  test :inspect do
    assert inspect(1..3) == "1..3"
    assert inspect(3..1) == "3..1"
  end

  defp not_range do
    1
  end
end
