Code.require_file "test_helper.exs", __DIR__

defmodule RangeTest do
  use ExUnit.Case, async: true

  test :precedence do
    assert Enum.to_list(1..3+2) == [1, 2, 3, 4, 5]
    assert Enum.to_list(1.0..1.5+0.2) == [1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7]
    assert 1..3 |> Enum.to_list == [1, 2, 3]
    assert 1.0..1.5 |> Enum.to_list == [1.0, 1.1, 1.2, 1.3, 1.4, 1.5]
  end

  test :op do
    assert (1..3).first == 1
    assert (1..3).last  == 3

    assert (1.0..1.5).first == 1.0
    assert (1.0..1.5).last == 1.5
  end

  test :range? do
    assert Range.range?(1..3)
    assert Range.range?(1.0..1.527)
    refute Range.range?(0)
  end

  test :enum do
    refute Enum.empty?(1..1)

    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
    refute Enum.member?(1..3, 4)
    refute Enum.member?(3..1, 0)
    refute Enum.member?(3..1, 4)

    assert Enum.member?(1.0..1.5, 1.4)
    refute Enum.member?(1.0..1.5, 0.6)
    refute Enum.member?(1.0..1.5, 1.6)

    assert Enum.count(1..3) == 3
    assert Enum.count(3..1) == 3

    assert Enum.count(1.0..1.5) == 6
    assert Enum.count(1.5..1.0) == 6

    assert Enum.map(1..3, &(&1 * 2)) == [2, 4, 6]
    assert Enum.map(3..1, &(&1 * 2)) == [6, 4, 2]

    assert Enum.map(1.0..1.5, &(&1 * 2)) == [2.0, 2.2, 2.4, 2.6, 2.8, 3.0]
  end

  test :inspect do
    assert inspect(1..3) == "1..3"
    assert inspect(3..1) == "3..1"
    assert inspect(1.0..1.5) == "1.0..1.5"
  end
end
