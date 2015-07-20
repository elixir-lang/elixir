Code.require_file "test_helper.exs", __DIR__

defmodule RangeTest do
  use ExUnit.Case, async: true

  test :precedence do
    assert Enum.to_list(1..3+2) == [1, 2, 3, 4, 5]
    assert 1..3 |> Enum.to_list == [1, 2, 3]
    assert "a".."c" |> Enum.to_list == ["a", "b", "c"]
  end

  test :op do
    assert (1..3).first == 1
    assert (1..3).last  == 3

    assert ("a".."z").first == "a"
    assert ("a".."z").last  == "z"
  end

  test :range? do
    assert Range.range?(1..3)
    refute Range.range?(0)

    assert Range.range?("a".."z")
    refute Range.range?("a")
  end

  test :enum do
    refute Enum.empty?(1..1)
    refute Enum.empty?("a".."a")

    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
    refute Enum.member?(1..3, 4)
    refute Enum.member?(3..1, 0)
    refute Enum.member?(3..1, 4)

    assert Enum.member?("a".."z", "n")
    refute Enum.member?("a".."z", "A")
    refute Enum.member?("a".."z", "Z")
    refute Enum.member?("a".."z", "`")
    refute Enum.member?("a".."z", "{")
    refute Enum.member?("z".."a", "A")
    refute Enum.member?("z".."a", "Z")
    refute Enum.member?("z".."a", "`")
    refute Enum.member?("z".."a", "{")

    assert Enum.count(1..3) == 3
    assert Enum.count(3..1) == 3

    assert Enum.count("a".."z") == 26
    assert Enum.count("z".."a") == 26

    assert Enum.map(1..3, &(&1 * 2)) == [2, 4, 6]
    assert Enum.map(3..1, &(&1 * 2)) == [6, 4, 2]

    assert Enum.map("a".."c", &("<#{&1}>")) == ["<a>", "<b>", "<c>"]
    assert Enum.map("c".."a", &("<#{&1}>")) == ["<c>", "<b>", "<a>"]
  end

  test :inspect do
    assert inspect(1..3) == "1..3"
    assert inspect(3..1) == "3..1"

    assert inspect("a".."z") == ~s("a".."z")
    assert inspect("z".."a") == ~s("z".."a")
  end
end
