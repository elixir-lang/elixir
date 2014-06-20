Code.require_file "test_helper.exs", __DIR__

defmodule RangeTest do
  use ExUnit.Case, async: true

  test :precedence do
    assert Enum.to_list(1..3+2) == [1, 2, 3, 4, 5]
    assert 1..3 |> Enum.to_list == [1, 2, 3]
  end

  test :op do
    assert (1..3).first == 1
    assert (1..3).last  == 3
    assert ("a".."z").first == "a"
    assert ("a".."z").last == "z"
    assert ('a'..'z').first == 'a'
    assert ('a'..'z').last == 'z'
  end

  test :range? do
    assert Range.range?(1..3)
    refute Range.range?(0)
  end

  test :enum do
    refute Enum.empty?(1..1)

    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
    refute Enum.member?(1..3, 4)
    refute Enum.member?(3..1, 0)
    refute Enum.member?(3..1, 4)

    assert Enum.member?("a".."z", "d")
    assert Enum.member?("z".."a", "x")
    refute Enum.member?("a".."z", "A")
    refute Enum.member?("a".."z", "é")

    assert Enum.member?('a'..'z', 'd')
    assert Enum.member?('z'..'a', 'x')
    refute Enum.member?('a'..'z', 'A')
    refute Enum.member?('a'..'z', 'é')

    assert Enum.count(1..3) == 3
    assert Enum.count(3..1) == 3

    assert Enum.count("a".."z") == 26
    assert Enum.count("A".."Z") == 26
    assert Enum.count("a".."é") == 137

    assert Enum.count('a'..'z') == 26
    assert Enum.count('A'..'Z') == 26
    assert Enum.count('a'..'é') == 137

    assert Enum.map(1..3, &(&1 * 2)) == [2, 4, 6]
    assert Enum.map(3..1, &(&1 * 2)) == [6, 4, 2]
  end

  test :inspect do
    assert inspect(1..3) == "1..3"
    assert inspect(3..1) == "3..1"
  end
end
