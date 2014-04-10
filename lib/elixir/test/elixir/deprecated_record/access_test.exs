Code.require_file "../test_helper.exs", __DIR__

defmodule Record.AccessTest do
  use ExUnit.Case, async: true

  defrecord User, age: 0, name: nil do
    def get_age(__MODULE__[age: age]) do
      age
    end
  end

  test "access with keywords" do
    assert User[] == { User, 0, nil }
    assert User[age: 1] == { User, 1, nil }
  end

  test "access with variable inside match" do
    assert get_var(User.new) == 0
    assert get_var(User.new(age: 1)) == 1
  end

  test "access match on record name" do
    assert is_user(User.new) == true
    assert is_user({ Access.AtomTest, 1 }) == false
    assert is_user({ User, 1, 2, 3 }) == false
  end

  test "access with field match" do
    assert is_zero(User.new) == true
    assert is_zero(User.new(age: 1)) == false
  end

  test "match (=)" do
    assert User[age: 1] = User.new(age: 1)
    refute User[age: 1] = User.new(age: 0)
  end

  test "access with underscore" do
    assert User[age: 0, name: 0] == User[_: 0]
    assert User[_: _] = User[_: "x"]
    refute match?(User[_: 0], User[_: 1])
  end

  test "access during record definition" do
    assert User.new(age: 13).get_age == 13
  end

  defp get_var(User[age: age]) do
    age
  end

  defp is_zero(User[age: 0]), do: true
  defp is_zero(User[age: _]),  do: false

  defp is_user(User[]), do: true
  defp is_user(_), do: false
end