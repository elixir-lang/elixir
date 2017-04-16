Code.require_file "test_helper.exs", __DIR__

defmodule MathTest do
  use ExUnit.Case, async: true

  test "random with an int as a parameter should return a number between 0 and the parameter" do
    random_number = Math.random(3)
    assert random_number <= 3
    assert random_number >= 1
  end

  test "random withouth parameters should return a number between 0 and 1" do
    random_number = Math.random()
    assert random_number <= 1
    assert random_number >= 0
  end
end
