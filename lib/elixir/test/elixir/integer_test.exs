Code.require_file "test_helper.exs", __DIR__

defmodule IntegerTest do
  use ExUnit.Case, async: true

  doctest Integer

  require Integer

  def test_is_odd_in_guards(number) when Integer.is_odd(number),
    do: number
  def test_is_odd_in_guards(_number),
    do: false

  def test_is_even_in_guards(number) when Integer.is_even(number),
    do: number
  def test_is_even_in_guards(_number),
    do: false

  test "is_odd" do
    assert Integer.is_odd(0) == false
    assert Integer.is_odd(1) == true
    assert Integer.is_odd(2) == false
    assert Integer.is_odd(3) == true
    assert Integer.is_odd(-1) == true
    assert Integer.is_odd(-2) == false
    assert Integer.is_odd(-3) == true
    assert test_is_odd_in_guards(10) == false
    assert test_is_odd_in_guards(11) == 11
  end

  test "is_even" do
    assert Integer.is_even(0) == true
    assert Integer.is_even(1) == false
    assert Integer.is_even(2) == true
    assert Integer.is_even(3) == false
    assert Integer.is_even(-1) == false
    assert Integer.is_even(-2) == true
    assert Integer.is_even(-3) == false
    assert test_is_even_in_guards(10) == 10
    assert test_is_even_in_guards(11) == false
  end

  test "digits" do
    assert Integer.digits(101) == [1, 0, 1]
    assert Integer.digits(1) == [1]
    assert Integer.digits(0) == [0]
    assert Integer.digits(0, 2) == [0]
    assert Integer.digits(58127, 2) == [1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]
  end

  test "undigits" do
    assert Integer.undigits([1, 0, 1]) == 101
    assert Integer.undigits([1]) == 1
    assert Integer.undigits([0]) == 0
    assert Integer.undigits([]) == 0
    assert Integer.undigits([1, 4], 16) == 0x14
    assert Integer.undigits([1, 4], 8) == 0o14
    assert Integer.undigits([1, 1], 2) == 0b11
    assert Integer.undigits([1, 2, 3, 4, 5]) == 12345
    assert Integer.undigits([1, 0, -5]) == 95
    assert Integer.undigits([-1, -1, -5]) == -115
  end

  test "parse" do
    assert Integer.parse("12") === {12, ""}
    assert Integer.parse("-12") === {-12, ""}
    assert Integer.parse("123456789") === {123456789, ""}
    assert Integer.parse("12.5") === {12, ".5"}
    assert Integer.parse("7.5e-3") === {7, ".5e-3"}
    assert Integer.parse("12x") === {12, "x"}
    assert Integer.parse("++1") === :error
    assert Integer.parse("--1") === :error
    assert Integer.parse("+-1") === :error
    assert Integer.parse("three") === :error

    assert Integer.parse("12", 10) === {12, ""}
    assert Integer.parse("-12", 12) === {-14, ""}
    assert Integer.parse("12345678", 9) === {6053444, ""}
    assert Integer.parse("3.14", 4) === {3, ".14"}
    assert Integer.parse("64eb", 16) === {25835, ""}
    assert Integer.parse("64eb", 10) === {64, "eb"}
    assert Integer.parse("10", 2) === {2, ""}
    assert Integer.parse("++4", 10) === :error

    # Base should be in range 2..36
    assert_raise ArgumentError, "invalid base 0", fn -> Integer.parse("2", 0) end
    assert_raise ArgumentError, "invalid base 41", fn -> Integer.parse("2", 41) end

    # Base should be an integer
    assert_raise ArgumentError, "invalid base 10.2", fn -> Integer.parse("2", 10.2) end
  end
end
