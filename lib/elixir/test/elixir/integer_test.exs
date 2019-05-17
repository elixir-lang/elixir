Code.require_file("test_helper.exs", __DIR__)

defmodule IntegerTest do
  use ExUnit.Case, async: true

  doctest Integer

  require Integer

  def test_is_odd_in_guards(number) when Integer.is_odd(number), do: number
  def test_is_odd_in_guards(atom) when is_atom(atom) and not Integer.is_odd(atom), do: :atom
  def test_is_odd_in_guards(_number), do: false

  def test_is_even_in_guards(number) when Integer.is_even(number), do: number
  def test_is_even_in_guards(atom) when is_atom(atom) and not Integer.is_even(atom), do: :atom
  def test_is_even_in_guards(_number), do: false

  test "is_odd/1" do
    assert Integer.is_odd(0) == false
    assert Integer.is_odd(1) == true
    assert Integer.is_odd(2) == false
    assert Integer.is_odd(3) == true
    assert Integer.is_odd(-1) == true
    assert Integer.is_odd(-2) == false
    assert Integer.is_odd(-3) == true
    assert test_is_odd_in_guards(10) == false
    assert test_is_odd_in_guards(11) == 11
    assert test_is_odd_in_guards(:not_integer) == :atom
  end

  test "is_even/1" do
    assert Integer.is_even(0) == true
    assert Integer.is_even(1) == false
    assert Integer.is_even(2) == true
    assert Integer.is_even(3) == false
    assert Integer.is_even(-1) == false
    assert Integer.is_even(-2) == true
    assert Integer.is_even(-3) == false
    assert test_is_even_in_guards(10) == 10
    assert test_is_even_in_guards(11) == false
    assert test_is_even_in_guards(:not_integer) == :atom
  end

  test "mod/2" do
    assert Integer.mod(3, 2) == 1
    assert Integer.mod(0, 10) == 0
    assert Integer.mod(30000, 2001) == 1986
    assert Integer.mod(-20, 11) == 2
  end

  test "mod/2 raises ArithmeticError when divisor is 0" do
    assert_raise ArithmeticError, fn -> Integer.mod(3, 0) end
    assert_raise ArithmeticError, fn -> Integer.mod(-50, 0) end
  end

  test "mod/2 raises ArithmeticError when non-integers used as arguments" do
    assert_raise ArithmeticError, fn -> Integer.mod(3.0, 2) end
    assert_raise ArithmeticError, fn -> Integer.mod(20, 1.2) end
  end

  test "floor_div/2" do
    assert Integer.floor_div(3, 2) == 1
    assert Integer.floor_div(0, 10) == 0
    assert Integer.floor_div(30000, 2001) == 14
    assert Integer.floor_div(-20, 11) == -2
  end

  test "floor_div/2 raises ArithmeticError when divisor is 0" do
    assert_raise ArithmeticError, fn -> Integer.floor_div(3, 0) end
    assert_raise ArithmeticError, fn -> Integer.floor_div(-50, 0) end
  end

  test "floor_div/2 raises ArithmeticError when non-integers used as arguments" do
    assert_raise ArithmeticError, fn -> Integer.floor_div(3.0, 2) end
    assert_raise ArithmeticError, fn -> Integer.floor_div(20, 1.2) end
  end

  test "digits/2" do
    assert Integer.digits(0) == [0]
    assert Integer.digits(0, 2) == [0]
    assert Integer.digits(1) == [1]
    assert Integer.digits(-1) == [-1]
    assert Integer.digits(123, 123) == [1, 0]
    assert Integer.digits(-123, 123) == [-1, 0]
    assert Integer.digits(456, 1000) == [456]
    assert Integer.digits(-456, 1000) == [-456]
    assert Integer.digits(123) == [1, 2, 3]
    assert Integer.digits(-123) == [-1, -2, -3]
    assert Integer.digits(58127, 2) == [1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]
    assert Integer.digits(-58127, 2) == [-1, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, 0, -1, -1, -1, -1]

    for n <- Enum.to_list(-1..1) do
      assert_raise FunctionClauseError, fn ->
        Integer.digits(10, n)
        Integer.digits(-10, n)
      end
    end
  end

  test "undigits/2" do
    assert Integer.undigits([]) == 0
    assert Integer.undigits([0]) == 0
    assert Integer.undigits([1]) == 1
    assert Integer.undigits([1, 0, 1]) == 101
    assert Integer.undigits([1, 4], 16) == 0x14
    assert Integer.undigits([1, 4], 8) == 0o14
    assert Integer.undigits([1, 1], 2) == 0b11
    assert Integer.undigits([1, 2, 3, 4, 5]) == 12345
    assert Integer.undigits([1, 0, -5]) == 95
    assert Integer.undigits([-1, -1, -5]) == -115
    assert Integer.undigits([0, 0, 0, -1, -1, -5]) == -115

    for n <- Enum.to_list(-1..1) do
      assert_raise FunctionClauseError, fn ->
        Integer.undigits([1, 0, 1], n)
      end
    end

    assert_raise ArgumentError, "invalid digit 17 in base 16", fn ->
      Integer.undigits([1, 2, 17], 16)
    end
  end

  test "parse/2" do
    assert Integer.parse("12") === {12, ""}
    assert Integer.parse("012") === {12, ""}
    assert Integer.parse("+12") === {12, ""}
    assert Integer.parse("-12") === {-12, ""}
    assert Integer.parse("123456789") === {123_456_789, ""}
    assert Integer.parse("12.5") === {12, ".5"}
    assert Integer.parse("7.5e-3") === {7, ".5e-3"}
    assert Integer.parse("12x") === {12, "x"}
    assert Integer.parse("++1") === :error
    assert Integer.parse("--1") === :error
    assert Integer.parse("+-1") === :error
    assert Integer.parse("three") === :error

    assert Integer.parse("12", 10) === {12, ""}
    assert Integer.parse("-12", 12) === {-14, ""}
    assert Integer.parse("12345678", 9) === {6_053_444, ""}
    assert Integer.parse("3.14", 4) === {3, ".14"}
    assert Integer.parse("64eb", 16) === {25835, ""}
    assert Integer.parse("64eb", 10) === {64, "eb"}
    assert Integer.parse("10", 2) === {2, ""}
    assert Integer.parse("++4", 10) === :error

    # Base should be in range 2..36
    assert_raise ArgumentError, "invalid base 1", fn -> Integer.parse("2", 1) end
    assert_raise ArgumentError, "invalid base 37", fn -> Integer.parse("2", 37) end

    # Base should be an integer
    assert_raise ArgumentError, "invalid base 10.2", fn -> Integer.parse("2", 10.2) end

    assert_raise ArgumentError, "invalid base nil", fn -> Integer.parse("2", nil) end
  end

  test "to_string/1" do
    assert Integer.to_string(42) == "42"
    assert Integer.to_string(+42) == "42"
    assert Integer.to_string(-42) == "-42"
    assert Integer.to_string(-0001) == "-1"

    for n <- [42.0, :forty_two, '42', "42"] do
      assert_raise ArgumentError, fn ->
        Integer.to_string(n)
      end
    end
  end

  test "to_string/2" do
    assert Integer.to_string(42, 2) == "101010"
    assert Integer.to_string(42, 10) == "42"
    assert Integer.to_string(42, 16) == "2A"
    assert Integer.to_string(+42, 16) == "2A"
    assert Integer.to_string(-42, 16) == "-2A"
    assert Integer.to_string(-042, 16) == "-2A"

    for n <- [42.0, :forty_two, '42', "42"] do
      assert_raise ArgumentError, fn ->
        Integer.to_string(n, 42)
      end
    end

    for n <- [-1, 0, 1, 37] do
      assert_raise ArgumentError, fn ->
        Integer.to_string(42, n)
      end

      assert_raise ArgumentError, fn ->
        Integer.to_string(n, n)
      end
    end
  end

  test "to_charlist/1" do
    assert Integer.to_charlist(42) == '42'
    assert Integer.to_charlist(+42) == '42'
    assert Integer.to_charlist(-42) == '-42'
    assert Integer.to_charlist(-0001) == '-1'

    for n <- [42.0, :forty_two, '42', "42"] do
      assert_raise ArgumentError, fn ->
        Integer.to_charlist(n)
      end
    end
  end

  test "to_char_list/1" do
    module = Integer
    assert module.to_char_list(42) == '42'
  end

  test "to_charlist/2" do
    assert Integer.to_charlist(42, 2) == '101010'
    assert Integer.to_charlist(42, 10) == '42'
    assert Integer.to_charlist(42, 16) == '2A'
    assert Integer.to_charlist(+42, 16) == '2A'
    assert Integer.to_charlist(-42, 16) == '-2A'
    assert Integer.to_charlist(-042, 16) == '-2A'

    for n <- [42.0, :forty_two, '42', "42"] do
      assert_raise ArgumentError, fn ->
        Integer.to_charlist(n, 42)
      end
    end

    for n <- [-1, 0, 1, 37] do
      assert_raise ArgumentError, fn ->
        Integer.to_charlist(42, n)
      end

      assert_raise ArgumentError, fn ->
        Integer.to_charlist(n, n)
      end
    end
  end

  test "to_char_list/2" do
    module = Integer
    assert module.to_char_list(42, 2) == '101010'
  end

  test "gcd/2" do
    assert Integer.gcd(1, 5) == 1
    assert Integer.gcd(2, 3) == 1
    assert Integer.gcd(8, 12) == 4
    assert Integer.gcd(-8, 12) == 4
    assert Integer.gcd(8, -12) == 4
    assert Integer.gcd(-8, -12) == 4
    assert Integer.gcd(27, 27) == 27
    assert Integer.gcd(-27, -27) == 27
    assert Integer.gcd(-27, 27) == 27
    assert Integer.gcd(0, 3) == 3
    assert Integer.gcd(0, -3) == 3
    assert Integer.gcd(3, 0) == 3
    assert Integer.gcd(-3, 0) == 3
    assert Integer.gcd(0, 0) == 0
  end
end
