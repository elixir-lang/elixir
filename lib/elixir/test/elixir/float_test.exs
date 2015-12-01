Code.require_file "test_helper.exs", __DIR__

defmodule FloatTest do
  use ExUnit.Case, async: true

  doctest Float

  test "parse" do
    assert Float.parse("12") === {12.0, ""}
    assert Float.parse("-12") === {-12.0, ""}
    assert Float.parse("-0.1") === {-0.1, ""}
    assert Float.parse("123456789") === {123456789.0, ""}
    assert Float.parse("12.5") === {12.5, ""}
    assert Float.parse("12.524235") === {12.524235, ""}
    assert Float.parse("-12.5") === {-12.5, ""}
    assert Float.parse("-12.524235") === {-12.524235, ""}
    assert Float.parse("0.3534091") === {0.3534091, ""}
    assert Float.parse("0.3534091elixir") === {0.3534091, "elixir"}
    assert Float.parse("7.5e3") === {7.5e3, ""}
    assert Float.parse("7.5e-3") === {7.5e-3, ""}
    assert Float.parse("12x") === {12.0, "x"}
    assert Float.parse("12.5x") === {12.5, "x"}
    assert Float.parse("-12.32453e10") === {-1.232453e11, ""}
    assert Float.parse("-12.32453e-10") === {-1.232453e-9, ""}
    assert Float.parse("0.32453e-10") === {3.2453e-11, ""}
    assert Float.parse("1.32453e-10") === {1.32453e-10, ""}
    assert Float.parse("1.32.45") === {1.32, ".45"}
    assert Float.parse("1.o") === {1.0, ".o"}
    assert Float.parse("+12.3E+4") === {1.23e5, ""}
    assert Float.parse("+12.3E-4x") === {0.00123, "x"}
    assert Float.parse("-1.23e-0xFF") === {-1.23, "xFF"}
    assert Float.parse("-1.e2") === {-1.0, ".e2"}
    assert Float.parse(".12") === :error
    assert Float.parse("--1.2") === :error
    assert Float.parse("++1.2") === :error
    assert Float.parse("pi") === :error
    assert Float.parse("1.7976931348623157e308") === {1.7976931348623157e308, ""}
    assert_raise ArgumentError, fn ->
      Float.parse("1.7976931348623159e308")
    end
  end

  test "floor" do
    assert Float.floor(12.524235) === 12.0
    assert Float.floor(-12.5) === -13.0
    assert Float.floor(-12.524235) === -13.0
    assert Float.floor(7.5e3) === 7500.0
    assert Float.floor(7.5432e3) === 7543.0
    assert Float.floor(7.5e-3) === 0.0
    assert Float.floor(-12.32453e4) === -123246.0
    assert Float.floor(-12.32453e-10) === -1.0
    assert Float.floor(0.32453e-10) === 0.0
    assert Float.floor(-0.32453e-10) === -1.0
    assert Float.floor(1.32453e-10) === 0.0
  end

  test "floor with precision" do
    assert Float.floor(12.524235, 0) === 12.0
    assert Float.floor(-12.524235, 0) === -13.0

    assert Float.floor(12.52, 2) === 12.52
    assert Float.floor(-12.52, 2) === -12.52

    assert Float.floor(12.524235, 2) === 12.52
    assert Float.floor(-12.524235, 3) === -12.525
  end

  test "ceil" do
    assert Float.ceil(12.524235) === 13.0
    assert Float.ceil(-12.5) === -12.0
    assert Float.ceil(-12.524235) === -12.0
    assert Float.ceil(7.5e3) === 7500.0
    assert Float.ceil(7.5432e3) === 7544.0
    assert Float.ceil(7.5e-3) === 1.0
    assert Float.ceil(-12.32453e4) === -123245.0
    assert Float.ceil(-12.32453e-10) === 0.0
    assert Float.ceil(0.32453e-10) === 1.0
    assert Float.ceil(-0.32453e-10) === 0.0
    assert Float.ceil(1.32453e-10) === 1.0
  end

  test "ceil with precision" do
    assert Float.ceil(12.524235, 0) === 13.0
    assert Float.ceil(-12.524235, 0) === -12.0

    assert Float.ceil(12.52, 2) === 12.52
    assert Float.ceil(-12.52, 2) === -12.52

    assert Float.ceil(12.524235, 2) === 12.53
    assert Float.ceil(-12.524235, 3) === -12.524
  end

  test "round" do
    assert Float.round(5.5675, 3) === 5.568
    assert Float.round(-5.5674, 3) === -5.567
    assert Float.round(5.5, 3) === 5.5
    assert Float.round(5.5e-10, 10) === 6.0e-10
    assert Float.round(5.5e-10, 8) === 0.0
    assert Float.round(5.0, 0) === 5.0
  end
end
