Code.require_file "test_helper.exs", __DIR__

defmodule FloatTest do
  use ExUnit.Case, async: true

  test :parse do
    assert Float.parse("12") === {12.0, ""}
    assert Float.parse("-12") === {-12.0, ""}
    assert Float.parse("123456789") === {123456789.0, ""}
    assert Float.parse("12.5") === {12.5, ""}
    assert Float.parse("12.524235") === {12.524235, ""}
    assert Float.parse("-12.5") === {-12.5, ""}
    assert Float.parse("-12.524235") === {-12.524235, ""}
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
    assert Float.parse("--1.2") === :error
    assert Float.parse("++1.2") === :error
    assert Float.parse("pi") === :error
  end

  test :floor do
    assert Float.floor(12) === 12
    assert Float.floor(-12) === -12
    assert Float.floor(12.524235) === 12
    assert Float.floor(-12.5) === -13
    assert Float.floor(-12.524235) === -13
    assert Float.floor(7.5e3) === 7500
    assert Float.floor(7.5432e3) === 7543
    assert Float.floor(7.5e-3) === 0
    assert Float.floor(-12.32453e4) === -123246
    assert Float.floor(-12.32453e-10) === -1
    assert Float.floor(0.32453e-10) === 0
    assert Float.floor(-0.32453e-10) === -1
    assert Float.floor(1.32453e-10) === 0
  end

  test :ceil do
    assert Float.ceil(12) === 12
    assert Float.ceil(-12) === -12
    assert Float.ceil(12.524235) === 13
    assert Float.ceil(-12.5) === -12
    assert Float.ceil(-12.524235) === -12
    assert Float.ceil(7.5e3) === 7500
    assert Float.ceil(7.5432e3) === 7544
    assert Float.ceil(7.5e-3) === 1
    assert Float.ceil(-12.32453e4) === -123245
    assert Float.ceil(-12.32453e-10) === 0
    assert Float.ceil(0.32453e-10) === 1
    assert Float.ceil(-0.32453e-10) === 0
    assert Float.ceil(1.32453e-10) === 1
  end
end
