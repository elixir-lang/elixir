Code.require_file "test_helper.exs", __DIR__

defmodule IntegerTest do
  use ExUnit.Case, async: true

  test :odd? do
    assert Integer.odd?(1) == true
    assert Integer.odd?(2) == false
    assert Integer.odd?(3) == true
    assert Integer.odd?(-1) == true
    assert Integer.odd?(-2) == false
    assert Integer.odd?(-3) == true
  end

  test :even? do
    assert Integer.even?(1) == false
    assert Integer.even?(2) == true
    assert Integer.even?(3) == false
    assert Integer.even?(-1) == false
    assert Integer.even?(-2) == true
    assert Integer.even?(-3) == false
  end

  test :parse do
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
  end
end
