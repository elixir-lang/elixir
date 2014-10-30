Code.require_file "test_helper.exs", __DIR__

defmodule IntegerTest do
  use ExUnit.Case, async: true
  require Integer

  test :odd? do
    assert Integer.is_odd(0) == false
    assert Integer.is_odd(1) == true
    assert Integer.is_odd(2) == false
    assert Integer.is_odd(3) == true
    assert Integer.is_odd(-1) == true
    assert Integer.is_odd(-2) == false
    assert Integer.is_odd(-3) == true
  end

  test :even? do
    assert Integer.is_even(0) == true
    assert Integer.is_even(1) == false
    assert Integer.is_even(2) == true
    assert Integer.is_even(3) == false
    assert Integer.is_even(-1) == false
    assert Integer.is_even(-2) == true
    assert Integer.is_even(-3) == false
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

  test ":to_string/1" do
    assert Integer.to_string(100) == "100"
    assert Integer.to_string(-100) == "-100"
  end

  test ":to_string/2" do
    assert Integer.to_string(100, 2) == "1100100"
    assert Integer.to_string(100, 3) == "10201"
    assert Integer.to_string(100, 4) == "1210"
    assert Integer.to_string(100, 16) == "64"
    assert Integer.to_string(-100, 16) == "-64"
  end

  test ":to_char_list/1" do
    assert Integer.to_char_list(100) == '100'
    assert Integer.to_char_list(-100) == '-100'
  end

  test ":to_char_list/2" do
    assert Integer.to_char_list(100, 2) == '1100100'
    assert Integer.to_char_list(100, 3) == '10201'
    assert Integer.to_char_list(100, 4) == '1210'
    assert Integer.to_char_list(100, 16) == '64'
    assert Integer.to_char_list(-100, 16) == '-64'
  end
end
