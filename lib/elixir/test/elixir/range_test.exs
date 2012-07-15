Code.require_file "../test_helper", __FILE__

defmodule RangeTest do
  use ExUnit.Case, async: true

  test :first do
    assert Range.new(first: 1, last: 3).first == 1
  end

  test :last do
    assert Range.new(first: 1, last: 3).last == 3
  end

  test :op do
    assert (1..3).first == 1
    assert (1..3).last  == 3
  end

  test :in do
    refute 0 in 1..3, "raw range assertion"
    assert 1 in 1..3, "raw range assertion"
    assert 2 in 1..3, "raw range assertion"
    assert 3 in 1..3, "raw range assertion"
    refute 4 in 1..3, "raw range assertion"
  end
end