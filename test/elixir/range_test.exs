Code.require_file "../test_helper", __FILE__

defmodule RangeTest do
  use ExUnit.Case

  test :first do
    assert Range.new(1, 3).first == 1
  end

  test :last do
    assert Range.new(1, 3).last == 3
  end

  test :op do
    assert (1..3).first == 1
    assert (1..3).last  == 3
  end
end