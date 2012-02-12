Code.require_file "../../test_helper", __FILE__

defmodule Kernel::LoopTest do
  use ExUnit::Case

  def test_do_loop do
    list = [1,2,3]

    result = loop list, [] do
    match: [h|t], acc
      recur t, [h*2|acc]
    match: [], acc
      acc
    end

    assert_equal [6,4,2], result
  end

  def test_do_loop_base do
    fun = fn do
    match: { 1, 2 }, []
      1
    match: [], x when x == []
      2
    end

    assert_equal 1, fun.({ 1, 2 }, [])
    assert_equal 2, fun.([], [])
  end
end
