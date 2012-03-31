Code.require_file "../../test_helper", __FILE__

defmodule Kernel.LoopTest do
  use ExUnit.Case

  test :do_loop do
    list = [1,2,3]

    result = loop list, [] do
    match: [h|t], acc
      recur t, [h*2|acc]
    match: [], acc
      acc
    end

    assert_equal [6,4,2], result
  end

  test :do_nested_loop do
    list = [[1,2],[2,3],[3,4]]

    result = loop list, [] do
    match: [h|t], acc
      result = loop h, 0 do
      match: [h|t], acc
        recur t, acc + h*2
      match: [], acc
        acc
      end
      recur t, [result|acc]
    match: [], acc
      acc
    end

    assert_equal [14,10,6], result
  end

  test :do_loop_base do
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
