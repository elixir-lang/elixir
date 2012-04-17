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

  test :do_argless_loop do
    Process.self <- 1
    Process.self <- 2
    Process.self <- 3

    result = loop do
      receive do
      match: x
        Process.put x, -x
        recur
      after: 0
        :ok
      end
    end

    assert_equal :ok, result
    assert_equal -1, Process.get(1)
    assert_equal -2, Process.get(2)
    assert_equal -3, Process.get(3)
  after:
    Process.delete(1)
    Process.delete(2)
    Process.delete(3)
  end
end
