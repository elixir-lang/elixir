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

    assert result == [6,4,2]
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

    assert result == [14,10,6]
  end

  test :do_loop_base do
    fun = fn do
    match: { 1, 2 }, []
      1
    match: [], x when x == []
      2
    end

    assert fun.({ 1, 2 }, []) == 1
    assert fun.([], []) == 2
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

    assert result == :ok
    assert Process.get(1) == -1
    assert Process.get(2) == -2
    assert Process.get(3) == -3
  after:
    Process.delete(1)
    Process.delete(2)
    Process.delete(3)
  end
end
