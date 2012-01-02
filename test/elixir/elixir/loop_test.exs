defmodule Elixir::LoopTest do
  use ExUnit::Case

  def test_do_loop do
    list = [1,2,3]

    [6,4,2] = loop list, [] do
    match: [h|t], acc
      recur t, [h*2|acc]
    match: [], acc
      acc
    end
  end

  def test_do_loop_base do
    fun = fn {
    match: { 1, 2 }, []
      1
    match: [], x when x == []
      2
    }

    1 = fun.({ 1, 2 }, [])
    2 = fun.([], [])
  end
end