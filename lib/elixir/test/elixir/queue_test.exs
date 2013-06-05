Code.require_file "test_helper.exs", __DIR__

defmodule QueueTest do
  use ExUnit.Case, async: true
  alias Queue, as: Q

  test :new do
    assert match?({ _, [], [] }, Q.new)
    assert match?({ _, [], [1, 2, 3] }, Q.new(1 .. 3))
  end

  test :empty? do
    assert Q.new |> Q.empty?
    refute Q.new(1 .. 3) |> Q.empty?
  end

  test :enq do
    assert match?({ _, [], [23] }, Q.new |> Q.enq(23))
    assert match?({ _, [], [23, 42] }, Q.new |> Q.enq(23) |> Q.enq(42))
    assert match?({ _, [1337], [23, 42] }, Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337))
  end

  test :deq do
    assert match?({ :empty, { _, [], [] } }, Q.new |> Q.deq(:empty))
    assert match?({ 23, { _, [], [] } }, Q.new |> Q.enq(23) |> Q.deq)
    assert match?({ 23, { _, [], [42] } }, Q.new |> Q.enq(23) |> Q.enq(42) |> Q.deq)
    assert match?({ 23, { _, [1337], [42] } }, Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> Q.deq)
  end

  test :deq! do
    assert_raise Queue.Empty, fn ->
      Q.new |> Q.deq!
    end
  end

  test :peek do
    assert Q.new |> Q.peek(:empty) == :empty
    assert Q.new |> Q.enq(23) |> Q.peek == 23
  end

  test :peek! do
    assert_raise Queue.Empty, fn ->
      Q.new |> Q.peek!
    end
  end

  test :reverse do
    assert match?({ _, [23], [] }, Q.new |> Q.enq(23) |> Q.reverse)
    assert match?({ _, [23, 42], [1337] }, Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> Q.reverse)
  end

  test :member? do
    refute Q.new |> Q.member?(23)
    assert Q.new |> Q.enq(23) |> Q.member?(23)
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> Q.member?(1337)
  end

  test :size do
    assert Q.new |> Q.size == 0
    assert Q.new |> Q.enq(23) |> Q.size == 1
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.size == 2
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> Q.size == 3
  end

  test :foldl do
    assert Q.new |> Q.foldl([], [&1 | &2]) == []
    assert Q.new |> Q.enq(23) |> Q.foldl([], [&1 | &2]) == [23]
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.foldl([], [&1 | &2]) == [42, 23]
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> Q.foldl([], [&1 | &2]) == [1337, 42, 23]
  end

  test :foldr do
    assert Q.new |> Q.foldr([], [&1 | &2]) == []
    assert Q.new |> Q.enq(23) |> Q.foldr([], [&1 | &2]) == [23]
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.foldr([], [&1 | &2]) == [23, 42]
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> Q.foldr([], [&1 | &2]) == [23, 42, 1337]
  end

  test :to_list do
    assert Q.new(1 .. 3) |> Q.to_list == [1, 2, 3]
    assert Q.new |> Q.enq(23) |> Q.to_list == [23]
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.to_list == [23, 42]
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> Q.to_list == [23, 42, 1337]
  end

  test :inspect do
    assert Q.new |> inspect == "#Queue<[]>"
    assert Q.new |> Q.enq(23) |> inspect == "#Queue<[23]>"
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> inspect == "#Queue<[23,42]>"
    assert Q.new |> Q.enq(23) |> Q.enq(42) |> Q.enq(1337) |> inspect == "#Queue<[23,42,1337]>"
  end
end
