Code.require_file "test_helper.exs", __DIR__

defmodule StackTest do
  use ExUnit.Case, async: true
  alias Stack, as: S

  test :new do
    assert match?({ _, [] }, S.new)
    assert match?({ _, [1, 2, 3] }, S.new(1 .. 3))
  end

  test :empty? do
    assert S.new |> S.empty?
    refute S.new(1 .. 3) |> S.empty?
  end

  test :push do
    assert match?({ _, [23] }, S.new |> S.push(23))
    assert match?({ _, [42, 23] }, S.new |> S.push(23) |> S.push(42))
    assert match?({ _, [1337, 42, 23] }, S.new |> S.push(23) |> S.push(42) |> S.push(1337))
  end

  test :pop do
    assert match?({ :empty, { _, [] } }, S.new |> S.pop(:empty))
    assert match?({ 23, { _, [] } }, S.new |> S.push(23) |> S.pop)
    assert match?({ 42, { _, [23] } }, S.new |> S.push(23) |> S.push(42) |> S.pop)
    assert match?({ 1337, { _, [42, 23] } }, S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> S.pop)
  end

  test :pop! do
    assert_raise Stack.Empty, fn ->
      S.new |> S.pop!
    end
  end

  test :peek do
    assert S.new |> S.peek(:empty) == :empty
    assert S.new |> S.push(23) |> S.peek == 23
  end

  test :peek! do
    assert_raise Stack.Empty, fn ->
      S.new |> S.peek!
    end
  end

  test :reverse do
    assert match?({ _, [23, 42] }, S.new |> S.push(23) |> S.push(42) |> S.reverse)
    assert match?({ _, [23, 42, 1337] }, S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> S.reverse)
  end

  test :member? do
    refute S.new |> S.member?(23)
    assert S.new |> S.push(23) |> S.member?(23)
    assert S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> S.member?(1337)
  end

  test :size do
    assert S.new |> S.size == 0
    assert S.new |> S.push(23) |> S.size == 1
    assert S.new |> S.push(23) |> S.push(42) |> S.size == 2
    assert S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> S.size == 3
  end

  test :foldl do
    assert S.new |> S.foldl([], [&1 | &2]) == []
    assert S.new |> S.push(23) |> S.foldl([], [&1 | &2]) == [23]
    assert S.new |> S.push(23) |> S.push(42) |> S.foldl([], [&1 | &2]) == [23, 42]
    assert S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> S.foldl([], [&1 | &2]) == [23, 42, 1337]
  end

  test :foldr do
    assert S.new |> S.foldr([], [&1 | &2]) == []
    assert S.new |> S.push(23) |> S.foldr([], [&1 | &2]) == [23]
    assert S.new |> S.push(23) |> S.push(42) |> S.foldr([], [&1 | &2]) == [42, 23]
    assert S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> S.foldr([], [&1 | &2]) == [1337, 42, 23]
  end

  test :to_list do
    assert S.new(1 .. 3) |> S.to_list == [1, 2, 3]
    assert S.new |> S.push(23) |> S.to_list == [23]
    assert S.new |> S.push(23) |> S.push(42) |> S.to_list == [42, 23]
    assert S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> S.to_list == [1337, 42, 23]
  end

  test :inspect do
    assert S.new |> inspect == "#Stack<[]>"
    assert S.new |> S.push(23) |> inspect == "#Stack<[23]>"
    assert S.new |> S.push(23) |> S.push(42) |> inspect == "#Stack<[42,23]>"
    assert S.new |> S.push(23) |> S.push(42) |> S.push(1337) |> inspect == "#Stack<[1337,42,23]>"
  end
end
