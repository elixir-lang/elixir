Code.require_file "../test_helper", __FILE__

defmodule ListTest do
  use ExUnit::Case

  test :brackets_function do
    assert_equal [1,2,3], :[].(1,2,3)
  end

  test :wrap do
    assert_equal [1,2,3], List.wrap [1,2,3]
    assert_equal [1], List.wrap 1
    assert_equal [], List.wrap nil
  end

  test :flatten do
    assert_equal [1,2,3], List.flatten([1,2,3])
    assert_equal [1,2,3], List.flatten([1,[2],3])
    assert_equal [1,2,3], List.flatten([[1,[2],3]])

    assert_equal [], List.flatten([])
    assert_equal [], List.flatten([[]])
  end

  test :flatten_with_tail do
    assert_equal [1,2,3,4,5], List.flatten([1,2,3], [4,5])
    assert_equal [1,2,3,4,5], List.flatten([1,[2],3], [4,5])
    assert_equal [1,2,3,4,5], List.flatten([[1,[2],3]], [4,5])
  end

  test :foldl do
    assert_equal 6, List.foldl([1,2,3], 0, fn(x,y) -> x + y end)
    assert_equal 16, List.foldl([1,2,3], 10, fn(x,y) -> x + y end)
    assert_equal 2, List.foldl([1,2,3,4], 0, fn(x,y) -> x - y end)
  end

  test :foldr do
    assert_equal 6, List.foldr([1,2,3], 0, fn(x,y) -> x + y end)
    assert_equal 16, List.foldr([1,2,3], 10, fn(x,y) -> x + y end)
    assert_equal -2, List.foldr([1,2,3,4], 0, fn(x,y) -> x - y end)
  end

  def test_member? do
    assert List.member? [1,2,3], 1
    refute List.member? [1,2,3], 0
    refute List.member? [], 0
  end

  test :seq do
    assert_equal [1,2,3], List.seq(1,3)
    assert_equal [1], List.seq(1,1)
    assert_equal [], List.seq(1,0)
  end

  test :prepend do
    assert_equal [0,1,2,3], List.prepend [1,0], [2,3]
  end

  test :append_1 do
    assert_equal [1,[2],3,4,5,6], List.append [[1,[2],3], [4], [5,6]]
  end

  test :append_2 do
    assert_equal [1,[2],3,4,5], List.append [1,[2],3], [4,5]
  end

  test :reverse do
    assert_equal [3,2,1], List.reverse [1,2,3]
  end

  test :uniq do
    assert_equal [1,2,3], List.uniq [1,2,3,2,1]
  end

  test :duplicate do
    assert_equal [1,1,1], List.duplicate 1, 3
    assert_equal [[1]], List.duplicate [1], 1
  end

  test :find_index do
    assert_equal nil, List.find_index([], 'a')
    assert_equal nil, List.find_index(['a'], 'b')
    assert_equal 1, List.find_index(['a'], 'a')
    assert_equal 4, List.find_index([1,2,4,3], 3)
  end
end
