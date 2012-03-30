Code.require_file "../test_helper", __FILE__

defmodule ListTest do
  use ExUnit.Case

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

  test :range_a do
    assert_equal [1,2,3], List.range_a(1,3)
    assert_equal [1], List.range_a(1, 1)
    assert_equal [], List.range_a(1, 0)
    assert_equal [], List.range_a(1, -1)
    assert_equal [1,3,5,7], List.range_a(1,8,2)
    assert_equal [1,3,5,7,9], List.range_a(1,9,2)
    assert_equal [7,4,1], List.range_a(7,-1,-3)
    assert_equal [], List.range_a(1,1,-1)
    assert_equal [], List.range_a(0,1,-1)
  end

  test :range_b do
    assert_equal [1,2,3], List.range_b(1,3)
    assert_equal [1], List.range_b(1, 1)
    assert_equal [5,4,3,2,1,0], List.range_b(5, 0)
    assert_equal [1,0], List.range_b(1, 0, -1)
    assert_equal [1,3,5,7], List.range_b(1,8,2)
    assert_equal [7,4,1], List.range_b(7,-1,-3)
    assert_equal [], List.range_b(2,1,1)
    assert_equal [], List.range_b(8,1,1)
    assert_equal [], List.range_b(1,8,-1)
    assert_equal [], List.range_b(1,1,-1)
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

  test :last do
    assert_equal nil, List.last []
    assert_equal 1, List.last [1]
    assert_equal 3, List.last [1, 2, 3]
  end
end
