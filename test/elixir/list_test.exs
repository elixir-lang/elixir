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

  test :range do
    assert_equal [1,2,3], List.range(1,3)
    assert_equal [1], List.range(1, 1)
    assert_equal [5,4,3,2,1,0], List.range(5, 0)
    assert_equal [1,0], List.range(1, 0, -1)
    assert_equal [1,3,5,7], List.range(1,8,2)
    assert_equal [7,4,1], List.range(7,-1,-3)
    assert_equal [], List.range(2,1,1)
    assert_equal [], List.range(8,1,1)
    assert_equal [], List.range(1,8,-1)
    assert_equal [], List.range(1,1,-1)
  end

  test :sort do
    assert_equal [1,2,3,4,5], List.sort [3, 5, 1, 2, 4]
    assert_equal [5,4,3,2,1], List.sort [3, 5, 1, 2, 4], &2 <= &1
    assert_equal ['0', '10', '11', '2', '3'], List.sort ['2', '3', '0', '11', '10']
    assert_equal ['0', '2', '3', '10', '11'], List.sort ['2', '3', '0', '11', '10'], fn(a, b) ->
      {na, _} = :string.to_integer a
      {nb, _} = :string.to_integer b
      na <= nb
    end
  end

  test :prepend do
    assert_equal [0,1,2,3], List.prepend [1,0], [2,3]
  end

  test :concat_1 do
    assert_equal [1,[2],3,4,5,6], List.concat [[1,[2],3], [4], [5,6]]
  end

  test :concat_2 do
    assert_equal [1,[2],3,4,5], List.concat [1,[2],3], [4,5]
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

  test :zip do
    assert_equal [{:a, 1}, {:b, 2}], List.zip [:a, :b], [1, 2]
    assert_equal [{:a, 1}, {:b, 2}], List.zip [:a, :b], [1, 2, 3, 4]
    assert_equal [{:a, 1}, {:b, 2}], List.zip [:a, :b, :c, :d], [1, 2]
    assert_equal [], List.zip [], [1]
    assert_equal [], List.zip [1], []
    assert_equal [], List.zip [], []
  end

  test :zip_tuples do
    assert_equal [{:a, 1}, {:b, 2}], List.zip {:a, :b}, {1, 2}
    assert_equal [{:a, 1}, {:b, 2}], List.zip [:a, :b], {1, 2}
    assert_equal [{:a, 1}, {:b, 2}], List.zip {:a, :b}, [1, 2]
  end

  test :zip_lists do
    assert_equal [{1, 2, 3}, {4, 5, 6}], List.zip [[1, 4], [2, 5], [3, 6]]
    assert_equal [{1, 2, 3}, {4, 5, 6}], List.zip [[1, 4], [2, 5, 0], [3, 6]]
    assert_equal [{1, 2, 3}], List.zip [[1], [2, 5], [3, 6]]
    assert_equal [], List.zip [[1, 4], [2, 5], []]
  end

  test :unzip do
    assert_equal [[1, 4], [2, 5], [3, 6]], List.unzip [{1, 2, 3}, {4, 5, 6}]
    assert_equal [[1, 4], [2, 5]], List.unzip [{1, 2, 3}, {4, 5}]
    assert_equal [[1, 4], [2, 5]], List.unzip [[1, 2, 3], [4, 5]]
  end
end
