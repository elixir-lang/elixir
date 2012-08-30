Code.require_file "../test_helper.exs", __FILE__

defmodule ListTest do
  use ExUnit.Case, async: true

  test :brackets_function do
    assert :[].(1,2,3) == [1,2,3]
  end

  test :wrap do
    assert List.wrap([1,2,3]) == [1,2,3]
    assert List.wrap(1) == [1]
    assert List.wrap(nil) == []
  end

  test :flatten do
    assert List.flatten([1,2,3]) == [1,2,3]
    assert List.flatten([1,[2],3]) == [1,2,3]
    assert List.flatten([[1,[2],3]]) == [1,2,3]

    assert List.flatten([]) == []
    assert List.flatten([[]]) == []
  end

  test :flatten_with_tail do
    assert List.flatten([1,2,3], [4,5]) == [1,2,3,4,5]
    assert List.flatten([1,[2],3], [4,5]) == [1,2,3,4,5]
    assert List.flatten([[1,[2],3]], [4,5]) == [1,2,3,4,5]
  end

  test :foldl do
    assert List.foldl([1,2,3], 0, fn x,y -> x + y end) == 6
    assert List.foldl([1,2,3], 10, fn x,y -> x + y end) == 16
    assert List.foldl([1,2,3,4], 0, fn x,y -> x - y end) == 2
  end

  test :foldr do
    assert List.foldr([1,2,3], 0, fn x,y -> x + y end) == 6
    assert List.foldr([1,2,3], 10, fn x,y -> x + y end) == 16
    assert List.foldr([1,2,3,4], 0, fn x,y -> x - y end) == -2
  end

  def test_member? do
    assert List.member? [1,2,3], 1
    refute List.member? [1,2,3], 0
    refute List.member? [], 0
  end

  test :range do
    assert List.range(1,3) == [1,2,3]
    assert List.range(1, 1) == [1]
    assert List.range(5, 0) == [5,4,3,2,1,0]
    assert List.range(1, 0, -1) == [1,0]
    assert List.range(1,8,2) == [1,3,5,7]
    assert List.range(7,-1,-3) == [7,4,1]
    assert List.range(2,1,1) == []
    assert List.range(8,1,1) == []
    assert List.range(1,8,-1) == []
    assert List.range(1,1,-1) == []
  end

  test :sort do
    assert List.sort([3, 5, 1, 2, 4]) == [1,2,3,4,5]
    assert List.sort([3, 5, 1, 2, 4], &2 <= &1) == [5,4,3,2,1]
    assert List.sort(['2', '3', '0', '11', '10']) == ['0', '10', '11', '2', '3']
    assert ['0', '2', '3', '10', '11'] == List.sort ['2', '3', '0', '11', '10'], fn a, b ->
      {na, _} = :string.to_integer a
      {nb, _} = :string.to_integer b
      na <= nb
    end
  end

  test :concat_1 do
    assert List.concat([[1,[2],3], [4], [5,6]]) == [1,[2],3,4,5,6]
  end

  test :concat_2 do
    assert List.concat([1,[2],3], [4,5]) == [1,[2],3,4,5]
  end

  test :reverse do
    assert Enum.reverse([1,2,3]) == [3,2,1]
  end

  test :uniq do
    assert List.uniq([1,2,3,2,1]) == [1,2,3]
  end

  test :duplicate do
    assert List.duplicate(1, 3) == [1,1,1]
    assert List.duplicate([1], 1) == [[1]]
  end

  test :last do
    assert List.last([]) == nil
    assert List.last([1]) == 1
    assert List.last([1, 2, 3]) == 3
  end

  test :zip do
    assert List.zip([:a, :b], [1, 2]) == [{:a, 1}, {:b, 2}]
    assert List.zip([:a, :b], [1, 2, 3, 4]) == [{:a, 1}, {:b, 2}]
    assert List.zip([:a, :b, :c, :d], [1, 2]) == [{:a, 1}, {:b, 2}]
    assert List.zip([], [1]) == []
    assert List.zip([1], []) == []
    assert List.zip([], []) == []
  end

  test :zip_tuples do
    assert List.zip({:a, :b}, {1, 2}) == [{:a, 1}, {:b, 2}]
    assert List.zip([:a, :b], {1, 2}) == [{:a, 1}, {:b, 2}]
    assert List.zip({:a, :b}, [1, 2]) == [{:a, 1}, {:b, 2}]
  end

  test :zip_lists do
    assert List.zip([[1, 4], [2, 5], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1, 4], [2, 5, 0], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1], [2, 5], [3, 6]]) == [{1, 2, 3}]
    assert List.zip([[1, 4], [2, 5], []]) == []
  end

  test :unzip do
    assert List.unzip([{1, 2, 3}, {4, 5, 6}]) == [[1, 4], [2, 5], [3, 6]]
    assert List.unzip([{1, 2, 3}, {4, 5}]) == [[1, 4], [2, 5]]
    assert List.unzip([[1, 2, 3], [4, 5]]) == [[1, 4], [2, 5]]
  end

  test :keyfind do
    assert List.keyfind([a: 1, b: 2], :a, 1) == { :a, 1 }
    assert List.keyfind([a: 1, b: 2], 2, 2) == { :b, 2 }
    assert List.keyfind([a: 1, b: 2], :c, 1) == nil
  end

  test :keyreplace do
    assert List.keyreplace([a: 1, b: 2], :a, 1, { :a, 3 }) == [a: 3, b: 2]
    assert List.keyreplace([a: 1], :b, 1, { :b, 2 }) == [a: 1]
  end

  test :keystore do
    assert List.keystore([a: 1, b: 2], :a, 1, { :a, 3 }) == [a: 3, b: 2]
    assert List.keystore([a: 1], :b, 1, { :b, 2 }) == [a: 1, b: 2]
  end

  test :keymember? do
    assert List.keymember?([a: 1, b: 2], :a, 1) == true
    assert List.keymember?([a: 1, b: 2], 2, 2) == true
    assert List.keymember?([a: 1, b: 2], :c, 1) == false
  end

  test :keydelete do
    assert List.keydelete([a: 1, b: 2], :a, 1) == [{ :b, 2 }]
    assert List.keydelete([a: 1, b: 2], 2, 2) == [{ :a, 1 }]
    assert List.keydelete([a: 1, b: 2], :c, 1) == [{ :a, 1 }, { :b, 2 }]
  end

  test :keytake do
    assert List.keytake([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], :a, 1)
      == { { :a, 1 }, [ { :b, 2 }, true, { :a, 2, nil } ] }
    assert List.keytake([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], 2, 2)
      == { { :b, 2 }, [ { :a, 1 }, true, { :a, 2, nil } ] }
    assert List.keytake([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], :c, 1)
      == { nil, [ { :a, 1 }, { :b, 2 }, true, { :a, 2, nil } ] }

  end

  test :keytakeall do
    assert List.keytakeall([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], :a, 1)
      == { [ { :a, 1 }, { :a, 2, nil } ], [ { :b, 2 }, true ] }
    assert List.keytakeall([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], 2, 2)
      == { [ { :b, 2 }, { :a, 2, nil } ], [ { :a, 1 }, true ] }
    assert List.keytakeall([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], :c, 1)
      == { [], [ { :a, 1 }, { :b, 2 }, true, { :a, 2, nil } ] }
  end

  test :keyfindall do
    assert List.keyfindall([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], :a, 1)
      == [ { :a, 1 }, { :a, 2, nil } ]
    assert List.keyfindall([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], 2, 2)
      == [ { :b, 2 }, { :a, 2, nil } ]
    assert List.keyfindall([{:a, 1}, {:b, 2}, true, {:a, 2, nil}], :c, 1)
      == []
  end
end