Code.require_file "test_helper.exs", __DIR__

defmodule ListTest do
  use ExUnit.Case, async: true

  test :cons_cell_precedence do
    assert [1|:lists.flatten([2, 3])] == [1, 2, 3]
  end

  test :optional_comma do
    assert [1] == [ 1, ]
    assert [1, 2, 3] == [1, 2, 3, ]
  end

  test :partial_application do
    assert ([&1, 2]).(1) == [1, 2]
    assert ([&1, &2]).(1, 2) == [1, 2]
    assert ([&2, &1]).(2, 1) == [1, 2]
    assert ([&1|&2]).(1, 2) == [1|2]
    assert ([&1, &2|&3]).(1, 2, 3) == [1, 2|3]
  end

  test :wrap do
    assert List.wrap([1, 2, 3]) == [1, 2, 3]
    assert List.wrap(1) == [1]
    assert List.wrap(nil) == []
  end

  test :flatten do
    assert List.flatten([1, 2, 3]) == [1, 2, 3]
    assert List.flatten([1, [2], 3]) == [1, 2, 3]
    assert List.flatten([[1, [2], 3]]) == [1, 2, 3]

    assert List.flatten([]) == []
    assert List.flatten([[]]) == []
  end

  test :flatten_with_tail do
    assert List.flatten([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]
    assert List.flatten([1, [2], 3], [4, 5]) == [1, 2, 3, 4, 5]
    assert List.flatten([[1, [2], 3]], [4, 5]) == [1, 2, 3, 4, 5]
  end

  test :foldl do
    assert List.foldl([1, 2, 3], 0, fn x, y -> x + y end) == 6
    assert List.foldl([1, 2, 3], 10, fn x, y -> x + y end) == 16
    assert List.foldl([1, 2, 3, 4], 0, fn x, y -> x - y end) == 2
  end

  test :foldr do
    assert List.foldr([1, 2, 3], 0, fn x, y -> x + y end) == 6
    assert List.foldr([1, 2, 3], 10, fn x, y -> x + y end) == 16
    assert List.foldr([1, 2, 3, 4], 0, fn x, y -> x - y end) == -2
  end

  test :concat_1 do
    assert List.concat([[1, [2], 3], [4], [5, 6]]) == [1, [2], 3, 4, 5, 6]
  end

  test :concat_2 do
    assert List.concat([1, [2], 3], [4, 5]) == [1, [2], 3, 4, 5]
  end

  test :reverse do
    assert Enum.reverse([1, 2, 3]) == [3, 2, 1]
  end

  test :duplicate do
    assert List.duplicate(1, 3) == [1, 1, 1]
    assert List.duplicate([1], 1) == [[1]]
  end

  test :last do
    assert List.last([]) == nil
    assert List.last([1]) == 1
    assert List.last([1, 2, 3]) == 3
  end

  test :zip do
    assert List.zip([[1, 4], [2, 5], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1, 4], [2, 5, 0], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1], [2, 5], [3, 6]]) == [{1, 2, 3}]
    assert List.zip([[1, 4], [2, 5], []]) == []
  end

  test :unzip do
    assert List.unzip([{1, 2, 3}, {4, 5, 6}]) == [[1, 4], [2, 5], [3, 6]]
    assert List.unzip([{1, 2, 3}, {4, 5}]) == [[1, 4], [2, 5]]
    assert List.unzip([[1, 2, 3], [4, 5]]) == [[1, 4], [2, 5]]
    assert List.unzip([]) == []
  end

  test :keyfind do
    assert List.keyfind([a: 1, b: 2], :a, 0) == { :a, 1 }
    assert List.keyfind([a: 1, b: 2], 2, 1) == { :b, 2 }
    assert List.keyfind([a: 1, b: 2], :c, 0) == nil
  end

  test :keyreplace do
    assert List.keyreplace([a: 1, b: 2], :a, 0, { :a, 3 }) == [a: 3, b: 2]
    assert List.keyreplace([a: 1], :b, 0, { :b, 2 }) == [a: 1]
  end

  test :keysort do
    assert List.keysort([a: 4, b: 3, c: 5], 1) == [b: 3, a: 4, c: 5]
    assert List.keysort([a: 4, c: 1, b: 2], 0) == [a: 4, b: 2, c: 1]
  end

  test :keystore do
    assert List.keystore([a: 1, b: 2], :a, 0, { :a, 3 }) == [a: 3, b: 2]
    assert List.keystore([a: 1], :b, 0, { :b, 2 }) == [a: 1, b: 2]
  end

  test :keymember? do
    assert List.keymember?([a: 1, b: 2], :a, 0) == true
    assert List.keymember?([a: 1, b: 2], 2, 1) == true
    assert List.keymember?([a: 1, b: 2], :c, 0) == false
  end

  test :keydelete do
    assert List.keydelete([a: 1, b: 2], :a, 0) == [{ :b, 2 }]
    assert List.keydelete([a: 1, b: 2], 2, 1) == [{ :a, 1 }]
    assert List.keydelete([a: 1, b: 2], :c, 0) == [{ :a, 1 }, { :b, 2 }]
  end

  test :insert_at do
    assert List.insert_at([1, 2, 3], 0, 0) == [0, 1, 2, 3]
    assert List.insert_at([1, 2, 3], 3, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], 2, 0) == [1, 2, 0, 3]
    assert List.insert_at([1, 2, 3], 10, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], -1, 0) == [1, 2, 0, 3]
    assert List.insert_at([1, 2, 3], -10, 0) == [0, 1, 2, 3]
  end
end
