Code.require_file "test_helper.exs", __DIR__

defmodule ListTest do
  use ExUnit.Case, async: true

  doctest List

  test "cons cell precedence" do
    assert [1|:lists.flatten([2, 3])] == [1, 2, 3]
  end

  test "optional comma" do
    assert [1] == [ 1, ]
    assert [1, 2, 3] == [1, 2, 3, ]
  end

  test "partial application" do
    assert (&[&1, 2]).(1) == [1, 2]
    assert (&[&1, &2]).(1, 2) == [1, 2]
    assert (&[&2, &1]).(2, 1) == [1, 2]
    assert (&[&1|&2]).(1, 2) == [1|2]
    assert (&[&1, &2|&3]).(1, 2, 3) == [1, 2|3]
  end

  test "wrap" do
    assert List.wrap([1, 2, 3]) == [1, 2, 3]
    assert List.wrap(1) == [1]
    assert List.wrap(nil) == []
  end

  test "flatten" do
    assert List.flatten([1, 2, 3]) == [1, 2, 3]
    assert List.flatten([1, [2], 3]) == [1, 2, 3]
    assert List.flatten([[1, [2], 3]]) == [1, 2, 3]

    assert List.flatten([]) == []
    assert List.flatten([[]]) == []
  end

  test "flatten with tail" do
    assert List.flatten([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]
    assert List.flatten([1, [2], 3], [4, 5]) == [1, 2, 3, 4, 5]
    assert List.flatten([[1, [2], 3]], [4, 5]) == [1, 2, 3, 4, 5]
  end

  test "foldl" do
    assert List.foldl([1, 2, 3], 0, fn x, y -> x + y end) == 6
    assert List.foldl([1, 2, 3], 10, fn x, y -> x + y end) == 16
    assert List.foldl([1, 2, 3, 4], 0, fn x, y -> x - y end) == 2
  end

  test "foldr" do
    assert List.foldr([1, 2, 3], 0, fn x, y -> x + y end) == 6
    assert List.foldr([1, 2, 3], 10, fn x, y -> x + y end) == 16
    assert List.foldr([1, 2, 3, 4], 0, fn x, y -> x - y end) == -2
  end

  test "reverse" do
    assert Enum.reverse([1, 2, 3]) == [3, 2, 1]
  end

  test "duplicate" do
    assert List.duplicate(1, 3) == [1, 1, 1]
    assert List.duplicate([1], 1) == [[1]]
  end

  test "last" do
    assert List.last([]) == nil
    assert List.last([1]) == 1
    assert List.last([1, 2, 3]) == 3
  end

  test "zip" do
    assert List.zip([[1, 4], [2, 5], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1, 4], [2, 5, 0], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1], [2, 5], [3, 6]]) == [{1, 2, 3}]
    assert List.zip([[1, 4], [2, 5], []]) == []
  end

  test "keyfind" do
    assert List.keyfind([a: 1, b: 2], :a, 0) == {:a, 1}
    assert List.keyfind([a: 1, b: 2], 2, 1) == {:b, 2}
    assert List.keyfind([a: 1, b: 2], :c, 0) == nil
  end

  test "keyreplace" do
    assert List.keyreplace([a: 1, b: 2], :a, 0, {:a, 3}) == [a: 3, b: 2]
    assert List.keyreplace([a: 1], :b, 0, {:b, 2}) == [a: 1]
  end

  test "keysort" do
    assert List.keysort([a: 4, b: 3, c: 5], 1) == [b: 3, a: 4, c: 5]
    assert List.keysort([a: 4, c: 1, b: 2], 0) == [a: 4, b: 2, c: 1]
  end

  test "keystore" do
    assert List.keystore([a: 1, b: 2], :a, 0, {:a, 3}) == [a: 3, b: 2]
    assert List.keystore([a: 1], :b, 0, {:b, 2}) == [a: 1, b: 2]
  end

  test "keymember?" do
    assert List.keymember?([a: 1, b: 2], :a, 0) == true
    assert List.keymember?([a: 1, b: 2], 2, 1) == true
    assert List.keymember?([a: 1, b: 2], :c, 0) == false
  end

  test "keydelete" do
    assert List.keydelete([a: 1, b: 2], :a, 0) == [{:b, 2}]
    assert List.keydelete([a: 1, b: 2], 2, 1) == [{:a, 1}]
    assert List.keydelete([a: 1, b: 2], :c, 0) == [{:a, 1}, {:b, 2}]
  end

  test "insert at" do
    assert List.insert_at([1, 2, 3], 0, 0) == [0, 1, 2, 3]
    assert List.insert_at([1, 2, 3], 3, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], 2, 0) == [1, 2, 0, 3]
    assert List.insert_at([1, 2, 3], 10, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], -1, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], -4, 0) == [0, 1, 2, 3]
    assert List.insert_at([1, 2, 3], -10, 0) == [0, 1, 2, 3]
  end

  test "replace at" do
    assert List.replace_at([1, 2, 3], 0, 0) == [0, 2, 3]
    assert List.replace_at([1, 2, 3], 1, 0) == [1, 0, 3]
    assert List.replace_at([1, 2, 3], 2, 0) == [1, 2, 0]
    assert List.replace_at([1, 2, 3], 3, 0) == [1, 2, 3]
    assert List.replace_at([1, 2, 3], -1, 0) == [1, 2, 0]
    assert List.replace_at([1, 2, 3], -4, 0) == [1, 2, 3]
  end

  test "update at" do
    assert List.update_at([1, 2, 3], 0, &(&1 + 1)) == [2, 2, 3]
    assert List.update_at([1, 2, 3], 1, &(&1 + 1)) == [1, 3, 3]
    assert List.update_at([1, 2, 3], 2, &(&1 + 1)) == [1, 2, 4]
    assert List.update_at([1, 2, 3], 3, &(&1 + 1)) == [1, 2, 3]
    assert List.update_at([1, 2, 3], -1, &(&1 + 1)) == [1, 2, 4]
    assert List.update_at([1, 2, 3], -4, &(&1 + 1)) == [1, 2, 3]
  end

  test "delete at" do
    Enum.each [-1, 0, 1], fn i ->
      assert [] = List.delete_at([], i)
    end
    assert List.delete_at([1, 2, 3], 0) == [2, 3]
    assert List.delete_at([1, 2, 3], 2) == [1, 2]
    assert List.delete_at([1, 2, 3], 3) == [1, 2, 3]
    assert List.delete_at([1, 2, 3], -1) == [1, 2]
    assert List.delete_at([1, 2, 3], -3) == [2, 3]
    assert List.delete_at([1, 2, 3], -4) == [1, 2, 3]
  end

  test "to string" do
    assert List.to_string([?æ, ?ß]) == "æß"
    assert List.to_string([?a, ?b, ?c]) == "abc"

    assert_raise UnicodeConversionError,
                 "invalid code point 57343", fn ->
      List.to_string([0xDFFF])
    end
    assert_raise UnicodeConversionError,
                 "invalid encoding starting at <<216, 0>>", fn ->
      List.to_string(["a", "b", <<0xD800 :: size(16)>>])
    end

    assert_raise ArgumentError, ~r"cannot convert the given list to a string", fn ->
      List.to_string([:a, :b])
    end
  end
end
