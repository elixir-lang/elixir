Code.require_file("test_helper.exs", __DIR__)

defmodule ListTest do
  use ExUnit.Case, async: true

  doctest List

  test "cons cell precedence" do
    assert [1 | List.flatten([2, 3])] == [1, 2, 3]
  end

  test "optional comma" do
    assert Code.eval_string("[1,]") == {[1], []}
    assert Code.eval_string("[1, 2, 3,]") == {[1, 2, 3], []}
  end

  test "partial application" do
    assert (&[&1, 2]).(1) == [1, 2]
    assert (&[&1, &2]).(1, 2) == [1, 2]
    assert (&[&2, &1]).(2, 1) == [1, 2]
    assert (&[&1 | &2]).(1, 2) == [1 | 2]
    assert (&[&1, &2 | &3]).(1, 2, 3) == [1, 2 | 3]
  end

  test "delete/2" do
    assert List.delete([:a, :b, :c], :a) == [:b, :c]
    assert List.delete([:a, :b, :b, :c], :b) == [:a, :b, :c]
  end

  test "wrap/1" do
    assert List.wrap([1, 2, 3]) == [1, 2, 3]
    assert List.wrap(1) == [1]
    assert List.wrap(nil) == []
  end

  test "flatten/1" do
    assert List.flatten([1, 2, 3]) == [1, 2, 3]
    assert List.flatten([1, [2], 3]) == [1, 2, 3]
    assert List.flatten([[1, [2], 3]]) == [1, 2, 3]

    assert List.flatten([]) == []
    assert List.flatten([[]]) == []
  end

  test "flatten/2" do
    assert List.flatten([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]
    assert List.flatten([1, [2], 3], [4, 5]) == [1, 2, 3, 4, 5]
    assert List.flatten([[1, [2], 3]], [4, 5]) == [1, 2, 3, 4, 5]
  end

  test "foldl/3" do
    assert List.foldl([1, 2, 3], 0, fn x, y -> x + y end) == 6
    assert List.foldl([1, 2, 3], 10, fn x, y -> x + y end) == 16
    assert List.foldl([1, 2, 3, 4], 0, fn x, y -> x - y end) == 2
    assert List.foldl([], 5, fn x, acc -> x + acc + 100 end) == 5
  end

  test "foldr/3" do
    assert List.foldr([1, 2, 3], 0, fn x, y -> x + y end) == 6
    assert List.foldr([1, 2, 3], 10, fn x, y -> x + y end) == 16
    assert List.foldr([1, 2, 3, 4], 0, fn x, y -> x - y end) == -2
    assert List.foldr([], 5, fn x, acc -> x + acc + 100 end) == 5
  end

  test "duplicate/2" do
    assert List.duplicate(1, 3) == [1, 1, 1]
    assert List.duplicate([1], 1) == [[1]]
  end

  test "first/1" do
    assert List.first([]) == nil
    assert List.first([1]) == 1
    assert List.first([1, 2, 3]) == 1
  end

  test "last/1" do
    assert List.last([]) == nil
    assert List.last([1]) == 1
    assert List.last([1, 2, 3]) == 3
  end

  test "zip/1" do
    assert List.zip([[1, 4], [2, 5], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1, 4], [2, 5, 0], [3, 6]]) == [{1, 2, 3}, {4, 5, 6}]
    assert List.zip([[1], [2, 5], [3, 6]]) == [{1, 2, 3}]
    assert List.zip([[1, 4], [2, 5], []]) == []
  end

  test "keyfind/4" do
    assert List.keyfind([a: 1, b: 2], :a, 0) == {:a, 1}
    assert List.keyfind([a: 1, b: 2], 2, 1) == {:b, 2}
    assert List.keyfind([a: 1, b: 2], :c, 0) == nil
  end

  test "keyreplace/4" do
    assert List.keyreplace([a: 1, b: 2], :a, 0, {:a, 3}) == [a: 3, b: 2]
    assert List.keyreplace([a: 1], :b, 0, {:b, 2}) == [a: 1]
  end

  test "keysort/2" do
    assert List.keysort([a: 4, b: 3, c: 5], 1) == [b: 3, a: 4, c: 5]
    assert List.keysort([a: 4, c: 1, b: 2], 0) == [a: 4, b: 2, c: 1]
  end

  test "keystore/4" do
    assert List.keystore([a: 1, b: 2], :a, 0, {:a, 3}) == [a: 3, b: 2]
    assert List.keystore([a: 1], :b, 0, {:b, 2}) == [a: 1, b: 2]
  end

  test "keymember?/3" do
    assert List.keymember?([a: 1, b: 2], :a, 0) == true
    assert List.keymember?([a: 1, b: 2], 2, 1) == true
    assert List.keymember?([a: 1, b: 2], :c, 0) == false
  end

  test "keydelete/3" do
    assert List.keydelete([a: 1, b: 2], :a, 0) == [{:b, 2}]
    assert List.keydelete([a: 1, b: 2], 2, 1) == [{:a, 1}]
    assert List.keydelete([a: 1, b: 2], :c, 0) == [{:a, 1}, {:b, 2}]
  end

  test "keytake/3" do
    assert List.keytake([a: 1, b: 2], :a, 0) == {{:a, 1}, [b: 2]}
    assert List.keytake([a: 1, b: 2], 2, 1) == {{:b, 2}, [a: 1]}
    assert List.keytake([a: 1, b: 2], :c, 0) == nil
  end

  test "insert_at/3" do
    assert List.insert_at([1, 2, 3], 0, 0) == [0, 1, 2, 3]
    assert List.insert_at([1, 2, 3], 3, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], 2, 0) == [1, 2, 0, 3]
    assert List.insert_at([1, 2, 3], 10, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], -1, 0) == [1, 2, 3, 0]
    assert List.insert_at([1, 2, 3], -4, 0) == [0, 1, 2, 3]
    assert List.insert_at([1, 2, 3], -10, 0) == [0, 1, 2, 3]
  end

  test "replace_at/3" do
    assert List.replace_at([1, 2, 3], 0, 0) == [0, 2, 3]
    assert List.replace_at([1, 2, 3], 1, 0) == [1, 0, 3]
    assert List.replace_at([1, 2, 3], 2, 0) == [1, 2, 0]
    assert List.replace_at([1, 2, 3], 3, 0) == [1, 2, 3]
    assert List.replace_at([1, 2, 3], -1, 0) == [1, 2, 0]
    assert List.replace_at([1, 2, 3], -4, 0) == [1, 2, 3]
  end

  test "update_at/3" do
    assert List.update_at([1, 2, 3], 0, &(&1 + 1)) == [2, 2, 3]
    assert List.update_at([1, 2, 3], 1, &(&1 + 1)) == [1, 3, 3]
    assert List.update_at([1, 2, 3], 2, &(&1 + 1)) == [1, 2, 4]
    assert List.update_at([1, 2, 3], 3, &(&1 + 1)) == [1, 2, 3]
    assert List.update_at([1, 2, 3], -1, &(&1 + 1)) == [1, 2, 4]
    assert List.update_at([1, 2, 3], -4, &(&1 + 1)) == [1, 2, 3]
  end

  test "delete_at/2" do
    for index <- [-1, 0, 1] do
      assert List.delete_at([], index) == []
    end

    assert List.delete_at([1, 2, 3], 0) == [2, 3]
    assert List.delete_at([1, 2, 3], 2) == [1, 2]
    assert List.delete_at([1, 2, 3], 3) == [1, 2, 3]
    assert List.delete_at([1, 2, 3], -1) == [1, 2]
    assert List.delete_at([1, 2, 3], -3) == [2, 3]
    assert List.delete_at([1, 2, 3], -4) == [1, 2, 3]
  end

  test "pop_at/3" do
    for index <- [-1, 0, 1] do
      assert List.pop_at([], index) == {nil, []}
    end

    assert List.pop_at([1], 1, 2) == {2, [1]}
    assert List.pop_at([1, 2, 3], 0) == {1, [2, 3]}
    assert List.pop_at([1, 2, 3], 2) == {3, [1, 2]}
    assert List.pop_at([1, 2, 3], 3) == {nil, [1, 2, 3]}
    assert List.pop_at([1, 2, 3], -1) == {3, [1, 2]}
    assert List.pop_at([1, 2, 3], -3) == {1, [2, 3]}
    assert List.pop_at([1, 2, 3], -4) == {nil, [1, 2, 3]}
  end

  describe "starts_with?/2" do
    test "list and prefix are equal" do
      assert List.starts_with?([], [])
      assert List.starts_with?([1], [1])
      assert List.starts_with?([1, 2, 3], [1, 2, 3])
    end

    test "proper lists" do
      refute List.starts_with?([1], [1, 2])
      assert List.starts_with?([1, 2, 3], [1, 2])
      refute List.starts_with?([1, 2, 3], [1, 2, 3, 4])
    end

    test "list is empty" do
      refute List.starts_with?([], [1])
      refute List.starts_with?([], [1, 2])
    end

    test "prefix is empty" do
      assert List.starts_with?([1], [])
      assert List.starts_with?([1, 2], [])
      assert List.starts_with?([1, 2, 3], [])
    end

    test "only accepts proper lists" do
      message = "no function clause matching in List.starts_with?/2"

      assert_raise FunctionClauseError, message, fn ->
        List.starts_with?([1 | 2], [1 | 2])
      end

      message = "no function clause matching in List.starts_with?/2"

      assert_raise FunctionClauseError, message, fn ->
        List.starts_with?([1, 2], 1)
      end
    end
  end

  test "to_string/1" do
    assert List.to_string([?æ, ?ß]) == "æß"
    assert List.to_string([?a, ?b, ?c]) == "abc"

    assert_raise UnicodeConversionError, "invalid code point 57343", fn ->
      List.to_string([0xDFFF])
    end

    assert_raise UnicodeConversionError, "invalid encoding starting at <<216, 0>>", fn ->
      List.to_string(["a", "b", <<0xD800::size(16)>>])
    end

    assert_raise ArgumentError, ~r"cannot convert the given list to a string", fn ->
      List.to_string([:a, :b])
    end
  end

  test "to_charlist/1" do
    assert List.to_charlist([0x00E6, 0x00DF]) == 'æß'
    assert List.to_charlist([0x0061, "bc"]) == 'abc'
    assert List.to_charlist([0x0064, "ee", ['p']]) == 'deep'

    assert_raise UnicodeConversionError, "invalid code point 57343", fn ->
      List.to_charlist([0xDFFF])
    end

    assert_raise UnicodeConversionError, "invalid encoding starting at <<216, 0>>", fn ->
      List.to_charlist(["a", "b", <<0xD800::size(16)>>])
    end

    assert_raise ArgumentError, ~r"cannot convert the given list to a charlist", fn ->
      List.to_charlist([:a, :b])
    end
  end

  describe "myers_difference/2" do
    test "follows paper implementation" do
      assert List.myers_difference([], []) == []
      assert List.myers_difference([], [1, 2, 3]) == [ins: [1, 2, 3]]
      assert List.myers_difference([1, 2, 3], []) == [del: [1, 2, 3]]
      assert List.myers_difference([1, 2, 3], [1, 2, 3]) == [eq: [1, 2, 3]]
      assert List.myers_difference([1, 2, 3], [1, 4, 2, 3]) == [eq: [1], ins: [4], eq: [2, 3]]
      assert List.myers_difference([1, 4, 2, 3], [1, 2, 3]) == [eq: [1], del: [4], eq: [2, 3]]
      assert List.myers_difference([1], [[1]]) == [del: [1], ins: [[1]]]
      assert List.myers_difference([[1]], [1]) == [del: [[1]], ins: [1]]
    end

    test "rearranges inserts and equals for smaller diffs" do
      assert List.myers_difference([3, 2, 0, 2], [2, 2, 0, 2]) ==
               [del: [3], ins: [2], eq: [2, 0, 2]]

      assert List.myers_difference([3, 2, 1, 0, 2], [2, 1, 2, 1, 0, 2]) ==
               [del: [3], ins: [2, 1], eq: [2, 1, 0, 2]]

      assert List.myers_difference([3, 2, 2, 1, 0, 2], [2, 2, 1, 2, 1, 0, 2]) ==
               [del: [3], eq: [2, 2, 1], ins: [2, 1], eq: [0, 2]]

      assert List.myers_difference([3, 2, 0, 2], [2, 2, 1, 0, 2]) ==
               [del: [3], eq: [2], ins: [2, 1], eq: [0, 2]]
    end
  end

  test "improper?/1" do
    assert List.improper?([1 | 2])
    assert List.improper?([1, 2, 3 | 4])
    refute List.improper?([])
    refute List.improper?([1])
    refute List.improper?([[1]])
    refute List.improper?([1, 2])
    refute List.improper?([1, 2, 3])

    assert_raise FunctionClauseError, fn ->
      List.improper?(%{})
    end
  end

  describe "ascii_printable?/2" do
    test "proper lists without limit" do
      assert List.ascii_printable?([])
      assert List.ascii_printable?('abc')
      refute(List.ascii_printable?('abc' ++ [0]))
      refute List.ascii_printable?('mañana')

      printable_chars = '\a\b\t\n\v\f\r\e' ++ Enum.to_list(32..126)
      non_printable_chars = '🌢áéíóúźç©¢🂭'

      assert List.ascii_printable?(printable_chars)

      for char <- printable_chars do
        assert List.ascii_printable?([char])
      end

      refute List.ascii_printable?(non_printable_chars)

      for char <- non_printable_chars do
        refute List.ascii_printable?([char])
      end
    end

    test "proper lists with limit" do
      assert List.ascii_printable?([], 100)
      assert List.ascii_printable?('abc' ++ [0], 2)
    end

    test "improper lists" do
      refute List.ascii_printable?('abc' ++ ?d)
      assert List.ascii_printable?('abc' ++ ?d, 3)
    end
  end
end
