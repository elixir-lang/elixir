Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.DiffTest do
  use ExUnit.Case, async: true

  import ExUnit.Diff

  defp formatter(:diff_insert, message) do
    "[" <> message <> "]"
  end

  defp formatter(:diff_delete, message) do
    "{" <> message <> "}"
  end

  defmodule User do
    defstruct [:age]
  end

  test "numbers" do
    int1 = 491512235
    int2 = 490512035
    assert format(int1, int2, &formatter/2) == "49{1}[0]512{2}[0]35 {(off by -1000200)}"
    assert format(42.0, 43.0, &formatter/2) == "4{2}[3].0 [(off by +1.0)]"
    assert format(int1, 43.0, &formatter/2) == nil
  end

  test "strings" do
    string1 = "fox hops over \"the dog"
    string2 = "fox jumps over the lazy cat"
    expected = ~S<"fox {ho}[jum]ps over {\"}the {dog}[lazy cat]">
    assert format(string1, string2, &formatter/2) == expected
    assert format(string1, <<193, 31>>, &formatter/2) == nil
  end

  test "lists" do
    list1 = ["One", :ok, nil, {}, :ok]
    list2 = ["Two", :ok, 0.0, {true}]
    expected = ~S<["{One}[Two]", :ok, {nil}[0.0], {[true]}, {:ok}]>
    assert format(list1, list2, &formatter/2) == expected

    keyword_list1 = [file: "nofile", line: 12]
    keyword_list2 = [file: nil, llne: 10]
    expected = ~S<[file: {"nofile"}[nil], l{i}[l]ne: 1{2}[0] {(off by -2)}]>
    assert format(keyword_list1, keyword_list2, &formatter/2) == expected

    char_list1 = 'fox hops over \'the dog'
    char_list2 = 'fox jumps over the lazy cat'
    expected = "'fox {ho}[jum]ps over {\\'}the {dog}[lazy cat]'"
    assert format(char_list1, char_list2, &formatter/2) == expected
  end

  test "tuples" do
    tuple1 = {:hex, '1.1'}
    tuple2 = {:hex, '0.1', [{:ex_doc}]}
    expected = "{:hex, '{1}[0].1', [[{:ex_doc}]]}"
    assert format(tuple1, tuple2, &formatter/2) == expected
    assert format(tuple1, {}, &formatter/2) == "{{:hex}, {'1.1'}}"
    assert format({}, tuple1, &formatter/2) == "{[:hex], ['1.1']}"
  end

  test "maps" do
    map1 = Enum.into(1..40, %{}, &{&1, &1}) |> Map.delete(33)
    map2 = Enum.reduce(5..10, map1, &Map.delete(&2, &1)) |> Map.put(33, 33) |> Map.put(23, 32)
    expected = "%{23 => {2}3[2] [(off by +9)], {8 => 8}, {7 => 7}, {6 => 6}, {10 => 10}, {9 => 9}, {5 => 5}, [33 => 33], ...}"
    assert format(map1, map2, &formatter/2) == expected

    map1 = %{baz: 12}
    map2 = %{foo: 12, bar: 12, baz: 12}
    assert format(map1, map2, &formatter/2) == "%{[bar: 12], [foo: 12], ...}"
    assert format(map2, map1, &formatter/2) == "%{{bar: 12}, {foo: 12}, ...}"
    assert format(map1, %{}, &formatter/2) == "%{{baz: 12}}"
    assert format(%{}, map1, &formatter/2) == "%{[baz: 12]}"
  end

  test "structs" do
    user1 = %User{age: 16}
    user2 = %User{age: 21}
    assert format(user1, user2, &formatter/2) == "%ExUnit.DiffTest.User{age: [2]1{6} [(off by +5)]}"
    assert format(%User{}, %{}, &formatter/2) == nil
    assert format(%User{}, %ExUnit.Test{}, &formatter/2) == nil
  end

  test "not supported" do
    bin1 = <<147, 1, 2, 31>>
    bin2 = <<193, 1, 31>>
    assert format(bin1, bin2, &formatter/2) == nil
    assert format(:foo, :bar, &formatter/2) == nil
    assert format(:foo, "bar", &formatter/2) == nil
  end
end
