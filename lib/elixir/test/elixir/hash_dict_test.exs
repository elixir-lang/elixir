Code.require_file "test_helper.exs", __DIR__

defmodule HashDictTest do
  use ExUnit.Case, async: true

  @dict Enum.into([foo: :bar], HashDict.new)

  test "is serializable as attribute" do
    assert @dict == Enum.into([foo: :bar], HashDict.new)
  end

  test "is accessible as attribute" do
    assert @dict[:foo] == :bar
  end

  test "small dict smoke test" do
    smoke_test(1..8)
    smoke_test(8..1)
  end

  test "medium dict smoke test" do
    smoke_test(1..80)
    smoke_test(80..1)
  end

  test "large dict smoke test" do
    smoke_test(1..1200)
    smoke_test(1200..1)
  end

  test "reduce/3 (via to_list)" do
    dict = filled_dict(8)
    list = dict |> HashDict.to_list
    assert length(list) == 8
    assert { 1, 1 } in list
    assert list == Enum.to_list(dict)

    dict = filled_dict(20)
    list = dict |> HashDict.to_list
    assert length(list) == 20
    assert { 1, 1 } in list
    assert list == Enum.to_list(dict)

    dict = filled_dict(120)
    list = dict |> HashDict.to_list
    assert length(list) == 120
    assert { 1, 1 } in list
    assert list == Enum.to_list(dict)
  end

  test "comparison when subsets" do
    d1 = Enum.into [a: 0], HashDict.new
    d2 = Enum.into [a: 0, b: 1], HashDict.new

    refute HashDict.equal?(d1, d2)
    refute HashDict.equal?(d2, d1)
  end

  defp smoke_test(range) do
    { dict, _ } = Enum.reduce range, { HashDict.new, 1 }, fn(x, { acc, i }) ->
      acc = HashDict.put(acc, x, x)
      assert HashDict.size(acc) == i
      { acc, i + 1 }
    end

    Enum.each range, fn(x) ->
      assert HashDict.get(dict, x) == x
    end

    { dict, _ } = Enum.reduce range, { dict, Enum.count(range) }, fn(x, { acc, i }) ->
      assert HashDict.size(acc) == i
      acc = HashDict.delete(acc, x)
      assert HashDict.size(acc) == i - 1
      assert HashDict.get(acc, x) == nil
      { acc, i - 1 }
    end

    assert dict == HashDict.new
  end

  defp filled_dict(range) do
    Enum.reduce 1..range, HashDict.new, &HashDict.put(&2, &1, &1)
  end
end
