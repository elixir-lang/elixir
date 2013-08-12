Code.require_file "test_helper.exs", __DIR__

defmodule HashDictTest do
  use ExUnit.Case, async: true

  @dict HashDict.new(foo: :bar)

  test :is_serializable_as_attribute do
    assert @dict == HashDict.new(foo: :bar)
  end

  test :smoke_small_range_test do
    smoke_test(1..8)
    smoke_test(8..1)
  end

  test :smoke_medium_range_test do
    smoke_test(1..80)
    smoke_test(80..1)
  end

  test :smoke_large_range_test do
    smoke_test(1..1200)
    smoke_test(1200..1)
  end

  test :fetch! do
    dict = filled_dict(8)
    assert HashDict.fetch!(dict, 1) == 1
    assert_raise KeyError, fn ->
      HashDict.fetch!(dict, 11)
    end
  end

  test :empty do
    assert HashDict.empty filled_dict(8)   == HashDict.new
    assert HashDict.empty filled_dict(20)  == HashDict.new
    assert HashDict.empty filled_dict(120) == HashDict.new
  end

  test :fetch do
    dict = filled_dict(8)
    assert HashDict.fetch(dict, 4)  == { :ok, 4 }
    assert HashDict.fetch(dict, 16) == :error
  end

  test :equal? do
    assert HashDict.equal?(filled_dict(3), filled_dict(3)) == true

    assert HashDict.equal?(HashDict.new([{:a, 1}, {:b, 2}]),
                           HashDict.new([{:a, 2}, {:b, 3}])) == false
  end

  test :has_key? do
    dict = filled_dict(8)
    assert HashDict.has_key? dict, 4
    refute HashDict.has_key? dict, 16

    dict = filled_dict(20)
    assert HashDict.has_key? dict, 10
    refute HashDict.has_key? dict, 30

    dict = filled_dict(120)
    assert HashDict.has_key? dict, 60
    refute HashDict.has_key? dict, 240
  end

  test :put_new do
    dict = filled_dict(8)

    dict = HashDict.put_new(dict, 1, 11)
    assert HashDict.get(dict, 1) == 1
    assert HashDict.size(dict) == 8

    dict = HashDict.put_new(dict, 11, 13)
    assert HashDict.get(dict, 11) == 13
    assert HashDict.size(dict) == 9

    dict = HashDict.put_new(dict, 11.0, 15)
    assert HashDict.get(dict, 11.0) == 15
    assert HashDict.get(dict, 11) == 13
    assert HashDict.size(dict) == 10
  end

  test :update do
    dict = filled_dict(8)

    dict = HashDict.update!(dict, 1, &1 * 2)
    assert HashDict.get(dict, 1) == 2
    assert HashDict.size(dict) == 8

    assert_raise KeyError, fn ->
      HashDict.update!(dict, 11, &1 * 2)
    end

    dict = HashDict.update(dict, 3, 3, &1 * 2)
    assert HashDict.get(dict, 3) == 6
    assert HashDict.size(dict) == 8

    dict = HashDict.update(dict, 11, 13, &1 * 2)
    assert HashDict.get(dict, 11) == 13
    assert HashDict.size(dict) == 9

    assert_raise KeyError, fn->
      HashDict.update!(dict, 11.0, &1 * 2)
    end

    dict = HashDict.update(dict, 11.0, 15, &1 * 2)
    assert HashDict.get(dict, 11.0) == 15
    assert HashDict.size(dict) == 10

    dict = HashDict.update(dict, 11.0, 15, &1 * 2)
    assert HashDict.get(dict, 11.0) == 30
    assert HashDict.get(dict, 11) == 13
    assert HashDict.size(dict) == 10
  end

  test :to_list do
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

  test :keys do
    list = filled_dict(8) |> HashDict.keys
    assert length(list) == 8
    assert 1 in list

    list = filled_dict(20) |> HashDict.keys
    assert length(list) == 20
    assert 1 in list

    list = filled_dict(120) |> HashDict.keys
    assert length(list) == 120
    assert 1 in list
  end

  test :values do
    list = filled_dict(8) |> HashDict.values
    assert length(list) == 8
    assert 1 in list

    list = filled_dict(20) |> HashDict.values
    assert length(list) == 20
    assert 1 in list

    list = filled_dict(120) |> HashDict.values
    assert length(list) == 120
    assert 1 in list
  end

  test :enum do
    dict = filled_dict(10)
    assert Enum.empty?(HashDict.new)
    refute Enum.empty?(dict)
    assert Enum.member?(dict, { 5, 5 })
    refute Enum.member?(dict, { 5, 8 })
    assert Enum.count(dict) == 10
    assert Enum.map(filled_dict(3), fn({ k, v }) -> k + v end) == [2, 4, 6]
  end

  test :access do
    assert filled_dict(8)[1] == 1
    assert filled_dict(8)[5] == 5
    assert filled_dict(8)[9] == nil
  end

  test :inspect do
    assert inspect(filled_dict(8)) =~ "#HashDict<"
  end

  test :small_range_merge do
    dict1 = filled_dict(8)
    dict2 = Enum.reduce 6..10, HashDict.new, fn(i, d) -> HashDict.put(d, i, i * 2) end

    dict = HashDict.merge(dict1, dict2)
    assert HashDict.get(dict, 3) == 3
    assert HashDict.get(dict, 6) == 12
    assert HashDict.get(dict, 10) == 20
    assert HashDict.size(dict) == 10

    dict = HashDict.merge(dict2, dict1)
    assert HashDict.get(dict, 3) == 3
    assert HashDict.get(dict, 6) == 6
    assert HashDict.get(dict, 10) == 20
    assert HashDict.size(dict) == 10

    dict = HashDict.merge(dict1, dict2, fn(k, v1, v2) -> 3*k + 2*v1 + v2 end)
    assert HashDict.get(dict, 3) == 3
    assert HashDict.get(dict, 6) == 6 * 7
    assert HashDict.get(dict, 10) == 20
    assert HashDict.size(dict) == 10

    list = Enum.map 6..10, fn x -> { x, x * 2 } end
    dict = HashDict.merge(dict1, list)
    assert HashDict.get(dict, 3) == 3
    assert HashDict.get(dict, 6) == 12
    assert HashDict.get(dict, 10) == 20
    assert HashDict.size(dict) == 10
  end

  test :medium_range_merge do
    dict1 = filled_dict(20)
    dict2 = Enum.reduce 18..22, HashDict.new, fn(i, d) -> HashDict.put(d, i, i * 2) end

    dict = HashDict.merge(dict1, dict2)
    assert HashDict.get(dict, 16) == 16
    assert HashDict.get(dict, 18) == 36
    assert HashDict.get(dict, 22) == 44
    assert HashDict.size(dict) == 22

    dict = HashDict.merge(dict2, dict1)
    assert HashDict.get(dict, 16) == 16
    assert HashDict.get(dict, 18) == 18
    assert HashDict.get(dict, 22) == 44
    assert HashDict.size(dict) == 22

    dict = HashDict.merge(dict1, dict2, fn(k, v1, v2) -> 3*k + 2*v1 + v2 end)
    assert HashDict.get(dict, 16) == 16
    assert HashDict.get(dict, 18) == 18 * 7
    assert HashDict.get(dict, 22) == 44
    assert HashDict.size(dict) == 22

    list = Enum.map 18..22, fn x -> { x, x * 2 } end
    dict = HashDict.merge(dict1, list)
    assert HashDict.get(dict, 16) == 16
    assert HashDict.get(dict, 18) == 36
    assert HashDict.get(dict, 22) == 44
    assert HashDict.size(dict) == 22
  end

  test :large_range_merge do
    dict1 = filled_dict(120)
    dict2 = Enum.reduce 118..122, HashDict.new, fn(i, d) -> HashDict.put(d, i, i * 2) end

    dict = HashDict.merge(dict1, dict2)
    assert HashDict.get(dict, 116) == 116
    assert HashDict.get(dict, 118) == 236
    assert HashDict.get(dict, 122) == 244
    assert HashDict.size(dict) == 122

    dict = HashDict.merge(dict2, dict1)
    assert HashDict.get(dict, 116) == 116
    assert HashDict.get(dict, 118) == 118
    assert HashDict.get(dict, 122) == 244
    assert HashDict.size(dict) == 122

    dict = HashDict.merge(dict1, dict2, fn(k, v1, v2) -> 3*k + 2*v1 + v2 end)
    assert HashDict.get(dict, 116) == 116
    assert HashDict.get(dict, 118) == 118 * 7
    assert HashDict.get(dict, 122) == 244
    assert HashDict.size(dict) == 122

    list = Enum.map 118..122, fn x -> { x, x * 2 } end
    dict = HashDict.merge(dict1, list)
    assert HashDict.get(dict, 116) == 116
    assert HashDict.get(dict, 118) == 236
    assert HashDict.get(dict, 122) == 244
    assert HashDict.size(dict) == 122
  end

  test :trie_contract do
    dict = filled_dict(120)
    dict = Enum.reduce 16..120, dict, fn(x, acc) -> HashDict.delete(acc, x) end
    assert (Enum.filter 1..120, fn(x) -> HashDict.get(dict, x) == x end) == (Enum.sort 1..15)
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

    dict
  end

  defp filled_dict(range) do
    Enum.reduce 1..range, HashDict.new, HashDict.put(&2, &1, &1)
  end
end
