Code.require_file "../test_helper.exs", __FILE__

defmodule HashDictTest do
  use ExUnit.Case, async: true

  test :smoke_small_range_test do
    smoke_test(8)
  end

  test :smoke_medium_range_test do
    smoke_test(80)
  end

  test :smoke_large_range_test do
    smoke_test(1200)
  end

  test :empty do
    assert HashDict.empty filled_dict(8)   == HashDict.new
    assert HashDict.empty filled_dict(20)  == HashDict.new
    assert HashDict.empty filled_dict(120) == HashDict.new
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

  test :to_list do
    list = filled_dict(8) |> HashDict.to_list
    assert length(list) == 8
    assert { 1, 1 } inlist list

    list = filled_dict(20) |> HashDict.to_list
    assert length(list) == 20
    assert { 1, 1 } inlist list

    list = filled_dict(120) |> HashDict.to_list
    assert length(list) == 120
    assert { 1, 1 } inlist list
  end

  test :keys do
    list = filled_dict(8) |> HashDict.keys
    assert length(list) == 8
    assert 1 inlist list

    list = filled_dict(20) |> HashDict.keys
    assert length(list) == 20
    assert 1 inlist list

    list = filled_dict(120) |> HashDict.keys
    assert length(list) == 120
    assert 1 inlist list
  end

  test :values do
    list = filled_dict(8) |> HashDict.values
    assert length(list) == 8
    assert 1 inlist list

    list = filled_dict(20) |> HashDict.values
    assert length(list) == 20
    assert 1 inlist list

    list = filled_dict(120) |> HashDict.values
    assert length(list) == 120
    assert 1 inlist list
  end

  defp smoke_test(range) do
    dict = Enum.reduce 1..range, HashDict.new, fn(x, acc) ->
      acc = HashDict.put(acc, x, x)
      assert HashDict.size(acc) == x
      acc
    end

    Enum.each 1..range, fn(x) ->
      assert HashDict.get(dict, x) == x
    end

    Enum.reduce range..1, dict, fn(x, acc) ->
      assert HashDict.size(acc) == x
      acc = HashDict.delete(acc, x)
      assert HashDict.size(acc) == x - 1
      assert HashDict.get(acc, x) == nil
      acc
    end
  end

  defp filled_dict(range) do
    Enum.reduce 1..range, HashDict.new, HashDict.put(&2, &1, &1)
  end
end
