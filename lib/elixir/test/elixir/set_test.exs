Code.require_file "test_helper.exs", __DIR__

defmodule SetTest do
  use ExUnit.Case, async: true

  test :union do
    assert Set.union(Set.new([1,2,3]), Set.new([3,4,5])) == Set.new([1,2,3,4,5])
    assert Set.union(Set.new, Set.new) == Set.new
  end

  test :intersection do
    assert Set.intersection(Set.new([1,2,3,4,6]), Set.new([3,4,5])) == Set.new([3,4])
    assert Set.intersection(Set.new, filled_set(8)) == Set.new
    assert Set.intersection(filled_set(8), Set.new) == Set.new
    assert Set.intersection(Set.new, Set.new) == Set.new
  end

  test :difference do
    assert Set.difference(Set.new([1,2,3,5]), Set.new([3,4,5])) == Set.new([1,2,4])
    assert Set.difference(Set.new, Set.new([1])) == Set.new([1])
    assert Set.difference(Set.new, Set.new) == Set.new
    assert Set.difference(Set.new([1]), Set.new) == Set.new([1])
  end

  test :member? do
    assert Set.member?(filled_set(8), 8)
    refute Set.member?(filled_set(8), 10)
  end

  test :empty do
    assert Set.empty filled_set(8)   == Set.new
    assert Set.empty filled_set(20)  == Set.new
    assert Set.empty filled_set(120) == Set.new
  end

  test :to_list do
    set = filled_set(8)
    list = set |> Set.to_list
    assert length(list) == 8
    assert 1 in list
    assert list == Enum.to_list(set)

    set = filled_set(20)
    list = set |> Set.to_list
    assert length(list) == 20
    assert 1 in list
    assert list == Enum.to_list(set)

    set = filled_set(120)
    list = set |> Set.to_list
    assert length(list) == 120
    assert 1 in list
    assert list == Enum.to_list(set)
  end

  test :delete do
    set = filled_set(8)
    assert Set.delete(set, 8) == filled_set(7)

    set = filled_set(8)
    assert Set.delete(set, 9) == set

    assert Set.delete(Set.new, 10) == Set.new
  end

  test "a set removes duplicates" do
    assert Set.new([1,1,2,3,3,3]) == Set.new([1,2,3])
  end

  test "a set comparison ignores the order" do
    assert Set.new([3,2,1]) == Set.new([1,2,3])

    assert Set.new([:c,:a,:b]) == Set.new([:a,:b,:c])

    assert Set.new(["c","a","b"]) == Set.new(["b", "c", "a"])
  end

  defp filled_set(range) do
    Enum.reduce 1..range, Set.new, Set.put(&2, &1)
  end
end

