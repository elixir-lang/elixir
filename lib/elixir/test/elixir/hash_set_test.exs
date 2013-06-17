Code.require_file "test_helper.exs", __DIR__

defmodule HashSetTest do
  use ExUnit.Case, async: true

  test "union with ordered sets" do
    assert HashSet.union(HashSet.new([1, 2, 3]), HashSet.new([3, 4, 5])) == HashSet.new([1, 2, 3, 4, 5])
    assert HashSet.union(HashSet.new, HashSet.new) == HashSet.new
  end

  test "union with nodes" do
    assert HashSet.union(filled_set(21), filled_set(22)) == filled_set(22)

    assert HashSet.union(filled_set(121), filled_set(120)) == filled_set(121)
  end

  test "intersection with ordered sets" do
    assert HashSet.intersection(HashSet.new([1, 2, 3, 4, 6]), HashSet.new([3, 4, 5])) == HashSet.new([3, 4])
    assert HashSet.intersection(HashSet.new, filled_set(8)) == HashSet.new
    assert HashSet.intersection(filled_set(8), HashSet.new) == HashSet.new
    assert HashSet.intersection(HashSet.new, HashSet.new) == HashSet.new
  end

  test "intersection with nodes" do
    assert HashSet.intersection(filled_set(21), filled_set(20)) == HashSet.new(filled_set(20))

    assert HashSet.intersection(filled_set(120), filled_set(121)) == HashSet.new(filled_set(120))
  end

  test "difference with ordered sets" do
    assert HashSet.difference(HashSet.new([1, 2, 3, 5]), HashSet.new([3, 4, 5])) == HashSet.new([1, 2])
    assert HashSet.difference(HashSet.new, HashSet.new([1])) == HashSet.new([])
    assert HashSet.difference(HashSet.new, HashSet.new) == HashSet.new
    assert HashSet.difference(HashSet.new([1]), HashSet.new) == HashSet.new([1])
  end

  test "difference with nodes" do
    assert HashSet.equal?(HashSet.difference(filled_set(20), filled_set(21)), HashSet.new([]))
    assert HashSet.equal?(HashSet.difference(filled_set(121), filled_set(120)), HashSet.new([121]))
  end

  test "member? with ordered sets"  do
    assert HashSet.member?(filled_set(8), 8)
    refute HashSet.member?(filled_set(8), 10)
  end

  test "member? with nodes"  do
    assert HashSet.member?(filled_set(20), 19)
    refute HashSet.member?(filled_set(20), 100)

    assert HashSet.member?(filled_set(120), 90)
    refute HashSet.member?(filled_set(120), 200)
  end

  test :subset? do
    assert HashSet.subset?(HashSet.new, HashSet.new)
    assert HashSet.subset?(filled_set(6), filled_set(10))
    assert HashSet.subset?(filled_set(6), filled_set(120))

    refute HashSet.subset?(filled_set(120), filled_set(6))
  end

  test :disjoint? do
    assert HashSet.disjoint?(HashSet.new, HashSet.new)
    assert HashSet.disjoint?(HashSet.new([1, 2, 3]), HashSet.new([4, 5 ,6]))
    refute HashSet.disjoint?(HashSet.new([1, 2, 3]), HashSet.new([3, 4 ,5]))
  end

  test :equal? do
    assert HashSet.equal?(HashSet.new, HashSet.new)
    assert HashSet.equal?(filled_set(20), HashSet.delete(filled_set(21), 21))
    assert HashSet.equal?(filled_set(120), filled_set(120))
  end

  test :empty do
    assert HashSet.empty filled_set(8)   == HashSet.new
    assert HashSet.empty filled_set(20)  == HashSet.new
    assert HashSet.empty filled_set(120) == HashSet.new
  end

  test :to_list do
    set = filled_set(8)
    list = set |> HashSet.to_list
    assert length(list) == 8
    assert 1 in list
    assert list == Enum.to_list(set)

    set = filled_set(20)
    list = set |> HashSet.to_list
    assert length(list) == 20
    assert 1 in list
    assert list == Enum.to_list(set)

    set = filled_set(120)
    list = set |> HashSet.to_list
    assert length(list) == 120
    assert 1 in list
    assert list == Enum.to_list(set)
  end

  test :delete do
    assert HashSet.delete(filled_set(8), 8) == filled_set(7)
    assert HashSet.delete(filled_set(8), 9) == filled_set(8)
    assert HashSet.delete(HashSet.new, 10) == HashSet.new

    assert HashSet.delete(filled_set(21), 21) == filled_set(20)

    assert HashSet.delete(filled_set(121), 121) == filled_set(120)
  end

  test :filter do
    assert HashSet.filter(HashSet.new([1, 2, 3]), fn m -> m == 2 end) == HashSet.new([2])
  end

  test "a set removes duplicates" do
    assert HashSet.new([1, 1, 2, 3, 3, 3]) == HashSet.new([1, 2, 3])
  end

  test "a set comparison ignores the order" do
    assert HashSet.new([3, 2, 1]) == HashSet.new([1, 2, 3])

    assert HashSet.new([:c, :a, :b]) == HashSet.new([:a, :b, :c])

    assert HashSet.new(["c", "a", "b"]) == HashSet.new(["b", "c", "a"])

    assert HashSet.new([1, 2, 3, 4, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                    13, 14, 15, 16, 17, 18, 19, 20]) == filled_set(20)
  end

  defp filled_set(range) do
    Enum.reduce 1..range, HashSet.new, HashSet.put(&2, &1)
  end
end

