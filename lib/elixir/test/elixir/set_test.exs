Code.require_file "test_helper.exs", __DIR__

defmodule SetTest do
  use ExUnit.Case, async: true

  test "union with ordered sets" do
    assert Set.union(Set.new([1, 2, 3]), Set.new([3, 4, 5])) == Set.new([1, 2, 3, 4, 5])
    assert Set.union(Set.new, Set.new) == Set.new
  end

  test "union with nodes" do
    assert Set.union(filled_set(21), filled_set(22)) == filled_set(22)

    assert Set.union(filled_set(121), filled_set(120)) == filled_set(121)
  end

  test "intersection with ordered sets" do
    assert Set.intersection(Set.new([1, 2, 3, 4, 6]), Set.new([3, 4, 5])) == Set.new([3, 4])
    assert Set.intersection(Set.new, filled_set(8)) == Set.new
    assert Set.intersection(filled_set(8), Set.new) == Set.new
    assert Set.intersection(Set.new, Set.new) == Set.new
  end

  test "intersection with nodes" do
    assert Set.intersection(filled_set(21), filled_set(20)) == Set.new(filled_set(20))

    assert Set.intersection(filled_set(120), filled_set(121)) == Set.new(filled_set(120))
  end

  test "difference with ordered sets" do
    assert Set.difference(Set.new([1, 2, 3, 5]), Set.new([3, 4, 5])) == Set.new([1, 2])
    assert Set.difference(Set.new, Set.new([1])) == Set.new([])
    assert Set.difference(Set.new, Set.new) == Set.new
    assert Set.difference(Set.new([1]), Set.new) == Set.new([1])
  end

  test "difference with nodes" do
    assert Set.equal?(Set.difference(filled_set(20), filled_set(21)), Set.new([]))
    assert Set.equal?(Set.difference(filled_set(121), filled_set(120)), Set.new([121]))
  end

  test "member? with ordered sets"  do
    assert Set.member?(filled_set(8), 8)
    refute Set.member?(filled_set(8), 10)
  end

  test "member? with nodes"  do
    assert Set.member?(filled_set(20), 19)
    refute Set.member?(filled_set(20), 100)

    assert Set.member?(filled_set(120), 90)
    refute Set.member?(filled_set(120), 200)
  end

  test :equal? do
    assert Set.equal?(Set.new, Set.new)
    assert Set.equal?(filled_set(20), Set.delete(filled_set(21), 21))
    assert Set.equal?(filled_set(120), filled_set(120))
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
    assert Set.delete(filled_set(8), 8) == filled_set(7)
    assert Set.delete(filled_set(8), 9) == filled_set(8)
    assert Set.delete(Set.new, 10) == Set.new

    assert Set.delete(filled_set(21), 21) == filled_set(20)

    assert Set.delete(filled_set(121), 121) == filled_set(120)
  end

  test "a set removes duplicates" do
    assert Set.new([1, 1, 2, 3, 3, 3]) == Set.new([1, 2, 3])
  end

  test "a set comparison ignores the order" do
    assert Set.new([3, 2, 1]) == Set.new([1, 2, 3])

    assert Set.new([:c, :a, :b]) == Set.new([:a, :b, :c])

    assert Set.new(["c", "a", "b"]) == Set.new(["b", "c", "a"])

    assert Set.new([1, 2, 3, 4, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                    13, 14, 15, 16, 17, 18, 19, 20]) == filled_set(20)
  end

  defp filled_set(range) do
    Enum.reduce 1..range, Set.new, Set.put(&2, &1)
  end
end

