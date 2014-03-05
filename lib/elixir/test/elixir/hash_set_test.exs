Code.require_file "test_helper.exs", __DIR__

defmodule HashSetTest do
  use ExUnit.Case, async: true

  test "union" do
    assert HashSet.union(filled_set(21), filled_set(22)) == filled_set(22)
    assert HashSet.union(filled_set(121), filled_set(120)) == filled_set(121)
  end

  test "intersection" do
    assert HashSet.intersection(filled_set(21), filled_set(20)) == filled_set(20)
    assert HashSet.equal?(HashSet.intersection(filled_set(120), filled_set(121)), filled_set(120))
  end

  test "difference" do
    assert HashSet.equal?(HashSet.difference(filled_set(20), filled_set(21)), HashSet.new)

    diff = HashSet.difference(filled_set(9000), filled_set(9000))
    assert HashSet.equal?(diff, HashSet.new)
    assert HashSet.size(diff) == 0
  end

  test "subset?" do
    assert HashSet.subset?(HashSet.new, HashSet.new)
    assert HashSet.subset?(filled_set(6), filled_set(10))
    assert HashSet.subset?(filled_set(6), filled_set(120))
    refute HashSet.subset?(filled_set(120), filled_set(6))
  end

  test "equal?" do
    assert HashSet.equal?(HashSet.new, HashSet.new)
    assert HashSet.equal?(filled_set(20), HashSet.delete(filled_set(21), 21))
    assert HashSet.equal?(filled_set(120), filled_set(120))
  end

  test "to_list" do
    set = filled_set(20)
    list = HashSet.to_list(set)
    assert length(list) == 20
    assert 1 in list
    assert Enum.sort(list) == Enum.sort(1..20)

    set = filled_set(120)
    list = HashSet.to_list(set)
    assert length(list) == 120
    assert 1 in list
    assert Enum.sort(list) == Enum.sort(1..120)
  end

  defp filled_set(range) do
    Enum.into 1..range, HashSet.new
  end
end

