Code.require_file "test_helper.exs", __DIR__

defmodule SetTest do
  use ExUnit.Case, async: true

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

