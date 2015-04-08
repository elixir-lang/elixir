Code.require_file "test_helper.exs", __DIR__

defmodule MapSetTest do
  use ExUnit.Case, async: true

  alias MapSet, as: S

  test "put" do
    assert S.equal?(S.put(S.new, 1), make([1]))
    assert S.equal?(S.put(make([1, 3, 4]), 2), make(1..4))
    assert S.equal?(S.put(make(5..100), 10), make(5..100))
  end

  test "union" do
    assert S.equal?(S.union(make([1, 3, 4]), S.new), make([1, 3, 4]))
    assert S.equal?(S.union(make(5..15), make(10..25)), make(5..25))
    assert S.equal?(S.union(make(1..120), make(1..100)), make(1..120))
  end

  test "intersection" do
    assert S.equal?(S.intersection(S.new, make(1..21)), S.new)
    assert S.equal?(S.intersection(make(1..21), make(4..24)), make(4..21))
    assert S.equal?(S.intersection(make(2..100), make(1..120)), make(2..100))
  end

  test "difference" do
    assert S.equal?(S.difference(make(2..20), S.new), make(2..20))
    assert S.equal?(S.difference(make(2..20), make(1..21)), S.new)
    assert S.equal?(S.difference(make(1..101), make(2..100)), make([1, 101]))
  end

  test "disjoint?" do
    assert S.disjoint?(S.new, S.new)
    assert S.disjoint?(make(1..6), make(8..20))
    refute S.disjoint?(make(1..6), make(5..15))
    refute S.disjoint?(make(1..120), make(1..6))
  end

  test "subset?" do
    assert S.subset?(S.new, S.new)
    assert S.subset?(make(1..6), make(1..10))
    assert S.subset?(make(1..6), make(1..120))
    refute S.subset?(make(1..120), make(1..6))
  end

  test "equal?" do
    assert S.equal?(S.new, S.new)
    refute S.equal?(make(1..20), make(2..21))
    assert S.equal?(make(1..120), make(1..120))
  end

  test "delete" do
    assert S.equal?(S.delete(S.new, 1), S.new)
    assert S.equal?(S.delete(make(1..4), 5), make(1..4))
    assert S.equal?(S.delete(make(1..4), 1), make(2..4))
    assert S.equal?(S.delete(make(1..4), 2), make([1, 3, 4]))
  end

  test "size" do
    assert S.size(S.new) == 0
    assert S.size(make(5..15)) == 11
    assert S.size(make(2..100)) == 99
  end

  test "to_list" do
    assert S.to_list(S.new) == []

    list = S.to_list(make(1..20))
    assert Enum.sort(list) == Enum.to_list(1..20)

    list = S.to_list(make(5..120))
    assert Enum.sort(list) == Enum.to_list(5..120)
  end

  defp make(collection) do
    Enum.into(collection, S.new)
  end
end
