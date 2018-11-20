Code.require_file("test_helper.exs", __DIR__)

defmodule MapSetTest do
  use ExUnit.Case, async: true

  doctest MapSet

  test "new/1" do
    result = MapSet.new(1..5)
    assert MapSet.equal?(result, Enum.into(1..5, MapSet.new()))
  end

  test "new/2" do
    result = MapSet.new(1..5, &(&1 + 2))
    assert MapSet.equal?(result, Enum.into(3..7, MapSet.new()))
  end

  test "put/2" do
    result = MapSet.put(MapSet.new(), 1)
    assert MapSet.equal?(result, MapSet.new([1]))

    result = MapSet.put(MapSet.new([1, 3, 4]), 2)
    assert MapSet.equal?(result, MapSet.new(1..4))

    result = MapSet.put(MapSet.new(5..100), 10)
    assert MapSet.equal?(result, MapSet.new(5..100))
  end

  test "union/2" do
    result = MapSet.union(MapSet.new([1, 3, 4]), MapSet.new())
    assert MapSet.equal?(result, MapSet.new([1, 3, 4]))

    result = MapSet.union(MapSet.new(5..15), MapSet.new(10..25))
    assert MapSet.equal?(result, MapSet.new(5..25))

    result = MapSet.union(MapSet.new(1..120), MapSet.new(1..100))
    assert MapSet.equal?(result, MapSet.new(1..120))
  end

  test "intersection/2" do
    result = MapSet.intersection(MapSet.new(), MapSet.new(1..21))
    assert MapSet.equal?(result, MapSet.new())

    result = MapSet.intersection(MapSet.new(1..21), MapSet.new(4..24))
    assert MapSet.equal?(result, MapSet.new(4..21))

    result = MapSet.intersection(MapSet.new(2..100), MapSet.new(1..120))
    assert MapSet.equal?(result, MapSet.new(2..100))
  end

  test "difference/2" do
    result = MapSet.difference(MapSet.new(2..20), MapSet.new())
    assert MapSet.equal?(result, MapSet.new(2..20))

    result = MapSet.difference(MapSet.new(2..20), MapSet.new(1..21))
    assert MapSet.equal?(result, MapSet.new())

    result = MapSet.difference(MapSet.new(1..101), MapSet.new(2..100))
    assert MapSet.equal?(result, MapSet.new([1, 101]))
  end

  test "disjoint?/2" do
    assert MapSet.disjoint?(MapSet.new(), MapSet.new())
    assert MapSet.disjoint?(MapSet.new(1..6), MapSet.new(8..20))
    refute MapSet.disjoint?(MapSet.new(1..6), MapSet.new(5..15))
    refute MapSet.disjoint?(MapSet.new(1..120), MapSet.new(1..6))
  end

  test "subset?/2" do
    assert MapSet.subset?(MapSet.new(), MapSet.new())
    assert MapSet.subset?(MapSet.new(1..6), MapSet.new(1..10))
    assert MapSet.subset?(MapSet.new(1..6), MapSet.new(1..120))
    refute MapSet.subset?(MapSet.new(1..120), MapSet.new(1..6))
  end

  test "equal?/2" do
    assert MapSet.equal?(MapSet.new(), MapSet.new())
    refute MapSet.equal?(MapSet.new(1..20), MapSet.new(2..21))
    assert MapSet.equal?(MapSet.new(1..120), MapSet.new(1..120))
  end

  test "delete/2" do
    result = MapSet.delete(MapSet.new(), 1)
    assert MapSet.equal?(result, MapSet.new())

    result = MapSet.delete(MapSet.new(1..4), 5)
    assert MapSet.equal?(result, MapSet.new(1..4))

    result = MapSet.delete(MapSet.new(1..4), 1)
    assert MapSet.equal?(result, MapSet.new(2..4))

    result = MapSet.delete(MapSet.new(1..4), 2)
    assert MapSet.equal?(result, MapSet.new([1, 3, 4]))
  end

  test "size/1" do
    assert MapSet.size(MapSet.new()) == 0
    assert MapSet.size(MapSet.new(5..15)) == 11
    assert MapSet.size(MapSet.new(2..100)) == 99
  end

  test "to_list/1" do
    assert MapSet.to_list(MapSet.new()) == []

    list = MapSet.to_list(MapSet.new(1..20))
    assert Enum.sort(list) == Enum.to_list(1..20)

    list = MapSet.to_list(MapSet.new(5..120))
    assert Enum.sort(list) == Enum.to_list(5..120)
  end

  test "MapSet v1 compatibility" do
    result = 1..5 |> map_set_v1() |> MapSet.new()
    assert MapSet.equal?(result, MapSet.new(1..5))

    result = MapSet.put(map_set_v1(1..5), 6)
    assert MapSet.equal?(result, MapSet.new(1..6))

    result = MapSet.union(map_set_v1(1..5), MapSet.new(6..10))
    assert MapSet.equal?(result, MapSet.new(1..10))

    result = MapSet.intersection(map_set_v1(1..10), MapSet.new(6..15))
    assert MapSet.equal?(result, MapSet.new(6..10))

    result = MapSet.difference(map_set_v1(1..10), MapSet.new(6..50))
    assert MapSet.equal?(result, MapSet.new(1..5))

    result = MapSet.delete(map_set_v1(1..10), 1)
    assert MapSet.equal?(result, MapSet.new(2..10))

    assert MapSet.size(map_set_v1(1..5)) == 5
    assert MapSet.to_list(map_set_v1(1..5)) == Enum.to_list(1..5)

    assert MapSet.disjoint?(map_set_v1(1..5), MapSet.new(10..15))
    refute MapSet.disjoint?(map_set_v1(1..5), MapSet.new(5..10))

    assert MapSet.subset?(map_set_v1(3..7), MapSet.new(1..10))
    refute MapSet.subset?(map_set_v1(7..12), MapSet.new(1..10))
  end

  test "inspect" do
    assert inspect(MapSet.new([?a])) == "#MapSet<[97]>"
  end

  defp map_set_v1(enumerable) do
    map = Map.new(enumerable, &{&1, true})
    %{__struct__: MapSet, map: map}
  end
end
