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

  test "symmetric_difference/2" do
    result = MapSet.symmetric_difference(MapSet.new(1..5), MapSet.new(3..8))
    assert MapSet.equal?(result, MapSet.new([1, 2, 6, 7, 8]))

    result = MapSet.symmetric_difference(MapSet.new(), MapSet.new())
    assert MapSet.equal?(result, MapSet.new())

    result = MapSet.symmetric_difference(MapSet.new(1..5), MapSet.new(1..5))
    assert MapSet.equal?(result, MapSet.new())

    result = MapSet.symmetric_difference(MapSet.new([1, 2, 3]), MapSet.new())
    assert MapSet.equal?(result, MapSet.new([1, 2, 3]))

    result = MapSet.symmetric_difference(MapSet.new(), MapSet.new([1, 2, 3]))
    assert MapSet.equal?(result, MapSet.new([1, 2, 3]))
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

  test "filter/2" do
    result = MapSet.filter(MapSet.new([1, nil, 2, false]), & &1)
    assert MapSet.equal?(result, MapSet.new(1..2))

    result = MapSet.filter(MapSet.new(1..10), &(&1 < 2 or &1 > 9))
    assert MapSet.equal?(result, MapSet.new([1, 10]))

    result = MapSet.filter(MapSet.new(~w(A a B b)), fn x -> String.downcase(x) == x end)
    assert MapSet.equal?(result, MapSet.new(~w(a b)))
  end

  test "reject/2" do
    result = MapSet.reject(MapSet.new(1..10), &(&1 < 8))
    assert MapSet.equal?(result, MapSet.new(8..10))

    result = MapSet.reject(MapSet.new(["a", :b, 1, 1.0]), &is_integer/1)
    assert MapSet.equal?(result, MapSet.new(["a", :b, 1.0]))

    result = MapSet.reject(MapSet.new(1..3), fn x -> rem(x, 2) == 0 end)
    assert MapSet.equal?(result, MapSet.new([1, 3]))
  end

  test "split_with" do
    assert MapSet.split_with(MapSet.new(), fn v -> rem(v, 2) == 0 end) ==
             {MapSet.new(), MapSet.new()}

    assert MapSet.split_with(MapSet.new([1, 2, 3]), fn v -> rem(v, 2) == 0 end) ==
             {MapSet.new([2]), MapSet.new([1, 3])}

    assert MapSet.split_with(MapSet.new([2, 4, 6]), fn v -> rem(v, 2) == 0 end) ==
             {MapSet.new([2, 4, 6]), MapSet.new([])}
  end

  test "inspect" do
    assert inspect(MapSet.new([?a])) == "MapSet.new([97])"
  end
end
