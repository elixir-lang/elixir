defmodule SortedSetTest do
  use ExUnit.Case, async: true
  doctest SortedSet


  test "it creates an empty set with size 0" do
    assert 0 == SortedSet.size SortedSet.new
  end

  test "it sorts an existing list on creation" do
    assert [1,3,5] == SortedSet.to_list SortedSet.new [1,5,3]
  end

  test "it can put an element into the set" do
    new_set = SortedSet.put SortedSet.new, 1
    assert [1] == SortedSet.to_list new_set
    assert 1 == SortedSet.size new_set
  end

  test "it puts elements in sorted order" do
    set = SortedSet.put(SortedSet.new(), 2)
    |> SortedSet.put(3)
    |> SortedSet.put(1)
    assert [1,2,3] == SortedSet.to_list(set)
    assert 3 == SortedSet.size set
  end

  test "it can delete from the set" do
    set = %SortedSet{members: [1,2,3,4,5], size: 5}
    new_set = SortedSet.delete(set, 3)
    assert [1,2,4,5] == SortedSet.to_list new_set
    assert 4 == SortedSet.size new_set

    assert [] == SortedSet.to_list SortedSet.delete(SortedSet.new(), 1)
  end

  test "it can perform a union on two sorted sets" do
    set1 = %SortedSet{members: [1,2,3,4,5], size: 5}
    set2 = %SortedSet{members: [1,3,5,7,9], size: 5}
    union = SortedSet.union(set1, set2)
    assert [1,2,3,4,5,7,9] == SortedSet.to_list union
    assert 7 == SortedSet.size union
  end

  test "it can tell if a set contains an item" do
    assert SortedSet.member?(SortedSet.new([1,2,3]), 1)
    assert not SortedSet.member?(SortedSet.new([1,2,3]), 4)
    assert not SortedSet.member?(SortedSet.new([]), 4)
  end

  test "it can tell if two sets are equal" do
    assert SortedSet.equal?(SortedSet.new, SortedSet.new)
    assert SortedSet.equal?(SortedSet.new([1,2,3,4]), SortedSet.new([1,2,3,4]))
    assert not SortedSet.equal?(SortedSet.new([1,2,3]), SortedSet.new([1,2,4]))

    # Ensure it isn't confused by subsets
    assert not SortedSet.equal?(SortedSet.new([1,2,3]), SortedSet.new([1,2]))
    # Or supersets
    assert not SortedSet.equal?(SortedSet.new([1,2]), SortedSet.new([1,2,3]))

  end

  test "it can tell if one set is the subset of another" do
    assert SortedSet.subset?(SortedSet.new, SortedSet.new)

    assert SortedSet.subset?(SortedSet.new([1,2,3]), SortedSet.new([1,2,3,4]))
    assert not SortedSet.subset?(SortedSet.new([1,2,3,4]), SortedSet.new([1,2,3]))
  end

  test "it can return the intersection of two sets" do
    intersection = SortedSet.intersection(SortedSet.new([1,2,3]), SortedSet.new([1,3,5,7,9]))
    assert [1,3] == SortedSet.to_list(intersection)

    intersection = SortedSet.intersection(SortedSet.new([1,3,5]), SortedSet.new([2,4,6]))
    assert [] == SortedSet.to_list(intersection)

    intersection = SortedSet.intersection(SortedSet.new, SortedSet.new([1,2,3]))
    assert [] == SortedSet.to_list(intersection)
  end

  test "it can find the difference between two sets" do
    # With members in common
    difference = SortedSet.difference(SortedSet.new([1,2,3]), SortedSet.new([1,3,5,7,9]))
    assert [2] == SortedSet.to_list(difference)

    # With no members in common
    difference = SortedSet.difference(SortedSet.new([1,2,3]), SortedSet.new([5,7,9]))
    assert [1,2,3] == SortedSet.to_list(difference)

    # When one set is empty
    difference = SortedSet.difference(SortedSet.new([]), SortedSet.new([5,7,9]))
    assert [] == SortedSet.to_list(difference)

    # When the other set is empty
    difference = SortedSet.difference(SortedSet.new([1,2,3]), SortedSet.new([]))
    assert [1,2,3] == SortedSet.to_list(difference)
  end

  test "it can tell if two sets are disjointed" do
    assert SortedSet.disjoint?(SortedSet.new([1,2,3]), SortedSet.new([4,5,6]))
    assert SortedSet.disjoint?(SortedSet.new(), SortedSet.new([4,5,6]))
    assert SortedSet.disjoint?(SortedSet.new([4,5,6]), SortedSet.new())
    assert not SortedSet.disjoint?(SortedSet.new([1,2,3]), SortedSet.new([3,4,5]))
  end

  test "it adheres to the Enumerable protocol" do
    assert Enum.member?(SortedSet.new([1,2]), 1)
    assert not Enum.member?(SortedSet.new(), 1)

    assert 3 == Enum.count(SortedSet.new([1,2,3]))
    assert 0 == Enum.count(SortedSet.new())

    assert 24 == Enum.reduce(SortedSet.new([1,2,3,4]), 1, fn (n, acc) -> n * acc end)
  end

  test "it adheres to the Collectable prototcol" do
    assert [1,2,3,4] == SortedSet.to_list(Enum.into([1,3,4,2,3,4], %SortedSet{}))
  end

  test "it implements the Inspect protocol" do
    assert "#SortedSet<[0, 1, 2, 5, 6]>" == inspect SortedSet.new [1,0,5,2,5,6,2]
    assert "#SortedSet<[]>" == inspect SortedSet.new

  end
end
