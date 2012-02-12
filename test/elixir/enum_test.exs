Code.require_file "../test_helper", __FILE__

defmodule EnumTest do
  use ExUnit::Case

  def test_all? do
    assert Enum.all?([2,4,6], fn(x, do: rem(x, 2) == 0))
    refute Enum.all?([2,3,4], fn(x, do: rem(x, 2) == 0))

    assert Enum.all?([2,4,6])
    refute Enum.all?([2,nil,4])

    assert Enum.all?([])
  end

  def test_any? do
    refute Enum.any?([2,4,6], fn(x, do: rem(x, 2) == 1))
    assert Enum.any?([2,3,4], fn(x, do: rem(x, 2) == 1))

    refute Enum.any?([false,false,false])
    assert Enum.any?([false,true,false])

    refute Enum.any?([])
  end

  def test_find do
    assert_equal nil, Enum.find([2,4,6], fn(x, do: rem(x, 2) == 1))
    assert_equal 0, Enum.find([2,4,6], 0, fn(x, do: rem(x, 2) == 1))
    assert_equal 3, Enum.find([2,3,4], fn(x, do: rem(x, 2) == 1))
  end

  def test_find_value do
    assert_equal nil, Enum.find_value([2,4,6], fn(x, do: rem(x, 2) == 1))
    assert_equal 0, Enum.find_value([2,4,6], 0, fn(x, do: rem(x, 2) == 1))
    assert Enum.find_value([2,3,4], fn(x, do: rem(x, 2) == 1))
  end

  def test_empty? do
    assert Enum.empty?([])
    refute Enum.empty?([1,2,3])
  end

  def test_each do
    assert_equal [], Enum.each([], fn(x, do: x))

    assert_equal [1,2,3], Enum.each([1,2,3], fn(x, do: Process.put(:enum_test_each, x * 2)))
    assert_equal 6, Process.get(:enum_test_each)
  after:
    Process.erase(:enum_test_each)
  end

  def test_filter do
    assert_equal [2], Enum.filter([1,2,3], fn(x, do: rem(x, 2) == 0))
    assert_equal [2,4,6], Enum.filter([2,4,6], fn(x, do: rem(x, 2) == 0))
  end

  def test_filter_with_match do
    assert_equal [1], Enum.filter [1,2,3], match?(1, &1)
    assert_equal [1,2], Enum.filter [1,2,3], match?(x when x < 3, &1)
    assert_equal [1,2,3], Enum.filter [1,2,3], match?(_, &1)
  end

  def test_foldl do
    assert_equal 1, Enum.foldl([], 1, fn(x, acc, do: x + acc))
    assert_equal 7, Enum.foldl([1,2,3], 1, fn(x, acc, do: x + acc))
  end

  def test_join_with_bin do
    assert_equal "", Enum.join([], " = ")
    assert_equal "1 = 2 = 3", Enum.join([1,2,3], " = ")
    assert_equal "1 = 2 = 3", Enum.join([1,"2",3], " = ")
  end

  def test_join_with_list do
    assert_equal '', Enum.join([], ' = ')
    assert_equal '1 = 2 = 3', Enum.join([1,2,3], ' = ')
    assert_equal '1 = 2 = 3', Enum.join([1,"2",3], ' = ')
  end

  def test_keyfind do
    list = [{ :a, 1 }, { :b, 2 }, { :c, 3 }]
    assert_equal { :a, 1 },  Enum.keyfind(list, :a, 1)
    assert_equal true, Enum.keyfind(list, :a, 2, true)
  end

  def test_map do
    assert_equal [], Enum.map([], fn(x) -> x * 2 end)
    assert_equal [2,4,6], Enum.map([1,2,3], fn(x) -> x * 2 end)
  end

  def test_mapfoldl do
    assert_equal { [], 1 }, Enum.mapfoldl([], 1, fn(x, acc, do: { x * 2, x + acc }))
    assert_equal { [2,4,6], 7 }, Enum.mapfoldl([1,2,3], 1, fn(x, acc, do: { x * 2, x + acc }))
  end

  def test_times_with_arity_0 do
    put(:times_with_arity, nil)
    assert_equal 0, Enum.times(0, fn do: Process.put(:times_with_arity, :ok))
    assert_equal nil, Process.get(:times_with_arity)
    assert_equal 3, Enum.times(3, fn do: Process.put(:times_with_arity, :ok))
    assert_equal :ok, Process.get(:times_with_arity)
  after:
    Process.erase(:times_with_arity)
  end

  def test_times_with_arity_1 do
    assert_equal 5, Enum.times(5, fn(x, do: Process.put(:times_with_arity, x)))
    assert_equal 5, Process.get(:times_with_arity)
  after:
    Process.erase(:times_with_arity)
  end

  def test_times_with_arity_2 do
    assert_equal 15, Enum.times(5, 0, fn(acc, x) -> acc + x end)
  end

  def test_enum_for do
    assert_equal [{1, 3}, {1, 4}, {2, 3}, {2, 4}], Enum.__for__([[1,2],[3,4]], fn(acc, y, x) -> [{x,y}|acc] end)
  end

  def test_for do
    assert_equal [{1, 3}, {1, 4}, {2, 3}, {2, 4}], for x in [1,2], y in [3,4], do: {x,y}

    lists = [1,{1,2},2,{2,1}]
    assert_equal [{1, 3}, {1, 4}, {2, 3}, {2, 4}], for {x,_} in lists, y in [3,4], do: {x,y}
    assert_equal [{1, 3}, {1, 4}], for {x,_} in lists, y in [3,4], x == 1, do: {x,y}
  end
end
