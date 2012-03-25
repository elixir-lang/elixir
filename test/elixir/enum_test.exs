Code.require_file "../test_helper", __FILE__

defmodule EnumTest do
  use ExUnit.Case

  test :all? do
    assert Enum.all?([2,4,6], fn(x, do: rem(x, 2) == 0))
    refute Enum.all?([2,3,4], fn(x, do: rem(x, 2) == 0))

    assert Enum.all?([2,4,6])
    refute Enum.all?([2,nil,4])

    assert Enum.all?([])
  end

  test :any? do
    refute Enum.any?([2,4,6], fn(x, do: rem(x, 2) == 1))
    assert Enum.any?([2,3,4], fn(x, do: rem(x, 2) == 1))

    refute Enum.any?([false,false,false])
    assert Enum.any?([false,true,false])

    refute Enum.any?([])
  end

  test :drop do
    assert_equal [1,2,3], Enum.drop [1,2,3], 0
    assert_equal [2,3], Enum.drop [1,2,3], 1
    assert_equal [3], Enum.drop [1,2,3], 2
    assert_equal [], Enum.drop [1,2,3], 3
    assert_equal [], Enum.drop [1,2,3], 4
    assert_equal [], Enum.drop [], 3
  end

  test :find do
    assert_equal nil, Enum.find([2,4,6], fn(x, do: rem(x, 2) == 1))
    assert_equal 0, Enum.find([2,4,6], 0, fn(x, do: rem(x, 2) == 1))
    assert_equal 3, Enum.find([2,3,4], fn(x, do: rem(x, 2) == 1))
  end

  test :find_value do
    assert_equal nil, Enum.find_value([2,4,6], fn(x, do: rem(x, 2) == 1))
    assert_equal 0, Enum.find_value([2,4,6], 0, fn(x, do: rem(x, 2) == 1))
    assert Enum.find_value([2,3,4], fn(x, do: rem(x, 2) == 1))
  end

  test :empty? do
    assert Enum.empty?([])
    refute Enum.empty?([1,2,3])
  end

  test :each do
    assert_equal [], Enum.each([], fn(x, do: x))

    assert_equal [1,2,3], Enum.each([1,2,3], fn(x, do: Process.put(:enum_test_each, x * 2)))
    assert_equal 6, Process.get(:enum_test_each)
  after:
    Process.delete(:enum_test_each)
  end

  test :entries do
    assert_equal [1,2,3], Enum.entries([1,2,3])
  end

  test :filter do
    assert_equal [2], Enum.filter([1,2,3], fn(x, do: rem(x, 2) == 0))
    assert_equal [2,4,6], Enum.filter([2,4,6], fn(x, do: rem(x, 2) == 0))
  end

  test :filter_with_match do
    assert_equal [1], Enum.filter [1,2,3], match?(1, &1)
    assert_equal [1,2], Enum.filter [1,2,3], match?(x when x < 3, &1)
    assert_equal [1,2,3], Enum.filter [1,2,3], match?(_, &1)
  end

  test :filter_map do
    assert_equal [4], Enum.filter_map [1,2,3], fn(x, do: rem(x, 2) == 0), &1 * 2
    assert_equal [4,8,12], Enum.filter_map [2,4,6], fn(x, do: rem(x, 2) == 0), &1 * 2
  end

  test :foldl do
    assert_equal 1, Enum.reduce([], 1, fn(x, acc, do: x + acc))
    assert_equal 7, Enum.reduce([1,2,3], 1, fn(x, acc, do: x + acc))
  end

  test :join_with_bin do
    assert_equal "", Enum.join([], " = ")
    assert_equal "1 = 2 = 3", Enum.join([1,2,3], " = ")
    assert_equal "1 = 2 = 3", Enum.join([1,"2",3], " = ")
    assert_equal "123", Enum.join([1,2,3])
  end

  test :join_with_list do
    assert_equal '', Enum.join([], ' = ')
    assert_equal '1 = 2 = 3', Enum.join([1,2,3], ' = ')
    assert_equal '1 = 2 = 3', Enum.join([1,"2",3], ' = ')
  end

  test :keyfind do
    list = [{ :a, 1 }, { :b, 2 }, { :c, 3 }]
    assert_equal { :a, 1 },  Enum.keyfind(list, :a, 1)
    assert_equal true, Enum.keyfind(list, :a, 2, true)
  end

  test :map do
    assert_equal [], Enum.map([], fn(x) -> x * 2 end)
    assert_equal [2,4,6], Enum.map([1,2,3], fn(x) -> x * 2 end)
  end

  test :mapfoldl do
    assert_equal { [], 1 }, Enum.map_reduce([], 1, fn(x, acc, do: { x * 2, x + acc }))
    assert_equal { [2,4,6], 7 }, Enum.map_reduce([1,2,3], 1, fn(x, acc, do: { x * 2, x + acc }))
  end

  test :partition do
    assert_equal { [2], [1,3] }, Enum.partition([1,2,3], fn(x, do: rem(x, 2) == 0))
    assert_equal { [2,4,6], [] }, Enum.partition([2,4,6], fn(x, do: rem(x, 2) == 0))
  end

  test :split do
    assert_equal { [], [1,2,3] }, Enum.split [1,2,3], 0
    assert_equal { [1], [2,3] }, Enum.split [1,2,3], 1
    assert_equal { [1,2], [3] }, Enum.split [1,2,3], 2
    assert_equal { [1,2,3], [] }, Enum.split [1,2,3], 3
    assert_equal { [1,2,3], [] }, Enum.split [1,2,3], 4
    assert_equal { [], [] }, Enum.split [], 3
  end

  test :take do
    assert_equal [], Enum.take [1,2,3], 0
    assert_equal [1], Enum.take [1,2,3], 1
    assert_equal [1,2], Enum.take [1,2,3], 2
    assert_equal [1,2,3], Enum.take [1,2,3], 3
    assert_equal [1,2,3], Enum.take [1,2,3], 4
    assert_equal [], Enum.take [], 3
  end

  test :times_with_arity_0 do
    Process.put(:times_with_arity, nil)
    assert_equal 0, Enum.times(0, fn do: Process.put(:times_with_arity, :ok))
    assert_equal nil, Process.get(:times_with_arity)
    assert_equal 3, Enum.times(3, fn do: Process.put(:times_with_arity, :ok))
    assert_equal :ok, Process.get(:times_with_arity)
  after:
    Process.delete(:times_with_arity)
  end

  test :times_with_arity_1 do
    assert_equal 5, Enum.times(5, fn(x, do: Process.put(:times_with_arity, x)))
    assert_equal 5, Process.get(:times_with_arity)
  after:
    Process.delete(:times_with_arity)
  end

  test :times_with_arity_2 do
    assert_equal 15, Enum.times(5, 0, fn(acc, x) -> acc + x end)
  end
end
