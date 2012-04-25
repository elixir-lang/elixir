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
    assert Enum.drop([1,2,3], 0) == [1,2,3]
    assert Enum.drop([1,2,3], 1) == [2,3]
    assert Enum.drop([1,2,3], 2) == [3]
    assert Enum.drop([1,2,3], 3) == []
    assert Enum.drop([1,2,3], 4) == []
    assert Enum.drop([], 3) == []
  end

  test :drop_while do
    assert Enum.drop_while([1,2,3,4,3,2,1], fn(x, do: x <= 3)) == [4,3,2,1]
    assert Enum.drop_while([1,2,3], fn(_, do: false)) == [1,2,3]
    assert Enum.drop_while([1,2,3], fn(x, do: x <= 3)) == []
    assert Enum.drop_while([], fn(_, do: false)) == []
  end

  test :find do
    assert Enum.find([2,4,6], fn(x, do: rem(x, 2) == 1)) == nil
    assert Enum.find([2,4,6], 0, fn(x, do: rem(x, 2) == 1)) == 0
    assert Enum.find([2,3,4], fn(x, do: rem(x, 2) == 1)) == 3
  end

  test :find_value do
    assert Enum.find_value([2,4,6], fn(x, do: rem(x, 2) == 1)) == nil
    assert Enum.find_value([2,4,6], 0, fn(x, do: rem(x, 2) == 1)) == 0
    assert Enum.find_value([2,3,4], fn(x, do: rem(x, 2) == 1))
  end

  test :empty? do
    assert Enum.empty?([])
    refute Enum.empty?([1,2,3])
  end

  test :each do
    assert Enum.each([], fn(x, do: x)) == []

    assert Enum.each([1,2,3], fn(x, do: Process.put(:enum_test_each, x * 2))) == [1,2,3]
    assert Process.get(:enum_test_each) == 6
  after:
    Process.delete(:enum_test_each)
  end

  test :entries do
    assert Enum.entries([1,2,3]) == [1,2,3]
  end

  test :filter do
    assert Enum.filter([1,2,3], fn(x, do: rem(x, 2) == 0)) == [2]
    assert Enum.filter([2,4,6], fn(x, do: rem(x, 2) == 0)) == [2,4,6]
  end

  test :filter_with_match do
    assert Enum.filter([1,2,3], match?(1, &1)) == [1]
    assert Enum.filter([1,2,3], match?(x when x < 3, &1)) == [1,2]
    assert Enum.filter([1,2,3], match?(_, &1)) == [1,2,3]
  end

  test :filter_map do
    assert Enum.filter_map([1,2,3], fn(x, do: rem(x, 2) == 0), &1 * 2) == [4]
    assert Enum.filter_map([2,4,6], fn(x, do: rem(x, 2) == 0), &1 * 2) == [4,8,12]
  end

  test :foldl do
    assert Enum.reduce([], 1, fn(x, acc, do: x + acc)) == 1
    assert Enum.reduce([1,2,3], 1, fn(x, acc, do: x + acc)) == 7
  end

  test :join_with_bin do
    assert Enum.join([], " = ") == ""
    assert Enum.join([1,2,3], " = ") == "1 = 2 = 3"
    assert Enum.join([1,"2",3], " = ") == "1 = 2 = 3"
    assert Enum.join([1,2,3]) == "123"
  end

  test :join_with_list do
    assert Enum.join([], ' = ') == ''
    assert Enum.join([1,2,3], ' = ') == '1 = 2 = 3'
    assert Enum.join([1,"2",3], ' = ') == '1 = 2 = 3'
  end

  test :keyfind do
    list = [{ :a, 1 }, { :b, 2 }, { :c, 3 }]
    assert Enum.keyfind(list, :a, 1) == { :a, 1 }
    assert Enum.keyfind(list, :a, 2, true) == true
  end

  test :map do
    assert Enum.map([], fn(x) -> x * 2 end) == []
    assert Enum.map([1,2,3], fn(x) -> x * 2 end) == [2,4,6]
  end

  test :map_reduce do
    assert Enum.map_reduce([], 1, fn(x, acc, do: { x * 2, x + acc })) == { [], 1 }
    assert Enum.map_reduce([1,2,3], 1, fn(x, acc, do: { x * 2, x + acc })) == { [2,4,6], 7 }
  end

  test :partition do
    assert Enum.partition([1,2,3], fn(x, do: rem(x, 2) == 0)) == { [2], [1,3] }
    assert Enum.partition([2,4,6], fn(x, do: rem(x, 2) == 0)) == { [2,4,6], [] }
  end

  test :split do
    assert Enum.split([1,2,3], 0) == { [], [1,2,3] }
    assert Enum.split([1,2,3], 1) == { [1], [2,3] }
    assert Enum.split([1,2,3], 2) == { [1,2], [3] }
    assert Enum.split([1,2,3], 3) == { [1,2,3], [] }
    assert Enum.split([1,2,3], 4) == { [1,2,3], [] }
    assert Enum.split([], 3) == { [], [] }
  end

  test :split_with do
    assert Enum.split_with([1,2,3], fn(_, do: false)) == { [1,2,3], [] }
    assert Enum.split_with([1,2,3], fn(_, do: true)) == { [], [1,2,3] }
    assert Enum.split_with([1,2,3], fn(x, do: x > 2)) == { [1,2], [3] }
    assert Enum.split_with([1,2,3], fn(x, do: x > 3)) == { [1,2,3], [] }
    assert Enum.split_with([], fn(_, do: true)) == { [], [] }
  end

  test :take do
    assert Enum.take([1,2,3], 0) == []
    assert Enum.take([1,2,3], 1) == [1]
    assert Enum.take([1,2,3], 2) == [1,2]
    assert Enum.take([1,2,3], 3) == [1,2,3]
    assert Enum.take([1,2,3], 4) == [1,2,3]
    assert Enum.take([], 3) == []
  end

  test :take_while do
    assert Enum.take_while([1,2,3], fn(x, do: x > 3)) == []
    assert Enum.take_while([1,2,3], fn(x, do: x <= 1)) == [1]
    assert Enum.take_while([1,2,3], fn(x, do: x <= 3)) == [1,2,3]
    assert Enum.take_while([], fn(_, do: true)) == []
  end

  test :times_with_arity_0 do
    Process.put(:times_with_arity, nil)
    assert Enum.times(0, fn do: Process.put(:times_with_arity, :ok)) == 0
    assert Process.get(:times_with_arity) == nil
    assert Enum.times(3, fn do: Process.put(:times_with_arity, :ok)) == 3
    assert Process.get(:times_with_arity) == :ok
  after:
    Process.delete(:times_with_arity)
  end

  test :times_with_arity_1 do
    assert Enum.times(5, fn(x, do: Process.put(:times_with_arity, x))) == 5
    assert Process.get(:times_with_arity) == 5
  after:
    Process.delete(:times_with_arity)
  end

  test :times_with_arity_2 do
    assert Enum.times(5, 0, fn(acc, x) -> acc + x end) == 15
  end
end
