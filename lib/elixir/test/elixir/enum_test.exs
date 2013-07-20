Code.require_file "test_helper.exs", __DIR__

defmodule EnumTest.List do
  use ExUnit.Case, async: true

  test :empty? do
    assert Enum.empty?([])
    refute Enum.empty?([1, 2, 3])
    refute Enum.empty?(1..3)
  end

  test :member? do
    assert Enum.member?([1, 2, 3], 2)
    refute Enum.member?([], 0)
    refute Enum.member?([1, 2, 3], 0)
    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
  end

  test :count do
    assert Enum.count([1, 2, 3]) == 3
    assert Enum.count([]) == 0
  end

  test :count_fun do
    assert Enum.count([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == 1
    assert Enum.count([], fn(x) -> rem(x, 2) == 0 end) == 0
  end

  test :all? do
    assert Enum.all?([2, 4, 6], fn(x) -> rem(x, 2) == 0 end)
    refute Enum.all?([2, 3, 4], fn(x) -> rem(x, 2) == 0 end)

    assert Enum.all?([2, 4, 6])
    refute Enum.all?([2, nil, 4])

    assert Enum.all?([])
  end

  test :any? do
    refute Enum.any?([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
    assert Enum.any?([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)

    refute Enum.any?([false, false, false])
    assert Enum.any?([false, true, false])

    refute Enum.any?([])
  end

  test :at do
    assert Enum.at([2, 4, 6], 0) == 2
    assert Enum.at([2, 4, 6], 2) == 6
    assert Enum.at([2, 4, 6], 4) == nil
    assert Enum.at([2, 4, 6], 4, :none) == :none
  end

  test :fetch! do
    assert Enum.fetch!([2, 4, 6], 0) == 2
    assert Enum.fetch!([2, 4, 6], 2) == 6
    assert_raise Enum.OutOfBoundsError, fn ->
      Enum.fetch!([2, 4, 6], 4)
    end
  end

  test :drop do
    assert Enum.drop([1, 2, 3], 0) == [1, 2, 3]
    assert Enum.drop([1, 2, 3], 1) == [2, 3]
    assert Enum.drop([1, 2, 3], 2) == [3]
    assert Enum.drop([1, 2, 3], 3) == []
    assert Enum.drop([1, 2, 3], 4) == []
    assert Enum.drop([1, 2, 3], -1) == [3]
    assert Enum.drop([], 3) == []
  end

  test :drop_while do
    assert Enum.drop_while([1, 2, 3, 4, 3, 2, 1], fn(x) -> x <= 3 end) == [4, 3, 2, 1]
    assert Enum.drop_while([1, 2, 3], fn(_) -> false end) == [1, 2, 3]
    assert Enum.drop_while([1, 2, 3], fn(x) -> x <= 3 end) == []
    assert Enum.drop_while([], fn(_) -> false end) == []
  end

  test :find do
    assert Enum.find([2, 4, 6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find([2, 4, 6], 0, fn(x) -> rem(x, 2) == 1 end) == 0
    assert Enum.find([2, 3, 4], fn(x) -> rem(x, 2) == 1 end) == 3
  end

  test :find_value do
    assert Enum.find_value([2, 4, 6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find_value([2, 4, 6], 0, fn(x) -> rem(x, 2) == 1 end) == 0
    assert Enum.find_value([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
  end

  test :find_index do
    assert Enum.find_index([2, 4, 6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find_index([2, 3, 4], fn(x) -> rem(x, 2) == 1 end) == 1
  end

  test :each do
    assert Enum.each([], fn(x) -> x end) == :ok
    assert Enum.each([1, 2, 3], fn(x) -> Process.put(:enum_test_each, x * 2) end) == :ok
    assert Process.get(:enum_test_each) == 6
  after
    Process.delete(:enum_test_each)
  end

  test :fetch do
    assert Enum.fetch([2, 4, 6], 0) == { :ok, 2 }
    assert Enum.fetch([2, 4, 6], 2) == { :ok, 6 }
    assert Enum.fetch([2, 4, 6], 4) == :error
  end

  test :first do
    assert Enum.first([]) == nil
    assert Enum.first([1, 2, 3]) == 1
  end

  test :filter do
    assert Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == [2]
    assert Enum.filter([2, 4, 6], fn(x) -> rem(x, 2) == 0 end) == [2, 4, 6]
  end

  test :filter_with_match do
    assert Enum.filter([1, 2, 3], match?(1, &1)) == [1]
    assert Enum.filter([1, 2, 3], match?(x when x < 3, &1)) == [1, 2]
    assert Enum.filter([1, 2, 3], match?(_, &1)) == [1, 2, 3]
  end

  test :filter_map do
    assert Enum.filter_map([1, 2, 3], fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4]
    assert Enum.filter_map([2, 4, 6], fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4, 8, 12]
  end

  test :reduce do
    assert Enum.reduce([], 1, fn(x, acc) -> x + acc end) == 1
    assert Enum.reduce([1, 2, 3], 1, fn(x, acc) -> x + acc end) == 7
  end

  test :join_with_bin do
    assert Enum.join([], " = ") == ""
    assert Enum.join([1, 2, 3], " = ") == "1 = 2 = 3"
    assert Enum.join([1, "2", 3], " = ") == "1 = 2 = 3"
    assert Enum.join([1, 2, 3]) == "123"
  end

  test :join_with_list do
    assert Enum.join([], ' = ') == ''
    assert Enum.join([1, 2, 3], ' = ') == '1 = 2 = 3'
    assert Enum.join([1, "2", 3], ' = ') == '1 = 2 = 3'
  end

  test :map_join_with_bin do
    assert Enum.map_join([], " = ", &1 * 2) == ""
    assert Enum.map_join([1, 2, 3], " = ", &1 * 2) == "2 = 4 = 6"
    assert Enum.map_join([1, 2, 3], &1 * 2) == "246"
  end

  test :map_join_with_list do
    assert Enum.map_join([], ' = ', &1 * 2) == ''
    assert Enum.map_join([1, 2, 3], ' = ', &1 * 2) == '2 = 4 = 6'
  end

  test :join_empty do
    fun = fn (acc, _) -> acc end
    assert Enum.join(fun, ".") == ""
    assert Enum.map_join(fun, ".", &1 + 0) == ""
    assert Enum.join(fun, '.') == ''
    assert Enum.map_join(fun, '.', &1 + 0) == ''
  end

  test :map do
    assert Enum.map([], fn x -> x * 2 end) == []
    assert Enum.map([1, 2, 3], fn x -> x * 2 end) == [2, 4, 6]
  end

  test :map_reduce do
    assert Enum.map_reduce([], 1, fn(x, acc) -> { x * 2, x + acc } end) == { [], 1 }
    assert Enum.map_reduce([1, 2, 3], 1, fn(x, acc) -> { x * 2, x + acc } end) == { [2, 4, 6], 7 }
  end

  test :partition do
    assert Enum.partition([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == { [2], [1, 3] }
    assert Enum.partition([2, 4, 6], fn(x) -> rem(x, 2) == 0 end) == { [2, 4, 6], [] }
  end

  test :reject do
    assert Enum.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == [1, 3]
    assert Enum.reject([2, 4, 6], fn(x) -> rem(x, 2) == 0 end) == []
  end

  test :reverse do
    assert Enum.reverse([]) == []
    assert Enum.reverse([1, 2, 3]) == [3, 2, 1]
  end

  test :shuffle do
    # set a fixed seed so the test can be deterministic
    :random.seed(1374, 347975, 449264)
    assert Enum.shuffle([1, 2, 3, 4, 5]) == [2, 4, 1, 5, 3]
  end

  test :sort do
    assert Enum.sort([5, 3, 2, 4, 1]) == [1, 2, 3, 4, 5]
    assert Enum.sort([5, 3, 2, 4, 1], &1 > &2) == [5, 4, 3, 2, 1]
  end

  test :split do
    assert Enum.split([1, 2, 3], 0) == { [], [1, 2, 3] }
    assert Enum.split([1, 2, 3], 1) == { [1], [2, 3] }
    assert Enum.split([1, 2, 3], 2) == { [1, 2], [3] }
    assert Enum.split([1, 2, 3], 3) == { [1, 2, 3], [] }
    assert Enum.split([1, 2, 3], 4) == { [1, 2, 3], [] }
    assert Enum.split([], 3) == { [], [] }
    assert Enum.split([1, 2, 3], -1) == { [1, 2], [3] }
    assert Enum.split([1, 2, 3], -2) == { [1], [2, 3] }
    assert Enum.split([1, 2, 3], -3) == { [], [1, 2, 3] }
    assert Enum.split([1, 2, 3], -10) == { [], [1, 2, 3] }
  end

  test :split_while do
    assert Enum.split_while([1, 2, 3], fn(_) -> false end) == { [], [1, 2, 3] }
    assert Enum.split_while([1, 2, 3], fn(_) -> true end) == { [1, 2, 3], [] }
    assert Enum.split_while([1, 2, 3], fn(x) -> x > 2 end) == { [], [1, 2, 3] }
    assert Enum.split_while([1, 2, 3], fn(x) -> x > 3 end) == { [], [1, 2, 3] }
    assert Enum.split_while([1, 2, 3], fn(x) -> x < 3 end) == { [1, 2], [3] }
    assert Enum.split_while([], fn(_) -> true end) == { [], [] }
  end

  test :take do
    assert Enum.take([1, 2, 3], 0) == []
    assert Enum.take([1, 2, 3], 1) == [1]
    assert Enum.take([1, 2, 3], 2) == [1, 2]
    assert Enum.take([1, 2, 3], 3) == [1, 2, 3]
    assert Enum.take([1, 2, 3], 4) == [1, 2, 3]
    assert Enum.take([1, 2, 3], -1) == [1, 2]
    assert Enum.take([], 3) == []
  end

  test :take_does_not_consume_next_without_a_need do
    import PathHelpers
    File.open!(fixture_path("one-liner.txt"), [], fn file ->
      iterator = IO.stream(file)
      assert Enum.take(iterator, 1) == ["ONE"]
      assert Enum.take(iterator, 5) == []
    end)
  end

  test :take_with_no_item_works_as_no_op do
    import PathHelpers
    iterator = File.stream!(fixture_path("one-liner.txt"))

    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
  end

  test :take_while do
    assert Enum.take_while([1, 2, 3], fn(x) -> x > 3 end) == []
    assert Enum.take_while([1, 2, 3], fn(x) -> x <= 1 end) == [1]
    assert Enum.take_while([1, 2, 3], fn(x) -> x <= 3 end) == [1, 2, 3]
    assert Enum.take_while([], fn(_) -> true end) == []
  end

  test :to_list do
    assert Enum.to_list([]) == []
    assert Enum.to_list(1 .. 3) == [1, 2, 3]
  end

  test :uniq do
    assert Enum.uniq([1, 2, 3, 2, 1]) == [1, 2, 3]
    assert Enum.uniq([1, 2, 3, 2, 1], fn x -> x end) == [1, 2, 3]
  end

  test :zip do
    assert Enum.zip([:a, :b], [1, 2]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b], [1, 2, 3, 4]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b, :c, :d], [1, 2]) == [{:a, 1}, {:b, 2}, {:c, nil}, {:d, nil}]
    assert Enum.zip([], [1]) == []
    assert Enum.zip([1], []) == [{ 1, nil }]
    assert Enum.zip([], []) == []
  end

  test :with_index do
    assert Enum.with_index([]) == []
    assert Enum.with_index([1,2,3]) == [{1,0},{2,1},{3,2}]
  end

  test :max do
    assert Enum.max([1]) == 1
    assert Enum.max([1, 2, 3]) == 3
    assert Enum.max([1, [], :a, {}]) == []
    assert_raise Enum.EmptyError, fn ->
      assert Enum.max([])
    end

    assert Enum.max(["a", "aa", "aaa"], fn(x) -> String.length(x) end) == "aaa"
    assert_raise Enum.EmptyError, fn ->
      Enum.max([], fn(x) -> String.length(x) end)
    end
  end

  test :min do
    assert Enum.min([1]) == 1
    assert Enum.min([1, 2, 3]) == 1
    assert Enum.min([[], :a, {}]) == :a
    assert_raise Enum.EmptyError, fn ->
      assert Enum.min([])
    end

    assert Enum.min(["a", "aa", "aaa"], fn(x) -> String.length(x) end) == "a"
    assert_raise Enum.EmptyError, fn ->
      Enum.min([], fn(x) -> String.length(x) end)
    end
  end
end

defmodule EnumTest.Range do
  use ExUnit.Case, async: true

  test :all? do
    range = Range.new(first: 0, last: 5)
    refute Enum.all?(range, fn(x) -> rem(x, 2) == 0 end)

    range = Range.new(first: 0, last: 1)
    assert Enum.all?(range, fn(x) -> x < 2 end)
    assert Enum.all?(range)

    range = Range.new(first: 1, last: 0)
    assert Enum.all?(range)
  end

  test :any? do
    range = Range.new(first: 0, last: 5)
    refute Enum.any?(range, &1 > 10)

    range = Range.new(first: 0, last: 5)
    assert Enum.any?(range, &1 > 3)

    range = Range.new(first: 1, last: 0)
    assert Enum.any?(range)
  end

  test :fetch! do
    assert Enum.fetch!(2..6, 0) == 2
    assert Enum.fetch!(2..6, 4) == 6
    assert Enum.fetch!(-2..-6, 0) == -2
    assert Enum.fetch!(-2..-6, 4) == -6

    assert_raise Enum.OutOfBoundsError, fn ->
      assert Enum.fetch!(2..6, 8)
    end

    assert_raise Enum.OutOfBoundsError, fn ->
      assert Enum.fetch!(-2..-6, 8)
    end
  end

  test :count do
    range = Range.new(first: 1, last: 5)
    assert Enum.count(range) == 5
    range = Range.new(first: 1, last: 1)
    assert Enum.count(range) == 1
  end

  test :count_fun do
    range = Range.new(first: 1, last: 5)
    assert Enum.count(range, fn(x) -> rem(x, 2) == 0 end) == 2
    range = Range.new(first: 1, last: 1)
    assert Enum.count(range, fn(x) -> rem(x, 2) == 0 end) == 0
  end

  test :drop do
    range = Range.new(first: 1, last: 3)
    assert Enum.drop(range, 0) == [1, 2, 3]
    assert Enum.drop(range, 1) == [2, 3]
    assert Enum.drop(range, 2) == [3]
    assert Enum.drop(range, 3) == []
    assert Enum.drop(range, 4) == []

    range = Range.new(first: 1, last: 0)
    assert Enum.drop(range, 3) == []
  end

  test :drop_while do
    range = Range.new(first: 0, last: 6)
    assert Enum.drop_while(range, fn(x) -> x <= 3 end) == [4, 5, 6]
    assert Enum.drop_while(range, fn(_) -> false end) == [0, 1, 2, 3, 4, 5, 6]

    range = Range.new(first: 0, last: 3)
    assert Enum.drop_while(range, fn(x) -> x <= 3 end) == []

    range = Range.new(first: 1, last: 0)
    assert Enum.drop_while(range, fn(_) -> false end) == [1, 0]
  end

  test :find do
    range = Range.new(first: 2, last: 6)
    assert Enum.find(range, fn(x) -> rem(x, 2) == 0 end) == 2
    assert Enum.find(range, fn(x) -> rem(x, 2) == 1 end) == 3
  end

  test :find_value do
    range = Range.new(first: 2, last: 6)
    assert Enum.find_value(range, fn(x) -> rem(x, 2) == 1 end)
  end

  test :find_index do
    range = Range.new(first: 2, last: 6)
    assert Enum.find_index(range, fn(x) -> rem(x, 2) == 1 end) == 1
  end

  test :empty? do
    range = Range.new(first: 1, last: 0)
    refute Enum.empty?(range)

    range = Range.new(first: 1, last: 2)
    refute Enum.empty?(range)
  end

  test :first do
    range = Range.new(first: 1, last: 0)
    assert Enum.first(range) == 1

    range = Range.new(first: 1, last: 2)
    assert Enum.first(range) == 1
  end

  test :each do
    try do
      range = Range.new(first: 1, last: 0)
      assert Enum.each(range, fn(x) -> x end) == :ok

      range = Range.new(first: 1, last: 3)
      assert Enum.each(range, fn(x) -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == 6
    after
      Process.delete(:enum_test_each)
    end

    try do
      range = Range.new(first: -1, last: -3)
      assert Enum.each(range, fn(x) -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == -6
    after
      Process.delete(:enum_test_each)
    end

  end

  test :filter do
    range = Range.new(first: 1, last: 3)
    assert Enum.filter(range, fn(x) -> rem(x, 2) == 0 end) == [2]

    range = Range.new(first: 1, last: 6)
    assert Enum.filter(range, fn(x) -> rem(x, 2) == 0 end) == [2, 4, 6]
  end

  test :filter_with_match do
    range = Range.new(first: 1, last: 3)
    assert Enum.filter(range, match?(1, &1)) == [1]
    assert Enum.filter(range, match?(x when x < 3, &1)) == [1, 2]
    assert Enum.filter(range, match?(_, &1)) == [1, 2, 3]
  end

  test :filter_map do
    range = Range.new(first: 1, last: 3)
    assert Enum.filter_map(range, fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4]

    range = Range.new(first: 2, last: 6)
    assert Enum.filter_map(range, fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4, 8, 12]
  end

  test :reduce do
    range = Range.new(first: 1, last: 0)
    assert Enum.reduce(range, 1, fn(x, acc) -> x + acc end) == 2

    range = Range.new(first: 1, last: 3)
    assert Enum.reduce(range, 1, fn(x, acc) -> x + acc end) == 7
  end

  test :reject do
    range = Range.new(first: 1, last: 3)
    assert Enum.reject(range, fn(x) -> rem(x, 2) == 0 end) == [1, 3]

    range = Range.new(first: 1, last: 6)
    assert Enum.reject(range, fn(x) -> rem(x, 2) == 0 end) == [1, 3, 5]
  end

  test :join_with_bin do
    range = Range.new(first: 1, last: 0)
    assert Enum.join(range, " = ") == "1 = 0"

    range = Range.new(first: 1, last: 3)
    assert Enum.join(range, " = ") == "1 = 2 = 3"
    assert Enum.join(range) == "123"
  end

  test :join_with_list do
    range = Range.new(first: 1, last: 0)
    assert Enum.join(range, ' = ') == '1 = 0'

    range = Range.new(first: 1, last: 3)
    assert Enum.join(range, ' = ') == '1 = 2 = 3'
  end

  test :map_join_with_bin do
    range = Range.new(first: 1, last: 0)
    assert Enum.map_join(range, " = ", &1 * 2) == "2 = 0"

    range = Range.new(first: 1, last: 3)
    assert Enum.map_join(range, " = ", &1 * 2) == "2 = 4 = 6"
    assert Enum.map_join(range, &1 * 2) == "246"
  end

  test :map_join_with_list do
    range = Range.new(first: 1, last: 0)
    assert Enum.map_join(range, ' = ', &1 * 2) == '2 = 0'

    range = Range.new(first: 1, last: 3)
    assert Enum.map_join(range, ' = ', &1 * 2) == '2 = 4 = 6'
  end

  test :map do
    range = Range.new(first: 1, last: 3)
    assert Enum.map(range, fn x -> x * 2 end) == [2, 4, 6]

    range = Range.new(first: -1, last: -3)
    assert Enum.map(range, fn x -> x * 2 end) == [-2, -4, -6]
  end

  test :map_reduce do
    range = Range.new(first: 1, last: 0)
    assert Enum.map_reduce(range, 1, fn(x, acc) -> { x * 2, x + acc } end) == { [2, 0], 2 }

    range = Range.new(first: 1, last: 3)
    assert Enum.map_reduce(range, 1, fn(x, acc) -> { x * 2, x + acc } end) == { [2, 4, 6], 7 }
  end

  test :partition do
    range = Range.new(first: 1, last: 3)
    assert Enum.partition(range, fn(x) -> rem(x, 2) == 0 end) == { [2], [1, 3] }
  end

  test :shuffle do
    # set a fixed seed so the test can be deterministic
    :random.seed(1374, 347975, 449264)
    assert Enum.shuffle(Range.new(first: 1, last: 5)) == [2, 4, 1, 5, 3]
  end

  test :sort do
    assert Enum.sort(Range.new(first: 3, last: 1)) == [1, 2, 3]
    assert Enum.sort(Range.new(first: 2, last: 1)) == [1, 2]
    assert Enum.sort(Range.new(first: 1, last: 1)) == [1]

    assert Enum.sort(Range.new(first: 3, last: 1), &1 > &2) == [3, 2, 1]
    assert Enum.sort(Range.new(first: 2, last: 1), &1 > &2) == [2, 1]
    assert Enum.sort(Range.new(first: 1, last: 1), &1 > &2) == [1]
  end

  test :split do
    range = Range.new(first: 1, last: 3)
    assert Enum.split(range, 0) == { [], [1, 2, 3] }
    assert Enum.split(range, 1) == { [1], [2, 3] }
    assert Enum.split(range, 2) == { [1, 2], [3] }
    assert Enum.split(range, 3) == { [1, 2, 3], [] }
    assert Enum.split(range, 4) == { [1, 2, 3], [] }
    assert Enum.split(range, -1) == { [1, 2], [3] }
    assert Enum.split(range, -2) == { [1], [2, 3] }
    assert Enum.split(range, -3) == { [], [1, 2, 3] }
    assert Enum.split(range, -10) == { [], [1, 2, 3] }

    range = Range.new(first: 1, last: 0)
    assert Enum.split(range, 3) == { [1, 0], [] }
  end

  test :split_while do
    range = Range.new(first: 1, last: 3)
    assert Enum.split_while(range, fn(_) -> false end) == { [], [1, 2, 3] }
    assert Enum.split_while(range, fn(_) -> true end) == { [1, 2, 3], [] }
    assert Enum.split_while(range, fn(x) -> x > 2 end) == { [], [1, 2, 3] }
    assert Enum.split_while(range, fn(x) -> x > 3 end) == { [], [1, 2, 3] }
    assert Enum.split_while(range, fn(x) -> x < 3 end) == { [1, 2], [3] }

    range = Range.new(first: 1, last: 0)
    assert Enum.split_while(range, fn(_) -> true end) == { [1, 0], [] }
  end

  test :take do
    range = Range.new(first: 1, last: 3)
    assert Enum.take(range, 0) == []
    assert Enum.take(range, 1) == [1]
    assert Enum.take(range, 2) == [1, 2]
    assert Enum.take(range, 3) == [1, 2, 3]
    assert Enum.take(range, 4) == [1, 2, 3]

    range = Range.new(first: 1, last: 0)
    assert Enum.take(range, 3) == [1, 0]
  end

  test :take_while do
    range = Range.new(first: 1, last: 3)
    assert Enum.take_while(range, fn(x) -> x > 3 end) == []
    assert Enum.take_while(range, fn(x) -> x <= 1 end) == [1]
    assert Enum.take_while(range, fn(x) -> x <= 3 end) == [1, 2, 3]
    assert Enum.take_while([], fn(_) -> true end) == []
  end

  test :uniq do
    assert Enum.uniq(1..3) == [1, 2, 3]
    assert Enum.uniq(1..3, fn x -> x end) == [1, 2, 3]
  end

  test :zip do
    assert Enum.zip([:a, :b], 1..2) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b], 1..4) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b, :c, :d], 1..2) == [{:a, 1}, {:b, 2}, {:c, nil}, {:d, nil}]

    assert Enum.zip(1..2, [:a, :b]) == [{1, :a}, {2, :b}]
    assert Enum.zip(1..4, [:a, :b]) == [{1, :a}, {2, :b}, {3, nil}, {4, nil}]
    assert Enum.zip(1..2, [:a, :b, :c, :d]) == [{1, :a}, {2, :b}]

    assert Enum.zip(1..2, 1..2) == [{1, 1}, {2, 2}]
    assert Enum.zip(1..4, 1..2) == [{1, 1}, {2, 2}, {3, nil}, {4, nil}]
    assert Enum.zip(1..2, 1..4) == [{1, 1}, {2, 2}]
  end

  test :with_index do
    assert Enum.with_index(1..3) == [{1,0},{2,1},{3,2}]
  end

  test :max do
    assert Enum.max(1..1) == 1
    assert Enum.max(1..3) == 3
    assert Enum.max(3..1) == 3

    assert Enum.max(1..1, fn(x) -> :math.pow(-2, x) end) == 1
    assert Enum.max(1..3, fn(x) -> :math.pow(-2, x) end) == 2
  end

  test :min do
    assert Enum.min([1]) == 1
    assert Enum.min([1, 2, 3]) == 1
    assert Enum.min([[], :a, {}]) == :a

    assert Enum.min(1..1, fn(x) -> :math.pow(-2, x) end) == 1
    assert Enum.min(1..3, fn(x) -> :math.pow(-2, x) end) == 3
  end
end

defmodule EnumTest.Others do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  test :reverse do
    assert Enum.reverse(URI.query_decoder("foo=bar&baz=bat")) ==
      [{ "baz", "bat" }, { "foo", "bar" }]
  end

  test :count do
    assert Enum.count(URI.query_decoder("foo=bar&baz=bat")) == 2
  end

  test :take do
    # Use IO to simulate side-effects
    reducible = fn(acc, fun) ->
      Enum.reduce([1, 2, 3], acc, fn(x, acc) ->
        IO.puts x
        fun.(x, acc)
      end)
    end

    assert capture_io(fn ->
      Enum.take(reducible, 1)
    end) == "1\n"
  end
end
