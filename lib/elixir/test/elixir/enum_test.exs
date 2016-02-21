Code.require_file "test_helper.exs", __DIR__

defmodule EnumTest do
  use ExUnit.Case, async: true
  doctest Enum

  test "empty?" do
    assert Enum.empty?([])
    refute Enum.empty?([1, 2, 3])
    refute Enum.empty?(1..3)
  end

  test "member?" do
    assert Enum.member?([1, 2, 3], 2)
    refute Enum.member?([], 0)
    refute Enum.member?([1, 2, 3], 0)
    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
  end

  test "count" do
    assert Enum.count([1, 2, 3]) == 3
    assert Enum.count([]) == 0
  end

  test "count fun" do
    assert Enum.count([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == 1
    assert Enum.count([], fn(x) -> rem(x, 2) == 0 end) == 0
  end

  test "all?" do
    assert Enum.all?([2, 4, 6], fn(x) -> rem(x, 2) == 0 end)
    refute Enum.all?([2, 3, 4], fn(x) -> rem(x, 2) == 0 end)

    assert Enum.all?([2, 4, 6])
    refute Enum.all?([2, nil, 4])

    assert Enum.all?([])
  end

  test "any?" do
    refute Enum.any?([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
    assert Enum.any?([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)

    refute Enum.any?([false, false, false])
    assert Enum.any?([false, true, false])

    assert Enum.any?([:foo, false, false])
    refute Enum.any?([false, nil, false])

    refute Enum.any?([])
  end

  test "at" do
    assert Enum.at([2, 4, 6], 0) == 2
    assert Enum.at([2, 4, 6], 2) == 6
    assert Enum.at([2, 4, 6], 4) == nil
    assert Enum.at([2, 4, 6], 4, :none) == :none
    assert Enum.at([2, 4, 6], -2) == 4
    assert Enum.at([2, 4, 6], -4) == nil
  end

  test "concat/1" do
    assert Enum.concat([[1, [2], 3], [4], [5, 6]]) == [1, [2], 3, 4, 5, 6]
    assert Enum.concat(1..3, []) == [1, 2, 3]

    assert Enum.concat([[], []]) == []
    assert Enum.concat([[]])     == []
    assert Enum.concat([])       == []

    assert Enum.concat([1..5, fn acc, _ -> acc end, [1]]) == [1, 2, 3, 4, 5, 1]
  end

  test "concat/2" do
    assert Enum.concat([], [1]) == [1]
    assert Enum.concat([1, [2], 3], [4, 5]) == [1, [2], 3, 4, 5]
    assert Enum.concat(1..3, []) == [1, 2, 3]

    assert Enum.concat([], []) == []

    assert Enum.concat(fn acc, _ -> acc end, [1]) == [1]
  end

  test "fetch!" do
    assert Enum.fetch!([2, 4, 6], 0) == 2
    assert Enum.fetch!([2, 4, 6], 2) == 6
    assert Enum.fetch!([2, 4, 6], -2) == 4

    assert_raise Enum.OutOfBoundsError, fn ->
      Enum.fetch!([2, 4, 6], 4)
    end

    assert_raise Enum.OutOfBoundsError, fn ->
      Enum.fetch!([2, 4, 6], -4)
    end
  end

  test "dedup" do
    assert Enum.dedup([1, 1, 2, 1, 1, 2, 1]) == [1, 2, 1, 2, 1]
    assert Enum.dedup([2, 1, 1, 2, 1]) == [2, 1, 2, 1]
    assert Enum.dedup([1, 2, 3, 4]) == [1, 2, 3, 4]
    assert Enum.dedup([1, 1.0, 2.0, 2]) == [1, 1.0, 2.0, 2]
    assert Enum.dedup([]) == []
    assert Enum.dedup([nil, nil, true, {:value, true}]) == [nil, true, {:value, true}]
    assert Enum.dedup([nil]) == [nil]
  end

  test "dedup by" do
    assert Enum.dedup_by([{1, :x}, {2, :y}, {2, :z}, {1, :x}], fn {x, _} -> x end)
      == [{1, :x}, {2, :y}, {1, :x}]

    assert Enum.dedup_by([5, 1, 2, 3, 2, 1], fn x -> x > 2 end) == [5, 1, 3, 2]
  end

  test "drop" do
    assert Enum.drop([1, 2, 3], 0) == [1, 2, 3]
    assert Enum.drop([1, 2, 3], 1) == [2, 3]
    assert Enum.drop([1, 2, 3], 2) == [3]
    assert Enum.drop([1, 2, 3], 3) == []
    assert Enum.drop([1, 2, 3], 4) == []
    assert Enum.drop([1, 2, 3], -1) == [1, 2]
    assert Enum.drop([1, 2, 3], -2) == [1]
    assert Enum.drop([1, 2, 3], -4) == []
    assert Enum.drop([], 3) == []
  end

  test "drop while" do
    assert Enum.drop_while([1, 2, 3, 4, 3, 2, 1], fn(x) -> x <= 3 end) == [4, 3, 2, 1]
    assert Enum.drop_while([1, 2, 3], fn(_) -> false end) == [1, 2, 3]
    assert Enum.drop_while([1, 2, 3], fn(x) -> x <= 3 end) == []
    assert Enum.drop_while([], fn(_) -> false end) == []
  end

  test "find" do
    assert Enum.find([2, 4, 6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find([2, 4, 6], 0, fn(x) -> rem(x, 2) == 1 end) == 0
    assert Enum.find([2, 3, 4], fn(x) -> rem(x, 2) == 1 end) == 3
  end

  test "find value" do
    assert Enum.find_value([2, 4, 6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find_value([2, 4, 6], 0, fn(x) -> rem(x, 2) == 1 end) == 0
    assert Enum.find_value([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
  end

  test "find index" do
    assert Enum.find_index([2, 4, 6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find_index([2, 3, 4], fn(x) -> rem(x, 2) == 1 end) == 1
  end

  test "each" do
    assert Enum.each([], fn(x) -> x end) == :ok
    assert Enum.each([1, 2, 3], fn(x) -> Process.put(:enum_test_each, x * 2) end) == :ok
    assert Process.get(:enum_test_each) == 6
  after
    Process.delete(:enum_test_each)
  end

  test "fetch" do
    assert Enum.fetch([2, 4, 6], 0) == {:ok, 2}
    assert Enum.fetch([2, 4, 6], 2) == {:ok, 6}
    assert Enum.fetch([2, 4, 6], 4) == :error
    assert Enum.fetch([2, 4, 6], -2) == {:ok, 4}
    assert Enum.fetch([2, 4, 6], -4) == :error
  end

  test "filter" do
    assert Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == [2]
    assert Enum.filter([2, 4, 6], fn(x) -> rem(x, 2) == 0 end) == [2, 4, 6]
  end

  test "filter with match" do
    assert Enum.filter([1, 2, 3], &match?(1, &1)) == [1]
    assert Enum.filter([1, 2, 3], &match?(x when x < 3, &1)) == [1, 2]
    assert Enum.filter([1, 2, 3], &match?(_, &1)) == [1, 2, 3]
  end

  test "filter map" do
    assert Enum.filter_map([1, 2, 3], fn(x) -> rem(x, 2) == 0 end, &(&1 * 2)) == [4]
    assert Enum.filter_map([2, 4, 6], fn(x) -> rem(x, 2) == 0 end, &(&1 * 2)) == [4, 8, 12]
  end

  test "flat map" do
    assert Enum.flat_map([], fn(x) -> [x, x] end) == []
    assert Enum.flat_map([1, 2, 3], fn(x) -> [x, x] end) == [1, 1, 2, 2, 3, 3]
    assert Enum.flat_map([1, 2, 3], fn(x) -> x..x+1 end) == [1, 2, 2, 3, 3, 4]
  end

  test "flat map reduce" do
    assert Enum.flat_map_reduce([1, 2, 3], 0, &{[&1, &2], &1 + &2}) ==
           {[1, 0, 2, 1, 3, 3], 6}

    assert Enum.flat_map_reduce(1..100, 0, fn i, acc ->
      if acc < 3, do: {[i], acc + 1}, else: {:halt, acc}
    end) == {[1, 2, 3], 3}
  end

  test "group by" do
    assert Enum.group_by([], fn -> nil end) == %{}
    assert Enum.group_by(1..6, &rem(&1, 3)) ==
           %{0 => [3, 6], 1 => [1, 4], 2 => [2, 5]}

    result = Enum.group_by(1..6, %{3 => :default}, &rem(&1, 3))
    assert result[0] == [3, 6]
    assert result[3] == :default
  end

  test "into" do
    assert Enum.into([a: 1, b: 2], %{}) == %{a: 1, b: 2}
    assert Enum.into([a: 1, b: 2], %{c: 3}) == %{a: 1, b: 2, c: 3}
    assert Enum.into(%{a: 1, b: 2}, []) == [a: 1, b: 2]
    assert Enum.into([1, 2, 3], "numbers: ", &to_string/1) == "numbers: 123"
  end

  test "intersperse" do
    assert Enum.intersperse([], true) == []
    assert Enum.intersperse([1], true) == [1]
    assert Enum.intersperse([1, 2, 3], true) == [1, true, 2, true, 3]
  end

  test "join" do
    assert Enum.join([], " = ") == ""
    assert Enum.join([1, 2, 3], " = ") == "1 = 2 = 3"
    assert Enum.join([1, "2", 3], " = ") == "1 = 2 = 3"
    assert Enum.join([1, 2, 3]) == "123"
    assert Enum.join(["", "", 1, 2, "", 3, "", "\n"], ";") == ";;1;2;;3;;\n"
    assert Enum.join([""]) == ""
  end

  test "map join" do
    assert Enum.map_join([], " = ", &(&1 * 2)) == ""
    assert Enum.map_join([1, 2, 3], " = ", &(&1 * 2)) == "2 = 4 = 6"
    assert Enum.map_join([1, 2, 3], &(&1 * 2)) == "246"
    assert Enum.map_join(["", "", 1, 2, "", 3, "", "\n"], ";", &(&1)) == ";;1;2;;3;;\n"
    assert Enum.map_join([""], "", &(&1)) == ""
  end

  test "join empty" do
    fun = fn (acc, _) -> acc end
    assert Enum.join(fun, ".") == ""
    assert Enum.map_join(fun, ".", &(&1 + 0)) == ""
  end

  test "map" do
    assert Enum.map([], fn x -> x * 2 end) == []
    assert Enum.map([1, 2, 3], fn x -> x * 2 end) == [2, 4, 6]
  end

  test "map reduce" do
    assert Enum.map_reduce([], 1, fn(x, acc) -> {x * 2, x + acc} end) == {[], 1}
    assert Enum.map_reduce([1, 2, 3], 1, fn(x, acc) -> {x * 2, x + acc} end) == {[2, 4, 6], 7}
  end

  test "partition" do
    assert Enum.partition([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == {[2], [1, 3]}
    assert Enum.partition([2, 4, 6], fn(x) -> rem(x, 2) == 0 end) == {[2, 4, 6], []}
  end

  test "reduce" do
    assert Enum.reduce([], 1, fn(x, acc) -> x + acc end) == 1
    assert Enum.reduce([1, 2, 3], 1, fn(x, acc) -> x + acc end) == 7

    assert Enum.reduce([1, 2, 3], fn(x, acc) -> x + acc end) == 6
    assert_raise Enum.EmptyError, fn ->
      Enum.reduce([], fn(x, acc) -> x + acc end)
    end
  end

  test "reduce while" do
    assert Enum.reduce_while(1..100, 0, fn i, acc ->
      if i <= 3, do: {:cont, acc + i}, else: {:halt, acc}
    end) == 6

    assert Enum.reduce_while([1, 2, 3], 1, fn i, acc -> {:cont, acc + i} end) == 7
    assert Enum.reduce_while([1, 2, 3], 1, fn _i, acc -> {:halt, acc} end) == 1
    assert Enum.reduce_while([], 0, fn _i, acc -> {:cont, acc} end) == 0
  end

  test "reject" do
    assert Enum.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end) == [1, 3]
    assert Enum.reject([2, 4, 6], fn(x) -> rem(x, 2) == 0 end) == []
  end

  test "reverse" do
    assert Enum.reverse([]) == []
    assert Enum.reverse([1, 2, 3]) == [3, 2, 1]
    assert Enum.reverse([1, 2, 3], [4, 5, 6]) == [3, 2, 1, 4, 5, 6]
  end

  test "reverse slice" do
    assert Enum.reverse_slice([], 1, 2) == []
    assert Enum.reverse_slice([1, 2, 3], 0, 0) == [1, 2, 3]
    assert Enum.reverse_slice([1, 2, 3], 0, 1) == [1, 2, 3]
    assert Enum.reverse_slice([1, 2, 3], 0, 2) == [2, 1, 3]
    assert Enum.reverse_slice([1, 2, 3], 0, 20000000) == [3, 2, 1]
    assert Enum.reverse_slice([1, 2, 3], 100, 2) == [1, 2, 3]
    assert Enum.reverse_slice([1, 2, 3], 10, 10) == [1, 2, 3]
  end

  test "random" do
    # corner cases, independent of the seed
    assert_raise Enum.EmptyError, fn -> Enum.random([]) end
    assert Enum.random([1]) == 1

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407414, 139258}
    seed2 = {1306, 421106, 567597}
    :rand.seed(:exsplus, seed1)
    assert Enum.random([1, 2]) == 1
    assert Enum.random([1, 2, 3]) == 1
    assert Enum.random([1, 2, 3, 4]) == 2
    assert Enum.random([1, 2, 3, 4, 5]) == 4
    :rand.seed(:exsplus, seed2)
    assert Enum.random([1, 2]) == 2
    assert Enum.random([1, 2, 3]) == 2
    assert Enum.random([1, 2, 3, 4]) == 3
    assert Enum.random([1, 2, 3, 4, 5]) == 5
  end

  test "take random" do
    # corner cases, independent of the seed
    assert_raise FunctionClauseError, fn -> Enum.take_random([1, 2], -1) end
    assert Enum.take_random([], 0) == []
    assert Enum.take_random([], 3) == []
    assert Enum.take_random([1], 0) == []
    assert Enum.take_random([1], 2) == [1]
    assert Enum.take_random([1, 2], 0) == []

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407414, 139258}
    seed2 = {1406, 421106, 567597}
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random([1, 2, 3], 1) == [1]
    assert Enum.take_random([1, 2, 3], 2) == [1, 3]
    assert Enum.take_random([1, 2, 3], 3) == [2, 1, 3]
    assert Enum.take_random([1, 2, 3], 4) == [3, 1, 2]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random([1, 2, 3], 1) == [3]
    assert Enum.take_random([1, 2, 3], 2) == [1, 2]
    assert Enum.take_random([1, 2, 3], 3) == [1, 2, 3]
    assert Enum.take_random([1, 2, 3], 4) == [1, 2, 3]

    # assert that every item in the sample comes from the input list
    list = for _<-1..100, do: make_ref
    for x <- Enum.take_random(list, 50) do
      assert x in list
    end
  end

  test "scan" do
    assert Enum.scan([1, 2, 3, 4, 5], &(&1 + &2)) == [1, 3, 6, 10, 15]
    assert Enum.scan([], &(&1 + &2)) == []

    assert Enum.scan([1, 2, 3, 4, 5], 0, &(&1 + &2)) == [1, 3, 6, 10, 15]
    assert Enum.scan([], 0, &(&1 + &2)) == []
  end

  test "shuffle" do
    # set a fixed seed so the test can be deterministic
    :rand.seed(:exsplus, {1374, 347975, 449264})
    assert Enum.shuffle([1, 2, 3, 4, 5]) == [2, 1, 3, 5, 4]
  end

  test "sort" do
    assert Enum.sort([5, 3, 2, 4, 1]) == [1, 2, 3, 4, 5]
    assert Enum.sort([5, 3, 2, 4, 1], &(&1 > &2)) == [5, 4, 3, 2, 1]
  end

  test "sort by" do
    collection = [
      [other_data: 1, sorted_data: 5],
      [other_data: 3, sorted_data: 4],
      [other_data: 4, sorted_data: 3],
      [other_data: 2, sorted_data: 2],
      [other_data: 5, sorted_data: 1]
    ]

    assert Enum.sort_by(
        collection,
        &(&1[:sorted_data])
      ) == [
        [other_data: 5, sorted_data: 1],
        [other_data: 2, sorted_data: 2],
        [other_data: 4, sorted_data: 3],
        [other_data: 3, sorted_data: 4],
        [other_data: 1, sorted_data: 5]
      ]
    assert Enum.sort_by(collection, &(&1[:sorted_data]), &>=/2) == collection
  end

  test "split" do
    assert Enum.split([1, 2, 3], 0) == {[], [1, 2, 3]}
    assert Enum.split([1, 2, 3], 1) == {[1], [2, 3]}
    assert Enum.split([1, 2, 3], 2) == {[1, 2], [3]}
    assert Enum.split([1, 2, 3], 3) == {[1, 2, 3], []}
    assert Enum.split([1, 2, 3], 4) == {[1, 2, 3], []}
    assert Enum.split([], 3) == {[], []}
    assert Enum.split([1, 2, 3], -1) == {[1, 2], [3]}
    assert Enum.split([1, 2, 3], -2) == {[1], [2, 3]}
    assert Enum.split([1, 2, 3], -3) == {[], [1, 2, 3]}
    assert Enum.split([1, 2, 3], -10) == {[], [1, 2, 3]}
  end

  test "split while" do
    assert Enum.split_while([1, 2, 3], fn(_) -> false end) == {[], [1, 2, 3]}
    assert Enum.split_while([1, 2, 3], fn(_) -> true end) == {[1, 2, 3], []}
    assert Enum.split_while([1, 2, 3], fn(x) -> x > 2 end) == {[], [1, 2, 3]}
    assert Enum.split_while([1, 2, 3], fn(x) -> x > 3 end) == {[], [1, 2, 3]}
    assert Enum.split_while([1, 2, 3], fn(x) -> x < 3 end) == {[1, 2], [3]}
    assert Enum.split_while([], fn(_) -> true end) == {[], []}
  end

  test "sum" do
    assert Enum.sum([]) == 0
    assert Enum.sum([1]) == 1
    assert Enum.sum([1, 2, 3]) == 6
    assert Enum.sum([1.1, 2.2, 3.3]) == 6.6
    assert_raise ArithmeticError, fn ->
      Enum.sum([{}])
    end
    assert_raise ArithmeticError, fn ->
      Enum.sum([1, {}])
    end
  end

  test "take" do
    assert Enum.take([1, 2, 3], 0) == []
    assert Enum.take([1, 2, 3], 1) == [1]
    assert Enum.take([1, 2, 3], 2) == [1, 2]
    assert Enum.take([1, 2, 3], 3) == [1, 2, 3]
    assert Enum.take([1, 2, 3], 4) == [1, 2, 3]
    assert Enum.take([1, 2, 3], -1) == [3]
    assert Enum.take([1, 2, 3], -2) == [2, 3]
    assert Enum.take([1, 2, 3], -4) == [1, 2, 3]
    assert Enum.take([], 3) == []
  end

  test "take every" do
    assert Enum.take_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 2) == [1, 3, 5, 7, 9]
    assert Enum.take_every([], 2) == []
    assert Enum.take_every([1, 2], 2) == [1]
    assert Enum.take_every([1, 2, 3], 0) == []
    assert Enum.take_every(1..3, 1) == [1, 2, 3]
    assert_raise FunctionClauseError, fn ->
      Enum.take_every([1, 2, 3], -1)
    end
    assert_raise FunctionClauseError, fn ->
      Enum.take_every(1..10, 3.33)
    end
  end

  test "take while" do
    assert Enum.take_while([1, 2, 3], fn(x) -> x > 3 end) == []
    assert Enum.take_while([1, 2, 3], fn(x) -> x <= 1 end) == [1]
    assert Enum.take_while([1, 2, 3], fn(x) -> x <= 3 end) == [1, 2, 3]
    assert Enum.take_while([], fn(_) -> true end) == []
  end

  test "to list" do
    assert Enum.to_list([]) == []
    assert Enum.to_list(1..3) == [1, 2, 3]
  end

  test "uniq by" do
    assert Enum.uniq_by([1, 2, 3, 2, 1], fn x -> x end) == [1, 2, 3]
  end

  test "uniq" do
    assert Enum.uniq([5, 1, 2, 3, 2, 1]) == [5, 1, 2, 3]
  end

  test "zip" do
    assert Enum.zip([:a, :b], [1, 2]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b], [1, 2, 3, 4]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b, :c, :d], [1, 2]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([], [1]) == []
    assert Enum.zip([1], []) == []
    assert Enum.zip([], [])  == []
  end

  test "unzip" do
    assert Enum.unzip([{:a, 1}, {:b, 2}, {:c, 3}]) == {[:a, :b, :c], [1, 2, 3]}
    assert Enum.unzip([]) == {[], []}
    assert Enum.unzip(%{a: 1, b: 2}) == {[:a, :b], [1, 2]}
    assert Enum.unzip([foo: "a", bar: "b"]) == {[:foo, :bar], ["a", "b"]}

    assert_raise FunctionClauseError, fn -> Enum.unzip([{:a, 1}, {:b, 2, "foo"}]) end
    assert_raise FunctionClauseError, fn -> Enum.unzip([{1, 2, {3, {4, 5}}}]) end
    assert_raise FunctionClauseError, fn -> Enum.unzip([1, 2, 3]) end
  end

  test "with index" do
    assert Enum.with_index([]) == []
    assert Enum.with_index([1, 2, 3]) == [{1, 0}, {2, 1}, {3, 2}]
    assert Enum.with_index([1, 2, 3], 10) == [{1, 10}, {2, 11}, {3, 12}]
  end

  test "max" do
    assert Enum.max([1]) == 1
    assert Enum.max([1, 2, 3]) == 3
    assert Enum.max([1, [], :a, {}]) == []
    assert_raise Enum.EmptyError, fn ->
      Enum.max([])
    end
  end

  test "max by" do
    assert Enum.max_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end) == "aaa"
    assert_raise Enum.EmptyError, fn ->
      Enum.max_by([], fn(x) -> String.length(x) end)
    end
  end

  test "min" do
    assert Enum.min([1]) == 1
    assert Enum.min([1, 2, 3]) == 1
    assert Enum.min([[], :a, {}]) == :a
    assert_raise Enum.EmptyError, fn ->
      Enum.min([])
    end
  end

  test "min by" do
    assert Enum.min_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end) == "a"
    assert_raise Enum.EmptyError, fn ->
      Enum.min_by([], fn(x) -> String.length(x) end)
    end
  end

  test "min max" do
    assert Enum.min_max([1]) == {1, 1}
    assert Enum.min_max([2, 3, 1]) == {1, 3}
    assert Enum.min_max([[], :a, {}]) == {:a, []}
    assert_raise Enum.EmptyError, fn ->
      Enum.min_max([])
    end
  end

  test "min max by" do
    assert Enum.min_max_by(["aaa", "a", "aa"], fn(x) -> String.length(x) end) == {"a", "aaa"}
    assert_raise Enum.EmptyError, fn ->
      Enum.min_max_by([], fn(x) -> String.length(x) end)
    end
  end

  test "chunk" do
    assert Enum.chunk([1, 2, 3, 4, 5], 2) == [[1, 2], [3, 4]]
    assert Enum.chunk([1, 2, 3, 4, 5], 2, 2, [6]) == [[1, 2], [3, 4], [5, 6]]
    assert Enum.chunk([1, 2, 3, 4, 5, 6], 3, 2) == [[1, 2, 3], [3, 4, 5]]
    assert Enum.chunk([1, 2, 3, 4, 5, 6], 2, 3) == [[1, 2], [4, 5]]
    assert Enum.chunk([1, 2, 3, 4, 5, 6], 3, 2, []) == [[1, 2, 3], [3, 4, 5], [5, 6]]
    assert Enum.chunk([1, 2, 3, 4, 5, 6], 3, 3, []) == [[1, 2, 3], [4, 5, 6]]
    assert Enum.chunk([1, 2, 3, 4, 5], 4, 4, 6..10) == [[1, 2, 3, 4], [5, 6, 7, 8]]
  end

  test "chunk by" do
    assert Enum.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1)) == [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
    assert Enum.chunk_by([1, 2, 3, 4], fn _ -> true end) == [[1, 2, 3, 4]]
    assert Enum.chunk_by([], fn _ -> true end) == []
    assert Enum.chunk_by([1], fn _ -> true end) == [[1]]
  end

  test "slice" do
    list = [1, 2, 3, 4, 5]
    assert Enum.slice(list, 0, 0) == []
    assert Enum.slice(list, 0, 1) == [1]
    assert Enum.slice(list, 0, 2) == [1, 2]
    assert Enum.slice(list, 1, 2) == [2, 3]
    assert Enum.slice(list, 1, 0) == []
    assert Enum.slice(list, 2, 5) == [3, 4, 5]
    assert Enum.slice(list, 2, 6) == [3, 4, 5]
    assert Enum.slice(list, 5, 5) == []
    assert Enum.slice(list, 6, 5) == []
    assert Enum.slice(list, 6, 0) == []
    assert Enum.slice(list, -6, 0) == []
    assert Enum.slice(list, -6, 5) == []
    assert Enum.slice(list, -2, 5) == [4, 5]
    assert Enum.slice(list, -3, 1) == [3]
    assert_raise FunctionClauseError, fn ->
      Enum.slice(list, 0, -1)
    end
    assert_raise FunctionClauseError, fn ->
      Enum.slice(list, 0.99, 0)
    end
    assert_raise FunctionClauseError, fn ->
      Enum.slice(list, 0, 0.99)
    end
  end

  test "slice range" do
    list = [1, 2, 3, 4, 5]
    assert Enum.slice(list, 0..0) == [1]
    assert Enum.slice(list, 0..1) == [1, 2]
    assert Enum.slice(list, 0..2) == [1, 2, 3]
    assert Enum.slice(list, 1..2) == [2, 3]
    assert Enum.slice(list, 1..0) == []
    assert Enum.slice(list, 2..5) == [3, 4, 5]
    assert Enum.slice(list, 2..6) == [3, 4, 5]
    assert Enum.slice(list, 4..4) == [5]
    assert Enum.slice(list, 5..5) == []
    assert Enum.slice(list, 6..5) == []
    assert Enum.slice(list, 6..0) == []
    assert Enum.slice(list, -6..0) == []
    assert Enum.slice(list, -6..5) == []
    assert Enum.slice(list, -5..-1) == [1, 2, 3, 4, 5]
    assert Enum.slice(list, -5..-3) == [1, 2, 3]
    assert Enum.slice(list, -6..-1) == []
    assert Enum.slice(list, -6..-3) == []
    assert_raise ArgumentError, fn ->
      x = 1.1
      Enum.slice(list, x..2)
    end
    assert_raise ArgumentError, fn ->
      x = 1.9
      Enum.slice(list, 1..x)
    end
  end
end

defmodule EnumTest.Range do
  use ExUnit.Case, async: true

  test "all?" do
    range = 0..5
    refute Enum.all?(range, fn(x) -> rem(x, 2) == 0 end)

    range = 0..1
    assert Enum.all?(range, fn(x) -> x < 2 end)
    assert Enum.all?(range)

    range = 1..0
    assert Enum.all?(range)
  end

  test "any?" do
    range = 0..5
    refute Enum.any?(range, &(&1 > 10))

    range = 0..5
    assert Enum.any?(range, &(&1 > 3))

    range = 1..0
    assert Enum.any?(range)
  end

  test "fetch!" do
    assert Enum.fetch!(2..6, 0) == 2
    assert Enum.fetch!(2..6, 4) == 6
    assert Enum.fetch!(2..6, -1) == 6
    assert Enum.fetch!(2..6, -2) == 5
    assert Enum.fetch!(-2..-6, 0) == -2
    assert Enum.fetch!(-2..-6, 4) == -6

    assert_raise Enum.OutOfBoundsError, fn ->
      Enum.fetch!(2..6, 8)
    end

    assert_raise Enum.OutOfBoundsError, fn ->
      Enum.fetch!(-2..-6, 8)
    end

    assert_raise Enum.OutOfBoundsError, fn ->
      Enum.fetch!(2..6, -8)
    end
  end

  test "count" do
    range = 1..5
    assert Enum.count(range) == 5
    range = 1..1
    assert Enum.count(range) == 1

    assert Enum.count([1, true, false, nil]) == 4
  end

  test "count fun" do
    range = 1..5
    assert Enum.count(range, fn(x) -> rem(x, 2) == 0 end) == 2
    range = 1..1
    assert Enum.count(range, fn(x) -> rem(x, 2) == 0 end) == 0

    assert Enum.count([1, true, false, nil], & &1) == 2
  end

  test "chunk" do
    assert Enum.chunk(1..5, 2) == [[1, 2], [3, 4]]
    assert Enum.chunk(1..5, 2, 2, [6]) == [[1, 2], [3, 4], [5, 6]]
    assert Enum.chunk(1..6, 3, 2) == [[1, 2, 3], [3, 4, 5]]
    assert Enum.chunk(1..6, 2, 3) == [[1, 2], [4, 5]]
    assert Enum.chunk(1..6, 3, 2, []) == [[1, 2, 3], [3, 4, 5], [5, 6]]
    assert Enum.chunk(1..5, 4, 4, 6..10) == [[1, 2, 3, 4], [5, 6, 7, 8]]
  end

  test "chunk by" do
    assert Enum.chunk_by(1..4, fn _ -> true end) == [[1, 2, 3, 4]]
    assert Enum.chunk_by(1..4, &(rem(&1, 2) == 1)) == [[1], [2], [3], [4]]
  end

  test "dedup" do
    assert Enum.dedup(1..3) == [1, 2, 3]
  end

  test "dedup by" do
    assert Enum.dedup_by(1..3, fn _ -> 1 end) == [1]
  end

  test "drop" do
    range = 1..3
    assert Enum.drop(range, 0) == [1, 2, 3]
    assert Enum.drop(range, 1) == [2, 3]
    assert Enum.drop(range, 2) == [3]
    assert Enum.drop(range, 3) == []
    assert Enum.drop(range, 4) == []
    assert Enum.drop(range, -1) == [1, 2]
    assert Enum.drop(range, -2) == [1]
    assert Enum.drop(range, -4) == []

    range = 1..0
    assert Enum.drop(range, 3) == []
  end

  test "drop while" do
    range = 0..6
    assert Enum.drop_while(range, fn(x) -> x <= 3 end) == [4, 5, 6]
    assert Enum.drop_while(range, fn(_) -> false end) == [0, 1, 2, 3, 4, 5, 6]

    range = 0..3
    assert Enum.drop_while(range, fn(x) -> x <= 3 end) == []

    range = 1..0
    assert Enum.drop_while(range, fn(_) -> nil end) == [1, 0]
  end

  test "find" do
    range = 2..6
    assert Enum.find(range, fn(x) -> rem(x, 2) == 0 end) == 2
    assert Enum.find(range, fn(x) -> rem(x, 2) == 1 end) == 3
    assert Enum.find(range, fn _ -> false end) == nil
    assert Enum.find(range, 0, fn _ -> false end) == 0
  end

  test "find value" do
    range = 2..6
    assert Enum.find_value(range, fn(x) -> rem(x, 2) == 1 end)
  end

  test "find index" do
    range = 2..6
    assert Enum.find_index(range, fn(x) -> rem(x, 2) == 1 end) == 1
  end

  test "empty?" do
    range = 1..0
    refute Enum.empty?(range)

    range = 1..2
    refute Enum.empty?(range)
  end

  test "each" do
    try do
      range = 1..0
      assert Enum.each(range, fn(x) -> x end) == :ok

      range = 1..3
      assert Enum.each(range, fn(x) -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == 6
    after
      Process.delete(:enum_test_each)
    end

    try do
      range = -1..-3
      assert Enum.each(range, fn(x) -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == -6
    after
      Process.delete(:enum_test_each)
    end
  end

  test "filter" do
    range = 1..3
    assert Enum.filter(range, fn(x) -> rem(x, 2) == 0 end) == [2]

    range = 1..6
    assert Enum.filter(range, fn(x) -> rem(x, 2) == 0 end) == [2, 4, 6]

    assert Enum.filter([1, 2, false, 3, nil], & &1) == [1, 2, 3]
  end

  test "filter with match" do
    range = 1..3
    assert Enum.filter(range, &match?(1, &1)) == [1]
    assert Enum.filter(range, &match?(x when x < 3, &1)) == [1, 2]
    assert Enum.filter(range, &match?(_, &1)) == [1, 2, 3]
  end

  test "filter map" do
    range = 1..3
    assert Enum.filter_map(range, fn(x) -> rem(x, 2) == 0 end, &(&1 * 2)) == [4]

    range = 2..6
    assert Enum.filter_map(range, fn(x) -> rem(x, 2) == 0 end, &(&1 * 2)) == [4, 8, 12]
  end

  test "flat map" do
    range = 1..3
    assert Enum.flat_map(range, fn(x) -> [x, x] end) == [1, 1, 2, 2, 3, 3]
  end

  test "intersperse" do
    range = 1..0
    assert Enum.intersperse(range, true) == [1, true, 0]

    range = 1..3
    assert Enum.intersperse(range, false) == [1, false, 2, false, 3]
  end

  test "into" do
    assert Enum.into([a: 1, b: 2], %{}) == %{a: 1, b: 2}
    assert Enum.into(%{a: 1, b: 2}, []) == [a: 1, b: 2]
    assert Enum.into(3..5, [1, 2]) == [1, 2, 3, 4, 5]
    assert Enum.into(1..5, []) == [1, 2, 3, 4, 5]
    assert Enum.into(1..5, [], fn x -> x * 2 end) == [2, 4, 6, 8, 10]
    assert Enum.into(1..3, "numbers: ", &to_string/1) == "numbers: 123"
  end

  test "join" do
    range = 1..0
    assert Enum.join(range, " = ") == "1 = 0"

    range = 1..3
    assert Enum.join(range, " = ") == "1 = 2 = 3"
    assert Enum.join(range) == "123"
  end

  test "map join" do
    range = 1..0
    assert Enum.map_join(range, " = ", &(&1 * 2)) == "2 = 0"

    range = 1..3
    assert Enum.map_join(range, " = ", &(&1 * 2)) == "2 = 4 = 6"
    assert Enum.map_join(range, &(&1 * 2)) == "246"
  end

  test "map" do
    range = 1..3
    assert Enum.map(range, fn x -> x * 2 end) == [2, 4, 6]

    range = -1..-3
    assert Enum.map(range, fn x -> x * 2 end) == [-2, -4, -6]
  end

  test "map reduce" do
    range = 1..0
    assert Enum.map_reduce(range, 1, fn(x, acc) -> {x * 2, x + acc} end) == {[2, 0], 2}

    range = 1..3
    assert Enum.map_reduce(range, 1, fn(x, acc) -> {x * 2, x + acc} end) == {[2, 4, 6], 7}
  end

  test "max" do
    assert Enum.max(1..1) == 1
    assert Enum.max(1..3) == 3
    assert Enum.max(3..1) == 3
  end

  test "max by" do
    assert Enum.max_by(1..1, fn(x) -> :math.pow(-2, x) end) == 1
    assert Enum.max_by(1..3, fn(x) -> :math.pow(-2, x) end) == 2
  end

  test "min" do
    assert Enum.min(1..1) == 1
    assert Enum.min(1..3) == 1
  end

  test "min by" do
    assert Enum.min_by(1..1, fn(x) -> :math.pow(-2, x) end) == 1
    assert Enum.min_by(1..3, fn(x) -> :math.pow(-2, x) end) == 3
  end

  test "partition" do
    range = 1..3
    assert Enum.partition(range, fn(x) -> rem(x, 2) == 0 end) == {[2], [1, 3]}
  end

  test "reduce" do
    range = 1..0
    assert Enum.reduce(range, 1, fn(x, acc) -> x + acc end) == 2

    range = 1..3
    assert Enum.reduce(range, 1, fn(x, acc) -> x + acc end) == 7

    range = 1..3
    assert Enum.reduce(range, fn(x, acc) -> x + acc end) == 6
  end

  test "reject" do
    range = 1..3
    assert Enum.reject(range, fn(x) -> rem(x, 2) == 0 end) == [1, 3]

    range = 1..6
    assert Enum.reject(range, fn(x) -> rem(x, 2) == 0 end) == [1, 3, 5]

    assert Enum.reject([1, true, nil, false, 2], & &1) == [nil, false]
  end

  test "reverse" do
    assert Enum.reverse(0..0) == [0]
    assert Enum.reverse(1..3) == [3, 2, 1]
    assert Enum.reverse(1..3, 4..6) == [3, 2, 1, 4, 5, 6]
    assert Enum.reverse([1, 2, 3], 4..6) == [3, 2, 1, 4, 5, 6]
    assert Enum.reverse(1..3, [4, 5, 6]) == [3, 2, 1, 4, 5, 6]
  end

  test "reverse slice" do
    assert Enum.reverse_slice(1..6, 2, 0) == [1, 2, 3, 4, 5, 6]
    assert Enum.reverse_slice(1..6, 2, 2) == [1, 2, 4, 3, 5, 6]
    assert Enum.reverse_slice(1..6, 2, 4) == [1, 2, 6, 5, 4, 3]
    assert Enum.reverse_slice(1..6, 2, 10000000) == [1, 2, 6, 5, 4, 3]
    assert Enum.reverse_slice(1..6, 10000000, 4) == [1, 2, 3, 4, 5, 6]
    assert Enum.reverse_slice(1..6, 50, 50) == [1, 2, 3, 4, 5, 6]
  end

  test "random" do
    # corner cases, independent of the seed
    assert Enum.random(1..1) == 1

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407414, 139258}
    seed2 = {1306, 421106, 567597}
    :rand.seed(:exsplus, seed1)
    assert Enum.random(1..2) == 1
    assert Enum.random(1..3) == 2
    assert Enum.random(3..1) == 3
    :rand.seed(:exsplus, seed2)
    assert Enum.random(1..2) == 1
    assert Enum.random(1..3) == 3
  end

  test "take random" do
    # corner cases, independent of the seed
    assert_raise FunctionClauseError, fn -> Enum.take_random(1..2, -1) end
    assert Enum.take_random(1..1, 0) == []
    assert Enum.take_random(1..1, 2) == [1]
    assert Enum.take_random(1..2, 0) == []

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407414, 139258}
    seed2 = {1406, 421106, 567597}
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(1..3, 1) == [3]
    assert Enum.take_random(1..3, 2) == [3, 1]
    assert Enum.take_random(1..3, 3) == [3, 1, 2]
    assert Enum.take_random(1..3, 4) == [1, 3, 2]
    assert Enum.take_random(3..1, 1) == [2]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(1..3, 1) == [1]
    assert Enum.take_random(1..3, 2) == [1, 2]
    assert Enum.take_random(1..3, 3) == [1, 3, 2]
    assert Enum.take_random(1..3, 4) == [3, 2, 1]
  end

  test "scan" do
    assert Enum.scan(1..5, &(&1 + &2)) == [1, 3, 6, 10, 15]
    assert Enum.scan(1..5, 0, &(&1 + &2)) == [1, 3, 6, 10, 15]
  end

  test "shuffle" do
    # set a fixed seed so the test can be deterministic
    :rand.seed(:exsplus, {1374, 347975, 449264})
    assert Enum.shuffle(1..5) == [2, 1, 3, 5, 4]
  end

  test "slice" do
    assert Enum.slice(1..5, 0, 0) == []
    assert Enum.slice(1..5, 0, 1) == [1]
    assert Enum.slice(1..5, 0, 2) == [1, 2]
    assert Enum.slice(1..5, 1, 2) == [2, 3]
    assert Enum.slice(1..5, 1, 0) == []
    assert Enum.slice(1..5, 2, 5) == [3, 4, 5]
    assert Enum.slice(1..5, 2, 6) == [3, 4, 5]
    assert Enum.slice(1..5, 5, 5) == []
    assert Enum.slice(1..5, 6, 5) == []
    assert Enum.slice(1..5, 6, 0) == []
    assert Enum.slice(1..5, -6, 0) == []
    assert Enum.slice(1..5, -6, 5) == []
    assert Enum.slice(1..5, -2, 5) == [4, 5]
    assert Enum.slice(1..5, -3, 1) == [3]
  end

  test "slice range" do
    assert Enum.slice(1..5, 0..0) == [1]
    assert Enum.slice(1..5, 0..1) == [1, 2]
    assert Enum.slice(1..5, 0..2) == [1, 2, 3]
    assert Enum.slice(1..5, 1..2) == [2, 3]
    assert Enum.slice(1..5, 1..0) == []
    assert Enum.slice(1..5, 2..5) == [3, 4, 5]
    assert Enum.slice(1..5, 2..6) == [3, 4, 5]
    assert Enum.slice(1..5, 4..4) == [5]
    assert Enum.slice(1..5, 5..5) == []
    assert Enum.slice(1..5, 6..5) == []
    assert Enum.slice(1..5, 6..0) == []
    assert Enum.slice(1..5, -6..0) == []
    assert Enum.slice(1..5, -6..5) == []
    assert Enum.slice(1..5, -5..-1) == [1, 2, 3, 4, 5]
    assert Enum.slice(1..5, -5..-3) == [1, 2, 3]
    assert Enum.slice(1..5, -6..-1) == []
    assert Enum.slice(1..5, -6..-3) == []
  end

  test "sort" do
    assert Enum.sort(3..1) == [1, 2, 3]
    assert Enum.sort(2..1) == [1, 2]
    assert Enum.sort(1..1) == [1]

    assert Enum.sort(3..1, &(&1 > &2)) == [3, 2, 1]
    assert Enum.sort(2..1, &(&1 > &2)) == [2, 1]
    assert Enum.sort(1..1, &(&1 > &2)) == [1]
  end

  test "split" do
    range = 1..3
    assert Enum.split(range, 0) == {[], [1, 2, 3]}
    assert Enum.split(range, 1) == {[1], [2, 3]}
    assert Enum.split(range, 2) == {[1, 2], [3]}
    assert Enum.split(range, 3) == {[1, 2, 3], []}
    assert Enum.split(range, 4) == {[1, 2, 3], []}
    assert Enum.split(range, -1) == {[1, 2], [3]}
    assert Enum.split(range, -2) == {[1], [2, 3]}
    assert Enum.split(range, -3) == {[], [1, 2, 3]}
    assert Enum.split(range, -10) == {[], [1, 2, 3]}

    range = 1..0
    assert Enum.split(range, 3) == {[1, 0], []}
  end

  test "split while" do
    range = 1..3
    assert Enum.split_while(range, fn(_) -> false end) == {[], [1, 2, 3]}
    assert Enum.split_while(range, fn(_) -> nil end) == {[], [1, 2, 3]}
    assert Enum.split_while(range, fn(_) -> true end) == {[1, 2, 3], []}
    assert Enum.split_while(range, fn(x) -> x > 2 end) == {[], [1, 2, 3]}
    assert Enum.split_while(range, fn(x) -> x > 3 end) == {[], [1, 2, 3]}
    assert Enum.split_while(range, fn(x) -> x < 3 end) == {[1, 2], [3]}
    assert Enum.split_while(range, fn(x) -> x end) == {[1, 2, 3], []}

    range = 1..0
    assert Enum.split_while(range, fn(_) -> true end) == {[1, 0], []}
  end

  test "sum" do
    assert Enum.sum(1..1) == 1
    assert Enum.sum(1..3) == 6
  end

  test "take" do
    range = 1..3
    assert Enum.take(range, 0) == []
    assert Enum.take(range, 1) == [1]
    assert Enum.take(range, 2) == [1, 2]
    assert Enum.take(range, 3) == [1, 2, 3]
    assert Enum.take(range, 4) == [1, 2, 3]
    assert Enum.take(range, -1) == [3]
    assert Enum.take(range, -2) == [2, 3]
    assert Enum.take(range, -4) == [1, 2, 3]

    range = 1..0
    assert Enum.take(range, 3) == [1, 0]
  end

  test "take every" do
    assert Enum.take_every(1..10, 2) == [1, 3, 5, 7, 9]
    assert Enum.take_every(1..2, 2) == [1]
    assert Enum.take_every(1..3, 0) == []
    assert_raise FunctionClauseError, fn ->
      Enum.take_every(1..3, -1)
    end
  end

  test "take while" do
    range = 1..3
    assert Enum.take_while(range, fn(x) -> x > 3 end) == []
    assert Enum.take_while(range, fn(x) -> x <= 1 end) == [1]
    assert Enum.take_while(range, fn(x) -> x <= 3 end) == [1, 2, 3]
    assert Enum.take_while(range, fn(x) -> x end) == [1, 2, 3]
    assert Enum.take_while(range, fn(_) -> nil end) == []
    assert Enum.take_while([], fn(_) -> true end) == []
  end

  test "uniq" do
    assert Enum.uniq(1..3) == [1, 2, 3]
  end

  test "uniq by" do
    assert Enum.uniq_by(1..3, fn x -> x end) == [1, 2, 3]
  end

  test "zip" do
    assert Enum.zip([:a, :b], 1..2) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b], 1..4) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b, :c, :d], 1..2) == [{:a, 1}, {:b, 2}]

    assert Enum.zip(1..2, [:a, :b]) == [{1, :a}, {2, :b}]
    assert Enum.zip(1..4, [:a, :b]) == [{1, :a}, {2, :b}]
    assert Enum.zip(1..2, [:a, :b, :c, :d]) == [{1, :a}, {2, :b}]

    assert Enum.zip(1..2, 1..2) == [{1, 1}, {2, 2}]
    assert Enum.zip(1..4, 1..2) == [{1, 1}, {2, 2}]
    assert Enum.zip(1..2, 1..4) == [{1, 1}, {2, 2}]
  end

  test "with index" do
    assert Enum.with_index(1..3) == [{1, 0}, {2, 1}, {3, 2}]
  end
end

defmodule EnumTest.Map do
  # Some cases are inlined for ranges which means we need
  # to verify them using maps or mapsets.
  use ExUnit.Case, async: true

  test "take random" do
    # corner cases, independent of the seed
    assert_raise FunctionClauseError, fn -> Enum.take_random(1..2, -1) end
    assert Enum.take_random(%{a: 1}, 0) == []
    assert Enum.take_random(%{a: 1}, 2) == [a: 1]
    assert Enum.take_random(%{a: 1, b: 2}, 0) == []

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    map = %{a: 1, b: 2, c: 3}
    seed1 = {1406, 407414, 139258}
    seed2 = {1406, 421106, 567597}
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(map, 1) == [a: 1]
    assert Enum.take_random(map, 2) == [a: 1, c: 3]
    assert Enum.take_random(map, 3) == [b: 2, a: 1, c: 3]
    assert Enum.take_random(map, 4) == [c: 3, a: 1, b: 2]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(map, 1) == [c: 3]
    assert Enum.take_random(map, 2) == [a: 1, b: 2]
    assert Enum.take_random(map, 3) == [a: 1, b: 2, c: 3]
    assert Enum.take_random(map, 4) == [a: 1, b: 2, c: 3]
  end
end

defmodule EnumTest.SideEffects do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO
  import PathHelpers

  test "take with side effects" do
    stream = Stream.unfold(1, fn x -> IO.puts x; {x, x + 1} end)
    assert capture_io(fn ->
      Enum.take(stream, 1)
    end) == "1\n"
  end

  test "take does not consume next without a need" do
    path = tmp_path("oneliner.txt")
    File.mkdir(Path.dirname(path))

    try do
      File.write!(path, "ONE")

      File.open!(path, [], fn file ->
        iterator = IO.stream(file, :line)
        assert Enum.take(iterator, 1) == ["ONE"]
        assert Enum.take(iterator, 5) == []
      end)
    after
      File.rm(path)
    end
  end

  test "take with no item works as no-op" do
    iterator = File.stream!(fixture_path("unknown.txt"))

    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
  end
end
