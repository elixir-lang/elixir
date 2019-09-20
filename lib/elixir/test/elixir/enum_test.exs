Code.require_file("test_helper.exs", __DIR__)

defmodule EnumTest do
  use ExUnit.Case, async: true
  doctest Enum

  defp assert_runs_enumeration_only_once(enum_fun) do
    enumerator =
      Stream.map([:element], fn element ->
        send(self(), element)
        element
      end)

    enum_fun.(enumerator)
    assert_received :element
    refute_received :element
  end

  test "all?/2" do
    assert Enum.all?([2, 4, 6])
    refute Enum.all?([2, nil, 4])
    assert Enum.all?([])

    assert Enum.all?([2, 4, 6], fn x -> rem(x, 2) == 0 end)
    refute Enum.all?([2, 3, 4], fn x -> rem(x, 2) == 0 end)
  end

  test "any?/2" do
    refute Enum.any?([2, 4, 6], fn x -> rem(x, 2) == 1 end)
    assert Enum.any?([2, 3, 4], fn x -> rem(x, 2) == 1 end)

    refute Enum.any?([false, false, false])
    assert Enum.any?([false, true, false])

    assert Enum.any?([:foo, false, false])
    refute Enum.any?([false, nil, false])

    refute Enum.any?([])
  end

  test "at/3" do
    assert Enum.at([2, 4, 6], 0) == 2
    assert Enum.at([2, 4, 6], 2) == 6
    assert Enum.at([2, 4, 6], 4) == nil
    assert Enum.at([2, 4, 6], 4, :none) == :none
    assert Enum.at([2, 4, 6], -2) == 4
    assert Enum.at([2, 4, 6], -4) == nil
  end

  test "chunk_every/2" do
    assert Enum.chunk_every([1, 2, 3, 4, 5], 2) == [[1, 2], [3, 4], [5]]
  end

  test "chunk_every/4" do
    assert Enum.chunk_every([1, 2, 3, 4, 5], 2, 2, [6]) == [[1, 2], [3, 4], [5, 6]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, :discard) == [[1, 2, 3], [3, 4, 5]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6], 2, 3, :discard) == [[1, 2], [4, 5]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, []) == [[1, 2, 3], [3, 4, 5], [5, 6]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6], 3, 3, []) == [[1, 2, 3], [4, 5, 6]]
    assert Enum.chunk_every([1, 2, 3, 4, 5], 4, 4, 6..10) == [[1, 2, 3, 4], [5, 6, 7, 8]]
    assert Enum.chunk_every([1, 2, 3, 4, 5], 2, 3, []) == [[1, 2], [4, 5]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6], 2, 3, []) == [[1, 2], [4, 5]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6, 7], 2, 3, []) == [[1, 2], [4, 5], [7]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6, 7], 2, 3, [8]) == [[1, 2], [4, 5], [7, 8]]
    assert Enum.chunk_every([1, 2, 3, 4, 5, 6, 7], 2, 4, []) == [[1, 2], [5, 6]]
  end

  test "chunk_by/2" do
    assert Enum.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1)) ==
             [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

    assert Enum.chunk_by([1, 2, 3, 4], fn _ -> true end) == [[1, 2, 3, 4]]
    assert Enum.chunk_by([], fn _ -> true end) == []
    assert Enum.chunk_by([1], fn _ -> true end) == [[1]]
  end

  test "chunk_while/4" do
    chunk_fun = fn i, acc ->
      cond do
        i > 10 ->
          {:halt, acc}

        rem(i, 2) == 0 ->
          {:cont, Enum.reverse([i | acc]), []}

        true ->
          {:cont, [i | acc]}
      end
    end

    after_fun = fn
      [] -> {:cont, []}
      acc -> {:cont, Enum.reverse(acc), []}
    end

    assert Enum.chunk_while([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [], chunk_fun, after_fun) ==
             [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]

    assert Enum.chunk_while(0..9, [], chunk_fun, after_fun) ==
             [[0], [1, 2], [3, 4], [5, 6], [7, 8], [9]]

    assert Enum.chunk_while(0..10, [], chunk_fun, after_fun) ==
             [[0], [1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]

    assert Enum.chunk_while(0..11, [], chunk_fun, after_fun) ==
             [[0], [1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]

    assert Enum.chunk_while([5, 7, 9, 11], [], chunk_fun, after_fun) == [[5, 7, 9]]
  end

  test "concat/1" do
    assert Enum.concat([[1, [2], 3], [4], [5, 6]]) == [1, [2], 3, 4, 5, 6]

    assert Enum.concat([[], []]) == []
    assert Enum.concat([[]]) == []
    assert Enum.concat([]) == []
  end

  test "concat/2" do
    assert Enum.concat([], [1]) == [1]
    assert Enum.concat([1, [2], 3], [4, 5]) == [1, [2], 3, 4, 5]

    assert Enum.concat([1, 2], 3..5) == [1, 2, 3, 4, 5]

    assert Enum.concat([], []) == []
    assert Enum.concat([], 1..3) == [1, 2, 3]

    assert Enum.concat(fn acc, _ -> acc end, [1]) == [1]
  end

  test "count/1" do
    assert Enum.count([1, 2, 3]) == 3
    assert Enum.count([]) == 0
    assert Enum.count([1, true, false, nil]) == 4
  end

  test "count/2" do
    assert Enum.count([1, 2, 3], fn x -> rem(x, 2) == 0 end) == 1
    assert Enum.count([], fn x -> rem(x, 2) == 0 end) == 0
    assert Enum.count([1, true, false, nil], & &1) == 2
  end

  test "dedup/1" do
    assert Enum.dedup([1, 1, 2, 1, 1, 2, 1]) == [1, 2, 1, 2, 1]
    assert Enum.dedup([2, 1, 1, 2, 1]) == [2, 1, 2, 1]
    assert Enum.dedup([1, 2, 3, 4]) == [1, 2, 3, 4]
    assert Enum.dedup([1, 1.0, 2.0, 2]) == [1, 1.0, 2.0, 2]
    assert Enum.dedup([]) == []
    assert Enum.dedup([nil, nil, true, {:value, true}]) == [nil, true, {:value, true}]
    assert Enum.dedup([nil]) == [nil]
  end

  test "dedup_by/2" do
    assert Enum.dedup_by([{1, :x}, {2, :y}, {2, :z}, {1, :x}], fn {x, _} -> x end) ==
             [{1, :x}, {2, :y}, {1, :x}]

    assert Enum.dedup_by([5, 1, 2, 3, 2, 1], fn x -> x > 2 end) == [5, 1, 3, 2]
  end

  test "drop/2" do
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

  test "drop_every/2" do
    assert Enum.drop_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 2) == [2, 4, 6, 8, 10]
    assert Enum.drop_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 3) == [2, 3, 5, 6, 8, 9]
    assert Enum.drop_every([], 2) == []
    assert Enum.drop_every([1, 2], 2) == [2]
    assert Enum.drop_every([1, 2, 3], 0) == [1, 2, 3]

    assert_raise FunctionClauseError, fn ->
      Enum.drop_every([1, 2, 3], -1)
    end
  end

  test "drop_while/2" do
    assert Enum.drop_while([1, 2, 3, 4, 3, 2, 1], fn x -> x <= 3 end) == [4, 3, 2, 1]
    assert Enum.drop_while([1, 2, 3], fn _ -> false end) == [1, 2, 3]
    assert Enum.drop_while([1, 2, 3], fn x -> x <= 3 end) == []
    assert Enum.drop_while([], fn _ -> false end) == []
  end

  test "each/2" do
    try do
      assert Enum.each([], fn x -> x end) == :ok
      assert Enum.each([1, 2, 3], fn x -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == 6
    after
      Process.delete(:enum_test_each)
    end
  end

  test "empty?/1" do
    assert Enum.empty?([])
    refute Enum.empty?([1, 2, 3])
    refute Enum.empty?(1..3)
  end

  test "fetch/2" do
    assert Enum.fetch([66], 0) == {:ok, 66}
    assert Enum.fetch([66], -1) == {:ok, 66}
    assert Enum.fetch([66], 1) == :error
    assert Enum.fetch([66], -2) == :error

    assert Enum.fetch([2, 4, 6], 0) == {:ok, 2}
    assert Enum.fetch([2, 4, 6], -1) == {:ok, 6}
    assert Enum.fetch([2, 4, 6], 2) == {:ok, 6}
    assert Enum.fetch([2, 4, 6], 4) == :error
    assert Enum.fetch([2, 4, 6], -2) == {:ok, 4}
    assert Enum.fetch([2, 4, 6], -4) == :error

    assert Enum.fetch([], 0) == :error
    assert Enum.fetch([], 1) == :error
  end

  test "fetch!/2" do
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

  test "filter/2" do
    assert Enum.filter([1, 2, 3], fn x -> rem(x, 2) == 0 end) == [2]
    assert Enum.filter([2, 4, 6], fn x -> rem(x, 2) == 0 end) == [2, 4, 6]

    assert Enum.filter([1, 2, false, 3, nil], & &1) == [1, 2, 3]
    assert Enum.filter([1, 2, 3], &match?(1, &1)) == [1]
    assert Enum.filter([1, 2, 3], &match?(x when x < 3, &1)) == [1, 2]
    assert Enum.filter([1, 2, 3], fn _ -> true end) == [1, 2, 3]
  end

  test "find/3" do
    assert Enum.find([2, 4, 6], fn x -> rem(x, 2) == 1 end) == nil
    assert Enum.find([2, 4, 6], 0, fn x -> rem(x, 2) == 1 end) == 0
    assert Enum.find([2, 3, 4], fn x -> rem(x, 2) == 1 end) == 3
  end

  test "find_index/2" do
    assert Enum.find_index([2, 4, 6], fn x -> rem(x, 2) == 1 end) == nil
    assert Enum.find_index([2, 3, 4], fn x -> rem(x, 2) == 1 end) == 1
    assert Stream.take(1..3, 3) |> Enum.find_index(fn _ -> false end) == nil
    assert Stream.take(1..6, 6) |> Enum.find_index(fn x -> x == 5 end) == 4
  end

  test "find_value/2" do
    assert Enum.find_value([2, 4, 6], fn x -> rem(x, 2) == 1 end) == nil
    assert Enum.find_value([2, 4, 6], 0, fn x -> rem(x, 2) == 1 end) == 0
    assert Enum.find_value([2, 3, 4], fn x -> rem(x, 2) == 1 end)
  end

  test "flat_map/2" do
    assert Enum.flat_map([], fn x -> [x, x] end) == []
    assert Enum.flat_map([1, 2, 3], fn x -> [x, x] end) == [1, 1, 2, 2, 3, 3]
    assert Enum.flat_map([1, 2, 3], fn x -> x..(x + 1) end) == [1, 2, 2, 3, 3, 4]
  end

  test "flat_map_reduce/3" do
    assert Enum.flat_map_reduce([1, 2, 3], 0, &{[&1, &2], &1 + &2}) == {[1, 0, 2, 1, 3, 3], 6}
  end

  test "group_by/3" do
    assert Enum.group_by([], fn _ -> raise "oops" end) == %{}
    assert Enum.group_by([1, 2, 3], &rem(&1, 2)) == %{0 => [2], 1 => [1, 3]}
  end

  test "intersperse/2" do
    assert Enum.intersperse([], true) == []
    assert Enum.intersperse([1], true) == [1]
    assert Enum.intersperse([1, 2, 3], true) == [1, true, 2, true, 3]
  end

  test "into/2" do
    assert Enum.into([a: 1, b: 2], %{}) == %{a: 1, b: 2}
    assert Enum.into([a: 1, b: 2], %{c: 3}) == %{a: 1, b: 2, c: 3}
    assert Enum.into(%{a: 1, b: 2}, []) == [a: 1, b: 2]
    assert Enum.into(1..3, []) == [1, 2, 3]
    assert Enum.into(["H", "i"], "") == "Hi"
  end

  test "into/3" do
    assert Enum.into([1, 2, 3], [], fn x -> x * 2 end) == [2, 4, 6]
    assert Enum.into([1, 2, 3], "numbers: ", &to_string/1) == "numbers: 123"

    assert_raise FunctionClauseError, fn ->
      Enum.into([2, 3], %{a: 1}, & &1)
    end
  end

  test "join/2" do
    assert Enum.join([], " = ") == ""
    assert Enum.join([1, 2, 3], " = ") == "1 = 2 = 3"
    assert Enum.join([1, "2", 3], " = ") == "1 = 2 = 3"
    assert Enum.join([1, 2, 3]) == "123"
    assert Enum.join(["", "", 1, 2, "", 3, "", "\n"], ";") == ";;1;2;;3;;\n"
    assert Enum.join([""]) == ""

    assert Enum.join(fn acc, _ -> acc end, ".") == ""
  end

  test "map/2" do
    assert Enum.map([], fn x -> x * 2 end) == []
    assert Enum.map([1, 2, 3], fn x -> x * 2 end) == [2, 4, 6]
  end

  test "map_every/3" do
    assert Enum.map_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 2, fn x -> x * 2 end) ==
             [2, 2, 6, 4, 10, 6, 14, 8, 18, 10]

    assert Enum.map_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 3, fn x -> x * 2 end) ==
             [2, 2, 3, 8, 5, 6, 14, 8, 9, 20]

    assert Enum.map_every([], 2, fn x -> x * 2 end) == []
    assert Enum.map_every([1, 2], 2, fn x -> x * 2 end) == [2, 2]

    assert Enum.map_every([1, 2, 3], 0, fn _x -> raise :i_should_have_never_been_invoked end) ==
             [1, 2, 3]

    assert Enum.map_every(1..3, 1, fn x -> x * 2 end) == [2, 4, 6]

    assert_raise FunctionClauseError, fn ->
      Enum.map_every([1, 2, 3], -1, fn x -> x * 2 end)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.map_every(1..10, 3.33, fn x -> x * 2 end)
    end

    assert Enum.map_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 9, fn x -> x + 1000 end) ==
             [1001, 2, 3, 4, 5, 6, 7, 8, 9, 1010]

    assert Enum.map_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 10, fn x -> x + 1000 end) ==
             [1001, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    assert Enum.map_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 100, fn x -> x + 1000 end) ==
             [1001, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  end

  test "map_join/3" do
    assert Enum.map_join([], " = ", &(&1 * 2)) == ""
    assert Enum.map_join([1, 2, 3], " = ", &(&1 * 2)) == "2 = 4 = 6"
    assert Enum.map_join([1, 2, 3], &(&1 * 2)) == "246"
    assert Enum.map_join(["", "", 1, 2, "", 3, "", "\n"], ";", & &1) == ";;1;2;;3;;\n"
    assert Enum.map_join([""], "", & &1) == ""
    assert Enum.map_join(fn acc, _ -> acc end, ".", &(&1 + 0)) == ""
  end

  test "map_reduce/3" do
    assert Enum.map_reduce([], 1, fn x, acc -> {x * 2, x + acc} end) == {[], 1}
    assert Enum.map_reduce([1, 2, 3], 1, fn x, acc -> {x * 2, x + acc} end) == {[2, 4, 6], 7}
  end

  test "max/1" do
    assert Enum.max([1]) == 1
    assert Enum.max([1, 2, 3]) == 3
    assert Enum.max([1, [], :a, {}]) == []

    assert Enum.max([1, 1.0]) === 1
    assert Enum.max([1.0, 1]) === 1.0

    assert_raise Enum.EmptyError, fn ->
      Enum.max([])
    end
  end

  test "max/2" do
    assert Enum.max([1], fn -> nil end) == 1
    assert Enum.max([1, 2, 3], fn -> nil end) == 3
    assert Enum.max([1, [], :a, {}], fn -> nil end) == []
    assert Enum.max([], fn -> :empty_value end) == :empty_value
    assert Enum.max(%{}, fn -> :empty_value end) == :empty_value
    assert_runs_enumeration_only_once(&Enum.max(&1, fn -> nil end))
  end

  test "max_by/2" do
    assert Enum.max_by(["a", "aa", "aaa"], fn x -> String.length(x) end) == "aaa"

    assert Enum.max_by([1, 1.0], & &1) === 1
    assert Enum.max_by([1.0, 1], & &1) === 1.0

    assert_raise Enum.EmptyError, fn ->
      Enum.max_by([], fn x -> String.length(x) end)
    end

    assert_raise Enum.EmptyError, fn ->
      Enum.max_by(%{}, & &1)
    end
  end

  test "max_by/3" do
    assert Enum.max_by(["a", "aa", "aaa"], fn x -> String.length(x) end, fn -> nil end) == "aaa"
    assert Enum.max_by([], fn x -> String.length(x) end, fn -> :empty_value end) == :empty_value
    assert Enum.max_by(%{}, & &1, fn -> :empty_value end) == :empty_value
    assert Enum.max_by(%{}, & &1, fn -> {:a, :tuple} end) == {:a, :tuple}
    assert_runs_enumeration_only_once(&Enum.max_by(&1, fn e -> e end, fn -> nil end))
  end

  test "member?/2" do
    assert Enum.member?([1, 2, 3], 2)
    refute Enum.member?([], 0)
    refute Enum.member?([1, 2, 3], 0)
  end

  test "min/1" do
    assert Enum.min([1]) == 1
    assert Enum.min([1, 2, 3]) == 1
    assert Enum.min([[], :a, {}]) == :a

    assert Enum.min([1, 1.0]) === 1
    assert Enum.min([1.0, 1]) === 1.0

    assert_raise Enum.EmptyError, fn ->
      Enum.min([])
    end
  end

  test "min/2" do
    assert Enum.min([1], fn -> nil end) == 1
    assert Enum.min([1, 2, 3], fn -> nil end) == 1
    assert Enum.min([[], :a, {}], fn -> nil end) == :a
    assert Enum.min([], fn -> :empty_value end) == :empty_value
    assert Enum.min(%{}, fn -> :empty_value end) == :empty_value
    assert_runs_enumeration_only_once(&Enum.min(&1, fn -> nil end))
  end

  test "min_by/2" do
    assert Enum.min_by(["a", "aa", "aaa"], fn x -> String.length(x) end) == "a"

    assert Enum.min_by([1, 1.0], & &1) === 1
    assert Enum.min_by([1.0, 1], & &1) === 1.0

    assert_raise Enum.EmptyError, fn ->
      Enum.min_by([], fn x -> String.length(x) end)
    end

    assert_raise Enum.EmptyError, fn ->
      Enum.min_by(%{}, & &1)
    end
  end

  test "min_by/3" do
    assert Enum.min_by(["a", "aa", "aaa"], fn x -> String.length(x) end, fn -> nil end) == "a"
    assert Enum.min_by([], fn x -> String.length(x) end, fn -> :empty_value end) == :empty_value
    assert Enum.min_by(%{}, & &1, fn -> :empty_value end) == :empty_value
    assert Enum.min_by(%{}, & &1, fn -> {:a, :tuple} end) == {:a, :tuple}
    assert_runs_enumeration_only_once(&Enum.min_by(&1, fn e -> e end, fn -> nil end))
  end

  test "min_max/1" do
    assert Enum.min_max([1]) == {1, 1}
    assert Enum.min_max([2, 3, 1]) == {1, 3}
    assert Enum.min_max([[], :a, {}]) == {:a, []}

    assert Enum.min_max([1, 1.0]) === {1, 1}
    assert Enum.min_max([1.0, 1]) === {1.0, 1.0}

    assert_raise Enum.EmptyError, fn ->
      Enum.min_max([])
    end
  end

  test "min_max/2" do
    assert Enum.min_max([1], fn -> nil end) == {1, 1}
    assert Enum.min_max([2, 3, 1], fn -> nil end) == {1, 3}
    assert Enum.min_max([[], :a, {}], fn -> nil end) == {:a, []}
    assert Enum.min_max([], fn -> {:empty_min, :empty_max} end) == {:empty_min, :empty_max}
    assert Enum.min_max(%{}, fn -> {:empty_min, :empty_max} end) == {:empty_min, :empty_max}
    assert_runs_enumeration_only_once(&Enum.min_max(&1, fn -> nil end))
  end

  test "min_max_by/2" do
    assert Enum.min_max_by(["aaa", "a", "aa"], fn x -> String.length(x) end) == {"a", "aaa"}

    assert Enum.min_max_by([1, 1.0], & &1) === {1, 1}
    assert Enum.min_max_by([1.0, 1], & &1) === {1.0, 1.0}

    assert_raise Enum.EmptyError, fn ->
      Enum.min_max_by([], fn x -> String.length(x) end)
    end
  end

  test "min_max_by/3" do
    assert Enum.min_max_by(["aaa", "a", "aa"], fn x -> String.length(x) end, fn -> nil end) ==
             {"a", "aaa"}

    assert Enum.min_max_by([], fn x -> String.length(x) end, fn -> {:no_min, :no_max} end) ==
             {:no_min, :no_max}

    assert Enum.min_max_by(%{}, fn x -> String.length(x) end, fn -> {:no_min, :no_max} end) ==
             {:no_min, :no_max}

    assert_runs_enumeration_only_once(&Enum.min_max_by(&1, fn x -> x end, fn -> nil end))
  end

  test "split_with/2" do
    assert Enum.split_with([], fn x -> rem(x, 2) == 0 end) == {[], []}
    assert Enum.split_with([1, 2, 3], fn x -> rem(x, 2) == 0 end) == {[2], [1, 3]}
    assert Enum.split_with([2, 4, 6], fn x -> rem(x, 2) == 0 end) == {[2, 4, 6], []}

    assert Enum.split_with(1..5, fn x -> rem(x, 2) == 0 end) == {[2, 4], [1, 3, 5]}
    assert Enum.split_with(-3..0, fn x -> x > 0 end) == {[], [-3, -2, -1, 0]}

    assert Enum.split_with(%{}, fn x -> rem(x, 2) == 0 end) == {[], []}

    assert Enum.split_with(%{a: 1, b: 2, c: 3}, fn {_k, v} -> rem(v, 2) == 0 end) ==
             {[b: 2], [a: 1, c: 3]}

    assert Enum.split_with(%{b: 2, d: 4, f: 6}, fn {_k, v} -> rem(v, 2) == 0 end) ==
             {[b: 2, d: 4, f: 6], []}
  end

  test "random/1" do
    # corner cases, independent of the seed
    assert_raise Enum.EmptyError, fn -> Enum.random([]) end
    assert Enum.random([1]) == 1

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407_414, 139_258}
    seed2 = {1306, 421_106, 567_597}
    :rand.seed(:exsplus, seed1)
    assert Enum.random([1, 2]) == 1
    assert Enum.random([1, 2]) == 2
    :rand.seed(:exsplus, seed1)
    assert Enum.random([1, 2]) == 1
    assert Enum.random([1, 2, 3]) == 2
    assert Enum.random([1, 2, 3, 4]) == 1
    assert Enum.random([1, 2, 3, 4, 5]) == 2
    :rand.seed(:exsplus, seed2)
    assert Enum.random([1, 2]) == 1
    assert Enum.random([1, 2, 3]) == 3
    assert Enum.random([1, 2, 3, 4]) == 2
    assert Enum.random([1, 2, 3, 4, 5]) == 4
  end

  test "reduce/2" do
    assert Enum.reduce([1, 2, 3], fn x, acc -> x + acc end) == 6

    assert_raise Enum.EmptyError, fn ->
      Enum.reduce([], fn x, acc -> x + acc end)
    end

    assert_raise Enum.EmptyError, fn ->
      Enum.reduce(%{}, fn _, acc -> acc end)
    end
  end

  test "reduce/3" do
    assert Enum.reduce([], 1, fn x, acc -> x + acc end) == 1
    assert Enum.reduce([1, 2, 3], 1, fn x, acc -> x + acc end) == 7
  end

  test "reduce_while/3" do
    assert Enum.reduce_while([1, 2, 3], 1, fn i, acc -> {:cont, acc + i} end) == 7
    assert Enum.reduce_while([1, 2, 3], 1, fn _i, acc -> {:halt, acc} end) == 1
    assert Enum.reduce_while([], 0, fn _i, acc -> {:cont, acc} end) == 0
  end

  test "reject/2" do
    assert Enum.reject([1, 2, 3], fn x -> rem(x, 2) == 0 end) == [1, 3]
    assert Enum.reject([2, 4, 6], fn x -> rem(x, 2) == 0 end) == []
    assert Enum.reject([1, true, nil, false, 2], & &1) == [nil, false]
  end

  test "reverse/1" do
    assert Enum.reverse([]) == []
    assert Enum.reverse([1, 2, 3]) == [3, 2, 1]
    assert Enum.reverse([5..5]) == [5..5]
  end

  test "reverse/2" do
    assert Enum.reverse([1, 2, 3], [4, 5, 6]) == [3, 2, 1, 4, 5, 6]
    assert Enum.reverse([1, 2, 3], []) == [3, 2, 1]
    assert Enum.reverse([5..5], [5]) == [5..5, 5]
  end

  test "reverse_slice/3" do
    assert Enum.reverse_slice([], 1, 2) == []
    assert Enum.reverse_slice([1, 2, 3], 0, 0) == [1, 2, 3]
    assert Enum.reverse_slice([1, 2, 3], 0, 1) == [1, 2, 3]
    assert Enum.reverse_slice([1, 2, 3], 0, 2) == [2, 1, 3]
    assert Enum.reverse_slice([1, 2, 3], 0, 20_000_000) == [3, 2, 1]
    assert Enum.reverse_slice([1, 2, 3], 100, 2) == [1, 2, 3]
    assert Enum.reverse_slice([1, 2, 3], 10, 10) == [1, 2, 3]
  end

  test "scan/2" do
    assert Enum.scan([1, 2, 3, 4, 5], &(&1 + &2)) == [1, 3, 6, 10, 15]
    assert Enum.scan([], &(&1 + &2)) == []
  end

  test "scan/3" do
    assert Enum.scan([1, 2, 3, 4, 5], 0, &(&1 + &2)) == [1, 3, 6, 10, 15]
    assert Enum.scan([], 0, &(&1 + &2)) == []
  end

  test "shuffle/1" do
    # set a fixed seed so the test can be deterministic
    :rand.seed(:exsplus, {1374, 347_975, 449_264})
    assert Enum.shuffle([1, 2, 3, 4, 5]) == [2, 1, 3, 5, 4]
  end

  test "slice/2" do
    list = [1, 2, 3, 4, 5]
    assert Enum.slice(list, 0..0) == [1]
    assert Enum.slice(list, 0..1) == [1, 2]
    assert Enum.slice(list, 0..2) == [1, 2, 3]
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

  test "slice/3" do
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

  test "sort/1" do
    assert Enum.sort([5, 3, 2, 4, 1]) == [1, 2, 3, 4, 5]
  end

  test "sort/2" do
    assert Enum.sort([5, 3, 2, 4, 1], &(&1 > &2)) == [5, 4, 3, 2, 1]
  end

  test "sort_by/3" do
    collection = [
      [other_data: 1, sorted_data: 5],
      [other_data: 3, sorted_data: 4],
      [other_data: 4, sorted_data: 3],
      [other_data: 2, sorted_data: 2],
      [other_data: 5, sorted_data: 1]
    ]

    assert Enum.sort_by(collection, & &1[:sorted_data]) == [
             [other_data: 5, sorted_data: 1],
             [other_data: 2, sorted_data: 2],
             [other_data: 4, sorted_data: 3],
             [other_data: 3, sorted_data: 4],
             [other_data: 1, sorted_data: 5]
           ]

    assert Enum.sort_by(collection, & &1[:sorted_data], &>=/2) == collection
  end

  test "split/2" do
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

  test "split_while/2" do
    assert Enum.split_while([1, 2, 3], fn _ -> false end) == {[], [1, 2, 3]}
    assert Enum.split_while([1, 2, 3], fn _ -> true end) == {[1, 2, 3], []}
    assert Enum.split_while([1, 2, 3], fn x -> x > 2 end) == {[], [1, 2, 3]}
    assert Enum.split_while([1, 2, 3], fn x -> x > 3 end) == {[], [1, 2, 3]}
    assert Enum.split_while([1, 2, 3], fn x -> x < 3 end) == {[1, 2], [3]}
    assert Enum.split_while([], fn _ -> true end) == {[], []}
  end

  test "sum/1" do
    assert Enum.sum([]) == 0
    assert Enum.sum([1]) == 1
    assert Enum.sum([1, 2, 3]) == 6
    assert Enum.sum([1.1, 2.2, 3.3]) == 6.6
    assert Enum.sum([-3, -2, -1, 0, 1, 2, 3]) == 0
    assert Enum.sum(42..42) == 42
    assert Enum.sum(11..17) == 98
    assert Enum.sum(17..11) == 98
    assert Enum.sum(11..-17) == Enum.sum(-17..11)

    assert_raise ArithmeticError, fn ->
      Enum.sum([{}])
    end

    assert_raise ArithmeticError, fn ->
      Enum.sum([1, {}])
    end
  end

  test "take/2" do
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

  test "take_every/2" do
    assert Enum.take_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 2) == [1, 3, 5, 7, 9]
    assert Enum.take_every([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 3) == [1, 4, 7, 10]
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

  test "take_random/2" do
    assert Enum.take_random(-42..-42, 1) == [-42]

    # corner cases, independent of the seed
    assert_raise FunctionClauseError, fn -> Enum.take_random([1, 2], -1) end
    assert Enum.take_random([], 0) == []
    assert Enum.take_random([], 3) == []
    assert Enum.take_random([1], 0) == []
    assert Enum.take_random([1], 2) == [1]
    assert Enum.take_random([1, 2], 0) == []

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407_414, 139_258}
    seed2 = {1406, 421_106, 567_597}
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random([1, 2, 3], 1) == [2]
    assert Enum.take_random([1, 2, 3], 2) == [3, 1]
    assert Enum.take_random([1, 2, 3], 3) == [1, 3, 2]
    assert Enum.take_random([1, 2, 3], 4) == [2, 3, 1]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random([1, 2, 3], 1) == [3]
    assert Enum.take_random([1, 2, 3], 2) == [1, 2]
    assert Enum.take_random([1, 2, 3], 3) == [1, 2, 3]
    assert Enum.take_random([1, 2, 3], 4) == [2, 1, 3]
    assert Enum.take_random([1, 2, 3], 129) == [3, 2, 1]

    # assert that every item in the sample comes from the input list
    list = for _ <- 1..100, do: make_ref()

    for x <- Enum.take_random(list, 50) do
      assert x in list
    end

    assert_raise FunctionClauseError, fn ->
      Enum.take_random(1..10, -1)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.take_random(1..10, 10.0)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.take_random(1..10, 128.1)
    end
  end

  test "take_while/2" do
    assert Enum.take_while([1, 2, 3], fn x -> x > 3 end) == []
    assert Enum.take_while([1, 2, 3], fn x -> x <= 1 end) == [1]
    assert Enum.take_while([1, 2, 3], fn x -> x <= 3 end) == [1, 2, 3]
    assert Enum.take_while([], fn _ -> true end) == []
  end

  test "to_list/1" do
    assert Enum.to_list([]) == []
  end

  test "uniq/1" do
    assert Enum.uniq([5, 1, 2, 3, 2, 1]) == [5, 1, 2, 3]
  end

  test "uniq_by/2" do
    assert Enum.uniq_by([1, 2, 3, 2, 1], fn x -> x end) == [1, 2, 3]
  end

  test "unzip/1" do
    assert Enum.unzip([{:a, 1}, {:b, 2}, {:c, 3}]) == {[:a, :b, :c], [1, 2, 3]}
    assert Enum.unzip([]) == {[], []}
    assert Enum.unzip(%{a: 1, b: 2}) == {[:a, :b], [1, 2]}
    assert Enum.unzip(foo: "a", bar: "b") == {[:foo, :bar], ["a", "b"]}

    assert_raise FunctionClauseError, fn -> Enum.unzip([{:a, 1}, {:b, 2, "foo"}]) end
    assert_raise FunctionClauseError, fn -> Enum.unzip([{1, 2, {3, {4, 5}}}]) end
    assert_raise FunctionClauseError, fn -> Enum.unzip([1, 2, 3]) end
  end

  test "with_index/2" do
    assert Enum.with_index([]) == []
    assert Enum.with_index([1, 2, 3]) == [{1, 0}, {2, 1}, {3, 2}]
    assert Enum.with_index([1, 2, 3], 10) == [{1, 10}, {2, 11}, {3, 12}]
  end

  test "zip/2" do
    assert Enum.zip([:a, :b], [1, 2]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b], [1, 2, 3, 4]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b, :c, :d], [1, 2]) == [{:a, 1}, {:b, 2}]

    assert Enum.zip([], [1]) == []
    assert Enum.zip([1], []) == []
    assert Enum.zip([], []) == []
  end

  test "zip/1" do
    assert Enum.zip([[:a, :b], [1, 2], ["foo", "bar"]]) == [{:a, 1, "foo"}, {:b, 2, "bar"}]

    assert Enum.zip([[:a, :b], [1, 2, 3, 4], ["foo", "bar", "baz", "qux"]]) ==
             [{:a, 1, "foo"}, {:b, 2, "bar"}]

    assert Enum.zip([[:a, :b, :c, :d], [1, 2], ["foo", "bar", "baz", "qux"]]) ==
             [{:a, 1, "foo"}, {:b, 2, "bar"}]

    assert Enum.zip([[:a, :b, :c, :d], [1, 2, 3, 4], ["foo", "bar"]]) ==
             [{:a, 1, "foo"}, {:b, 2, "bar"}]

    assert Enum.zip([1..10, ["foo", "bar"]]) == [{1, "foo"}, {2, "bar"}]

    assert Enum.zip([]) == []
    assert Enum.zip([[]]) == []
    assert Enum.zip([[1]]) == [{1}]

    assert Enum.zip([[], [], [], []]) == []
    assert Enum.zip(%{}) == []
  end
end

defmodule EnumTest.Range do
  # Ranges use custom callbacks for protocols in many operations.
  use ExUnit.Case, async: true

  test "all?/2" do
    assert Enum.all?(0..1)
    assert Enum.all?(1..0)
    refute Enum.all?(0..5, fn x -> rem(x, 2) == 0 end)
    assert Enum.all?(0..1, fn x -> x < 2 end)
  end

  test "any?/2" do
    assert Enum.any?(1..0)
    refute Enum.any?(0..5, &(&1 > 10))
    assert Enum.any?(0..5, &(&1 > 3))
  end

  test "at/3" do
    assert Enum.at(2..6, 0) == 2
    assert Enum.at(2..6, 4) == 6
    assert Enum.at(2..6, 6) == nil
    assert Enum.at(2..6, 6, :none) == :none
    assert Enum.at(2..6, -2) == 5
    assert Enum.at(2..6, -8) == nil
  end

  test "chunk_every/2" do
    assert Enum.chunk_every(1..5, 2) == [[1, 2], [3, 4], [5]]
  end

  test "chunk_every/4" do
    assert Enum.chunk_every(1..5, 2, 2) == [[1, 2], [3, 4], [5]]
    assert Enum.chunk_every(1..6, 3, 2, :discard) == [[1, 2, 3], [3, 4, 5]]
    assert Enum.chunk_every(1..6, 2, 3, :discard) == [[1, 2], [4, 5]]
    assert Enum.chunk_every(1..6, 3, 2, []) == [[1, 2, 3], [3, 4, 5], [5, 6]]
    assert Enum.chunk_every(1..5, 4, 4, 6..10) == [[1, 2, 3, 4], [5, 6, 7, 8]]
  end

  test "chunk_by/2" do
    assert Enum.chunk_by(1..4, fn _ -> true end) == [[1, 2, 3, 4]]
    assert Enum.chunk_by(1..4, &(rem(&1, 2) == 1)) == [[1], [2], [3], [4]]
  end

  test "concat/1" do
    assert Enum.concat([1..2, 4..6]) == [1, 2, 4, 5, 6]
    assert Enum.concat([1..5, fn acc, _ -> acc end, [1]]) == [1, 2, 3, 4, 5, 1]
  end

  test "concat/2" do
    assert Enum.concat(1..3, 4..5) == [1, 2, 3, 4, 5]
    assert Enum.concat(1..3, [4, 5]) == [1, 2, 3, 4, 5]
    assert Enum.concat(1..3, []) == [1, 2, 3]
    assert Enum.concat(1..3, 0..0) == [1, 2, 3, 0]
  end

  test "count/1" do
    assert Enum.count(1..5) == 5
    assert Enum.count(1..1) == 1
  end

  test "count/2" do
    assert Enum.count(1..5, fn x -> rem(x, 2) == 0 end) == 2
    assert Enum.count(1..1, fn x -> rem(x, 2) == 0 end) == 0
  end

  test "dedup/1" do
    assert Enum.dedup(1..3) == [1, 2, 3]
  end

  test "dedup_by/2" do
    assert Enum.dedup_by(1..3, fn _ -> 1 end) == [1]
  end

  test "drop/2" do
    assert Enum.drop(1..3, 0) == [1, 2, 3]
    assert Enum.drop(1..3, 1) == [2, 3]
    assert Enum.drop(1..3, 2) == [3]
    assert Enum.drop(1..3, 3) == []
    assert Enum.drop(1..3, 4) == []
    assert Enum.drop(1..3, -1) == [1, 2]
    assert Enum.drop(1..3, -2) == [1]
    assert Enum.drop(1..3, -4) == []
    assert Enum.drop(1..0, 3) == []
  end

  test "drop_every/2" do
    assert Enum.drop_every(1..10, 2) == [2, 4, 6, 8, 10]
    assert Enum.drop_every(1..10, 3) == [2, 3, 5, 6, 8, 9]
    assert Enum.drop_every(0..0, 2) == []
    assert Enum.drop_every(1..2, 2) == [2]
    assert Enum.drop_every(1..3, 0) == [1, 2, 3]
    assert Enum.drop_every(1..3, 1) == []

    assert_raise FunctionClauseError, fn ->
      Enum.drop_every(1..10, 3.33)
    end
  end

  test "drop_while/2" do
    assert Enum.drop_while(0..6, fn x -> x <= 3 end) == [4, 5, 6]
    assert Enum.drop_while(0..6, fn _ -> false end) == [0, 1, 2, 3, 4, 5, 6]
    assert Enum.drop_while(0..3, fn x -> x <= 3 end) == []
    assert Enum.drop_while(1..0, fn _ -> nil end) == [1, 0]
  end

  test "each/2" do
    try do
      assert Enum.each(1..0, fn x -> x end) == :ok
      assert Enum.each(1..3, fn x -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == 6
    after
      Process.delete(:enum_test_each)
    end

    try do
      assert Enum.each(-1..-3, fn x -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == -6
    after
      Process.delete(:enum_test_each)
    end
  end

  test "empty?/1" do
    refute Enum.empty?(1..0)
    refute Enum.empty?(1..2)
  end

  test "fetch/2" do
    # ascending order
    assert Enum.fetch(-10..20, 4) == {:ok, -6}
    assert Enum.fetch(-10..20, -4) == {:ok, 17}
    # ascending order, first
    assert Enum.fetch(-10..20, 0) == {:ok, -10}
    assert Enum.fetch(-10..20, -31) == {:ok, -10}
    # ascending order, last
    assert Enum.fetch(-10..20, -1) == {:ok, 20}
    assert Enum.fetch(-10..20, 30) == {:ok, 20}
    # ascending order, out of bound
    assert Enum.fetch(-10..20, 31) == :error
    assert Enum.fetch(-10..20, -32) == :error

    # descending order
    assert Enum.fetch(20..-10, 4) == {:ok, 16}
    assert Enum.fetch(20..-10, -4) == {:ok, -7}
    # descending order, first
    assert Enum.fetch(20..-10, 0) == {:ok, 20}
    assert Enum.fetch(20..-10, -31) == {:ok, 20}
    # descending order, last
    assert Enum.fetch(20..-10, -1) == {:ok, -10}
    assert Enum.fetch(20..-10, 30) == {:ok, -10}
    # descending order, out of bound
    assert Enum.fetch(20..-10, 31) == :error
    assert Enum.fetch(20..-10, -32) == :error

    # edge cases
    assert Enum.fetch(42..42, 0) == {:ok, 42}
    assert Enum.fetch(42..42, -1) == {:ok, 42}
    assert Enum.fetch(42..42, 2) == :error
    assert Enum.fetch(42..42, -2) == :error
  end

  test "fetch!/2" do
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

  test "filter/2" do
    assert Enum.filter(1..3, fn x -> rem(x, 2) == 0 end) == [2]
    assert Enum.filter(1..6, fn x -> rem(x, 2) == 0 end) == [2, 4, 6]

    assert Enum.filter(1..3, &match?(1, &1)) == [1]
    assert Enum.filter(1..3, &match?(x when x < 3, &1)) == [1, 2]
    assert Enum.filter(1..3, fn _ -> true end) == [1, 2, 3]
  end

  test "find/3" do
    assert Enum.find(2..6, fn x -> rem(x, 2) == 0 end) == 2
    assert Enum.find(2..6, fn x -> rem(x, 2) == 1 end) == 3
    assert Enum.find(2..6, fn _ -> false end) == nil
    assert Enum.find(2..6, 0, fn _ -> false end) == 0
  end

  test "find_index/2" do
    assert Enum.find_index(2..6, fn x -> rem(x, 2) == 1 end) == 1
  end

  test "find_value/3" do
    assert Enum.find_value(2..6, fn x -> rem(x, 2) == 1 end)
  end

  test "flat_map/2" do
    assert Enum.flat_map(1..3, fn x -> [x, x] end) == [1, 1, 2, 2, 3, 3]
  end

  test "flat_map_reduce/3" do
    assert Enum.flat_map_reduce(1..100, 0, fn i, acc ->
             if acc < 3, do: {[i], acc + 1}, else: {:halt, acc}
           end) == {[1, 2, 3], 3}
  end

  test "group_by/3" do
    assert Enum.group_by(1..6, &rem(&1, 3)) == %{0 => [3, 6], 1 => [1, 4], 2 => [2, 5]}

    assert Enum.group_by(1..6, &rem(&1, 3), &(&1 * 2)) ==
             %{0 => [6, 12], 1 => [2, 8], 2 => [4, 10]}
  end

  test "intersperse/2" do
    assert Enum.intersperse(1..0, true) == [1, true, 0]
    assert Enum.intersperse(1..3, false) == [1, false, 2, false, 3]
  end

  test "into/2" do
    assert Enum.into(1..5, []) == [1, 2, 3, 4, 5]
  end

  test "into/3" do
    assert Enum.into(1..5, [], fn x -> x * 2 end) == [2, 4, 6, 8, 10]
    assert Enum.into(1..3, "numbers: ", &to_string/1) == "numbers: 123"
  end

  test "join/2" do
    assert Enum.join(1..0, " = ") == "1 = 0"
    assert Enum.join(1..3, " = ") == "1 = 2 = 3"
    assert Enum.join(1..3) == "123"
  end

  test "map/2" do
    assert Enum.map(1..3, fn x -> x * 2 end) == [2, 4, 6]
    assert Enum.map(-1..-3, fn x -> x * 2 end) == [-2, -4, -6]
  end

  test "map_every/3" do
    assert Enum.map_every(1..10, 2, fn x -> x * 2 end) == [2, 2, 6, 4, 10, 6, 14, 8, 18, 10]

    assert Enum.map_every(-1..-10, 2, fn x -> x * 2 end) ==
             [-2, -2, -6, -4, -10, -6, -14, -8, -18, -10]

    assert Enum.map_every(1..2, 2, fn x -> x * 2 end) == [2, 2]
    assert Enum.map_every(1..3, 0, fn x -> x * 2 end) == [1, 2, 3]

    assert_raise FunctionClauseError, fn ->
      Enum.map_every(1..3, -1, fn x -> x * 2 end)
    end
  end

  test "map_join/3" do
    assert Enum.map_join(1..0, " = ", &(&1 * 2)) == "2 = 0"
    assert Enum.map_join(1..3, " = ", &(&1 * 2)) == "2 = 4 = 6"
    assert Enum.map_join(1..3, &(&1 * 2)) == "246"
  end

  test "map_reduce/3" do
    assert Enum.map_reduce(1..0, 1, fn x, acc -> {x * 2, x + acc} end) == {[2, 0], 2}
    assert Enum.map_reduce(1..3, 1, fn x, acc -> {x * 2, x + acc} end) == {[2, 4, 6], 7}
  end

  test "max/1" do
    assert Enum.max(1..1) == 1
    assert Enum.max(1..3) == 3
    assert Enum.max(3..1) == 3
  end

  test "max_by/2" do
    assert Enum.max_by(1..1, fn x -> :math.pow(-2, x) end) == 1
    assert Enum.max_by(1..3, fn x -> :math.pow(-2, x) end) == 2
  end

  test "member?/2" do
    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
  end

  test "min/1" do
    assert Enum.min(1..1) == 1
    assert Enum.min(1..3) == 1
  end

  test "min_by/2" do
    assert Enum.min_by(1..1, fn x -> :math.pow(-2, x) end) == 1
    assert Enum.min_by(1..3, fn x -> :math.pow(-2, x) end) == 3
  end

  test "min_max/1" do
    assert Enum.min_max(1..1) == {1, 1}
    assert Enum.min_max(1..3) == {1, 3}
    assert Enum.min_max(3..1) == {1, 3}
  end

  test "min_max_by/2" do
    assert Enum.min_max_by(1..1, fn x -> x end) == {1, 1}
    assert Enum.min_max_by(1..3, fn x -> x end) == {1, 3}
  end

  test "split_with/2" do
    assert Enum.split_with(1..3, fn x -> rem(x, 2) == 0 end) == {[2], [1, 3]}
  end

  test "random/1" do
    # corner cases, independent of the seed
    assert Enum.random(1..1) == 1

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407_414, 139_258}
    seed2 = {1306, 421_106, 567_597}
    :rand.seed(:exsplus, seed1)
    assert Enum.random(1..2) == 1
    assert Enum.random(1..3) == 2
    assert Enum.random(3..1) == 1

    :rand.seed(:exsplus, seed2)
    assert Enum.random(1..2) == 1
    assert Enum.random(1..3) == 3
  end

  test "reduce/2" do
    assert Enum.reduce(1..3, fn x, acc -> x + acc end) == 6
  end

  test "reduce/3" do
    assert Enum.reduce(1..0, 1, fn x, acc -> x + acc end) == 2
    assert Enum.reduce(1..3, 1, fn x, acc -> x + acc end) == 7
  end

  test "reduce_while/3" do
    assert Enum.reduce_while(1..100, 0, fn i, acc ->
             if i <= 3, do: {:cont, acc + i}, else: {:halt, acc}
           end) == 6
  end

  test "reject/2" do
    assert Enum.reject(1..3, fn x -> rem(x, 2) == 0 end) == [1, 3]
    assert Enum.reject(1..6, fn x -> rem(x, 2) == 0 end) == [1, 3, 5]
  end

  test "reverse/1" do
    assert Enum.reverse(0..0) == [0]
    assert Enum.reverse(1..3) == [3, 2, 1]
    assert Enum.reverse(-3..5) == [5, 4, 3, 2, 1, 0, -1, -2, -3]
    assert Enum.reverse(5..5) == [5]
  end

  test "reverse/2" do
    assert Enum.reverse(1..3, 4..6) == [3, 2, 1, 4, 5, 6]
    assert Enum.reverse([1, 2, 3], 4..6) == [3, 2, 1, 4, 5, 6]
    assert Enum.reverse(1..3, [4, 5, 6]) == [3, 2, 1, 4, 5, 6]
    assert Enum.reverse(-3..5, MapSet.new([-3, -2])) == [5, 4, 3, 2, 1, 0, -1, -2, -3, -3, -2]
    assert Enum.reverse(5..5, [5]) == [5, 5]
  end

  test "reverse_slice/3" do
    assert Enum.reverse_slice(1..6, 2, 0) == [1, 2, 3, 4, 5, 6]
    assert Enum.reverse_slice(1..6, 2, 2) == [1, 2, 4, 3, 5, 6]
    assert Enum.reverse_slice(1..6, 2, 4) == [1, 2, 6, 5, 4, 3]
    assert Enum.reverse_slice(1..6, 2, 10_000_000) == [1, 2, 6, 5, 4, 3]
    assert Enum.reverse_slice(1..6, 10_000_000, 4) == [1, 2, 3, 4, 5, 6]
    assert Enum.reverse_slice(1..6, 50, 50) == [1, 2, 3, 4, 5, 6]
  end

  test "scan/2" do
    assert Enum.scan(1..5, &(&1 + &2)) == [1, 3, 6, 10, 15]
  end

  test "scan/3" do
    assert Enum.scan(1..5, 0, &(&1 + &2)) == [1, 3, 6, 10, 15]
  end

  test "shuffle/1" do
    # set a fixed seed so the test can be deterministic
    :rand.seed(:exsplus, {1374, 347_975, 449_264})
    assert Enum.shuffle(1..5) == [2, 1, 3, 5, 4]
  end

  test "slice/2" do
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

    assert_raise ArgumentError, fn ->
      x = 1.1
      Enum.slice(1..5, x..2)
    end

    assert_raise ArgumentError, fn ->
      x = 1.9
      Enum.slice(1..5, 1..x)
    end

    assert Enum.slice(5..1, 0..0) == [5]
    assert Enum.slice(5..1, 0..1) == [5, 4]
    assert Enum.slice(5..1, 0..2) == [5, 4, 3]
    assert Enum.slice(5..1, 1..2) == [4, 3]
    assert Enum.slice(5..1, 1..0) == []
    assert Enum.slice(5..1, 2..5) == [3, 2, 1]
    assert Enum.slice(5..1, 2..6) == [3, 2, 1]
    assert Enum.slice(5..1, 4..4) == [1]
    assert Enum.slice(5..1, 5..5) == []
    assert Enum.slice(5..1, 6..5) == []
    assert Enum.slice(5..1, 6..0) == []
    assert Enum.slice(5..1, -6..0) == []
    assert Enum.slice(5..1, -6..5) == []
    assert Enum.slice(5..1, -5..-1) == [5, 4, 3, 2, 1]
    assert Enum.slice(5..1, -5..-3) == [5, 4, 3]
    assert Enum.slice(5..1, -6..-1) == []
  end

  test "slice/3" do
    assert Enum.slice(1..5, 0, 0) == []
    assert Enum.slice(1..5, 0, 1) == [1]
    assert Enum.slice(1..5, 0, 2) == [1, 2]
    assert Enum.slice(1..5, 1, 2) == [2, 3]
    assert Enum.slice(1..5, 1, 0) == []
    assert Enum.slice(1..5, 2, 3) == [3, 4, 5]
    assert Enum.slice(1..5, 2, 6) == [3, 4, 5]
    assert Enum.slice(1..5, 5, 5) == []
    assert Enum.slice(1..5, 6, 5) == []
    assert Enum.slice(1..5, 6, 0) == []
    assert Enum.slice(1..5, -6, 0) == []
    assert Enum.slice(1..5, -6, 5) == []
    assert Enum.slice(1..5, -2, 5) == [4, 5]
    assert Enum.slice(1..5, -3, 1) == [3]

    assert_raise FunctionClauseError, fn ->
      Enum.slice(1..5, 0, -1)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.slice(1..5, 0.99, 0)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.slice(1..5, 0, 0.99)
    end

    assert Enum.slice(5..1, 0, 0) == []
    assert Enum.slice(5..1, 0, 1) == [5]
    assert Enum.slice(5..1, 0, 2) == [5, 4]
    assert Enum.slice(5..1, 1, 2) == [4, 3]
    assert Enum.slice(5..1, 1, 0) == []
    assert Enum.slice(5..1, 2, 3) == [3, 2, 1]
    assert Enum.slice(5..1, 2, 6) == [3, 2, 1]
    assert Enum.slice(5..1, 4, 4) == [1]
    assert Enum.slice(5..1, 5, 5) == []
    assert Enum.slice(5..1, 6, 5) == []
    assert Enum.slice(5..1, 6, 0) == []
    assert Enum.slice(5..1, -6, 0) == []
    assert Enum.slice(5..1, -6, 5) == []
  end

  test "sort/1" do
    assert Enum.sort(3..1) == [1, 2, 3]
    assert Enum.sort(2..1) == [1, 2]
    assert Enum.sort(1..1) == [1]
  end

  test "sort/2" do
    assert Enum.sort(3..1, &(&1 > &2)) == [3, 2, 1]
    assert Enum.sort(2..1, &(&1 > &2)) == [2, 1]
    assert Enum.sort(1..1, &(&1 > &2)) == [1]
  end

  test "sort_by/2" do
    assert Enum.sort_by(3..1, & &1) == [1, 2, 3]
  end

  test "split/2" do
    assert Enum.split(1..3, 0) == {[], [1, 2, 3]}
    assert Enum.split(1..3, 1) == {[1], [2, 3]}
    assert Enum.split(1..3, 2) == {[1, 2], [3]}
    assert Enum.split(1..3, 3) == {[1, 2, 3], []}
    assert Enum.split(1..3, 4) == {[1, 2, 3], []}
    assert Enum.split(1..3, -1) == {[1, 2], [3]}
    assert Enum.split(1..3, -2) == {[1], [2, 3]}
    assert Enum.split(1..3, -3) == {[], [1, 2, 3]}
    assert Enum.split(1..3, -10) == {[], [1, 2, 3]}
    assert Enum.split(1..0, 3) == {[1, 0], []}
  end

  test "split_while/2" do
    assert Enum.split_while(1..3, fn _ -> false end) == {[], [1, 2, 3]}
    assert Enum.split_while(1..3, fn _ -> nil end) == {[], [1, 2, 3]}
    assert Enum.split_while(1..3, fn _ -> true end) == {[1, 2, 3], []}
    assert Enum.split_while(1..3, fn x -> x > 2 end) == {[], [1, 2, 3]}
    assert Enum.split_while(1..3, fn x -> x > 3 end) == {[], [1, 2, 3]}
    assert Enum.split_while(1..3, fn x -> x < 3 end) == {[1, 2], [3]}
    assert Enum.split_while(1..3, fn x -> x end) == {[1, 2, 3], []}
    assert Enum.split_while(1..0, fn _ -> true end) == {[1, 0], []}
  end

  test "sum/1" do
    assert Enum.sum(0..0) == 0
    assert Enum.sum(1..1) == 1
    assert Enum.sum(1..3) == 6
    assert Enum.sum(0..100) == 5050
    assert Enum.sum(10..100) == 5005
    assert Enum.sum(100..10) == 5005
    assert Enum.sum(-10..-20) == -165
    assert Enum.sum(-10..2) == -52
  end

  test "take/2" do
    assert Enum.take(1..3, 0) == []
    assert Enum.take(1..3, 1) == [1]
    assert Enum.take(1..3, 2) == [1, 2]
    assert Enum.take(1..3, 3) == [1, 2, 3]
    assert Enum.take(1..3, 4) == [1, 2, 3]
    assert Enum.take(1..3, -1) == [3]
    assert Enum.take(1..3, -2) == [2, 3]
    assert Enum.take(1..3, -4) == [1, 2, 3]
    assert Enum.take(1..0, 3) == [1, 0]
  end

  test "take_every/2" do
    assert Enum.take_every(1..10, 2) == [1, 3, 5, 7, 9]
    assert Enum.take_every(1..2, 2) == [1]
    assert Enum.take_every(1..3, 0) == []

    assert_raise FunctionClauseError, fn ->
      Enum.take_every(1..3, -1)
    end
  end

  test "take_random/2" do
    # corner cases, independent of the seed
    assert_raise FunctionClauseError, fn -> Enum.take_random(1..2, -1) end
    assert Enum.take_random(1..1, 0) == []
    assert Enum.take_random(1..1, 1) == [1]
    assert Enum.take_random(1..1, 2) == [1]
    assert Enum.take_random(1..2, 0) == []

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    seed1 = {1406, 407_414, 139_258}
    seed2 = {1406, 421_106, 567_597}
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(1..3, 1) == [2]
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(1..3, 2) == [2, 3]
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(1..3, 3) == [2, 3, 1]
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(1..3, 4) == [2, 3, 1]
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(3..1, 1) == [2]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(1..3, 1) == [3]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(1..3, 2) == [3, 2]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(1..3, 3) == [3, 2, 1]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(1..3, 4) == [3, 2, 1]

    # make sure optimizations don't change fixed seeded tests
    :rand.seed(:exsplus, {101, 102, 103})
    one = Enum.take_random(1..100, 1)
    :rand.seed(:exsplus, {101, 102, 103})
    two = Enum.take_random(1..100, 2)
    assert hd(one) == hd(two)
  end

  test "take_while/2" do
    assert Enum.take_while(1..3, fn x -> x > 3 end) == []
    assert Enum.take_while(1..3, fn x -> x <= 1 end) == [1]
    assert Enum.take_while(1..3, fn x -> x <= 3 end) == [1, 2, 3]
    assert Enum.take_while(1..3, fn x -> x end) == [1, 2, 3]
    assert Enum.take_while(1..3, fn _ -> nil end) == []
  end

  test "to_list/1" do
    assert Enum.to_list([1, 2, 3]) == [1, 2, 3]
    assert Enum.to_list(MapSet.new(1..3)) == [1, 2, 3]
    assert Enum.to_list(1..3) == [1, 2, 3]
  end

  test "uniq/1" do
    assert Enum.uniq(1..3) == [1, 2, 3]
  end

  test "uniq_by/2" do
    assert Enum.uniq_by(1..3, fn x -> x end) == [1, 2, 3]
  end

  test "unzip/1" do
    assert_raise FunctionClauseError, fn -> Enum.unzip(1..3) end
  end

  test "with_index/2" do
    assert Enum.with_index(1..3) == [{1, 0}, {2, 1}, {3, 2}]
    assert Enum.with_index(1..3, 3) == [{1, 3}, {2, 4}, {3, 5}]
  end

  test "zip/2" do
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
end

defmodule EnumTest.Map do
  # Maps use different protocols path than lists and ranges in the cases below.
  use ExUnit.Case, async: true

  test "random/1" do
    map = %{a: 1, b: 2, c: 3}
    seed1 = {1406, 407_414, 139_258}
    seed2 = {1406, 421_106, 567_597}
    :rand.seed(:exsplus, seed1)
    assert Enum.random(map) == {:c, 3}
    assert Enum.random(map) == {:b, 2}
    assert Enum.random(map) == {:c, 3}

    :rand.seed(:exsplus, seed2)
    assert Enum.random(map) == {:a, 1}
    assert Enum.random(map) == {:a, 1}
  end

  test "take_random/2" do
    # corner cases, independent of the seed
    assert_raise FunctionClauseError, fn -> Enum.take_random(1..2, -1) end
    assert Enum.take_random(%{a: 1}, 0) == []
    assert Enum.take_random(%{a: 1}, 2) == [a: 1]
    assert Enum.take_random(%{a: 1, b: 2}, 0) == []

    # set a fixed seed so the test can be deterministic
    # please note the order of following assertions is important
    map = %{a: 1, b: 2, c: 3}
    seed1 = {1406, 407_414, 139_258}
    seed2 = {1406, 421_106, 567_597}
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(map, 1) == [b: 2]
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(map, 2) == [b: 2, c: 3]
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(map, 3) == [b: 2, c: 3, a: 1]
    :rand.seed(:exsplus, seed1)
    assert Enum.take_random(map, 4) == [b: 2, c: 3, a: 1]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(map, 1) == [c: 3]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(map, 2) == [c: 3, b: 2]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(map, 3) == [c: 3, b: 2, a: 1]
    :rand.seed(:exsplus, seed2)
    assert Enum.take_random(map, 4) == [c: 3, b: 2, a: 1]
  end

  test "reverse/1" do
    assert Enum.reverse(%{}) == []
    assert Enum.reverse(MapSet.new()) == []
    assert Enum.reverse(%{a: 1, b: 2, c: 3}) == [c: 3, b: 2, a: 1]
  end

  test "reverse/2" do
    assert Enum.reverse([a: 1, b: 2, c: 3, a: 1], %{x: 1, y: 2, z: 3}) ==
             [a: 1, c: 3, b: 2, a: 1, x: 1, y: 2, z: 3]

    assert Enum.reverse([], %{a: 1}) == [a: 1]
    assert Enum.reverse([], %{}) == []
    assert Enum.reverse(%{a: 1}, []) == [a: 1]
    assert Enum.reverse(MapSet.new(), %{}) == []
  end

  test "fetch/2" do
    map = %{a: 1, b: 2, c: 3, d: 4, e: 5}
    assert Enum.fetch(map, 0) == {:ok, {:a, 1}}
    assert Enum.fetch(map, -2) == {:ok, {:d, 4}}
    assert Enum.fetch(map, -6) == :error
    assert Enum.fetch(map, 5) == :error
    assert Enum.fetch(%{}, 0) == :error

    assert Stream.take(map, 3) |> Enum.fetch(3) == :error
    assert Stream.take(map, 5) |> Enum.fetch(4) == {:ok, {:e, 5}}
  end

  test "slice/2" do
    map = %{a: 1, b: 2, c: 3, d: 4, e: 5}
    assert Enum.slice(map, 0..0) == [a: 1]
    assert Enum.slice(map, 0..1) == [a: 1, b: 2]
    assert Enum.slice(map, 0..2) == [a: 1, b: 2, c: 3]
  end

  test "slice/3" do
    map = %{a: 1, b: 2, c: 3, d: 4, e: 5}
    assert Enum.slice(map, 1, 2) == [b: 2, c: 3]
    assert Enum.slice(map, 1, 0) == []
    assert Enum.slice(map, 2, 5) == [c: 3, d: 4, e: 5]
    assert Enum.slice(map, 2, 6) == [c: 3, d: 4, e: 5]
    assert Enum.slice(map, 5, 5) == []
    assert Enum.slice(map, 6, 5) == []
    assert Enum.slice(map, 6, 0) == []
    assert Enum.slice(map, -6, 0) == []
    assert Enum.slice(map, -6, 5) == []
    assert Enum.slice(map, -2, 5) == [d: 4, e: 5]
    assert Enum.slice(map, -3, 1) == [c: 3]

    assert_raise FunctionClauseError, fn ->
      Enum.slice(map, 0, -1)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.slice(map, 0.99, 0)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.slice(map, 0, 0.99)
    end

    assert Enum.slice(map, 0, 0) == []
    assert Enum.slice(map, 0, 1) == [a: 1]
    assert Enum.slice(map, 0, 2) == [a: 1, b: 2]
    assert Enum.slice(map, 1, 2) == [b: 2, c: 3]
    assert Enum.slice(map, 1, 0) == []
    assert Enum.slice(map, 2, 5) == [c: 3, d: 4, e: 5]
    assert Enum.slice(map, 2, 6) == [c: 3, d: 4, e: 5]
    assert Enum.slice(map, 5, 5) == []
    assert Enum.slice(map, 6, 5) == []
    assert Enum.slice(map, 6, 0) == []
    assert Enum.slice(map, -6, 0) == []
    assert Enum.slice(map, -6, 5) == []
    assert Enum.slice(map, -2, 5) == [d: 4, e: 5]
    assert Enum.slice(map, -3, 1) == [c: 3]

    assert_raise FunctionClauseError, fn ->
      Enum.slice(map, 0, -1)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.slice(map, 0.99, 0)
    end

    assert_raise FunctionClauseError, fn ->
      Enum.slice(map, 0, 0.99)
    end
  end
end

defmodule EnumTest.SideEffects do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO
  import PathHelpers

  test "take/2 with side effects" do
    stream =
      Stream.unfold(1, fn x ->
        IO.puts(x)
        {x, x + 1}
      end)

    assert capture_io(fn ->
             Enum.take(stream, 1)
           end) == "1\n"
  end

  test "take/2 does not consume next without a need" do
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

  test "take/2 with no elements works as no-op" do
    iterator = File.stream!(fixture_path("unknown.txt"))

    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
    assert Enum.take(iterator, 0) == []
  end
end

defmodule EnumTest.Function do
  use ExUnit.Case, async: true

  test "raises Protocol.UndefinedError for funs of wrong arity" do
    assert_raise Protocol.UndefinedError, fn ->
      Enum.to_list(fn -> nil end)
    end
  end
end
