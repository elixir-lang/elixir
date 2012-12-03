Code.require_file "../test_helper.exs", __FILE__

defmodule EnumTest do
  use ExUnit.Case, async: true

  test :reverse do
    assert Enum.reverse(URI.query_decoder("foo=bar&baz=bat")) ==
      [{ "baz", "bat" }, { "foo", "bar" }]
  end

  test :count do
    assert Enum.count(URI.query_decoder("foo=bar&baz=bat")) == 2
  end
end

defmodule EnumTest.List do
  use ExUnit.Case, async: true

  test :all? do
    assert Enum.all?([2,4,6], fn(x) -> rem(x, 2) == 0 end)
    refute Enum.all?([2,3,4], fn(x) -> rem(x, 2) == 0 end)

    assert Enum.all?([2,4,6])
    refute Enum.all?([2,nil,4])

    assert Enum.all?([])
  end

  test :any? do
    refute Enum.any?([2,4,6], fn(x) -> rem(x, 2) == 1 end)
    assert Enum.any?([2,3,4], fn(x) -> rem(x, 2) == 1 end)

    refute Enum.any?([false,false,false])
    assert Enum.any?([false,true,false])

    refute Enum.any?([])
  end

  test :at! do
    assert Enum.at!([2,4,6], 0) == 2
    assert Enum.at!([2,4,6], 2) == 6
    assert_raise Enum.OutOfBoundsError, fn ->
      Enum.at!([2,4,6], 4)
    end
  end

  test :count do
    assert Enum.count([1,2,3]) == 3
    assert Enum.count([]) == 0
  end

  test :count_fun do
    assert Enum.count([1,2,3], fn(x) -> rem(x, 2) == 0 end) == 1
    assert Enum.count([], fn(x) -> rem(x, 2) == 0 end) == 0
  end

  test :drop do
    assert Enum.drop([1,2,3], 0) == [1,2,3]
    assert Enum.drop([1,2,3], 1) == [2,3]
    assert Enum.drop([1,2,3], 2) == [3]
    assert Enum.drop([1,2,3], 3) == []
    assert Enum.drop([1,2,3], 4) == []
    assert Enum.drop([1,2,3], -1) == [3]
    assert Enum.drop([], 3) == []
  end

  test :drop_while do
    assert Enum.drop_while([1,2,3,4,3,2,1], fn(x) -> x <= 3 end) == [4,3,2,1]
    assert Enum.drop_while([1,2,3], fn(_) -> false end) == [1,2,3]
    assert Enum.drop_while([1,2,3], fn(x) -> x <= 3 end) == []
    assert Enum.drop_while([], fn(_) -> false end) == []
  end

  test :find do
    assert Enum.find([2,4,6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find([2,4,6], 0, fn(x) -> rem(x, 2) == 1 end) == 0
    assert Enum.find([2,3,4], fn(x) -> rem(x, 2) == 1 end) == 3
  end

  test :find_value do
    assert Enum.find_value([2,4,6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find_value([2,4,6], 0, fn(x) -> rem(x, 2) == 1 end) == 0
    assert Enum.find_value([2,3,4], fn(x) -> rem(x, 2) == 1 end)
  end

  test :find_index do
    assert Enum.find_index([2,4,6], fn(x) -> rem(x, 2) == 1 end) == nil
    assert Enum.find_index([2,3,4], fn(x) -> rem(x, 2) == 1 end) == 1
  end

  test :empty? do
    assert Enum.empty?([])
    refute Enum.empty?([1,2,3])
  end

  test :each do
    try do
      assert Enum.each([], fn(x) -> x end) == :ok

      assert Enum.each([1,2,3], fn(x) -> Process.put(:enum_test_each, x * 2) end) == :ok
      assert Process.get(:enum_test_each) == 6
    after
      Process.delete(:enum_test_each)
    end
  end

  test :first do
    assert Enum.first([]) == nil
    assert Enum.first([1,2,3]) == 1
  end

  test :filter do
    assert Enum.filter([1,2,3], fn(x) -> rem(x, 2) == 0 end) == [2]
    assert Enum.filter([2,4,6], fn(x) -> rem(x, 2) == 0 end) == [2,4,6]
  end

  test :filter_with_match do
    assert Enum.filter([1,2,3], match?(1, &1)) == [1]
    assert Enum.filter([1,2,3], match?(x when x < 3, &1)) == [1,2]
    assert Enum.filter([1,2,3], match?(_, &1)) == [1,2,3]
  end

  test :filter_map do
    assert Enum.filter_map([1,2,3], fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4]
    assert Enum.filter_map([2,4,6], fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4,8,12]
  end

  test :reduce do
    assert Enum.reduce([], 1, fn(x, acc) -> x + acc end) == 1
    assert Enum.reduce([1,2,3], 1, fn(x, acc) -> x + acc end) == 7
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

  test :map_join_with_bin do
    assert Enum.map_join([], " = ", &1 * 2) == ""
    assert Enum.map_join([1,2,3], " = ", &1 * 2) == "2 = 4 = 6"
    assert Enum.map_join([1,2,3], &1 * 2) == "246"
  end

  test :map_join_with_list do
    assert Enum.map_join([], ' = ', &1 * 2) == ''
    assert Enum.map_join([1,2,3], ' = ', &1 * 2) == '2 = 4 = 6'
  end

  test :map do
    assert Enum.map([], fn x -> x * 2 end) == []
    assert Enum.map([1,2,3], fn x -> x * 2 end) == [2,4,6]
  end

  test :map_reduce do
    assert Enum.map_reduce([], 1, fn(x, acc) -> { x * 2, x + acc } end) == { [], 1 }
    assert Enum.map_reduce([1,2,3], 1, fn(x, acc) -> { x * 2, x + acc } end) == { [2,4,6], 7 }
  end

  test :partition do
    assert Enum.partition([1,2,3], fn(x) -> rem(x, 2) == 0 end) == { [2], [1,3] }
    assert Enum.partition([2,4,6], fn(x) -> rem(x, 2) == 0 end) == { [2,4,6], [] }
  end

  test :reverse do
    assert Enum.reverse([]) == []
    assert Enum.reverse([1,2,3]) == [3,2,1]
  end

  test :sort do
    assert Enum.sort([5,3,2,4,1]) == [1,2,3,4,5]
    assert Enum.sort([5,3,2,4,1], &1 > &2) == [5,4,3,2,1]
  end

  test :split do
    assert Enum.split([1,2,3], 0) == { [], [1,2,3] }
    assert Enum.split([1,2,3], 1) == { [1], [2,3] }
    assert Enum.split([1,2,3], 2) == { [1,2], [3] }
    assert Enum.split([1,2,3], 3) == { [1,2,3], [] }
    assert Enum.split([1,2,3], 4) == { [1,2,3], [] }
    assert Enum.split([], 3) == { [], [] }
    assert Enum.split([1,2,3], -1) == { [1,2], [3] }
    assert Enum.split([1,2,3], -2) == { [1], [2,3] }
    assert Enum.split([1,2,3], -3) == { [], [1,2,3] }
    assert Enum.split([1,2,3], -10) == { [], [1,2,3] }
  end

  test :split_while do
    assert Enum.split_while([1,2,3], fn(_) -> false end) == { [], [1,2,3] }
    assert Enum.split_while([1,2,3], fn(_) -> true end) == { [1,2,3], [] }
    assert Enum.split_while([1,2,3], fn(x) -> x > 2 end) == { [], [1,2,3] }
    assert Enum.split_while([1,2,3], fn(x) -> x > 3 end) == { [], [1,2,3] }
    assert Enum.split_while([1,2,3], fn(x) -> x < 3 end) == { [1,2], [3] }
    assert Enum.split_while([], fn(_) -> true end) == { [], [] }
  end

  test :take do
    assert Enum.take([1,2,3], 0) == []
    assert Enum.take([1,2,3], 1) == [1]
    assert Enum.take([1,2,3], 2) == [1,2]
    assert Enum.take([1,2,3], 3) == [1,2,3]
    assert Enum.take([1,2,3], 4) == [1,2,3]
    assert Enum.take([1,2,3], -1) == [1,2]
    assert Enum.take([], 3) == []
  end

  test :take_while do
    assert Enum.take_while([1,2,3], fn(x) -> x > 3 end) == []
    assert Enum.take_while([1,2,3], fn(x) -> x <= 1 end) == [1]
    assert Enum.take_while([1,2,3], fn(x) -> x <= 3 end) == [1,2,3]
    assert Enum.take_while([], fn(_) -> true end) == []
  end

  test :uniq do
    assert Enum.uniq([1,2,3,2,1]) == [1,2,3]
  end

  test :zip do
    assert Enum.zip([:a, :b], [1, 2]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b], [1, 2, 3, 4]) == [{:a, 1}, {:b, 2}]
    assert Enum.zip([:a, :b, :c, :d], [1, 2]) == [{:a, 1}, {:b, 2}, {:c, nil}, {:d, nil}]
    assert Enum.zip([], [1]) == []
    assert Enum.zip([1], []) == [{ 1, nil }]
    assert Enum.zip([], []) == []
  end
end

defmodule EnumTest.Dict.Common do
  defmacro __using__(module) do
    quote do
      use ExUnit.Case, async: true

      test :all? do
        dict = unquote(module).new [{2,2}, {3,4}, {4,6}]
        assert Enum.all?(dict, fn({_, v}) -> rem(v, 2) == 0 end)
        refute Enum.all?(dict, fn({k, _}) -> rem(k, 2) == 0 end)

        assert Enum.all?(unquote(module).new)
      end

      test :any? do
        dict = unquote(module).new [{2,2}, {3,4}, {4,6}]
        refute Enum.any?(dict, fn({_, v}) -> rem(v, 2) == 1 end)
        assert Enum.any?(dict, fn({k, _}) -> rem(k, 2) == 1 end)

        refute Enum.any?(unquote(module).new)
      end

      test :count do
        dict = unquote(module).new [{2,2}, {3,4}, {4,6}]
        assert Enum.count(dict) == 3
        assert Enum.count(unquote(module).new) == 0
      end

      test :count_fun do
        dict = unquote(module).new [{2,2}, {3,4}, {4,6}]
        assert Enum.count(dict, fn({x,_}) -> rem(x, 2) == 0 end) == 2
        assert Enum.count(unquote(module).new, fn(x) -> rem(x, 2) == 0 end) == 0
      end

      test :find do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        assert nil == Enum.find(dict, fn({_, v}) -> v == 0 end)
        assert :ok == Enum.find(dict, :ok, fn({_, v}) -> v == 0 end)
        assert {:a, 1} == Enum.find(dict, fn({_, v}) -> rem(v, 2) == 1 end)
        assert {:b, 2} == Enum.find(dict, fn({_, v}) -> rem(v, 2) == 0 end)
      end

      test :find_value do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        assert nil == Enum.find_value(dict, fn({_, v}) -> v == 0 end)
        assert :ok == Enum.find_value(dict, :ok, fn({_, v}) -> v == 0 end)
        assert Enum.find_value(dict, fn({_, v}) -> rem(v, 2) == 1 end)
      end

      test :empty? do
        assert Enum.empty?(unquote(module).new)
        refute Enum.empty?(unquote(module).new [a: 1])
      end

      test :each do
        try do
          empty_dict = unquote(module).new
          assert Enum.each(empty_dict, fn(x) -> x end) == :ok

          dict = unquote(module).new [{"one",1}, {"two",2}, {"three",3}]
          assert Enum.each(dict, fn({k, v}) -> Process.put(k, v * 2) end) == :ok
          assert Process.get("one")
          assert Process.get("two")
          assert Process.get("three")
        after
          Process.delete("one")
          Process.delete("two")
          Process.delete("three")
        end
      end

      test :filter do
        all  = [a: 1, b: 2, c: 3, d: 4]
        odd  = [a: 1, c: 3]
        even = [b: 2, d: 4]
        dict = unquote(module).new(all)

        assert Enum.filter(dict, fn({_, v}) -> rem(v, 2) == 1 end) == odd
        assert Enum.filter(dict, fn({_, v}) -> rem(v, 2) == 0 end) == even
        assert Enum.filter(dict, fn(x) -> x end) == all
      end

      test :filter_map do
        odd_dict  = unquote(module).new [a: 1, b: 2, c: 3]
        even_dict = unquote(module).new [a: 2, b: 4, c: 6]

        assert Enum.filter_map(odd_dict,
                 fn({_, v}) -> rem(v, 2) == 0 end,
                 fn({k, v}) -> { k, v * 2 } end) == [b: 4]

        assert Enum.filter_map(even_dict,
                 fn({_, v}) -> rem(v, 2) == 0 end,
                 fn({k, v}) -> { k, v * 2 } end) == [a: 4, b: 8, c: 12]
      end

      test :join do
        dict = unquote(module).new [a: 1, b: 2, c: 3]

        assert_raise Protocol.UndefinedError, fn ->
          Enum.join dict, ","
        end
        assert_raise Protocol.UndefinedError, fn ->
          Enum.join dict, ','
        end
      end

      test :reduce do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        assert 1 == Enum.reduce(unquote(module).new, 1, fn(x, acc) -> x + acc end)
        assert 7 == Enum.reduce(dict, 1, fn({_, v}, acc) -> v + acc end)
      end

      test :map do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        assert Enum.map(dict, fn {k, v} -> { k, v * 2 } end) == [a: 2, b: 4, c: 6]
        assert Enum.map(unquote(module).new, fn x -> x * 2 end) == []
      end

      test :map_reduce do
        dict   = unquote(module).new [a: 1, b: 2, c: 3]
        double = [a: 2, b: 4, c: 6]
        assert Enum.map_reduce(dict, 1, fn({k, v}, acc) -> { {k, v * 2}, v + acc } end) == { double, 7 }
        assert Enum.map_reduce(unquote(module).new, 1, fn(x, acc) -> { x * 2, x + acc } end) == { [], 1 }
      end

      test :partition do
        all     = [a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7]
        below_4 = [a: 1, b: 2, c: 3]
        above_4 = [d: 4, e: 5, f: 6, g: 7]
        dict    = unquote(module).new all

        assert { below_4, above_4 } == Enum.partition(dict, fn({_k, v}) -> v < 4 end)
        assert { all, [] } == Enum.partition(dict, fn({_k, v}) -> v < 10 end)
      end

      test :sort do
        dict = unquote(module).new [{:b,2},{:c,3},{:a,1},{:d,4}]
        assert Enum.sort(dict) == [a: 1, b: 2, c: 3, d: 4]
      end
    end
  end
end

defmodule EnumTest.OrdDict do
  use EnumTest.Dict.Common, OrdDict

  test :drop do
    dict = OrdDict.new [a: 1, b: 2, c: 3]
    assert Enum.drop(dict, 2) == [c: 3]
    assert Enum.drop(dict, 3) == []
    assert Enum.drop(dict, 4) == []
  end

  test :drop_while do
    dict = OrdDict.new [a: 1, b: 2, c: 3]
    assert Enum.drop_while(dict, fn({_k, v}) -> v < 3 end) == [c: 3]
  end

  test :first do
    dict = OrdDict.new []
    assert Enum.first(dict) == nil

    dict = OrdDict.new [a: 1, b: 2, c: 3]
    assert Enum.first(dict) == {:a, 1}
  end

  test :split do
    dict = OrdDict.new [a: 1, b: 2, c: 3]
    assert Enum.split(dict, 1) == { [a: 1], [b: 2, c: 3] }
    assert Enum.split(dict, 0) == { [], [a: 1, b: 2, c: 3] }
    assert Enum.split(dict, 3) == { [a: 1, b: 2, c: 3], [] }
    assert Enum.split(dict, 4) == { [a: 1, b: 2, c: 3], [] }
    assert Enum.split(dict, -1) == { [a: 1, b: 2], [c: 3] }
    assert Enum.split(dict, -2) == { [a: 1], [b: 2, c: 3] }
    assert Enum.split(dict, -3) == { [], [a: 1, b: 2, c: 3] }
    assert Enum.split(dict, -10) == { [], [a: 1, b: 2, c: 3] }
  end

  test :split_while do
    dict = OrdDict.new [a: 1, b: 3, c: 2, d: 4]
    assert Enum.split_while(dict, fn({_k, v}) -> rem(v, 2) == 1 end) == { [a: 1, b: 3], [c: 2, d: 4] }
  end

  test :take do
    dict = OrdDict.new [a: 1, b: 2, c: 3, d: 4]
    assert Enum.take(dict, 2) == [a: 1, b: 2]
    assert Enum.take(dict, 0) == []
    assert Enum.take(dict, 4) == [a: 1, b: 2, c: 3, d: 4]
    assert Enum.take(dict, 5) == [a: 1, b: 2, c: 3, d: 4]
  end

  test :take_while do
    dict = OrdDict.new [a: 1, b: 3, c: 2, d: 4]
    assert Enum.take_while(dict, fn({_k, v}) -> rem(v, 2) == 1 end) == [a: 1, b: 3]
    assert Enum.take_while(dict, fn({_k, v}) -> v < 10 end) == [a: 1, b: 3, c: 2, d: 4]
    assert Enum.take_while(dict, fn({_k, v}) -> v < 1 end) == []
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

  test :at! do
    assert Enum.at!(2..6, 0) == 2
    assert Enum.at!(2..6, 4) == 6
    assert Enum.at!(-2..-6, 0) == -2
    assert Enum.at!(-2..-6, 4) == -6

    assert_raise Enum.OutOfBoundsError, fn ->
      assert Enum.at!(2..6, 8)
    end

    assert_raise Enum.OutOfBoundsError, fn ->
      assert Enum.at!(-2..-6, 8)
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
    assert Enum.drop(range, 0) == [1,2,3]
    assert Enum.drop(range, 1) == [2,3]
    assert Enum.drop(range, 2) == [3]
    assert Enum.drop(range, 3) == []
    assert Enum.drop(range, 4) == []

    range = Range.new(first: 1, last: 0)
    assert Enum.drop(range, 3) == []
  end

  test :drop_while do
    range = Range.new(first: 0, last: 6)
    assert Enum.drop_while(range, fn(x) -> x <= 3 end) == [4,5,6]
    assert Enum.drop_while(range, fn(_) -> false end) == [0,1,2,3,4,5,6]

    range = Range.new(first: 0, last: 3)
    assert Enum.drop_while(range, fn(x) -> x <= 3 end) == []

    range = Range.new(first: 1, last: 0)
    assert Enum.drop_while(range, fn(_) -> false end) == [1,0]
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
    assert Enum.filter(range, fn(x) -> rem(x, 2) == 0 end) == [2,4,6]
  end

  test :filter_with_match do
    range = Range.new(first: 1, last: 3)
    assert Enum.filter(range, match?(1, &1)) == [1]
    assert Enum.filter(range, match?(x when x < 3, &1)) == [1,2]
    assert Enum.filter(range, match?(_, &1)) == [1,2,3]
  end

  test :filter_map do
    range = Range.new(first: 1, last: 3)
    assert Enum.filter_map(range, fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4]

    range = Range.new(first: 2, last: 6)
    assert Enum.filter_map(range, fn(x) -> rem(x, 2) == 0 end, &1 * 2) == [4,8,12]
  end

  test :reduce do
    range = Range.new(first: 1, last: 0)
    assert Enum.reduce(range, 1, fn(x, acc) -> x + acc end) == 2

    range = Range.new(first: 1, last: 3)
    assert Enum.reduce(range, 1, fn(x, acc) -> x + acc end) == 7
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
    assert Enum.map(range, fn x -> x * 2 end) == [2,4,6]

    range = Range.new(first: -1, last: -3)
    assert Enum.map(range, fn x -> x * 2 end) == [-2,-4,-6]
  end

  test :map_reduce do
    range = Range.new(first: 1, last: 0)
    assert Enum.map_reduce(range, 1, fn(x, acc) -> { x * 2, x + acc } end) == { [2, 0], 2 }

    range = Range.new(first: 1, last: 3)
    assert Enum.map_reduce(range, 1, fn(x, acc) -> { x * 2, x + acc } end) == { [2,4,6], 7 }
  end

  test :partition do
    range = Range.new(first: 1, last: 3)
    assert Enum.partition(range, fn(x) -> rem(x, 2) == 0 end) == { [2], [1,3] }
  end

  test :sort do
    assert Enum.sort(Range.new(first: 3, last: 1)) == [1,2,3]
    assert Enum.sort(Range.new(first: 2, last: 1)) == [1,2]
    assert Enum.sort(Range.new(first: 1, last: 1)) == [1]

    assert Enum.sort(Range.new(first: 3, last: 1), &1 > &2) == [3,2,1]
    assert Enum.sort(Range.new(first: 2, last: 1), &1 > &2) == [2,1]
    assert Enum.sort(Range.new(first: 1, last: 1), &1 > &2) == [1]
  end

  test :split do
    range = Range.new(first: 1, last: 3)
    assert Enum.split(range, 0) == { [], [1,2,3] }
    assert Enum.split(range, 1) == { [1], [2,3] }
    assert Enum.split(range, 2) == { [1,2], [3] }
    assert Enum.split(range, 3) == { [1,2,3], [] }
    assert Enum.split(range, 4) == { [1,2,3], [] }
    assert Enum.split(range, -1) == { [1,2], [3] }
    assert Enum.split(range, -2) == { [1], [2,3] }
    assert Enum.split(range, -3) == { [], [1,2,3] }
    assert Enum.split(range, -10) == { [], [1,2,3] }

    range = Range.new(first: 1, last: 0)
    assert Enum.split(range, 3) == { [1,0], [] }
  end

  test :split_while do
    range = Range.new(first: 1, last: 3)
    assert Enum.split_while(range, fn(_) -> false end) == { [], [1,2,3] }
    assert Enum.split_while(range, fn(_) -> true end) == { [1,2,3], [] }
    assert Enum.split_while(range, fn(x) -> x > 2 end) == { [], [1,2,3] }
    assert Enum.split_while(range, fn(x) -> x > 3 end) == { [], [1,2,3] }
    assert Enum.split_while(range, fn(x) -> x < 3 end) == { [1,2], [3] }

    range = Range.new(first: 1, last: 0)
    assert Enum.split_while(range, fn(_) -> true end) == { [1, 0], [] }
  end

  test :take do
    range = Range.new(first: 1, last: 3)
    assert Enum.take(range, 0) == []
    assert Enum.take(range, 1) == [1]
    assert Enum.take(range, 2) == [1,2]
    assert Enum.take(range, 3) == [1,2,3]
    assert Enum.take(range, 4) == [1,2,3]

    range = Range.new(first: 1, last: 0)
    assert Enum.take(range, 3) == [1,0]
  end

  test :take_while do
    range = Range.new(first: 1, last: 3)
    assert Enum.take_while(range, fn(x) -> x > 3 end) == []
    assert Enum.take_while(range, fn(x) -> x <= 1 end) == [1]
    assert Enum.take_while(range, fn(x) -> x <= 3 end) == [1,2,3]
    assert Enum.take_while([], fn(_) -> true end) == []
  end

  test :uniq do
    assert Enum.uniq(1..3) == [1,2,3]
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
end