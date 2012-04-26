Code.require_file "../test_helper", __FILE__

defmodule EnumTest.Common do
  use ExUnit.Case

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

defmodule EnumTest.List do
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

  test :reduce do
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
end

defmodule EnumTest.Dict.Common do
  defmacro __using__(module, _opts // []) do
    quote do
      use ExUnit.Case

      test :all? do
        dict = unquote(module).new [2, 3, 4], [2, 4, 6]
        assert Enum.all?(dict, fn({_, v}, do: rem(v, 2) == 0))
        refute Enum.all?(dict, fn({k, _}, do: rem(k, 2) == 0))

        assert Enum.all?(unquote(module).new)
      end

      test :any? do
        dict = unquote(module).new [2, 3, 4], [2, 4, 6]
        refute Enum.any?(dict, fn({_, v}, do: rem(v, 2) == 1))
        assert Enum.any?(dict, fn({k, _}, do: rem(k, 2) == 1))

        refute Enum.any?(unquote(module).new)
      end

      test :find do
        dict = unquote(module).new [:a, :b, :c], [1, 2, 3]
        assert nil == Enum.find(dict, fn({_, v}, do: v == 0))
        assert :ok == Enum.find(dict, :ok, fn({_, v}, do: v == 0))
        assert {:a, 1} == Enum.find(dict, fn({_, v}, do: rem(v, 2) == 1))
        assert {:b, 2} == Enum.find(dict, fn({_, v}, do: rem(v, 2) == 0))
      end

      test :find_value do
        dict = unquote(module).new [:a, :b, :c], [1, 2, 3]
        assert nil == Enum.find_value(dict, fn({_, v}, do: v == 0))
        assert :ok == Enum.find_value(dict, :ok, fn({_, v}, do: v == 0))
        assert Enum.find_value(dict, fn({_, v}, do: rem(v, 2) == 1))
      end

      test :empty? do
        assert Enum.empty?(unquote(module).new)
        refute Enum.empty?(unquote(module).new {:a, 1})
      end

      test :each do
        empty_dict = unquote(module).new
        assert empty_dict == Enum.each(empty_dict, fn(x, do: x))

        dict = unquote(module).new ["one", "two", "three"], [1, 2, 3]
        assert dict == Enum.each(dict, fn({k, v}, do: Process.put(k, v * 2)))
        assert 2 == Process.get("one")
        assert 4 == Process.get("two")
        assert 6 == Process.get("three")
      after:
        Process.delete("one")
        Process.delete("two")
        Process.delete("three")
      end

      test :entries do
        dict = unquote(module).new ["one", "two", "three"], [1, 2, 3]
        assert [{"one", 1}, {"three", 3}, {"two", 2}] == List.sort Enum.entries(dict)
      end

      test :filter do
        dict = unquote(module).new ['a', 'b', 'c', 'd'], [1, 2, 3, 4]
        odd_dict = unquote(module).new ['a', 'c'], [1, 3]
        even_dict = unquote(module).new ['b', 'd'], [2, 4]

        assert odd_dict == Enum.filter dict, fn({_, v}, do: rem(v, 2) == 1)
        assert even_dict == Enum.filter dict, fn({_, v}, do: rem(v, 2) == 0)
        assert dict == Enum.filter dict, fn(x, do: x)
      end

      test :filter_with_match do
        dict = unquote(module).new ['a', 'b', 'c', 'd'], [1, 2, 3, 4]
        dict_2 = unquote(module).new ['a', 'b'], [1, 2]

        assert unquote(module).new({'a', 1}) == Enum.filter dict, match?({_, 1}, &1)
        assert dict_2 == Enum.filter dict, match?({_, v} when v < 3, &1)
        assert dict == Enum.filter dict, match?(_, &1)
      end

      test :filter_map do
        odd_dict = unquote(module).new [a: 1, b: 2, c: 3]
        even_dict = unquote(module).new [a: 2, b: 4, c: 6]

        assert unquote(module).new(b: 4) == Enum.filter_map(odd_dict,
                                               fn({_, v}, do: rem(v, 2) == 0),
                                               fn({k, v}, do: { k, v * 2 }))
        assert unquote(module).new(a: 4, b: 8, c: 12) == Enum.filter_map(even_dict,
                                                                  fn({_, v}, do: rem(v, 2) == 0),
                                                                  fn({k, v}, do: { k, v * 2 }))
      end

      test :reduce do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        assert 1 == Enum.reduce(unquote(module).new, 1, fn(x, acc, do: x + acc))
        assert 7 == Enum.reduce(dict, 1, fn({_, v}, acc, do: v + acc))
      end

      test :keyfind do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        assert {:b, 2} == Enum.keyfind dict, 2, 2
      end

      test :map do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        double_dict = unquote(module).new [a: 2, b: 4, c: 6]
        assert double_dict == Enum.map(dict, fn({k, v}) -> { k, v * 2 } end)
        assert unquote(module).new == Enum.map(unquote(module).new, fn(x) -> x * 2 end)
      end

      test :map_reduce do
        dict = unquote(module).new [a: 1, b: 2, c: 3]
        double_dict = unquote(module).new [a: 2, b: 4, c: 6]
        assert { double_dict, 7 } == Enum.map_reduce(dict, 1, fn({k, v}, acc, do: { {k, v * 2}, v + acc }))
        assert { unquote(module).new, 1 } == Enum.map_reduce(unquote(module).new, 1, fn(x, acc, do: { x * 2, x + acc }))
      end

      test :partition do
        dict = unquote(module).new [:a, :b, :c, :d, :e, :f, :g], [1, 2, 3, 4, 5, 6, 7]
        below_4 = unquote(module).new [{:a, 1}, {:b, 2}, {:c, 3}]
        above_4 = unquote(module).new [{:d, 4}, {:e, 5}, {:f, 6}, {:g, 7}]

        assert { below_4, above_4 } == Enum.partition(dict, fn({_k, v}, do: v < 4))
        assert { dict, unquote(module).new } == Enum.partition(dict, fn({_k, v}, do: v < 10))
      end
    end
  end
end

defmodule EnumTest.HashDict do
  require EnumTest.Dict.Common
  EnumTest.Dict.Common.__using__(HashDict)

  test :drop do
    assert_raise UndefinedFunctionError, fn ->
      Enum.drop HashDict.new, 5
    end
  end

  test :drop_while do
    assert_raise UndefinedFunctionError, fn ->
      Enum.drop_while HashDict.new, fn(x, do: x)
    end
  end

  test :join do
    assert_raise UndefinedFunctionError, fn ->
      Enum.join HashDict.new, ""
    end
  end

  test :split do
    assert_raise UndefinedFunctionError, fn ->
      Enum.split HashDict.new, 5
    end
  end

  test :split_with do
    assert_raise UndefinedFunctionError, fn ->
      Enum.split_with HashDict.new, fn(x, do: x)
    end
  end

  test :take do
    assert_raise UndefinedFunctionError, fn ->
      Enum.take HashDict.new, 5
    end
  end

  test :take_while do
    assert_raise UndefinedFunctionError, fn ->
      Enum.take_while HashDict.new, fn(x, do: x)
    end
  end
end

defmodule EnumTest.Orddict do
  require EnumTest.Dict.Common
  EnumTest.Dict.Common.__using__(Orddict)

  test :drop do
    dict = Orddict.new [:a, :b, :c], [1, 2, 3]
    short_dict = Orddict.new {:c, 3}
    assert short_dict == Enum.drop dict, 2
    assert Orddict.new == Enum.drop dict, 3
    assert Orddict.new == Enum.drop dict, 4
  end

  test :drop_while do
    dict = Orddict.new [:a, :b, :c], [1, 2, 3]
    short_dict = Orddict.new {:c, 3}
    assert short_dict == Enum.drop_while dict, fn({_k, v}, do: v < 3)
  end

  test :join do
    dict = Orddict.new [:a, :b, :c], [1, 2, 3]
    assert_raise UndefinedFunctionError, fn ->
      Enum.join dict, ","
    end
    assert_raise UndefinedFunctionError, fn ->
      Enum.join dict, ','
    end
  end

  test :split do
    dict = Orddict.new [:a, :b, :c], [1,2,3]
    assert { Orddict.new([a: 1]), Orddict.new([b: 2, c: 3]) } == Enum.split dict, 1
    assert { Orddict.new(), Orddict.new([a: 1, b: 2, c: 3]) } == Enum.split dict, 0
    assert { Orddict.new([a: 1, b: 2, c: 3]), Orddict.new() } == Enum.split dict, 3
    assert { Orddict.new([a: 1, b: 2, c: 3]), Orddict.new() } == Enum.split dict, 4
  end

  test :split_with do
    dict = Orddict.new [:a, :b, :c, :d], [1, 3, 2, 4]
    odd_dict = Orddict.new [:a, :b], [1, 3]
    even_dict = Orddict.new [:c, :d], [2, 4]
    assert { odd_dict, even_dict } == Enum.split_with dict, fn({_k, v}, do: rem(v, 2) == 0)
  end

  test :take do
    dict = Orddict.new [:a, :b, :c, :d], [1, 3, 2, 4]
    short_dict = Orddict.new [:a, :b], [1, 3]
    assert short_dict == Enum.take dict, 2
    assert Orddict.new == Enum.take dict, 0
    assert dict == Enum.take dict, 4
    assert dict == Enum.take dict, 5
  end

  test :take_while do
    dict = Orddict.new [:a, :b, :c, :d], [1, 3, 2, 4]
    short_dict = Orddict.new [:a, :b], [1, 3]
    assert short_dict == Enum.take_while dict, fn({_k, v}, do: rem(v, 2) == 1)
    assert dict == Enum.take_while dict, fn({_k, v}, do: v < 10)
    assert Orddict.new == Enum.take_while dict, fn({_k, v}, do: v < 1)
  end
end
