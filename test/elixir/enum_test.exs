Code.require_file "../test_helper", __FILE__

defmodule EnumTest do
  use ExUnit::Case

  def test_all? do
    true  = Enum.all? [2,4,6], fn(x, do: rem(x, 2) == 0)
    false = Enum.all? [2,3,4], fn(x, do: rem(x, 2) == 0)

    true  = Enum.all? [2,4,6]
    false = Enum.all? [2,nil,4]

    true  = Enum.all? []
  end

  def test_any? do
    false = Enum.any? [2,4,6], fn(x, do: rem(x, 2) == 1)
    true  = Enum.any? [2,3,4], fn(x, do: rem(x, 2) == 1)

    false = Enum.any? [false,false,false]
    true  = Enum.any? [false,true,false]

    false = Enum.any? []
  end

  def test_detect do
    nil = Enum.detect [2,4,6], fn(x, do: rem(x, 2) == 1)
    0   = Enum.detect [2,4,6], 0, fn(x, do: rem(x, 2) == 1)
    3   = Enum.detect [2,3,4], fn(x, do: rem(x, 2) == 1)
  end

  def test_detect_value do
    nil  = Enum.detect_value [2,4,6], fn(x, do: rem(x, 2) == 1)
    0    = Enum.detect_value [2,4,6], 0, fn(x, do: rem(x, 2) == 1)
    true = Enum.detect_value [2,3,4], fn(x, do: rem(x, 2) == 1)
  end

  def test_empty? do
    true  = Enum.empty? []
    false = Enum.empty? [1,2,3]
  end

  def test_each do
    [] = Enum.each [], fn(x, do: x)

    [1,2,3] = Enum.each [1,2,3], fn(x, do: Process.put(:enum_test_each, x * 2))
    6 = Process.get(:enum_test_each)
  after:
    Process.erase(:enum_test_each)
  end

  def test_filter do
    [2]     = Enum.filter [1,2,3], fn(x, do: rem(x, 2) == 0)
    [2,4,6] = Enum.filter [2,4,6], fn(x, do: rem(x, 2) == 0)
  end

  def test_foldl do
    1 = Enum.foldl [], 1, fn(x, acc, do: x + acc)
    7 = Enum.foldl [1,2,3], 1, fn(x, acc, do: x + acc)
  end

  def test_join_with_bin do
    ""            = Enum.join [], " = "
    "1 = 2 = 3"   = Enum.join [1,2,3], " = "
    "1 = {2} = 3" = Enum.join [1,{ 2 },3], " = "
  end

  def test_join_with_list do
    ''            = Enum.join [], ' = '
    '1 = 2 = 3'   = Enum.join [1,2,3], ' = '
    '1 = {2} = 3' = Enum.join [1,{ 2 },3], ' = '
  end

  def test_map do
    [] = Enum.map [], fn(x) -> x * 2 end
    [2,4,6] = Enum.map [1,2,3], fn(x) -> x * 2 end
  end

  def test_mapfoldl do
    { [], 1 } = Enum.mapfoldl [], 1, fn(x, acc, do: { x * 2, x + acc })
    { [2,4,6], 7 } = Enum.mapfoldl [1,2,3], 1, fn(x, acc, do: { x * 2, x + acc })
  end

  def test_times_with_arity_0 do
    put(:times_with_arity, nil)
    0 = Enum.times 0, fn do: Process.put(:times_with_arity, :ok)
    nil = Process.get(:times_with_arity)
    3 = Enum.times 3, fn do: Process.put(:times_with_arity, :ok)
    :ok = Process.get(:times_with_arity)
  after:
    Process.erase(:times_with_arity)
  end

  def test_times_with_arity_1 do
    5 = Enum.times 5, fn(x, do: Process.put(:times_with_arity, x))
    5 = Process.get(:times_with_arity)
  after:
    Process.erase(:times_with_arity)
  end

  def test_times_with_arity_2 do
    15 = Enum.times 5, 0, fn(acc, x) -> acc + x end
  end

  def test_enum_for do
    [{1, 3}, {1, 4}, {2, 3}, {2, 4}] = Enum.__for__ [[1,2],[3,4]], fn(acc, y, x) -> [{x,y}|acc] end
  end

  def test_for do
    [{1, 3}, {1, 4}, {2, 3}, {2, 4}] = for x in [1,2], y in [3,4], do: {x,y}

    lists = [1,{1,2},2,{2,1}]
    [{1, 3}, {1, 4}, {2, 3}, {2, 4}] = for {x,_} in lists, y in [3,4], do: {x,y}
    [{1, 3}, {1, 4}] = for {x,_} in lists, y in [3,4], x == 1, do: {x,y}
  end
end
