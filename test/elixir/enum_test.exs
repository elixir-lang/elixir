Code.require_file "../test_helper", __FILE__

defmodule EnumTest do
  use ExUnit::Case

  def test_all? do
    true  = Enum.all? [2,4,6], fn(x) { rem(x, 2) == 0 }
    false = Enum.all? [2,3,4], fn(x) { rem(x, 2) == 0 }

    true  = Enum.all? [2,4,6]
    false = Enum.all? [2,nil,4]

    true  = Enum.all? []
  end

  def test_any? do
    false = Enum.any? [2,4,6], fn(x) { rem(x, 2) == 1 }
    true  = Enum.any? [2,3,4], fn(x) { rem(x, 2) == 1 }

    false = Enum.any? [false,false,false]
    true  = Enum.any? [false,true,false]

    false = Enum.any? []
  end

  def test_detect do
    nil = Enum.detect [2,4,6], fn(x) { rem(x, 2) == 1 }
    0   = Enum.detect [2,4,6], 0, fn(x) { rem(x, 2) == 1 }
    3   = Enum.detect [2,3,4], fn(x) { rem(x, 2) == 1 }
  end

  def test_detect_value do
    nil  = Enum.detect_value [2,4,6], fn(x) { rem(x, 2) == 1 }
    0    = Enum.detect_value [2,4,6], 0, fn(x) { rem(x, 2) == 1 }
    true = Enum.detect_value [2,3,4], fn(x) { rem(x, 2) == 1 }
  end

  def test_empty? do
    true  = Enum.empty? []
    false = Enum.empty? [1,2,3]
  end

  def test_each do
    [] = Enum.each [], fn(x) { x }

    [1,2,3] = Enum.each [1,2,3], fn(x) { put(:enum_test_each, x * 2) }
    6 = get(:enum_test_each)
  after:
    erase(:enum_test_each)
  end

  def test_foldl do
    1 = Enum.foldl [], 1, fn(x, acc) { x + acc }
    7 = Enum.foldl [1,2,3], 1, fn(x, acc) { x + acc }
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
    [] = Enum.map [], fn(x) { x * 2 }
    [2,4,6] = Enum.map [1,2,3], fn(x) { x * 2 }
  end

  def test_mapfoldl do
    { [], 1 } = Enum.mapfoldl [], 1, fn(x, acc) { { x * 2, x + acc } }
    { [2,4,6], 7 } = Enum.mapfoldl [1,2,3], 1, fn(x, acc) { { x * 2, x + acc } }
  end
end