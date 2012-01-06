Code.require_file "../test_helper", __FILE__

defmodule EnumTest do
  use ExUnit::Case

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