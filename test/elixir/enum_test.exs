Code.require_file "../test_helper", __FILE__

defmodule EnumTest do
  use ExUnit::Case

  def test_map do
    [2,4,6] = Enum.map [1,2,3], fn(x) { x * 2 }
  end

  def test_each do
    [1,2,3] = Enum.each [1,2,3], fn(x) { put(:enum_test_each, x * 2) }
    6 = get(:enum_test_each)
  after:
    erase(:enum_test_each)
  end
end