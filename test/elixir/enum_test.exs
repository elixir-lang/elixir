Code.require_file "../test_helper", __FILE__

defmodule EnumTest do
  use ExUnit::Case

  def test_map do
    [2,4,6] = Enum.map([1,2,3], fn(x) { x * 2 })
  end
end