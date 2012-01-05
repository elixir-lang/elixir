Code.require_file "../test_helper", __FILE__

defmodule ListTest do
  use ExUnit::Case

  def test_join_with_bin do
    "1 = 2 = 3" = List.join([1,2,3], " = ")
    "1 = {2} = 3" = List.join([1,{ 2 },3], " = ")
  end

  def test_join_with_list do
    '1 = 2 = 3' = List.join([1,2,3], ' = ')
    '1 = {2} = 3' = List.join([1,{ 2 },3], ' = ')
  end
end