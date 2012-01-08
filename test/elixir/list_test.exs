Code.require_file "../test_helper", __FILE__

defmodule ListTest do
  use ExUnit::Case

  def test_flatten do
    [1,2,3] = List.flatten([1,2,3])
    [1,2,3] = List.flatten([1,[2],3])
    [1,2,3] = List.flatten([[1,[2],3]])

    [] = List.flatten([])
    [] = List.flatten([[]])
  end

  def test_flatten_with_tail do
    [1,2,3,4,5] = List.flatten([1,2,3], [4,5])
    [1,2,3,4,5] = List.flatten([1,[2],3], [4,5])
    [1,2,3,4,5] = List.flatten([[1,[2],3]], [4,5])
  end
end