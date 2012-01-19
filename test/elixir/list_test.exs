Code.require_file "../test_helper", __FILE__

defmodule ListTest do
  use ExUnit::Case

  def test_implicit_char_list_concat do
    'foobar' = 'foo' 'bar'
    'foobar' = 'foo' '#{:bar}'
  end

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

  def test_member? do
    true  = List.member? [1,2,3], 1
    false = List.member? [1,2,3], 0
    false = List.member? [], 0
  end

  def test_seq do
    [1,2,3] = List.seq(1,3)
    [1]     = List.seq(1,1)
  end

  def test_append_1 do
    [1,[2],3,4,5,6] = List.append [[1,[2],3], [4], [5,6]]
  end

  def test_append_2 do
    [1,[2],3,4,5] = List.append [1,[2],3], [4,5]
  end

  def test_reverse do
    [3,2,1] = List.reverse [1,2,3]
  end

  def test_uniq do
    [1,2,3] = List.uniq [1,2,3,2,1]
  end
end