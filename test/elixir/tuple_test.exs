defmodule TupleTest do
  use ExUnit::Case

  def test_elem do
    :b = elem({ :a, :b, :c }, 2)
  end

  def test_setelem do
    { :a, :d, :c } = setelem({ :a, :b, :c }, 2, :d)
  end
end