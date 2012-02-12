Code.require_file "../test_helper", __FILE__

defmodule TupleTest do
  use ExUnit::Case

  def test_elem do
    assert_equal :b, elem({ :a, :b, :c }, 2)
  end

  def test_setelem do
    assert_equal { :a, :d, :c }, setelem({ :a, :b, :c }, 2, :d)
  end
end
