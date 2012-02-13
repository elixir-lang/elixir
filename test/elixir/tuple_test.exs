Code.require_file "../test_helper", __FILE__

defmodule TupleTest do
  use ExUnit::Case

  test :elem do
    assert_equal :b, elem({ :a, :b, :c }, 2)
  end

  test :setelem do
    assert_equal { :a, :d, :c }, setelem({ :a, :b, :c }, 2, :d)
  end
end
