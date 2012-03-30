Code.require_file "../test_helper", __FILE__

defmodule Access.TupleTest do
  use ExUnit.Case

  test :literal do
    assert_equal :a, { :a, :b, :c }[1]
  end

  test :positive_integer do
    tuple = { :a, :b, :c }
    assert_equal nil, tuple[0]
    assert_equal :a,  tuple[1]
    assert_equal :b,  tuple[2]
    assert_equal :c,  tuple[3]
    assert_equal nil, tuple[4]
  end

  test :negative_integer do
    tuple = { :a, :b, :c }
    assert_equal nil, tuple[-4]
    assert_equal :a,  tuple[-3]
    assert_equal :b,  tuple[-2]
    assert_equal :c,  tuple[-1]
  end

  test :access do
    assert_equal :c, Tuple.access { :a, :b, :c }, -1
  end
end