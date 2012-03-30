Code.require_file "../test_helper", __FILE__

defmodule Access.TupleTest do
  use ExUnit.Case

  test :integer do
    tuple = { :a, :b, :c }
    assert_equal :a, tuple[1]
    assert_equal :a, { :a, :b, :c }[1]
  end
end