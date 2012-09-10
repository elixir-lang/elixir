Code.require_file "../test_helper.exs", __FILE__

defmodule TupleTest do
  use ExUnit.Case, async: true

  test :elem do
    assert elem({ :a, :b, :c }, 1) == :b
  end

  test :setelem do
    assert setelem({ :a, :b, :c }, 1, :d) == { :a, :d, :c }
  end
end
