Code.require_file "../test_helper.exs", __FILE__

defmodule TupleTest do
  use ExUnit.Case, async: true

  test :elem do
    assert elem({ :a, :b, :c }, 1) == :b
  end

  test :setelem do
    assert setelem({ :a, :b, :c }, 1, :d) == { :a, :d, :c }
  end

  test :optional_comma do
    assert :{}.(1,) == { 1, }
    assert :{}.(1, 2, 3) == { 1, 2, 3, }
  end

  test :partial_application do
    assert ({ &1, 2 }).(1) == { 1, 2 }
    assert ({ &1, &2 }).(1, 2) == { 1, 2 }
    assert ({ &2, &1 }).(2, 1) == { 1, 2 }
  end
end