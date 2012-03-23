Code.require_file "../../test_helper", __FILE__

defmodule Kernel::QuoteTest::Hygiene do
  defmacro no_interference do
    quote do: a = 1
  end

  defmacro no_hygiene do
    quote hygiene: false do
      a = 1
    end
  end

  defmacro write_interference do
    quote do: var!(a) = 1
  end

  defmacro read_interference do
    quote do: 10 = var!(a)
  end
end

defmodule Kernel::QuoteTest do
  use ExUnit::Case
  import __MODULE__::Hygiene

  test :no_interference do
    a = 10
    no_interference
    assert_equal 10, a
  end

  test :no_hygiene do
    no_hygiene
    assert_equal 1, a
  end

  test :write_interference do
    write_interference
    assert_equal 1, a
  end

  test :read_interference do
    a = 10
    read_interference
  end

  test :list do
    assert_equal [1,2,3], quote do: [1,2,3]
  end

  test :tuple do
    assert_equal {:a,1}, quote do: { :a, 1 }
  end
end
