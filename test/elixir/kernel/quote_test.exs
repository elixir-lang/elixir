Code.require_file "../../test_helper", __FILE__

defmodule Kernel::QuoteTest::Hygiene do
  defmacro no_interference do
    quote { a = 1 }
  end

  defmacro write_interference do
    quote { var!(a) = 1 }
  end

  defmacro read_interference do
    quote { 10 = var!(a) }
  end
end

defmodule Kernel::QuoteTest do
  use ExUnit::Case
  import __MODULE__::Hygiene

  def test_no_interference do
    a = 10
    no_interference
    10 = a
  end

  def test_write_interference do
    write_interference
    1 = a
  end

  def test_read_interference do
    a = 10
    read_interference
  end
end