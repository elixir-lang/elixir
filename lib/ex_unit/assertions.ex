defexception ExUnit::AssertionError, message: "assertion failed"

defmodule ExUnit::Assertions do
  def assert_included(base, container) do
    if Erlang.string.str(container, base) == 0 do
      raise ExUnit::AssertionError, message: "Expected #{inspect container} to include #{inspect base}"
    end
  end

  def assert_equal(expected, received) do
    if expected != received do
      raise ExUnit::AssertionError, message: "Expected #{inspect received} to be equal to #{inspect expected}"
    end
  end

  def assert(expected) do
    unless expected do
      raise ExUnit::AssertionError, message: "Expected #{inspect expected} to be true"
    end
  end

  def refute(expected) do
    if expected do
      raise ExUnit::AssertionError, message: "Expected #{inspect expected} to be false"
    end
  end
end
