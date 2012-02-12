defexception ExUnit::AssertionError, message: "assertion failed"

defmodule ExUnit::Assertions do
  # FIXME: Make this works with double quoted strings
  def assert_included(base, container) do
    assert_included(base, container, "Expected #{inspect container} to include #{inspect base}")
  end

  def assert_included(base, container, message) do
    assert(Erlang.string.str(container, base) != 0, message)
  end

  def assert_equal(expected, received) do
    assert_equal(expected, received, "Expected #{inspect received} to be equal to #{inspect expected}")
  end

  def assert_equal(expected, received, message) do
    assert(expected == received, message)
  end

  def refute(not_expected) do
    refute(not_expected, "Expected #{inspect not_expected} to be false")
  end

  def refute(not_expected, message) do
    not assert(!not_expected, message)
  end

  def assert(expected) do
    assert(expected, "Expected #{inspect expected} to be true")
  end

  def assert(expected, message) do
    unless expected do
      raise ExUnit::AssertionError, message: message
    end
    true
  end
end
