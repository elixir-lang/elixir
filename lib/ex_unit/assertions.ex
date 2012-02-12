defexception ExUnit::AssertionError, message: "assertion failed"

defmodule ExUnit::Assertions do
  # Asserts the `expected` value matches `received`. Differently
  # from `assert_equal`, `assert_match` uses underscore and
  # therefore allows a developer to match against a specific
  # part of the `received` structure.
  #
  # ## Examples
  #
  #     assert_match { 1, _, 3 }, { 1, 2, 3 }
  #
  defmacro assert_match(expected, received) do
    escaped = ExUnit::Escaper.escape(expected)
    quote do
      value = unquote(received)
      assert match?(unquote(expected), value),
        "Expected #{inspect value} to match #{inspect unquote(escaped)}"
    end
  end

  # Asserts the `base` value is member of `container`.
  #
  # ## Examples
  #
  #     assert_member 'foo', 'foobar'
  #
  def assert_member(base, container) do
    assert_member(base, container, "Expected #{inspect container} to include #{inspect base}")
  end

  def assert_member(base, container, message) do
    assert(Erlang.string.str(container, base) != 0, message)
  end

  # Asserts the `expected` value is equal to `received`.
  #
  # ## Examples
  #
  #     assert_equal 0, 0
  #
  def assert_equal(expected, received) do
    assert_equal(expected, received, "Expected #{inspect received} to be equal to #{inspect expected}")
  end

  def assert_equal(expected, received, message) do
    assert(expected == received, message)
  end

  # Asserts the `not_expected` value is false.
  #
  # ## Examples
  #
  #     refute false
  #
  def refute(not_expected) do
    refute(not_expected, "Expected #{inspect not_expected} to be false")
  end

  def refute(not_expected, message) do
    not assert(!not_expected, message)
  end

  # Fails with a message.
  #
  # ## Examples
  #
  #     flunk "This should raise an error"
  #
  def flunk do
    flunk "Epic Fail!"
  end

  def flunk(message) do
    assert false, message
  end

  # Asserts the `expected` value is true.
  #
  # ## Examples
  #
  #     assert true
  #
  def assert(expected) do
    assert(expected, "Expected #{inspect expected} to be true")
  end

  def assert(expected, message) when is_binary(message) do
    unless expected, do:
      raise ExUnit::AssertionError, message: message
    true
  end
end
