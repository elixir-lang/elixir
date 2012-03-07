defexception ExUnit::AssertionError, message: "assertion failed"

defmodule ExUnit::Assertions do
  @doc """
  Asserts the `expected` value matches `received`. Differently
  from `assert_equal`, `assert_match` uses underscore and
  therefore allows a developer to match against a specific
  part of the `received` structure.

  ## Examples

      assert_match { 1, _, 3 }, { 1, 2, 3 }

  """
  defmacro assert_match(expected, received) do
    quote do
      try do
        unquote(expected) = unquote(received)
        true
      rescue: x in [MatchError]
        raise ExUnit::AssertionError, message: x.message
      end
    end
  end

  @doc """
  Asserts the `base` value is member of `container`.

  ## Examples

      assert_member 'foo', 'foobar'

  """
  def assert_member(base, container) do
    assert_member(base, container, "Expected #{inspect container} to include #{inspect base}")
  end

  def assert_member(base, container, message) do
    assert(Erlang.string.str(container, base) != 0, message)
  end

  @doc """
  Asserts the `expected` value is equal to `received`.

  ## Examples

      assert_equal 0, 0

  """
  def assert_equal(expected, received) do
    assert_equal(expected, received, "Expected #{inspect received} to be equal to #{inspect expected}")
  end

  def assert_equal(expected, received, message) do
    assert(expected == received, message)
  end

  @doc """
  Asserts the `exception` is raised during `function` execution with the expected message.

  ## Examples

      assert_raises ArithmeticError, "bad argument in arithmetic expression", fn ->
        1 + "test"
      end
  """
  def assert_raises(exception, expected_message, function) do
    error = assert_raises(exception, function)
    assert_equal expected_message, error.message
  end

  @doc """
  Asserts the `exception` is raised during `function` execution.

  ## Examples

      assert_raises ArithmeticError, fn ->
        1 + "test"
      end

  """
  def assert_raises(exception, function) do
    function.()
    flunk "Expected #{exception} exception but nothing was raised"
  rescue: error in [exception]
    error
  rescue: error
    name = error.__record__(:name)

    if name == ExUnit::AssertionError do
      raise(error)
    else:
      flunk "Expected exception #{exception}, got #{name}"
    end
  end

  @doc """
  Asserts the `expression` with binary operator.

  ## Examples

      assert_operator 2 > 1
      assert_operator 1 < 2

  """
  defmacro assert_operator(expression) do
    {operator, _, [left, right]} = expression
    quote do
      left  = unquote(left)
      right = unquote(right)
      message = "Expected #{left} to be #{unquote(operator)} #{right}"
      assert unquote(operator).(left, right), message
    end
  end

  @doc """
  Asserts the `expression` with binary operator and uses the expected `message.

  ## Examples

      assert_operator 2 > 1, "2 is always greater than 1"

  """
  defmacro assert_operator(expression, message) do
    {operator, _, [left, right]} = expression
    quote do
      assert unquote(operator).(unquote(left), unquote(right)), unquote(message)
    end
  end

  @doc """
  Asserts the `not_expected` value is false.

  ## Examples

      refute false

  """
  def refute(not_expected) do
    refute(not_expected, "Expected #{inspect not_expected} to be false")
  end

  def refute(not_expected, message) do
    not assert(!not_expected, message)
  end

  @doc """
  Asserts the `expected` value is not equal to `received`.

  ## Examples

      refute_equal 0, 1

  """
  def refute_equal(expected, received) do
    refute_equal(expected, received, "Expected #{inspect received} to not be equal to #{inspect expected}")
  end

  def refute_equal(expected, received, message) do
    refute(expected == received, message)
  end

  @doc """
  Fails with a message.

  ## Examples

      flunk "This should raise an error"

  """
  def flunk do
    flunk "Epic Fail!"
  end

  def flunk(message) do
    assert false, message
  end

  @doc """
  Asserts the `expected` value is true.

  ## Examples

      assert true

  """
  def assert(expected) do
    assert(expected, "Expected #{inspect expected} to be true")
  end

  def assert(expected, message) when is_binary(message) do
    unless expected, do:
      raise ExUnit::AssertionError, message: message
    true
  end
end
