defexception ExUnit.AssertionError, message: "assertion failed"

defmodule ExUnit.Assertions do
  @doc """
  Asserts the `expected` value is true.

  `assert` in general tries to be smart and provide a good
  reporting whenever there is a failure. For example,
  `assert 10 > 15` is going to fail with a message:

      Expected 10 to be more than 15

  ## Examples

      assert true

  """
  defmacro assert(expected) do
    translate_assertion(expected)
  end

  @doc """
  Asserts the `expected` value is true.
  If it fails, raises the given message.

  ## Examples

      assert false, "it will never be true"

  """
  def assert(expected, message) when is_binary(message) do
    unless expected, do:
      raise ExUnit.AssertionError, message: message
    true
  end

  @doc """
  Asserts the `expected` value matches `received`. This relies
  on Elixir's pattern match instead of simply comparing terms.

  ## Examples

      assert_match { 1, _, 3 }, { 1, 2, 3 }

  """
  defmacro assert_match(expected, received) do
    quote do
      try do
        unquote(expected) = unquote(received)
        true
      rescue: x in [MatchError]
        raise ExUnit.AssertionError, message: x.message
      end
    end
  end

  @doc """
  Asserts the value is a member of the given enumerable.
  Used to check if an item belongs to a list.

  ## Examples

      assert_member "foo", ["foo", "bar"]

  """
  def assert_member(base, container, message // nil) do
    message = message || "Expected #{inspect container} to include #{inspect base}"
    assert(Enum.find(container, &1 == base), message)
  end

  @doc """
  Asserts the value is a valid accessor of the container.

  ## Examples

      assert_access %r"foo", "foobar"

  """
  def assert_access(base, container, message // nil) do
    message = message || "Expected #{inspect base} to access #{inspect container}"
    assert(container[base], message)
  end

  @doc """
  Asserts the `expected` value is equal to `received`.

  ## Examples

      assert_equal 0, 0

  """
  def assert_equal(expected, received, message // nil) do
    message = message || "Expected #{inspect received} to be equal to #{inspect expected}"
    assert(expected == received, message)
  end

  @doc """
  Asserts the `exception` is raised during `function` execution with the expected message.

  ## Examples

      assert_raise ArithmeticError, "bad argument in arithmetic expression", fn ->
        1 + "test"
      end
  """
  def assert_raise(exception, expected_message, function) do
    error = assert_raise(exception, function)
    assert_equal expected_message, error.message
  end

  @doc """
  Asserts the `exception` is raised during `function` execution.

  ## Examples

      assert_raise ArithmeticError, fn ->
        1 + "test"
      end

  """
  def assert_raise(exception, function) do
    function.()
    flunk "Expected #{inspect exception} exception but nothing was raised"
  rescue: error in [exception]
    error
  rescue: error
    name = error.__record__(:name)

    if name == ExUnit.AssertionError do
      raise(error)
    else:
      flunk "Expected exception #{inspect exception}, got #{inspect name}"
    end
  end

  @doc """
  Asserts the `enum` collection is empty.

  ## Examples

      assert_empty []
      assert_empty [1, 2]

  """
  def assert_empty(enum, message // nil) do
    message = message || "Expected #{inspect enum} to be empty"
    assert Enum.empty?(enum), message
  end

  @doc """
  Asserts the `value` is nil.

  """
  def assert_nil(value, message // nil) do
    message = message || "Expected #{inspect value} to be nil"
    assert value == nil, message
  end

  @doc """
  Asserts the `expected` and `received` are within `delta`.

  ## Examples

      assert_in_delta 1.1, 1.5, 0.2
      assert_in_delta 10, 15, 4

  """
  def assert_in_delta(expected, received, delta, message // nil) do
    diff = abs(expected - received)
    message = message ||
      "Expected |#{inspect expected} - #{inspect received}| (#{inspect diff}) to be < #{inspect delta}"
    assert diff < delta, message
  end

  @doc """
  Asserts the throw `expected` during `function` execution.

  ## Examples

      assert_throw 1, fn ->
        throw 1
      end

  """
  def assert_throw(expected, function) do
    assert_catch(:throw, expected, function)
  end

  @doc """
  Asserts the exit `expected` during `function` execution.

  ## Examples

      assert_exit 1, fn ->
        exit 1
      end

  """
  def assert_exit(expected, function) do
    assert_catch(:exit, expected, function)
  end

  @doc """
  Asserts the error `expected` during `function` execution.

  ## Examples

      assert_error :function_clause, fn ->
        List.flatten(1)
      end

  """
  def assert_error(expected, function) do
    assert_catch(:error, expected, function)
  end

  defp assert_catch(expected_type, expected_value, function) do
    function.()
    flunk "Expected #{expected_type} #{inspect expected_value}, got nothing"
  catch: ^expected_type, ^expected_value
    expected_value
  catch: ^expected_type, actual_value
    flunk "Expected #{expected_type} #{inspect expected_value}, got #{inspect actual_value}"
  end

  @doc """
  Asserts the `not_expected` value is false.

  ## Examples

      refute false

  """
  def refute(not_expected, message // nil) do
    message = message || "Expected #{inspect not_expected} to be false"
    not assert(!not_expected, message)
  end

  @doc """
  Assets the `expected` value does not match `received`. This uses
  Elixir's pattern matching instead of simply comparing terms.

  ## Examples

      refute_match { 1, _, 3 }, { 1, 2, 3 }

  """
  defmacro refute_match(expected, received) do
    quote do
      try do
        unquote(expected) = unquote(received)
        flunk "Unexpected right side #{inspect unquote(received)} match"
      rescue: x in [MatchError]
        true
      end
    end
  end

  @doc """
  Asserts the `expected` value is not equal to `received`.

  ## Examples

      refute_equal 0, 1

  """
  def refute_equal(expected, received, message // nil) do
    message = message || "Expected #{inspect received} to not be equal to #{inspect expected}"
    refute(expected == received, message)
  end

  @doc """
  Asserts the `enum` collection is not empty.

  ## Examples

      refute_empty []
      refute_empty [1, 2]

  """
  def refute_empty(enum, message // nil) do
    message = message || "Expected #{inspect enum} to not be empty"
    refute Enum.empty?(enum), message
  end

  @doc """
  Asserts the `value` is not nil.
  """
  def refute_nil(value, message // nil) do
    message = message || "Expected #{inspect value} to not be nil"
    refute value == nil, message
  end

  @doc """
  Asserts the `expected` and `received` are not within `delta`.

  ## Examples

      refute_in_delta 1.1, 1.2, 0.2
      refute_in_delta 10, 11, 2

  """
  def refute_in_delta(expected, received, delta, message // nil) do
    diff = abs(expected - received)
    message = message ||
      "Expected |#{inspect expected} - #{inspect received}| (#{inspect diff}) to not be < #{inspect delta}"
    refute diff < delta, message
  end

  @doc """
  Asserts the value is not a member of the given enumerable.
  Used to check if an item belongs to a list.

  ## Examples

      refute_member "baz", ["foo", "bar"]

  """
  def refute_member(base, container, message // nil) do
    message = message || "Expected #{inspect container} to not include #{inspect base}"
    refute(Enum.find(container, &1 == base), message)
  end

  @doc """
  Asserts the value is not a valid accessor of the container.

  ## Examples

      refute_access %r"baz", "foobar"

  """
  def refute_access(base, container, message // nil) do
    message = message || "Expected #{inspect base} to not access #{inspect container}"
    refute(container[base], message)
  end

  @doc """
  Fails with a message.

  ## Examples

      flunk "This should raise an error"

  """
  def flunk(message // "Epic Fail!") do
    assert false, message
  end

  ## Helpers

  defp translate_assertion({ :==, _, [expected, actual] }) do
    assert_operator :==, expected, actual, "equal to (==)"
  end

  defp translate_assertion({ :<, _, [expected, actual] }) do
    assert_operator :<, expected, actual, "less than"
  end

  defp translate_assertion({ :>, _, [expected, actual] }) do
    assert_operator :>, expected, actual, "more than"
  end

  defp translate_assertion({ :<=, _, [expected, actual] }) do
    assert_operator :<=, expected, actual, "less than or equal to"
  end

  defp translate_assertion({ :>=, _, [expected, actual] }) do
    assert_operator :>=, expected, actual, "more than or equal to"
  end

  defp translate_assertion({ :===, _, [expected, actual] }) do
    assert_operator :===, expected, actual, "equal to (===)"
  end

  defp translate_assertion({ :!==, _, [expected, actual] }) do
    assert_operator :!==, expected, actual, "not equal to (!==)"
  end

  defp translate_assertion({ :!=, _, [expected, actual] }) do
    assert_operator :!=, expected, actual, "not equal to (!=)"
  end

  defp translate_assertion(expected) do
    quote do
      value = unquote(expected)
      assert value, "Expected #{inspect value} to be true"
    end
  end

  defp assert_operator(operator, expected, actual, text) do
    quote do
      left  = unquote(expected)
      right = unquote(actual)
      assert unquote(operator).(left, right),
        "Expected #{inspect left} to be #{unquote(text)} #{inspect right}"
    end
  end
end
