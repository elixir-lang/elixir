defexception ExUnit.AssertionError, message: "assertion failed"

defmodule ExUnit.Assertions do
  @moduledoc """
  This module contains a set of assertions functions that are
  imported by default into your test cases.

  In general, a developer will want to use the general
  `assert` macro in tests. The macro tries to be smart
  and provide good reporting whenever there is a failure.
  For example, `assert some_fun() == 10` will fail (assuming
  `some_fun()` returns 13):

      Expected 10 to be equal to 13

  This module also provides other small convenient functions
  like `assert_match`, `assert_member` and `assert_raise` to
  easily handle other common cases as, respectively, asserting
  if two terms match, asserting if an item belongs to a list or
  if a function raises an exception.
  """

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
    unless expected do
      raise ExUnit.AssertionError, message: message
    end
    true
  end

  ## START HELPERS

  defmacrop negation?(op) do
    quote do: (var!(op) == :! or var!(op) == :not)
  end

  defp translate_assertion({ :==, _, [left, right] }) do
    { expected, actual } = guess_expected_and_actual(left, right)
    assert_operator :==, expected, actual, "equal to (==)"
  end

  defp translate_assertion({ :<, _, [left, right] }) do
    assert_operator :<, left, right, "less than"
  end

  defp translate_assertion({ :>, _, [left, right] }) do
    assert_operator :>, left, right, "more than"
  end

  defp translate_assertion({ :<=, _, [left, right] }) do
    assert_operator :<=, left, right, "less than or equal to"
  end

  defp translate_assertion({ :>=, _, [left, right] }) do
    assert_operator :>=, left, right, "more than or equal to"
  end

  defp translate_assertion({ :===, _, [left, right] }) do
    { expected, actual } = guess_expected_and_actual(left, right)
    assert_operator :===, expected, actual, "equal to (===)"
  end

  defp translate_assertion({ :!==, _, [left, right] }) do
    { expected, actual } = guess_expected_and_actual(left, right)
    assert_operator :!==, expected, actual, "not equal to (!==)"
  end

  defp translate_assertion({ :!=, _, [left, right] }) do
    { expected, actual } = guess_expected_and_actual(left, right)
    assert_operator :!=, expected, actual, "not equal to (!=)"
  end

  defp translate_assertion({ :access, _, [container, base] }) do
    quote do
      container = unquote(container)
      base = unquote(base)
      assert(container[base], "Expected #{inspect base} to access #{inspect container}")
    end
  end

  defp translate_assertion({ op, _, [{ :access, _, [container, base] }] }) when negation?(op) do
    quote do
      container = unquote(container)
      base = unquote(base)
      assert(!container[base], "Expected #{inspect base} to not access #{inspect container}")
    end
  end

  defp translate_assertion(expected) do
    quote do
      value = unquote(expected)
      assert value, "Expected #{inspect value} to be true"
    end
  end

  defp guess_expected_and_actual(left, right) do
    case right do
      { fun, i, _ } when is_integer(i) and (fun != :<<>> or fun != :{}) ->
        { left, right }
      _ ->
        { right, left }
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

  ## END HELPERS

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
      rescue
        x in [MatchError] ->
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
  Asserts the `exception` is raised during `function` execution with the expected message.

  ## Examples

      assert_raise ArithmeticError, "bad argument in arithmetic expression", fn ->
        1 + "test"
      end
  """
  def assert_raise(exception, expected_message, function) do
    error = assert_raise(exception, function)
    assert error.message == expected_message
  end

  @doc """
  Asserts the `exception` is raised during `function` execution.

  ## Examples

      assert_raise ArithmeticError, fn ->
        1 + "test"
      end

  """
  def assert_raise(exception, function) do
    try do
      function.()
      flunk "Expected #{inspect exception} exception but nothing was raised"
    rescue
      error in [exception] -> error
      error ->
        name = error.__record__(:name)

        if name == ExUnit.AssertionError do
          raise(error)
        else
          flunk "Expected exception #{inspect exception}, got #{inspect name}"
        end
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
    try do
      function.()
      flunk "Expected #{expected_type} #{inspect expected_value}, got nothing"
    catch
      ^expected_type, ^expected_value ->
        expected_value
      ^expected_type, actual_value ->
        flunk "Expected #{expected_type} #{inspect expected_value}, got #{inspect actual_value}"
    end
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
      rescue
        x in [MatchError] -> true
      end
    end
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
  Fails with a message.

  ## Examples

      flunk "This should raise an error"

  """
  def flunk(message // "Epic Fail!") do
    assert false, message
  end
end
