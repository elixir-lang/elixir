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
  like `assert_in_delta` and `assert_raise` to easily handle other
  common cases as checking a float number or handling exceptions.
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
    translate_assertion(expected, fn ->
      quote do
        value = unquote(expected)
        assert value, "Expected #{inspect value} to be true"
      end
    end)
  end

  @doc """
  Refutes the `expected` value is true.

  `refute` in general tries to be smart and provide a good
  reporting whenever there is a failure.

  ## Examples

      refute false

  """
  defmacro refute(expected) do
    contents = translate_assertion({ :!, 0, [expected] }, fn ->
      quote do
        value = unquote(expected)
        assert !value, "Expected #{inspect value} to be false"
      end
    end)

    { :!, 0, [contents] }
  end

  ## START HELPERS

  defmacrop negation?(op) do
    quote do: (var!(op) == :! or var!(op) == :not)
  end

  defp translate_assertion({ :=, _, [_, _] } = expr, _else) do
    expr
  end

  defp translate_assertion({ :==, _, [left, right] }, _else) do
    assert_operator :==, left, right, "be equal to (==)"
  end

  defp translate_assertion({ :<, _, [left, right] }, _else) do
    assert_operator :<, left, right, "be less than"
  end

  defp translate_assertion({ :>, _, [left, right] }, _else) do
    assert_operator :>, left, right, "be more than"
  end

  defp translate_assertion({ :<=, _, [left, right] }, _else) do
    assert_operator :<=, left, right, "be less than or equal to"
  end

  defp translate_assertion({ :>=, _, [left, right] }, _else) do
    assert_operator :>=, left, right, "be more than or equal to"
  end

  defp translate_assertion({ :===, _, [left, right] }, _else) do
    assert_operator :===, left, right, "be equal to (===)"
  end

  defp translate_assertion({ :!==, _, [left, right] }, _else) do
    assert_operator :!==, left, right, "be not equal to (!==)"
  end

  defp translate_assertion({ :!=, _, [left, right] }, _else) do
    assert_operator :!=, left, right, "be not equal to (!=)"
  end

  defp translate_assertion({ :=~, _, [left, right] }, _else) do
    assert_operator :=~, left, right, "match (=~)"
  end

  defp translate_assertion({ :inlist, _, [left, right] }, _else) do
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert(List.member?(right, left), "Expected #{inspect left} to be in #{inspect right}")
    end
  end

  ## Negative versions

  defp translate_assertion({ op, _, [{ :=, _, [expected, received] }] }, _else) when negation?(op) do
    quote do
      try do
        unquote(expected) = x = unquote(received)
        flunk "Unexpected right side #{inspect x} match"
      rescue
        _ in [MatchError] -> true
      end
    end
  end

  defp translate_assertion({ op, _, [{ :=~, _, [left, right] }] }, _else) when negation?(op) do
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert(!(left =~ right), "Expected #{inspect left} to not match #{inspect right}")
    end
  end

  defp translate_assertion({ op, _, [{ :inlist, _, [left, right] }] }, _else) when negation?(op) do
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert(!List.member?(right, left), "Expected #{inspect left} to not be in #{inspect right}")
    end
  end

  ## Fallback

  defp translate_assertion(_expected, fallback) do
    fallback.()
  end

  defp assert_operator(operator, expected, actual, text) do
    quote do
      left  = unquote(expected)
      right = unquote(actual)
      assert unquote(operator).(left, right),
        "Expected #{inspect left} to #{unquote(text)} #{inspect right}"
    end
  end

  ## END HELPERS

  @doc """
  Asserts the `expected` value is true.
  If it fails, raises the given `message`.

  ## Examples

      assert false, "it will never be true"

  """
  def assert(expected, message) when is_binary(message) do
    unless expected, do: flunk message
    true
  end

  @doc """
  Asserts a message was received and is in the current process mailbox.
  The given `expected` content must to be a match pattern.

  Timeout is set to 0, so there is no waiting time.

  ## Examples

      self <- :hello
      assert_received :hello

  You can also match against specific patterns:

      self <- { :hello, "world" }
      assert_received { :hello, _ }

  """
  defmacro assert_received(expected, message // nil) do
    binary = Macro.to_binary(expected)

    quote do
      receive do
        unquote(expected) = received -> received
      after
        0 -> flunk unquote(message) || "Expected to have received message matching #{unquote binary}"
      end
    end
  end

  @doc """
  Asserts the `exception` is raised during `function` execution with
  the `expected_message`. Returns the rescued exception, fails otherwise.

  ## Examples

      assert_raise ArithmeticError, "bad argument in arithmetic expression", fn ->
        1 + "test"
      end
  """
  def assert_raise(exception, message, function) when is_binary(message) and is_function(function) do
    error = assert_raise(exception, function)
    assert error.message == message,
      "Expected #{inspect exception} to be raised with message #{inspect message}, got: #{error.message}"
    error
  end

  @doc """
  Asserts the `exception` is raised during `function` execution.
  Returns the rescued exception, fails otherwise.

  ## Examples

      assert_raise ArithmeticError, fn ->
        1 + "test"
      end

  """
  def assert_raise(exception, function) when is_function(function) do
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
          flunk "Expected exception #{inspect exception}, got #{inspect name} (#{error.message})"
        end
    end
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
  Asserts the given `expression` will throw a value.
  Returns the thrown value or fails otherwise.

  ## Examples

      assert catch_throw(throw 1) == 1

  """
  defmacro catch_throw(expression) do
    do_catch(:throw, expression)
  end

  @doc """
  Asserts the given `expression` will exit.
  Returns the exit status/message or fails otherwise.

  ## Examples

      assert catch_exit(exit 1) == 1

  """
  defmacro catch_exit(expression) do
    do_catch(:exit, expression)
  end

  @doc """
  Asserts the given `expression` will cause an error.
  Returns the error or fails otherwise.

  ## Examples

      assert catch_error(error 1) == 1

  """
  defmacro catch_error(expression) do
    do_catch(:error, expression)
  end

  defp do_catch(kind, expr) do
    quote do
      try do
        unquote(expr)
        flunk "Expected to catch #{unquote(kind)}, got nothing"
      rescue
        e in [ExUnit.AssertionError] -> raise(e)
      catch
        unquote(kind), what_we_got -> what_we_got
      end
    end
  end

  @doc """
  Asserts the `not_expected` value is nil or false.
  In case it is a truthy value, raises the given message.

  ## Examples

      refute true, "This will obviously fail"

  """
  def refute(not_expected, message) do
    not assert(!not_expected, message)
  end

  @doc """
  Asserts a message was not received (i.e. it is not in the current process mailbox).
  The `not_expected` contents must be a match pattern.

  Timeout is set to 0, so there is no waiting time.

  ## Examples

      self <- :hello
      refute_received :bye

  """
  defmacro refute_received(not_expected, message // nil) do
    binary = Macro.to_binary(not_expected)

    quote do
      receive do
        unquote(not_expected) = actual ->
          flunk unquote(message) || "Expected to not have received message matching #{unquote binary}, got #{inspect actual}"
      after
        0 -> false
      end
    end
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
  Fails with a message.

  ## Examples

      flunk "This should raise an error"

  """
  @spec flunk :: no_return
  @spec flunk(String.t) :: no_return
  def flunk(message // "Epic Fail!") do
    raise ExUnit.AssertionError, message: message
  end
end
