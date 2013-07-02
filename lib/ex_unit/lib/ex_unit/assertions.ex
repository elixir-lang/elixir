defexception ExUnit.AssertionError,  message: "assertion failed"

defexception ExUnit.ExpectationError, expected: nil, actual: nil, reason: "",
    negation: false, prelude: "Expected", description: nil do
  def message(exception) do
    if desc = exception.description do
      "#{exception.prelude} #{desc} #{exception.full_reason} " <>
        "#{exception.expected}. Instead got #{exception.actual}"
    else
      "#{exception.prelude} #{exception.expected} " <>
        "#{exception.full_reason} #{exception.actual}"
    end
  end

  def full_reason(exception) do
    "to" <> if(exception.negation, do: " not ", else: " ") <> exception.reason
  end
end

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

        unless value do
          raise ExUnit.ExpectationError,
            description: unquote(Macro.to_string(expected)),
            reason: "be",
            expected: "true",
            actual: inspect(value)
        end

        value
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
    contents = translate_assertion({ :!, [], [expected] }, fn ->
      quote do
        value = unquote(expected)

        if value do
          raise ExUnit.ExpectationError,
            description: unquote(Macro.to_string(expected)),
            reason: "be",
            expected: "false",
            actual: inspect(value)
        end

        true
      end
    end)

    { :!, [], [contents] }
  end

  ## START HELPERS

  defp translate_assertion({ :=, _, [left, right] }, _else) do
    quote do
      right = unquote(right)
      case right do
        unquote(left) ->
          right
        _ ->
          raise ExUnit.ExpectationError,
            expected: inspect(right),
            actual: unquote(Macro.to_string(left)),
            reason: "match pattern (=)"
      end
    end
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

  defp translate_assertion({ :in, _, [left, right] }, _else) do
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert Enum.member?(right, left), left, right, reason: "be in"
    end
  end

  ## Negative versions

  defp translate_assertion({ :!, _, [{ :=, _, [left, right] }] }, _else) do
    quote do
      right = unquote(right)
      case right do
        unquote(left) ->
          raise ExUnit.ExpectationError,
            expected: inspect(right),
            actual: unquote(Macro.to_string(left)),
            reason: "match pattern (=)",
            negation: true
          _ ->
            nil
      end
    end
  end

  defp translate_assertion({ :!, _, [{ :=~, _, [left, right] }] }, _else) do
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert !(left =~ right), left, right, reason: "match (=~)", negation: true
    end
  end

  defp translate_assertion({ negation, _, [{ :in, _, [left, right] }] }, _else) when negation in [:!, :not] do
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert !Enum.member?(right, left), left, right, reason: "be in", negation: true
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
      assert unquote(operator)(left, right), left, right, reason: unquote(text)
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
    unless expected, do: raise(ExUnit.AssertionError, message: message)
    true
  end

  @doc """
  Asserts the `expected` value is true.
  If it fails, it raises an expectation error
  using the given expected and actual values.

  ## Examples

      assert this > that, this, that, reason: "more than"

  """
  def assert(value, expected, actual, opts) do
    unless value do
      raise ExUnit.ExpectationError,
        Keyword.merge([expected: inspect(expected), actual: inspect(actual)], opts)
    end
    true
  end

  @doc """
  Assets a message was or is going to be received. Differently from
  `assert_received`, it has a default timeout time of 100 miliseconds.

  The given `expected` content must be a pattern.

  ## Examples

      assert_receive :hello

  Asserts against a larger timeout:

      assert_receive :hello, 20_000

  You can also match against specific patterns:

      assert_receive { :hello, _ }

  """
  defmacro assert_receive(expected, timeout // 100, message // nil) do
    do_assert_receive(expected, timeout, message)
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
    do_assert_receive(expected, 0, message)
  end

  defp do_assert_receive(expected, timeout, message) do
    binary = Macro.to_string(expected)

    quote do
      receive do
        unquote(expected) = received -> received
      after
        unquote(timeout) ->
          flunk unquote(message) || "Expected to have received message matching #{unquote binary}"
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
  def assert_raise(exception, message, function) when is_function(function) do
    error = assert_raise(exception, function)

    is_match = case message do
      re  when is_regex(re)   -> error.message =~ re
      bin when is_binary(bin) -> error.message == bin
    end

    assert is_match, message, error.message,
      prelude: "Expected error message", reason: "match"

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

        if name in [ExUnit.AssertionError, ExUnit.ExpectationError] do
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
        e in [ExUnit.AssertionError, ExUnit.ExpectationError] -> raise(e)
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
  Asserts a message was not received and won't be during
  a timeout value.

  The `not_expected` contents must be a match pattern.

  ## Examples

      refute_receive :bye

  Refute received with a explicit timeout:

      refute_receive :bye, 1000

  """
  defmacro refute_receive(not_expected, timeout // 100, message // nil) do
    do_refute_receive(not_expected, timeout, message)
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
    do_refute_receive(not_expected, 0, message)
  end

  defp do_refute_receive(not_expected, timeout, message) do
    binary = Macro.to_string(not_expected)

    quote do
      receive do
        unquote(not_expected) = actual ->
          flunk unquote(message) || "Expected to not have received message matching #{unquote binary}, got #{inspect actual}"
      after
        unquote(timeout) -> false
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
  def flunk(message // "Flunked!") do
    raise ExUnit.AssertionError, message: message
  end
end
