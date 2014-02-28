defmodule ExUnit.NoValueSupplied do
  def no_value, do: {:__no__, :__meaningful__, :__value__}
end

defexception ExUnit.AssertionError, [
                    left:      ExUnit.NoValueSupplied.no_value,
                    right:     ExUnit.NoValueSupplied.no_value,
                    value:     ExUnit.NoValueSupplied.no_value,
                    message:   ExUnit.NoValueSupplied.no_value,
                    operator:  ExUnit.NoValueSupplied.no_value,
                    expr:      "missing failing expression" ] do

end

defmodule ExUnit.Assertions do
  @moduledoc """
  This module contains a set of assertion functions that are
  imported by default into your test cases.

  In general, a developer will want to use the general
  `assert` macro in tests. This macro tries to be smart
  and provide good reporting whenever there is a failure.
  For example, `assert some_fun() == 10` will fail (assuming
  `some_fun()` returns 13):

     Comparison (using ==) failed in:
     code: some_fun() == 10
     lhs:  13
     rhs:  10

  This module also provides other convenience functions
  like `assert_in_delta` and `assert_raise` to easily handle other
  common cases such as checking a floating point number or handling exceptions.
  """

  @doc """
  Asserts it's argument is true.

  `assert` tries to be smart and provide good
  reporting whenever there is a failure. In particular, if
  given a match expression, it will report any failure in terms
  of that match. Given 
 
      assert [one] = [two]

  you'll see:

      match (=) failed
      code: [one] = [two]
      rhs:  [2]

  If the expression is a comparison operator, the message
  will show the values of the two sides. The assertion

      assert 1+2+3+4 > 15

   will fail with the message:

      Comparison (using >) failed in:
      code: 1+2+3+4 > 15
      lhs:  10
      rhs:  15
  """
  defmacro assert(assertion = { :=, _, [left, right] }) do
    code = Macro.to_string(assertion)
    { :case, meta, args } =
      quote do
        case right do
          unquote(left) ->
            right
          _ ->
            raise ExUnit.AssertionError,
              right: Macro.to_string(right),
              expr: unquote(code),
              message: "match (=) failed"
        end
      end

    quote do
      right = unquote(right)
      unquote({ :case, [{:export_head,true}|meta], args })
    end
  end


  defmacro assert(assertion) do
    case translate_assertion(assertion) do
      nil ->
        # Default message in case no transform was performed
        quote do
          value = unquote(assertion)

          unless value do
            raise ExUnit.AssertionError,
              expr:  unquote(Macro.to_string(assertion)),
              message: "#{inspect value} is not truthy"
          end

          value
        end

      value -> 
        value
    end
  end

  @doc """
  This is a negative assertion, failing if its parameter
  is truthy.

  ## Examples

      refute age < 0

  """

  defmacro refute(assertion = { :=, _, [left, right] }) do
    code = Macro.to_string(assertion)
    { :case, meta, args } =
      quote do
        case right do
          unquote(left) ->
            raise ExUnit.AssertionError,
              right: Macro.to_string(right),
              expr: unquote(code),
              message: "match (=) succeeded, but should have failed"
          _ ->
            right
        end
      end

    quote do
      right = unquote(right)
      unquote({ :case, [{:export_head,true}|meta], args })
    end
  end


  defmacro refute(assertion) do
    case translate_assertion({ :!, [], [assertion] }) do
      nil ->
        # Default message in case no transform was performed
        quote do
          value = unquote(assertion)

          if value do
            raise ExUnit.AssertionError,
              expr:  unquote(Macro.to_string(assertion)),
              message: "#{inspect value} should be false or nil"
          end

          value
        end

      value -> 
        { :!, [], [value] }
    end
  end

  ## START HELPERS



  defp translate_assertion(expr = { operator, _, [left, right] }) 
  when operator in [ :==, :<, :>, :<=, :>=, :===, :=~, :!==, :!= ]  do
    assert_operator operator, left, right, expr
  end


  defp translate_assertion(expr = { :in, _, [left, right] }) do
    code = Macro.to_string(expr)
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert_internal Enum.member?(right, left), 
                           left: left, right: right, expr: unquote(code)
    end
  end

  ## Negative versions

  defp translate_assertion({ :!, _, [{ :=, _, [left, right] }] }) do
    quote do
      right = unquote(right)
      case right do
        unquote(left) ->
          raise ExUnit.AssertionError,
            expected: inspect(right),
            actual: unquote(Macro.to_string(left)),
            assertion: "match pattern (=)",
            negation: true
          _ ->
            nil
      end
    end
  end

  defp translate_assertion(expr = { negation, _, [{ :in, _, [left, right] }] }) 
  when negation in [:!, :not] do
    code = Macro.to_string(expr)
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert_internal !Enum.member?(right, left), 
                           left: left, right: right, expr: unquote(code)
    end
  end

  defp translate_assertion(expr = {:!, [], [{ operator, _, [left, right] }]}) 
  when operator in [ :==, :<, :>, :<=, :>=, :===, :=~, :!==, :!= ]  do
    refute_operator operator, left, right, expr
  end

  ## Fallback

  defp translate_assertion(_expected) do
    nil
  end

  defp assert_operator(operator, left, right, expr) do
    expr = Macro.to_string(expr)
    quote location: :keep do
      left  = unquote(left)
      right = unquote(right)
      assert_internal unquote(operator)(left, right), left: left, right: right, expr: unquote(expr), operator: to_string(unquote(operator))
    end
  end

  defp refute_operator(operator, left, right, expr) do
    expr = Macro.to_string(expr)
    quote location: :keep do
      left  = unquote(left)
      right = unquote(right)
      assert_internal not(unquote(operator)(left, right)), left: left, right: right, expr: unquote(expr), operator: to_string(unquote(operator))
    end
  end

  def assert_internal(successful, opts) do
    unless successful, do: raise(ExUnit.AssertionError, opts)
    true
  end
  ## END HELPERS

  @doc """
  Asserts `value` is true, displaying the given `message` otherwise.

  ## Examples

      assert false, "it will never be true"

  """
  def assert(value, message) when is_binary(message) do
    assert_internal(value, message: message, expr: ExUnit.NoValueSupplied.no_value)
  end

  @doc """
  Asserts `value` is true.
  If it fails, it raises an expectation error
  using the given `left` and `right` values.

  You probably don't need to use this—the regular `assert` function
  handles this for you.

  ## Examples

      assert this > that, this, that, "more than"

  """
  def assert(value, left, right, message) when is_binary(message) do
    assert_internal(value, left: left, right: right, 
                    message: message, expr: ExUnit.NoValueSupplied.no_value)
  end

  def assert(value, expected, actual, opts) do
    unless value do
      raise ExUnit.AssertionError,
        Keyword.merge([expected: inspect(expected), actual: inspect(actual)], opts)
    end
    true
  end

  @doc """
  Asserts a message was or is going to be received. Unlike
  `assert_received`, it has a default timeout of 100 milliseconds.

  The `expected` argument is a pattern.

  ## Examples

      assert_receive :hello

  Asserts against a larger timeout:

      assert_receive :hello, 20_000

  You can also match against specific patterns:

      assert_receive { :hello, _ }

      x = 5
      assert_receive { :count, ^x }

  """
  defmacro assert_receive(expected, timeout \\ 100, message \\ nil) do
    do_assert_receive(expected, timeout, message)
  end

  @doc """
  Asserts a message was received and is in the current process' mailbox.
  Timeout is set to 0, so there is no waiting time.

  The `expected` argument is a pattern.

  ## Examples

      send self, :hello
      assert_received :hello

  You can also match against specific patterns:

      send self, { :hello, "world" }
      assert_received { :hello, _ }

  """
  defmacro assert_received(expected, message \\ nil) do
    do_assert_receive(expected, 0, message)
  end

  defp do_assert_receive(expected, timeout, message) do
    binary = Macro.to_string(expected)

    { :receive, meta, args } =
      quote do
        receive do
          unquote(expected) = received -> received
        after
          unquote(timeout) ->
            flunk unquote(message) || "No message matching #{unquote binary}"
        end
      end

    { :receive, [{:export_head, true}|meta], args }
  end

  @doc """
  Asserts the `exception` is raised during `function` execution with
  the `expected_message`. Returns the rescued exception, fails otherwise. 
  The expected record can be a string or a regular expression.

  ## Examples

      assert_raise ArithmeticError, "bad argument in arithmetic expression", fn ->
        1 + "test"
      end
  """
  def assert_raise(exception, message, function) when is_function(function) do
    error = assert_raise(exception, function)

    is_match = cond do
      is_binary(message) -> error.message == message
      Regex.regex?(message) -> error.message =~ message
    end

    msg = "Wrong message for #{inspect exception}. Expected #{inspect message}, got #{inspect error.message}"
    assert_internal is_match, message: msg, expr:  ExUnit.NoValueSupplied.no_value

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
      flunk "Expected exception '#{inspect exception}' but nothing was raised"
    rescue
      error in [exception] -> error
      error ->
        name = error.__record__(:name)

        if name in [ExUnit.AssertionError, ExUnit.AssertionError] do
          raise(error)
        else
          flunk "Expected exception '#{inspect exception}' but got #{inspect name}(#{error.message})"
        end
    end
  end

  @doc """
  Asserts that `val1` and `val2` differ by no more than `delta`.


  ## Examples

      assert_in_delta 1.1, 1.5, 0.2
      assert_in_delta 10, 15, 4

  """
  def assert_in_delta(val1, val2, delta, message \\ nil) do
    diff = abs(val1 - val2)
    message = message ||
      "Expected the difference between #{inspect val1} and " <>
      "#{inspect val2} (#{inspect diff}) to be less than #{inspect delta}"
    assert diff < delta, message
  end

  @doc """
  Asserts `expression` will throw a value.
  Returns the thrown value or fails otherwise.

  ## Examples

      assert catch_throw(throw 1) == 1

  """
  defmacro catch_throw(expression) do
    do_catch(:throw, expression)
  end

  @doc """
  Asserts `expression` will exit.
  Returns the exit status/message or fails otherwise.

  ## Examples

      assert catch_exit(exit 1) == 1

  """
  defmacro catch_exit(expression) do
    do_catch(:exit, expression)
  end

  @doc """
  Asserts `expression` will cause an error.
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
        e in [ExUnit.AssertionError, ExUnit.AssertionError] -> raise(e)
      catch
        unquote(kind), what_we_got -> what_we_got
      end
    end
  end

  @doc """
  Asserts `value` is `nil` or `false` (that is, `value` is not truthy).

  ## Examples

      refute true, "This will obviously fail"

  """
  def refute(value, message) do
    not assert(!value, message)
  end

  @doc """
      refute_receive message, timeout \\ 100, message \\ nil

  Asserts `message` was not received (and won't be received) within
  the `timeout` period.

  The `not_expected` argument is a match pattern.

  ## Examples

      refute_receive :bye

  Refute received with a explicit timeout:

      refute_receive :bye, 1000

  """
  defmacro refute_receive(not_expected, timeout \\ 100, message \\ nil) do
    do_refute_receive(not_expected, timeout, message)
  end

  @doc """
  Asserts a message was not received (i.e. it is not in the current process mailbox).
  The `not_expected` argument must be a match pattern.

  Timeout is set to 0, so there is no waiting time.

  ## Examples

      send self, :hello
      refute_received :bye

  """
  defmacro refute_received(not_expected, message \\ nil) do
    do_refute_receive(not_expected, 0, message)
  end

  defp do_refute_receive(not_expected, timeout, message) do
    binary = Macro.to_string(not_expected)

    quote do
      receive do
        unquote(not_expected) = actual ->
          flunk unquote(message) || "Unexpectedly received message #{inspect actual} (which matched #{unquote binary})"
      after
        unquote(timeout) -> false
      end
    end
  end

  @doc """
  Asserts `val1` and `val2` are not within `delta`.

  If you supply `message`, information about the values will
  automatically be appended to it.

  ## Examples

      refute_in_delta 1.1, 1.2, 0.2
      refute_in_delta 10, 11, 2

  """
  def refute_in_delta(val1, val2, delta, message \\ nil) do
    diff = abs(val1 - val2)
    message = if message do
      message <> " (difference between #{inspect val1} " <>
      "and #{inspect val2} is less than #{inspect delta})"
    else
      "Expected the difference between #{inspect val1} and " <>
      "#{inspect val2} (#{inspect diff}) to be more than #{inspect delta}"
    end
    refute diff < delta, message
  end

  @doc """
  Fails with a message.

  ## Examples

      flunk "This should raise an error"

  """
  @spec flunk :: no_return
  @spec flunk(String.t) :: no_return
  def flunk(message \\ "Flunked!") do
    assert_internal false, message: message, expr:  ExUnit.NoValueSupplied.no_value
  end
end
