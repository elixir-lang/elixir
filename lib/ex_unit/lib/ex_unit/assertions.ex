defmodule ExUnit.AssertionError do
  @no_value :ex_unit_no_meaningful_value

  defexception left:    @no_value,
               right:   @no_value,
               message: @no_value,
               expr:    @no_value

  @doc """
  Indicates no meaningful value for a field.
  """
  def no_value do
    @no_value
  end
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
  Asserts its argument is `true`.

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

      Assertion with > failed
      code: 1+2+3+4 > 15
      lhs:  10
      rhs:  15
  """
  defmacro assert({:=, _, [left, right]} = assertion) do
    code = Macro.escape(assertion)
    {:case, meta, args} =
      quote do
        case right do
          unquote(left) ->
            right
          _ ->
            raise ExUnit.AssertionError,
              right: right,
              expr: unquote(code),
              message: "match (=) failed"
        end
      end

    quote do
      right = unquote(right)
      unquote({:case, [{:export_head, true}|meta], args})
    end
  end

  defmacro assert(assertion) do
    case translate_assertion(assertion) do
      nil ->
        quote do
          value = unquote(assertion)

          unless value do
            raise ExUnit.AssertionError,
              expr: unquote(Macro.escape(assertion)),
              message: "Expected truthy, got #{inspect value}"
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
  defmacro refute({:=, _, [left, right]} = assertion) do
    code = Macro.escape(assertion)
    {:case, meta, args} =
      quote do
        case right do
          unquote(left) ->
            raise ExUnit.AssertionError,
              right: right,
              expr: unquote(code),
              message: "match (=) succeeded, but should have failed"
          _ ->
            right
        end
      end

    quote do
      right = unquote(right)
      unquote({:case, [{:export_head, true}|meta], args})
    end
  end

  defmacro refute(assertion) do
    case translate_assertion({:!, [], [assertion]}) do
      nil ->
        quote do
          value = unquote(assertion)

          if value do
            raise ExUnit.AssertionError,
              expr: unquote(Macro.escape(assertion)),
              message: "Expected false or nil, got #{inspect value}"
          end

          value
        end

      value ->
        {:!, [], [value]}
    end
  end

  ## START HELPERS

  @operator [:==, :<, :>, :<=, :>=, :===, :=~, :!==, :!=, :in]

  defp translate_assertion({operator, _, [left, right]} = expr) when operator in @operator  do
    expr = Macro.escape(expr)
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert unquote(operator)(left, right),
             left: left,
             right: right,
             expr: unquote(expr),
             message: unquote("Assertion with #{operator} failed")
    end
  end

  defp translate_assertion({:!, [], [{operator, _, [left, right]} = expr]}) when operator in @operator do
    expr = Macro.escape(expr)
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert not(unquote(operator)(left, right)),
             left: left,
             right: right,
             expr: unquote(expr),
             message: unquote("Refute with #{operator} failed")
    end
  end

  defp translate_assertion(_expected) do
    nil
  end

  ## END HELPERS

  @doc """
  Asserts `value` is `true`, displaying the given `message` otherwise.

  ## Examples

      assert false, "it will never be true"

  """
  def assert(value, message) when is_binary(message) do
    assert(value, message: message)
  end

  def assert(value, opts) when is_list(opts) do
    unless value, do: raise(ExUnit.AssertionError, opts)
    true
  end

  @doc """
  Asserts a message was or is going to be received.

  Unlike `assert_received`, it has a default timeout
  of 100 milliseconds.

  The `expected` argument is a pattern.

  ## Examples

      assert_receive :hello

  Asserts against a larger timeout:

      assert_receive :hello, 20_000

  You can also match against specific patterns:

      assert_receive {:hello, _}

      x = 5
      assert_receive {:count, ^x}

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

      send self, {:hello, "world"}
      assert_received {:hello, _}

  """
  defmacro assert_received(expected, message \\ nil) do
    do_assert_receive(expected, 0, message)
  end

  defp do_assert_receive(expected, timeout, message) do
    binary = Macro.to_string(expected)

    pattern =
      case expected do
        {:when, meta, [left, right]} ->
          {:when, meta, [quote(do: unquote(left) = received), right]}
        left ->
          quote(do: unquote(left) = received)
      end

    {:receive, meta, args} =
      quote do
        receive do
          unquote(pattern) ->
            received
        after
          timeout ->
            message = unquote(message) || "No message matching #{unquote(binary)} after #{timeout}ms"
            flunk(message <> ExUnit.Assertions.__mailbox__(self()))
        end
      end

    quote do
      timeout = unquote(timeout)
      unquote({:receive, [{:export_head, true}|meta], args})
    end
  end

  @max_mailbox_length 10

  @doc false
  def __mailbox__(pid) do
    {:messages, messages} = Process.info(pid, :messages)
    length = length(messages)
    mailbox = Enum.take(messages, @max_mailbox_length) |> Enum.map_join("\n", &inspect/1)
    mailbox_message(length, mailbox)
  end

  defp mailbox_message(0, _mailbox), do: ". The process mailbox is empty."
  defp mailbox_message(length, mailbox) when length > 10 do
    ". Process mailbox:\n" <> mailbox
      <> "\nShowing only #{@max_mailbox_length} of #{length} messages."
  end
  defp mailbox_message(_length, mailbox) do
    ". Process mailbox:\n" <> mailbox
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

    is_match = cond do
      is_binary(message) -> Exception.message(error) == message
      Regex.regex?(message) -> Exception.message(error) =~ message
    end

    msg = "Wrong message for #{inspect exception}. " <>
          "Expected #{inspect message}, got #{inspect Exception.message(error)}"
    assert is_match, message: msg

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
    rescue
      error ->
        stacktrace = System.stacktrace
        name = error.__struct__

        cond do
          name == exception ->
            error
          name == ExUnit.AssertionError ->
            reraise(error, stacktrace)
          true ->
            flunk "Expected exception #{inspect exception} but got #{inspect name} (#{Exception.message(error)})"
        end
    else
      _ -> flunk "Expected exception #{inspect exception} but nothing was raised"
    end
  end

  @doc """
  Asserts that `value1` and `value2` differ by no more than `delta`.


  ## Examples

      assert_in_delta 1.1, 1.5, 0.2
      assert_in_delta 10, 15, 4

  """
  def assert_in_delta(value1, value2, delta, message \\ nil) do
    diff = abs(value1 - value2)
    message = message ||
      "Expected the difference between #{inspect value1} and " <>
      "#{inspect value2} (#{inspect diff}) to be less than #{inspect delta}"
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
        e in [ExUnit.AssertionError] ->
          reraise(e, System.stacktrace)
      catch
        unquote(kind), we_got -> we_got
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
    receive_clause = refute_receive_clause(not_expected, message)

    quote do
      receive do
        unquote(receive_clause)
      after
        unquote(timeout) -> false
      end
    end
  end

 defp refute_receive_clause(not_expected, nil) do
  binary = Macro.to_string(not_expected)
  quote do
    unquote(not_expected) = actual ->
      flunk "Unexpectedly received message #{inspect actual} (which matched #{unquote binary})"
    end
  end

  defp refute_receive_clause(not_expected, message) do
    quote do
      unquote(not_expected) -> flunk unquote(message)
    end
  end

  @doc """
  Asserts `value1` and `value2` are not within `delta`.

  If you supply `message`, information about the values will
  automatically be appended to it.

  ## Examples

      refute_in_delta 1.1, 1.2, 0.2
      refute_in_delta 10, 11, 2

  """
  def refute_in_delta(value1, value2, delta, message \\ nil) do
    diff = abs(value1 - value2)
    message = if message do
      message <> " (difference between #{inspect value1} " <>
      "and #{inspect value2} is less than #{inspect delta})"
    else
      "Expected the difference between #{inspect value1} and " <>
      "#{inspect value2} (#{inspect diff}) to be more than #{inspect delta}"
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
  def flunk(message \\ "Flunked!") when is_binary(message) do
    assert false, message: message
  end
end
