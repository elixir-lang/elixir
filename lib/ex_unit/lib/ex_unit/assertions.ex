defmodule ExUnit.AssertionError do
  @moduledoc """
  Raised to signal an assertion error.
  """

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

defmodule ExUnit.MultiError do
  @moduledoc """
  Raised to signal multiple errors happened in a test case.
  """

  defexception [errors: []]

  def message(exception) do
    "got the following errors:\n\n  * " <>
      Enum.map_join(exception, "\n  * ", &Exception.message/1)
  end
end

defmodule ExUnit.Assertions do
  @moduledoc """
  This module contains a set of assertion functions that are
  imported by default into your test cases.

  In general, a developer will want to use the general
  `assert` macro in tests. This macro introspects your code
  and provide good reporting whenever there is a failure.
  For example, `assert some_fun() == 10` will fail (assuming
  `some_fun()` returns 13):

      Comparison (using ==) failed in:
      code: some_fun() == 10
      lhs:  13
      rhs:  10

  This module also provides other convenience functions
  like `assert_in_delta` and `assert_raise` to easily handle
  other common cases such as checking a floating point number
  or handling exceptions.
  """

  @doc """
  Asserts its argument is a truthy value.

  `assert` instrospects the underlying expression and provide
  good  reporting whenever there is a failure. For example,
  if the expression uses the comparison operator, the message
  will show the values of the two sides. The assertion

      assert 1+2+3+4 > 15

   will fail with the message:

      Assertion with > failed
      code: 1+2+3+4 > 15
      lhs:  10
      rhs:  15

  Similarly, if a match expression is given, it will report
  any failure in terms of that match. Given

      assert [one] = [two]

  you'll see:

      match (=) failed
      code: [one] = [two]
      rhs:  [2]

  Keep in mind that `assert` does not change its semantics
  based on the expression. In other words, the expression
  is still required to return a truthy value. For example,
  the following will fail:

      assert nil = some_function_that_returns_nil()

  Even though the match works, `assert` still expects a truth
  value. In such cases, simply use `Kernel.==/2` or
  `Kernel.match?/2`.
  """
  defmacro assert({:=, _, [left, right]} = assertion) do
    code = Macro.escape(assertion)

    left = Macro.expand(left, __CALLER__)
    vars = collect_vars_from_pattern(left)

    # If the match works, we need to check if the value
    # is not nil nor false. We need to rewrite the if
    # to avoid silly warnings though.
    return =
        no_warning(quote do
          if right do
            right
          else
            raise ExUnit.AssertionError,
              expr: expr,
              message: "Expected truthy, got #{inspect right}"
          end
        end)

    quote do
      right = unquote(right)
      expr  = unquote(code)
      unquote(vars) =
        case right do
          unquote(left) ->
            unquote(return)
            unquote(vars)
          _ ->
            raise ExUnit.AssertionError,
              right: right,
              expr: expr,
              message: "match (=) failed"
        end
      right
    end
  end

  defmacro assert({:match?, meta, [left, right]} = assertion) do
    code   = Macro.escape(assertion)
    match? = {:match?, meta, [left, Macro.var(:right, __MODULE__)]}
    quote do
      right = unquote(right)
      assert unquote(match?),
        right: right,
        expr: unquote(code),
        message: "match (match?) failed"
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
  A negative assertion, expects the expression to be `false` or `nil`.

  Keep in mind that `refute` does not change the semantics of
  the given expression. In other words, the following will fail:

      refute {:ok, _} = some_function_that_returns_error_tuple()

  The code above will fail because the `=` operator always fails
  when the sides do not match and `refute/2` does not change it.

  The correct way to write the refutation above is to use
  `Kernel.match?/2`:

      refute match? {:ok, _}, some_function_that_returns_error_tuple()

  ## Examples

      refute age < 0

  """
  defmacro refute({:match?, meta, [left, right]} = assertion) do
    code   = Macro.escape(assertion)
    match? = {:match?, meta, [left, Macro.var(:right, __MODULE__)]}
    quote do
      right = unquote(right)
      refute unquote(match?),
        right: right,
        expr: unquote(code),
        message: "match (match?) succeeded, but should have failed"
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

  defp translate_assertion({operator, meta, [left, right]} = expr) when operator in @operator  do
    expr = Macro.escape(expr)
    call = {operator, meta, [Macro.var(:left, __MODULE__), Macro.var(:right, __MODULE__)]}
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert unquote(call),
             left: left,
             right: right,
             expr: unquote(expr),
             message: unquote("Assertion with #{operator} failed")
    end
  end

  defp translate_assertion({:!, _, [{operator, meta, [left, right]} = expr]}) when operator in @operator do
    expr = Macro.escape(expr)
    call = {operator, meta, [Macro.var(:left, __MODULE__), Macro.var(:right, __MODULE__)]}
    quote do
      left  = unquote(left)
      right = unquote(right)
      assert not(unquote(call)),
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
  defmacro assert_receive(expected,
                          timeout \\ Application.fetch_env!(:ex_unit, :assert_receive_timeout),
                          message \\ nil) do
    do_assert_receive(expected, timeout, message, __CALLER__)
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
    do_assert_receive(expected, 0, message, __CALLER__)
  end

  defp do_assert_receive(expected, timeout, message, caller) do
    binary = Macro.to_string(expected)

    # Expand before extracting metadata
    expected = Macro.expand(expected, caller)
    vars = collect_vars_from_pattern(expected)
    pins = collect_pins_from_pattern(expected)

    pattern =
      case expected do
        {:when, meta, [left, right]} ->
          {:when, meta, [quote(do: unquote(left) = received), right]}
        left ->
          quote(do: unquote(left) = received)
      end

    quote do
      timeout = unquote(timeout)

      {received, unquote(vars)} =
        receive do
          unquote(pattern) ->
            {received, unquote(vars)}
        after
          timeout ->
            message = unquote(message) || "No message matching #{unquote(binary)} after #{timeout}ms."
            flunk(message <> ExUnit.Assertions.__pins__(unquote(pins))
                          <> ExUnit.Assertions.__mailbox__(self()))
        end

      _ = unquote(vars) # Silence warnings
      received
    end
  end

  @indent "\n  "
  @max_mailbox_length 10

  @doc false
  def __mailbox__(pid) do
    {:messages, messages} = Process.info(pid, :messages)
    length = length(messages)
    mailbox =
      messages
      |> Enum.take(@max_mailbox_length)
      |> Enum.map_join(@indent, &inspect/1)
    mailbox_message(length, @indent <> mailbox)
  end

  @doc false
  def __pins__([]), do: ""
  def __pins__(pins) do
    content =
      pins
      |> Enum.reverse()
      |> Enum.map_join(@indent, fn {name, var} -> "#{name} = #{inspect(var)}" end)
    "\nThe following variables were pinned:" <> @indent <> content
  end

  defp mailbox_message(0, _mailbox), do: "\nThe process mailbox is empty."
  defp mailbox_message(length, mailbox) when length > 10 do
    "\nProcess mailbox:" <> mailbox
      <> "\nShowing only #{@max_mailbox_length} of #{length} messages."
  end
  defp mailbox_message(_length, mailbox) do
    "\nProcess mailbox:" <> mailbox
  end

  defp collect_pins_from_pattern(expr) do
    {_, pins} =
      Macro.prewalk(expr, [], fn
        {:^, _, [{name, _, _} = var]}, acc ->
          {:ok, [{name, var}|acc]}
        form, acc ->
          {form, acc}
      end)
    Enum.uniq_by(pins, &elem(&1, 0))
  end

  defp collect_vars_from_pattern(expr) do
    Macro.prewalk(expr, [], fn
      {:::, _, [left, _]}, acc ->
        {[left], acc}
      {skip, _, [_]}, acc when skip in [:^, :@] ->
        {:ok, acc}
      {:_, _, context}, acc when is_atom(context) ->
        {:ok, acc}
      {name, _, context}, acc when is_atom(name) and is_atom(context) ->
        {:ok, [{name, [generated: true], context}|acc]}
      node, acc ->
        {node, acc}
    end)
    |> elem(1)
  end

  defp no_warning({name, meta, args}) do
    {name, [line: -1] ++ meta, args}
  end

  @doc """
  Asserts the `exception` is raised during `function` execution with
  the expected `message`, which can be a `Regex` or an exact `String`.
  Returns the rescued exception, fails otherwise.

  ## Examples

      assert_raise ArithmeticError, "bad argument in arithmetic expression", fn ->
        1 + "test"
      end

      assert_raise RuntimeError, ~r/^Today's lucky number is 0\.\d+!$/, fn ->
        raise "Today's lucky number is #{:rand.uniform}!"
      end
  """
  def assert_raise(exception, message, function) when is_function(function) do
    error = assert_raise(exception, function)

    is_match = cond do
      is_binary(message) -> Exception.message(error) == message
      Regex.regex?(message) -> Exception.message(error) =~ message
    end

    msg = "Wrong message for #{inspect exception}\n" <>
          "Expected:\n" <>
          "  #{inspect message}\n" <>
          "Got:\n" <>
          "  #{inspect Exception.message(error)}"
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
            reraise ExUnit.AssertionError, [message: "Expected exception #{inspect exception} but got #{inspect name} (#{Exception.message(error)})"], stacktrace
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
        _ = unquote(expr)
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

  Refute received with an explicit timeout:

      refute_receive :bye, 1000

  """
  defmacro refute_receive(not_expected,
                          timeout \\ Application.fetch_env!(:ex_unit, :refute_receive_timeout),
                          message \\ nil) do
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
