defmodule ExUnit.AssertionError do
  @moduledoc """
  Raised to signal an assertion error.
  """

  @no_value :ex_unit_no_meaningful_value

  defexception left: @no_value,
               right: @no_value,
               mailbox: @no_value,
               message: @no_value,
               expr: @no_value,
               args: @no_value,
               doctest: @no_value,
               context: :expr

  @doc """
  Indicates no meaningful value for a field.
  """
  def no_value do
    @no_value
  end

  @impl true
  def message(exception) do
    "\n\n" <> ExUnit.Formatter.format_assertion_error(exception)
  end
end

defmodule ExUnit.MultiError do
  @moduledoc """
  Raised to signal multiple errors happened in a test case.
  """

  defexception errors: []

  @impl true
  def message(%{errors: errors}) do
    "got the following errors:\n\n" <>
      Enum.map_join(errors, "\n\n", fn {kind, error, stack} ->
        Exception.format_banner(kind, error, stack)
      end)
  end
end

defmodule ExUnit.Assertions do
  @moduledoc """
  This module contains a set of assertion functions that are
  imported by default into your test cases.

  In general, a developer will want to use the general
  `assert` macro in tests. This macro introspects your code
  and provides good reporting whenever there is a failure.
  For example, `assert some_fun() == 10` will fail (assuming
  `some_fun()` returns `13`):

      Comparison (using ==) failed in:
      code:  assert some_fun() == 10
      left:  13
      right: 10

  This module also provides other convenience functions
  like `assert_in_delta` and `assert_raise` to easily handle
  other common cases such as checking a floating-point number
  or handling exceptions.
  """

  @doc """
  Asserts its argument is a truthy value.

  `assert` introspects the underlying expression and provides
  good reporting whenever there is a failure. For example,
  if the expression uses the comparison operator, the message
  will show the values of the two sides. The assertion

      assert 1 + 2 + 3 + 4 > 15

   will fail with the message:

      Assertion with > failed
      code:  assert 1 + 2 + 3 + 4 > 15
      left:  10
      right: 15

  Similarly, if a match expression is given, it will report
  any failure in terms of that match. Given

      assert [1] = [2]

  you'll see:

      match (=) failed
      code:  assert [1] = [2]
      left: [1]
      right: [2]

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
    code = escape_quoted(:assert, assertion)

    left = __expand_pattern__(left, __CALLER__)
    vars = collect_vars_from_pattern(left)
    pins = collect_pins_from_pattern(left, Macro.Env.vars(__CALLER__))

    # If the match works, we need to check if the value
    # is not nil nor false. We need to rewrite the if
    # to avoid silly warnings though.
    return =
      suppress_warning(
        quote do
          case right do
            x when x in [nil, false] ->
              raise ExUnit.AssertionError,
                expr: expr,
                message: "Expected truthy, got #{inspect(right)}"

            _ ->
              :ok
          end
        end
      )

    match_expr =
      suppress_warning(
        quote do
          case right do
            unquote(left) ->
              unquote(return)
              unquote(vars)

            _ ->
              left = unquote(Macro.escape(left))

              raise ExUnit.AssertionError,
                left: left,
                right: right,
                expr: expr,
                message: "match (=) failed" <> ExUnit.Assertions.__pins__(unquote(pins)),
                context: {:match, unquote(pins)}
          end
        end
      )

    quote do
      right = unquote(right)
      expr = unquote(code)
      unquote(vars) = unquote(match_expr)
      right
    end
  end

  defmacro assert({:match?, meta, [left, right]} = assertion) do
    code = escape_quoted(:assert, assertion)
    match? = {:match?, meta, [left, Macro.var(:right, __MODULE__)]}

    left = __expand_pattern__(left, __CALLER__)
    pins = collect_pins_from_pattern(left, Macro.Env.vars(__CALLER__))

    quote do
      right = unquote(right)
      left = unquote(Macro.escape(left))

      assert unquote(match?),
        right: right,
        left: left,
        expr: unquote(code),
        message: "match (match?) failed" <> ExUnit.Assertions.__pins__(unquote(pins)),
        context: {:match, unquote(pins)}
    end
  end

  defmacro assert(assertion) do
    if translated = translate_assertion(:assert, assertion, __CALLER__) do
      translated
    else
      {args, value} = extract_args(assertion, __CALLER__)

      quote do
        value = unquote(value)

        unless value do
          raise ExUnit.AssertionError,
            args: unquote(args),
            expr: unquote(escape_quoted(:assert, assertion)),
            message: "Expected truthy, got #{inspect(value)}"
        end

        value
      end
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

      refute match?({:ok, _}, some_function_that_returns_error_tuple())

  ## Examples

      refute age < 0

  """
  defmacro refute({:match?, meta, [left, right]} = assertion) do
    code = escape_quoted(:refute, assertion)
    match? = {:match?, meta, [left, Macro.var(:right, __MODULE__)]}

    left = __expand_pattern__(left, __CALLER__)
    pins = collect_pins_from_pattern(left, Macro.Env.vars(__CALLER__))

    quote do
      right = unquote(right)
      left = unquote(Macro.escape(left))

      refute unquote(match?),
        right: right,
        left: left,
        expr: unquote(code),
        message:
          "match (match?) succeeded, but should have failed" <>
            ExUnit.Assertions.__pins__(unquote(pins)),
        context: {:match, unquote(pins)}
    end
  end

  defmacro refute(assertion) do
    if translated = translate_assertion(:refute, assertion, __CALLER__) do
      {:!, [], [translated]}
    else
      {args, value} = extract_args(assertion, __CALLER__)

      quote do
        value = unquote(value)

        if value do
          raise ExUnit.AssertionError,
            args: unquote(args),
            expr: unquote(escape_quoted(:refute, assertion)),
            message: "Expected false or nil, got #{inspect(value)}"
        end

        value
      end
    end
  end

  ## START HELPERS

  @operator [:==, :<, :>, :<=, :>=, :===, :=~, :!==, :!=, :in]

  defp translate_assertion(:assert, {operator, meta, [_, _]} = expr, caller)
       when operator in @operator do
    left = Macro.var(:left, __MODULE__)
    right = Macro.var(:right, __MODULE__)
    call = {operator, meta, [left, right]}
    equality_check? = operator in [:<, :>, :!==, :!=]
    message = "Assertion with #{operator} failed"
    translate_operator(:assert, expr, call, message, equality_check?, caller)
  end

  defp translate_assertion(:refute, {operator, meta, [_, _]} = expr, caller)
       when operator in @operator do
    left = Macro.var(:left, __MODULE__)
    right = Macro.var(:right, __MODULE__)
    call = {:not, meta, [{operator, meta, [left, right]}]}
    equality_check? = operator in [:<=, :>=, :===, :==, :=~]
    message = "Refute with #{operator} failed"
    translate_operator(:refute, expr, call, message, equality_check?, caller)
  end

  defp translate_assertion(_kind, _expected, _caller) do
    nil
  end

  defp translate_operator(kind, {_, _, [left, right]} = expr, call, message, true, _caller) do
    expr = escape_quoted(kind, expr)

    quote do
      left = unquote(left)
      right = unquote(right)

      if ExUnit.Assertions.__equal__?(left, right) do
        assert false,
          left: left,
          expr: unquote(expr),
          message: unquote(message <> ", both sides are exactly equal")
      else
        assert unquote(call),
          left: left,
          right: right,
          expr: unquote(expr),
          message: unquote(message)
      end
    end
  end

  defp translate_operator(kind, {_, _, [left, right]} = expr, call, message, false, _caller) do
    expr = escape_quoted(kind, expr)

    quote do
      left = unquote(left)
      right = unquote(right)

      assert unquote(call),
        left: left,
        right: right,
        expr: unquote(expr),
        message: unquote(message)
    end
  end

  @doc false
  def __equal__?(left, right) do
    left === right
  end

  defp escape_quoted(kind, expr) do
    Macro.escape({kind, [], [expr]}, prune_metadata: true)
  end

  defp extract_args({root, meta, [_ | _] = args} = expr, env) do
    arity = length(args)

    reserved? =
      is_atom(root) and (Macro.special_form?(root, arity) or Macro.operator?(root, arity))

    all_quoted_literals? = Enum.all?(args, &Macro.quoted_literal?/1)

    case Macro.expand_once(expr, env) do
      ^expr when not reserved? and not all_quoted_literals? ->
        vars = for i <- 1..arity, do: Macro.var(:"arg#{i}", __MODULE__)

        quoted =
          quote do
            {unquote_splicing(vars)} = {unquote_splicing(args)}
            unquote({root, meta, vars})
          end

        {vars, quoted}

      other ->
        {ExUnit.AssertionError.no_value(), other}
    end
  end

  defp extract_args(expr, _env) do
    {ExUnit.AssertionError.no_value(), expr}
  end

  ## END HELPERS

  @doc """
  Asserts `value` is truthy, displaying the given `message` otherwise.

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
  Asserts that a message matching `pattern` was or is going to be received
  within the `timeout` period, specified in milliseconds.

  Unlike `assert_received`, it has a default `timeout`
  of 100 milliseconds.

  The `pattern` argument must be a match pattern. Flunks with `failure_message`
  if a message matching `pattern` is not received.

  ## Examples

      assert_receive :hello

  Asserts against a larger timeout:

      assert_receive :hello, 20_000

  You can also match against specific patterns:

      assert_receive {:hello, _}

      x = 5
      assert_receive {:count, ^x}

  """
  defmacro assert_receive(
             pattern,
             timeout \\ Application.fetch_env!(:ex_unit, :assert_receive_timeout),
             failure_message \\ nil
           ) do
    code = escape_quoted(:assert_receive, pattern)
    assert_receive(pattern, timeout, failure_message, __CALLER__, code)
  end

  @doc """
  Asserts that a message matching `pattern` was received and is in the
  current process' mailbox.

  The `pattern` argument must be a match pattern. Flunks with `failure_message`
  if a message matching `pattern` was not received.

  Timeout is set to 0, so there is no waiting time.

  ## Examples

      send(self(), :hello)
      assert_received :hello

      send(self(), :bye)
      assert_received :hello, "Oh No!"
      ** (ExUnit.AssertionError) Oh No!
      Process mailbox:
        :bye

  You can also match against specific patterns:

      send(self(), {:hello, "world"})
      assert_received {:hello, _}

  """
  defmacro assert_received(pattern, failure_message \\ nil) do
    code = escape_quoted(:assert_received, pattern)
    assert_receive(pattern, 0, failure_message, __CALLER__, code)
  end

  defp assert_receive(pattern, timeout, failure_message, caller, code) do
    # Expand before extracting metadata
    expanded_pattern = __expand_pattern__(pattern, caller)
    vars = collect_vars_from_pattern(expanded_pattern)
    pins = collect_pins_from_pattern(expanded_pattern, Macro.Env.vars(caller))

    pattern =
      case expanded_pattern do
        {:when, meta, [left, right]} ->
          {:when, meta, [quote(do: unquote(left) = received), right]}

        left ->
          quote(do: unquote(left) = received)
      end

    quoted_pattern =
      quote do
        case message do
          unquote(pattern) ->
            _ = unquote(vars)
            true

          _ ->
            false
        end
      end

    pattern_finder =
      quote do
        fn message ->
          unquote(suppress_warning(quoted_pattern))
        end
      end

    timeout =
      if is_integer(timeout) do
        timeout
      else
        quote do: ExUnit.Assertions.__timeout__(unquote(timeout))
      end

    failure_message =
      failure_message ||
        quote do
          ExUnit.Assertions.__timeout__(
            unquote(Macro.escape(expanded_pattern)),
            unquote(code),
            unquote(pins),
            unquote(pattern_finder),
            timeout
          )
        end

    quote do
      timeout = unquote(timeout)

      {received, unquote(vars)} =
        receive do
          unquote(pattern) -> {received, unquote(vars)}
        after
          timeout -> flunk(unquote(failure_message))
        end

      received
    end
  end

  @indent "\n  "
  @max_mailbox_length 10

  @doc false
  def __timeout__(timeout) when is_integer(timeout) and timeout >= 0, do: timeout

  def __timeout__(timeout),
    do: raise(ArgumentError, "timeout must be a non-negative integer, got: #{inspect(timeout)}")

  @doc false
  def __timeout__(pattern, code, pins, pattern_finder, timeout) do
    {:messages, messages} = Process.info(self(), :messages)
    binary = Macro.to_string(pattern)

    if Enum.any?(messages, pattern_finder) do
      """
      Found message matching #{binary} after #{timeout}ms.

      This means the message was delivered too close to the timeout value, you may want to either:

        1. Give an increased timeout to `assert_receive/2`
        2. Increase the default timeout to all `assert_receive` in your
           test_helper.exs by setting ExUnit.configure(assert_receive_timeout: ...)
      """
    else
      {message, mailbox} = format_mailbox(messages)

      raise ExUnit.AssertionError,
        mailbox: mailbox,
        expr: code,
        message:
          "Assertion failed, no matching message after #{timeout}ms" <>
            ExUnit.Assertions.__pins__(pins) <> message,
        context: {:match, pins}
    end
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

  defp format_mailbox(messages) do
    length = length(messages)
    mailbox = Enum.take(messages, -@max_mailbox_length)

    {mailbox_message(length), mailbox}
  end

  defp mailbox_message(0), do: "\nThe process mailbox is empty."

  defp mailbox_message(1) do
    "\nShowing 1 of 1 message in the mailbox"
  end

  defp mailbox_message(length) when length > @max_mailbox_length do
    "\nShowing #{@max_mailbox_length} of #{length} messages in the mailbox"
  end

  defp mailbox_message(length) do
    "\nShowing #{length} of #{length} messages in the mailbox"
  end

  defp collect_pins_from_pattern(expr, vars) do
    {_, pins} =
      Macro.prewalk(expr, [], fn
        {:^, _, [{name, _, nil} = var]}, acc ->
          if {name, nil} in vars do
            {:ok, [{name, var} | acc]}
          else
            {:ok, acc}
          end

        form, acc ->
          {form, acc}
      end)

    Enum.uniq_by(pins, &elem(&1, 0))
  end

  defp collect_vars_from_pattern({:when, _, [left, right]}) do
    pattern = collect_vars_from_pattern(left)

    vars =
      for {name, _, context} = var <- collect_vars_from_pattern(right),
          Enum.any?(pattern, &match?({^name, _, ^context}, &1)),
          do: var

    pattern ++ vars
  end

  defp collect_vars_from_pattern(expr) do
    Macro.prewalk(expr, [], fn
      {:"::", _, [left, _]}, acc ->
        {[left], acc}

      {skip, _, [_]}, acc when skip in [:^, :@] ->
        {:ok, acc}

      {:_, _, context}, acc when is_atom(context) ->
        {:ok, acc}

      {_, [{:expanded, expanded} | _], _}, acc ->
        {[expanded], acc}

      {name, meta, context}, acc when is_atom(name) and is_atom(context) ->
        {:ok, [{name, [generated: true] ++ meta, context} | acc]}

      node, acc ->
        {node, acc}
    end)
    |> elem(1)
  end

  @doc false
  def __expand_pattern__({:when, meta, [left, right]}, caller) do
    left = prewalk_expand_pattern(left, Macro.Env.to_match(caller))
    right = prewalk_expand_pattern(right, %{caller | context: :guard})
    {:when, meta, [left, right]}
  end

  def __expand_pattern__(expr, caller) do
    prewalk_expand_pattern(expr, Macro.Env.to_match(caller))
  end

  defp prewalk_expand_pattern(expr, caller) do
    Macro.prewalk(expr, fn
      {:__aliases__, _, _} = expr ->
        Macro.expand(expr, caller)

      {left, meta, right} = expr ->
        case Macro.expand(expr, caller) do
          ^expr -> expr
          other -> {left, [expanded: other] ++ meta, right}
        end

      other ->
        Macro.expand(other, caller)
    end)
  end

  defp suppress_warning({name, meta, [expr, [do: clauses]]}) do
    clauses =
      Enum.map(clauses, fn {:->, meta, args} ->
        {:->, [generated: true] ++ meta, args}
      end)

    {name, meta, [expr, [do: clauses]]}
  end

  @doc ~S"""
  Asserts the `exception` is raised during `function` execution with
  the expected `message`, which can be a `Regex` or an exact `String`.
  Returns the rescued exception, fails otherwise.

  ## Examples

      assert_raise ArithmeticError, "bad argument in arithmetic expression", fn ->
        1 + "test"
      end

      assert_raise RuntimeError, ~r/^today's lucky number is 0\.\d+!$/, fn ->
        raise "today's lucky number is #{:rand.uniform()}!"
      end

  """
  def assert_raise(exception, message, function) when is_function(function) do
    error = assert_raise(exception, function)

    match? =
      cond do
        is_binary(message) -> Exception.message(error) == message
        Regex.regex?(message) -> Exception.message(error) =~ message
      end

    message =
      "Wrong message for #{inspect(exception)}\n" <>
        "expected:\n  #{inspect(message)}\n" <>
        "actual:\n" <> "  #{inspect(Exception.message(error))}"

    unless match?, do: flunk(message)

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
        name = error.__struct__

        cond do
          name == exception ->
            check_error_message(name, error)
            error

          name == ExUnit.AssertionError ->
            reraise(error, __STACKTRACE__)

          true ->
            message =
              "Expected exception #{inspect(exception)} " <>
                "but got #{inspect(name)} (#{Exception.message(error)})"

            reraise ExUnit.AssertionError, [message: message], __STACKTRACE__
        end
    else
      _ -> flunk("Expected exception #{inspect(exception)} but nothing was raised")
    end
  end

  defp check_error_message(module, error) do
    module.message(error)
  catch
    kind, reason ->
      message =
        "Got exception #{inspect(module)} but it failed to produce a message with:\n\n" <>
          Exception.format(kind, reason, __STACKTRACE__)

      flunk(message)
  end

  @doc """
  Asserts that `value1` and `value2` differ by no more than `delta`.

  This difference is inclusive, so the test will pass if the difference
  and the `delta` are equal.

  ## Examples

      assert_in_delta 1.1, 1.5, 0.2
      assert_in_delta 10, 15, 2
      assert_in_delta 10, 15, 5

  """
  def assert_in_delta(value1, value2, delta, message \\ nil)

  def assert_in_delta(_, _, delta, _) when delta < 0 do
    raise ArgumentError, "delta must always be a positive number, got: #{inspect(delta)}"
  end

  def assert_in_delta(value1, value2, delta, message) do
    diff = abs(value1 - value2)

    message =
      message ||
        "Expected the difference between #{inspect(value1)} and " <>
          "#{inspect(value2)} (#{inspect(diff)}) to be less than or equal to #{inspect(delta)}"

    assert diff <= delta, message
  end

  @doc """
  Asserts `expression` will throw a value.

  Returns the thrown value or fails otherwise.

  ## Examples

      assert catch_throw(throw(1)) == 1

  """
  defmacro catch_throw(expression) do
    do_catch(:throw, expression)
  end

  @doc """
  Asserts `expression` will exit.

  Returns the exit status/message of the current process or fails otherwise.

  ## Examples

      assert catch_exit(exit(1)) == 1

  To assert exits from linked processes started from the test, trap exits
  with `Process.flag/2` and assert the exit message with `assert_received/2`.

      Process.flag(:trap_exit, true)
      pid = spawn_link(fn -> Process.exit(self(), :normal) end)
      assert_receive {:EXIT, ^pid, :normal}

  """
  defmacro catch_exit(expression) do
    do_catch(:exit, expression)
  end

  @doc """
  Asserts `expression` will cause an error.

  Returns the error or fails otherwise.

  ## Examples

      assert catch_error(error(1)) == 1

  """
  defmacro catch_error(expression) do
    do_catch(:error, expression)
  end

  defp do_catch(kind, expr) do
    quote do
      try do
        _ = unquote(expr)
        flunk("Expected to catch #{unquote(kind)}, got nothing")
      rescue
        e in [ExUnit.AssertionError] ->
          reraise(e, __STACKTRACE__)
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
  Asserts that a message matching `pattern` was not received (and won't be received)
  within the `timeout` period, specified in milliseconds.

  The `pattern` argument must be a match pattern. Flunks with `failure_message`
  if a message matching `pattern` is received.

  ## Examples

      refute_receive :bye

  Refute received with an explicit timeout:

      refute_receive :bye, 1000

  """
  defmacro refute_receive(
             pattern,
             timeout \\ Application.fetch_env!(:ex_unit, :refute_receive_timeout),
             failure_message \\ nil
           ) do
    do_refute_receive(pattern, timeout, failure_message)
  end

  @doc """
  Asserts a message matching `pattern` was not received (i.e. it is not in the
  current process' mailbox).

  The `pattern` argument must be a match pattern. Flunks with `failure_message`
  if a message matching `pattern` was received.

  Timeout is set to 0, so there is no waiting time.

  ## Examples

      send(self(), :hello)
      refute_received :bye

      send(self(), :hello)
      refute_received :hello, "Oh No!"
      ** (ExUnit.AssertionError) Oh No!
      Process mailbox:
        :bye

  """
  defmacro refute_received(pattern, failure_message \\ nil) do
    do_refute_receive(pattern, 0, failure_message)
  end

  defp do_refute_receive(pattern, timeout, failure_message) do
    receive_clause = refute_receive_clause(pattern, failure_message)

    quote do
      receive do
        unquote(receive_clause)
      after
        unquote(timeout) -> false
      end
    end
  end

  defp refute_receive_clause(pattern, nil) do
    binary = Macro.to_string(pattern)

    quote do
      unquote(pattern) = actual ->
        flunk(
          "Unexpectedly received message #{inspect(actual)} (which matched #{unquote(binary)})"
        )
    end
  end

  defp refute_receive_clause(pattern, failure_message) do
    quote do
      unquote(pattern) -> flunk(unquote(failure_message))
    end
  end

  @doc """
  Asserts `value1` and `value2` are not within `delta`.

  This difference is exclusive, so the test will fail if the difference
  and the delta are equal.

  If you supply `message`, information about the values will
  automatically be appended to it.

  ## Examples

      refute_in_delta 1.1, 1.2, 0.2
      refute_in_delta 10, 11, 2

  """
  def refute_in_delta(value1, value2, delta, message \\ nil) do
    diff = abs(value1 - value2)

    message =
      if message do
        message <>
          " (difference between #{inspect(value1)} " <>
          "and #{inspect(value2)} is less than #{inspect(delta)})"
      else
        "Expected the difference between #{inspect(value1)} and " <>
          "#{inspect(value2)} (#{inspect(diff)}) to be more than #{inspect(delta)}"
      end

    refute diff < delta, message
  end

  @doc """
  Fails with a message.

  ## Examples

      flunk("This should raise an error")

  """
  @spec flunk :: no_return
  @spec flunk(String.t()) :: no_return
  def flunk(message \\ "Flunked!") when is_binary(message) do
    assert false, message: message
  end
end
