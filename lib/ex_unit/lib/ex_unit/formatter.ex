defmodule ExUnit.Formatter do
  @moduledoc """
  Helper functions for formatting and the formatting protocols.

  Formatters are `GenServer`s specified during ExUnit configuration
  that receive a series of events as casts.

  The following events are possible:

    * `{:suite_started, opts}` -
      the suite has started with the specified options to the runner.

    * `{:suite_finished, times_us}` -
      the suite has finished. Returns several measurements in microseconds
      for running the suite. See `t:times_us` for more information.

    * `{:module_started, test_module}` -
      a test module has started. See `ExUnit.TestModule` for details.

    * `{:module_finished, test_module}` -
      a test module has finished. See `ExUnit.TestModule` for details.

    * `{:test_started, test}` -
      a test has started. See `ExUnit.Test` for details.

    * `{:test_finished, test}` -
      a test has finished. See `ExUnit.Test` for details.

    * `{:sigquit, [test | test_module]}` -
      the VM is going to shutdown. It receives the test cases (or test
      module in case of `setup_all`) still running.

  The formatter will also receive the following events but they are deprecated
  and should be ignored:

    * `{:case_started, test_module}` -
      a test module has started. See `ExUnit.TestModule` for details.

    * `{:case_finished, test_module}` -
      a test module has finished. See `ExUnit.TestModule` for details.

  The full ExUnit configuration is passed as the argument to `c:GenServer.init/1` callback when the
  formatters are started. If you need to do runtime configuration of a
  formatter, you can add any configuration needed by using `ExUnit.configure/1`
  or `ExUnit.start/1`, and this will then be included in the options passed to
  the `c:GenServer.init/1` callback.
  """

  @type id :: term
  @type test :: ExUnit.Test.t()

  @typedoc """
  The times spent on several parts of the test suite.

  The following properties can be computed:

      sync = run - (async || 0)
      total = run + (load || 0)

  `async` is nil when there are no async tests.
  `load` is nil when the test suite is running and loading
  tests concurrently.
  """
  @type times_us :: %{
          run: pos_integer,
          async: pos_integer | nil,
          load: pos_integer | nil
        }

  import Exception, only: [format_stacktrace_entry: 1, format_file_line: 3]

  alias ExUnit.Diff
  alias Inspect.Algebra

  @counter_padding "     "
  @mailbox_label_padding @counter_padding <> "  "
  @formatter_exceptions [ExUnit.AssertionError, FunctionClauseError]
  @no_value ExUnit.AssertionError.no_value()

  @doc """
  Formats time taken running the test suite.

  ## Examples

      iex> format_times(%{run: 10000, async: nil, load: nil})
      "Finished in 0.01 seconds (0.00s async, 0.01s sync)"

      iex> format_times(%{run: 10000, async: nil, load: 20000})
      "Finished in 0.03 seconds (0.02s on load, 0.00s async, 0.01s sync)"

      iex> format_times(%{run: 10000, async: nil, load: 200_000})
      "Finished in 0.2 seconds (0.2s on load, 0.00s async, 0.01s sync)"

      iex> format_times(%{run: 100_000, async: 50000, load: 200_000})
      "Finished in 0.3 seconds (0.2s on load, 0.05s async, 0.05s sync)"

  """
  @spec format_times(times_us) :: String.t()
  def format_times(times) do
    run_us = normalize_us(times.run)
    load_us = normalize_us(times.load)
    async_us = normalize_us(times.async)
    sync_us = run_us - async_us
    total_us = run_us + load_us

    maybe_load =
      if times.load do
        "#{format_us(load_us)}s on load, "
      else
        ""
      end

    "Finished in #{format_us(total_us)} seconds " <>
      "(#{maybe_load}#{format_us(async_us)}s async, #{format_us(sync_us)}s sync)"
  end

  defp normalize_us(nil), do: 0
  defp normalize_us(us), do: div(us, 10000)

  defp format_us(us) do
    if us < 10 do
      "0.0#{us}"
    else
      us = div(us, 10)
      "#{div(us, 10)}.#{rem(us, 10)}"
    end
  end

  # Deprecate me on Elixir v1.16
  @doc false
  def format_time(run, load) do
    format_times(%{run: run, load: load, async: nil})
  end

  @doc """
  Formats filters used to constrain cases to be run.

  ## Examples

      iex> format_filters([run: true, slow: false], :include)
      "Including tags: [run: true, slow: false]"

  """
  @spec format_filters(keyword, atom) :: String.t()
  def format_filters(filters, type) do
    case type do
      :exclude -> "Excluding tags: #{inspect(filters)}"
      :include -> "Including tags: #{inspect(filters)}"
    end
  end

  @doc """
  Receives a test and formats its failure.
  """
  def format_test_failure(test, failures, counter, width, formatter) do
    %ExUnit.Test{name: name, module: module, tags: tags} = test

    test_info(with_counter(counter, "#{name} (#{inspect(module)})"), formatter) <>
      test_location(with_location(tags), formatter) <>
      Enum.map_join(Enum.with_index(failures), "", fn {{kind, reason, stack}, index} ->
        {text, stack} = format_kind_reason(test, kind, reason, stack, width, formatter)

        failure_header(failures, index) <>
          text <> format_stacktrace(stack, module, name, formatter)
      end)
  end

  @doc false
  def format_assertion_error(%ExUnit.AssertionError{} = struct) do
    format_exception(%{}, struct, [], :infinity, fn _, msg -> msg end, "") |> elem(0)
  end

  defp format_exception(test, %ExUnit.AssertionError{} = struct, stack, width, formatter, pad) do
    label_padding_size = if has_value?(struct.right), do: 7, else: 6
    padding_size = label_padding_size + byte_size(@counter_padding)

    code_multiline =
      if struct.doctest != @no_value,
        do: &pad_multiline(&1, padding_size),
        else: &code_multiline(&1, padding_size)

    formatted =
      [
        note: if_value(struct.message, &format_message(&1, formatter)),
        doctest: if_value(struct.doctest, &pad_multiline(&1, 2 + byte_size(@counter_padding))),
        code: if_value(struct.expr, code_multiline),
        code: unless_value(struct.expr, fn -> get_code(test, stack) || @no_value end),
        arguments: if_value(struct.args, &format_args(&1, width))
      ]
      |> Kernel.++(format_context(struct, formatter, padding_size, width))
      |> format_meta(formatter, pad, label_padding_size)
      |> IO.iodata_to_binary()

    {formatted, stack}
  end

  defp format_exception(test, %FunctionClauseError{} = struct, stack, _width, formatter, _pad) do
    {blamed, stack} = Exception.blame(:error, struct, stack)
    banner = Exception.format_banner(:error, struct)
    blamed = FunctionClauseError.blame(blamed, &inspect/1, &blame_match(&1, formatter))
    message = error_info(banner, formatter) <> "\n" <> pad(String.trim_leading(blamed, "\n"))
    {message <> format_code(test, stack, formatter), stack}
  end

  @doc false
  @deprecated "Use ExUnit.Formatter.format_test_all_failure/5 instead"
  def format_test_case_failure(test_case, failures, counter, width, formatter) do
    format_test_all_failure(test_case, failures, counter, width, formatter)
  end

  @doc """
  Receives a test module and formats its failure.
  """
  def format_test_all_failure(test_module, failures, counter, width, formatter) do
    name = test_module.name

    test_module_info(with_counter(counter, "#{inspect(name)}: "), formatter) <>
      Enum.map_join(Enum.with_index(failures), "", fn {{kind, reason, stack}, index} ->
        {text, stack} = format_kind_reason(test_module, kind, reason, stack, width, formatter)
        failure_header(failures, index) <> text <> format_stacktrace(stack, name, nil, formatter)
      end)
  end

  defp format_kind_reason(test, :error, %mod{} = struct, stack, width, formatter)
       when mod in @formatter_exceptions do
    format_exception(test, struct, stack, width, formatter, @counter_padding)
  end

  defp format_kind_reason(test, kind, reason, stack, width, formatter) do
    case linked_or_trapped_exit(kind, reason) do
      {header, wrapped_reason, wrapped_stack} ->
        struct = Exception.normalize(:error, wrapped_reason, wrapped_stack)

        {formatted_reason, wrapped_stack} =
          format_exception(test, struct, wrapped_stack, width, formatter, @counter_padding)

        formatted_stack = format_stacktrace(wrapped_stack, test.module, test.name, formatter)
        {error_info(header, formatter) <> pad(formatted_reason <> formatted_stack), stack}

      :error ->
        {reason, stack} = Exception.blame(kind, reason, stack)
        message = error_info(Exception.format_banner(kind, reason), formatter)
        {message <> format_code(test, stack, formatter), stack}
    end
  end

  defp linked_or_trapped_exit({:EXIT, pid}, {reason, [_ | _] = stack})
       when reason.__struct__ in @formatter_exceptions
       when reason == :function_clause do
    {"** (EXIT from #{inspect(pid)}) an exception was raised:\n", reason, stack}
  end

  defp linked_or_trapped_exit(:exit, {{reason, [_ | _] = stack}, {mod, fun, args}})
       when is_atom(mod) and is_atom(fun) and is_list(args) and
              reason.__struct__ in @formatter_exceptions
       when is_atom(mod) and is_atom(fun) and is_list(args) and reason == :function_clause do
    {
      "** (exit) exited in: #{Exception.format_mfa(mod, fun, args)}\n   ** (EXIT) an exception was raised:",
      reason,
      stack
    }
  end

  defp linked_or_trapped_exit(_kind, _reason), do: :error

  defp format_code(test, stack, formatter) do
    if snippet = get_code(test, stack) do
      "     " <> formatter.(:extra_info, "code: ") <> snippet <> "\n"
    else
      ""
    end
  end

  defp get_code(%{module: module, name: name}, stack) do
    info =
      Enum.find_value(stack, fn
        {^module, ^name, _, info} -> info
        _ -> nil
      end)

    file = info[:file]
    line = info[:line]

    if line > 0 && file && File.exists?(file) do
      file |> File.stream!() |> Enum.at(line - 1) |> String.trim()
    end
  rescue
    _ -> nil
  end

  defp get_code(%{}, _) do
    nil
  end

  defp blame_match(%{match?: true, node: node}, _formatter),
    do: Macro.to_string(node)

  defp blame_match(%{match?: false, node: node}, formatter),
    do: formatter.(:blame_diff, Macro.to_string(node))

  defp format_meta(fields, formatter, padding, padding_size) do
    for {label, value} <- fields, has_value?(value) do
      [padding, format_label(label, formatter, padding_size), value, "\n"]
    end
  end

  defp if_value(value, fun) do
    if has_value?(value) do
      fun.(value)
    else
      value
    end
  end

  defp unless_value(value, fun) do
    if has_value?(value) do
      @no_value
    else
      fun.()
    end
  end

  defp has_value?(value) do
    value != @no_value
  end

  defp format_label(:note, _formatter, _padding_size), do: ""

  defp format_label(label, formatter, padding_size) do
    formatter.(:extra_info, String.pad_trailing("#{label}:", padding_size))
  end

  defp format_message(value, formatter) do
    value = String.replace(value, "\n", "\n" <> @counter_padding)
    formatter.(:error_info, value)
  end

  defp format_args(args, width) do
    entries =
      for {arg, i} <- Enum.with_index(args, 1) do
        """

                 # #{i}
                 #{inspect_multiline(arg, 9, width)}
        """
      end

    ["\n" | entries]
  end

  @assertions [
    :assert,
    :assert_raise,
    :assert_receive,
    :assert_received,
    :refute,
    :refute_receive,
    :refute_received
  ]

  defp code_multiline({fun, _, [expr]}, padding_size) when fun in @assertions do
    pad_multiline(Atom.to_string(fun) <> " " <> Macro.to_string(expr), padding_size)
  end

  defp code_multiline(expr, padding_size) do
    pad_multiline(Macro.to_string(expr), padding_size)
  end

  defp inspect_multiline(expr, padding_size, width) do
    width = if width == :infinity, do: width, else: width - padding_size

    expr
    |> Algebra.to_doc(%Inspect.Opts{width: width})
    |> Algebra.group()
    |> Algebra.nest(padding_size)
    |> Algebra.format(width)
  end

  defp format_context(%{context: {:mailbox, _pins, []}}, _, _, _) do
    []
  end

  defp format_context(
         %{left: left, context: {:mailbox, pins, mailbox}},
         formatter,
         padding_size,
         width
       ) do
    formatted_mailbox =
      for message <- mailbox do
        {pattern, value} =
          format_sides(
            left,
            message,
            {:match, pins},
            formatter,
            padding_size + 5,
            width
          )

        [
          "\n",
          @mailbox_label_padding,
          format_label(:pattern, formatter, 9),
          pattern,
          "\n",
          @mailbox_label_padding,
          format_label(:value, formatter, 9),
          value
        ]
      end

    [mailbox: Enum.join(formatted_mailbox, "\n")]
  end

  defp format_context(
         %{left: left, right: right, context: context},
         formatter,
         padding_size,
         width
       ) do
    {left, right} = format_sides(left, right, context, formatter, padding_size, width)
    [left: left, right: right]
  end

  defp format_sides(left, right, context, formatter, padding_size, width) do
    inspect = &inspect_multiline(&1, padding_size, width)
    content_width = if width == :infinity, do: width, else: width - padding_size

    case format_diff(left, right, context, formatter) do
      {result, _env} ->
        left =
          result.left
          |> Diff.to_algebra(&colorize_diff_delete(&1, formatter))
          |> Algebra.nest(padding_size)
          |> Algebra.format(content_width)

        right =
          result.right
          |> Diff.to_algebra(&colorize_diff_insert(&1, formatter))
          |> Algebra.nest(padding_size)
          |> Algebra.format(content_width)

        {left, right}

      nil when is_atom(context) ->
        {if_value(left, inspect), if_value(right, inspect)}

      nil ->
        {if_value(left, &code_multiline(&1, padding_size)), if_value(right, inspect)}
    end
  end

  defp format_diff(left, right, context, formatter) do
    if has_value?(left) and has_value?(right) and formatter.(:diff_enabled?, false) do
      find_diff(left, right, context)
    end
  end

  defp colorize_diff_delete(doc, formatter) do
    format = colorize_format(doc, :diff_delete, :diff_delete_whitespace)
    formatter.(format, doc)
  end

  defp colorize_diff_insert(doc, formatter) do
    format = colorize_format(doc, :diff_insert, :diff_insert_whitespace)
    formatter.(format, doc)
  end

  defp colorize_format(content, normal, whitespace) when is_binary(content) do
    if String.trim_leading(content) == "", do: whitespace, else: normal
  end

  defp colorize_format(_doc, normal, _whitespace) do
    normal
  end

  defp find_diff(left, right, context) do
    task = Task.async(Diff, :compute, [left, right, context])

    case Task.yield(task, 1500) || Task.shutdown(task, :brutal_kill) do
      {:ok, diff} -> diff
      nil -> nil
    end
  end

  defp format_stacktrace([], _case, _test, _color) do
    ""
  end

  defp format_stacktrace(stacktrace, test_case, test, color) do
    extra_info("stacktrace:", color) <>
      Enum.map_join(stacktrace, fn entry ->
        stacktrace_info(format_stacktrace_entry(entry, test_case, test), color)
      end)
  end

  defp format_stacktrace_entry({test_case, test, _, location}, test_case, test) do
    format_file_line(location[:file], location[:line], " (test)")
  end

  defp format_stacktrace_entry(entry, _test_case, _test) do
    format_stacktrace_entry(entry)
  end

  defp with_location(tags) do
    path = "#{Path.relative_to_cwd(tags[:file])}:#{tags[:line]}"

    if prefix = Application.get_env(:ex_unit, :test_location_relative_path) do
      Path.join(prefix, path)
    else
      path
    end
  end

  defp failure_header([_], _), do: ""
  defp failure_header(_, i), do: "\n#{@counter_padding}Failure ##{i + 1}\n"

  defp with_counter(counter, msg) when counter < 10 do
    "  #{counter}) #{msg}"
  end

  defp with_counter(counter, msg) when counter < 100 do
    " #{counter}) #{msg}"
  end

  defp with_counter(counter, msg) do
    "#{counter}) #{msg}"
  end

  defp test_module_info(msg, nil),
    do: msg <> "failure on setup_all callback, all tests have been invalidated\n"

  defp test_module_info(msg, formatter),
    do: test_module_info(formatter.(:test_module_info, msg), nil)

  defp test_info(msg, nil), do: msg <> "\n"
  defp test_info(msg, formatter), do: test_info(formatter.(:test_info, msg), nil)

  defp test_location(msg, nil), do: "     " <> msg <> "\n"
  defp test_location(msg, formatter), do: test_location(formatter.(:location_info, msg), nil)

  defp pad(msg) do
    "     " <> pad_multiline(msg, 5) <> "\n"
  end

  defp pad_multiline(expr, padding_size) when is_binary(expr) do
    padding = String.duplicate(" ", padding_size)
    String.replace(expr, "\n", "\n" <> padding)
  end

  defp error_info(msg, nil), do: pad(msg)
  defp error_info(msg, formatter), do: pad(formatter.(:error_info, msg))

  defp extra_info(msg, nil), do: pad(msg)
  defp extra_info(msg, formatter), do: pad(formatter.(:extra_info, msg))

  defp stacktrace_info("", _formatter), do: ""
  defp stacktrace_info(msg, nil), do: "       " <> msg <> "\n"

  defp stacktrace_info(msg, formatter),
    do: stacktrace_info(formatter.(:stacktrace_info, msg), nil)
end
