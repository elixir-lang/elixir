defmodule ExUnit.Formatter do
  @moduledoc """
  Helper functions for formatting and the formatting protocols.

  Formatters are `GenServer`s specified during ExUnit configuration
  that receive a series of events as casts.

  The following events are possible:

    * `{:suite_started, opts}` -
      the suite has started with the specified options to the runner.

    * `{:suite_finished, run_us, load_us}` -
      the suite has finished. `run_us` and `load_us` are the run and load
      times in microseconds respectively.

    * `{:case_started, test_case}` -
      a test case has started. See `ExUnit.TestCase` for details.

    * `{:case_finished, test_case}` -
      a test case has finished. See `ExUnit.TestCase` for details.

    * `{:test_started, test}` -
      a test has started. See `ExUnit.Test` for details.

    * `{:test_finished, test}` -
      a test has finished. See `ExUnit.Test` for details.

  """

  @type id :: term
  @type test_case :: ExUnit.TestCase.t
  @type test :: ExUnit.Test.t
  @type run_us :: pos_integer
  @type load_us :: pos_integer | nil

  import Exception, only: [format_stacktrace_entry: 1, format_file_line: 3]

  @counter_padding "     "
  @no_value ExUnit.AssertionError.no_value

  @doc """
  Formats time taken running the test suite.

  It receives the time spent running the tests and
  optionally the time spent loading the test suite.

  ## Examples

      iex> format_time(10000, nil)
      "Finished in 0.01 seconds"

      iex> format_time(10000, 20000)
      "Finished in 0.03 seconds (0.02s on load, 0.01s on tests)"

      iex> format_time(10000, 200000)
      "Finished in 0.2 seconds (0.2s on load, 0.01s on tests)"

  """
  @spec format_time(run_us, load_us) :: String.t
  def format_time(run_us, nil) do
    "Finished in #{run_us |> normalize_us |> format_us} seconds"
  end

  def format_time(run_us, load_us) do
    run_us  = run_us |> normalize_us
    load_us = load_us |> normalize_us

    total_us = run_us + load_us
    "Finished in #{format_us total_us} seconds (#{format_us load_us}s on load, #{format_us run_us}s on tests)"
  end

  defp normalize_us(us) do
    div(us, 10000)
  end

  defp format_us(us) do
    if us < 10 do
      "0.0#{us}"
    else
      us = div us, 10
      "#{div(us, 10)}.#{rem(us, 10)}"
    end
  end

  @doc """
  Formats filters used to constrain cases to be run.

  ## Examples

      iex> format_filters([run: true, slow: false], :include)
      "Including tags: [run: true, slow: false]"

  """
  @spec format_filters(keyword, atom) :: String.t
  def format_filters(filters, type) do
    case type do
      :include -> "Including tags: #{inspect filters}"
      :exclude -> "Excluding tags: #{inspect filters}"
    end
  end

  @doc """
  Receives a test and formats its failure.
  """
  def format_test_failure(test, failures, counter, width, formatter) do
    %ExUnit.Test{name: name, case: case, tags: tags} = test

    test_info(with_counter(counter, "#{name} (#{inspect case})"), formatter) <>
      test_location(with_location(tags), formatter) <>
      Enum.map_join(Enum.with_index(failures), "", fn {{kind, reason, stack}, index} ->
        {text, stack} = format_kind_reason(test, kind, reason, stack, width, formatter)
        failure_header(failures, index) <> text <> format_stacktrace(stack, case, name, formatter)
       end) <>
      report(tags, failures, width, formatter)
  end


  @doc false
  def format_assertion_error(%ExUnit.AssertionError{} = struct) do
    format_assertion_error(%{}, struct, [], :infinity, fn _, msg -> msg end, "")
  end

  def format_assertion_error(test, struct, stack, width, formatter, counter_padding) do
    label_padding_size = if has_value?(struct.right), do: 7, else: 6
    padding_size = label_padding_size + byte_size(@counter_padding)
    inspect = &inspect_multiline(&1, padding_size, width)
    {left, right} = format_sides(struct, formatter, inspect)
    binding = if(struct.binding == [], do: ExUnit.AssertionError.no_value(), else: struct.binding)

    [
      note: if_value(struct.message, &format_message(&1, formatter)),
      code: if_value(struct.expr, &code_multiline(&1, padding_size)),
      code: unless_value(struct.expr, fn -> get_code(test, stack) || @no_value end),
      left: left,
      right: right,
      variables: if_value(binding, &format_binding(&1, width)),
    ]
    |> format_meta(formatter, label_padding_size)
    |> make_into_lines(counter_padding)
  end

  defp report(tags, failures, width, formatter) do
    case Map.take(tags, List.wrap(tags[:report])) do
      report when map_size(report) == 0 ->
        ""
      report ->
        report_spacing(failures) <>
          extra_info("tags:", formatter) <>
          Enum.map_join(report, "", fn {key, value} ->
            prefix = "       #{key}: "
            prefix <> inspect_multiline(value, byte_size(prefix), width) <> "\n"
          end)
    end
  end

  defp report_spacing([_]), do: ""
  defp report_spacing(_), do: "\n"

  @doc """
  Receives a test case and formats its failure.
  """
  def format_test_case_failure(test_case, failures, counter, width, formatter) do
    %ExUnit.TestCase{name: name} = test_case
    test_case_info(with_counter(counter, "#{inspect name}: "), formatter) <>
      Enum.map_join(Enum.with_index(failures), "", fn {{kind, reason, stack}, index} ->
        {text, stack} = format_kind_reason(test_case, kind, reason, stack, width, formatter)
        failure_header(failures, index) <> text <> format_stacktrace(stack, name, nil, formatter)
      end)
  end

  defp format_kind_reason(test, :error, %ExUnit.AssertionError{} = struct, stack, width, formatter) do
    {format_assertion_error(test, struct, stack, width, formatter, @counter_padding), stack}
  end

  defp format_kind_reason(test, :error, %FunctionClauseError{} = struct, stack, _width, formatter) do
    {blamed, stack} = Exception.blame(:error, struct, stack)
    banner = Exception.format_banner(:error, struct)
    blamed = FunctionClauseError.blame(blamed, &inspect/1, &blame_match(&1, &2, formatter))
    message = error_info(banner, formatter) <> "\n" <> pad(String.trim_leading(blamed, "\n"))
    {message <> format_code(test, stack, formatter), stack}
  end

  defp format_kind_reason(test, kind, reason, stack, _width, formatter) do
    message = error_info(Exception.format_banner(kind, reason), formatter)
    {message <> format_code(test, stack, formatter), stack}
  end

  defp format_code(test, stack, formatter) do
    if snippet = get_code(test, stack) do
      "     " <> formatter.(:extra_info, "code: ") <> snippet <> "\n"
    else
      ""
    end
  end

  defp get_code(%{case: case, name: name}, stack) do
    info = Enum.find_value(stack, fn {^case, ^name, _, info} -> info; _ -> nil end)
    file = info[:file]
    line = info[:line]
    if line > 0 && file && File.exists?(file) do
      file |> File.stream! |> Enum.at(line - 1) |> String.trim
    end
  end
  defp get_code(%{}, _) do
    nil
  end

  defp blame_match(%{match?: true, node: node}, _, formatter),
    do: formatter.(:blame_same, Macro.to_string(node))
  defp blame_match(%{match?: false, node: node}, _, formatter),
    do: formatter.(:blame_diff, Macro.to_string(node))
  defp blame_match(_, string, _formatter),
    do: string

  defp format_meta(fields, formatter, padding_size) do
    for {label, value} <- fields, has_value?(value) do
      format_label(label, formatter, padding_size) <> value
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

  defp code_multiline(expr, padding_size) when is_binary(expr) do
    padding = String.duplicate(" ", padding_size)
    String.replace(expr, "\n", "\n" <> padding)
  end
  defp code_multiline({fun, _, [expr]}, padding_size) when is_atom(fun) do
    code_multiline(Atom.to_string(fun) <> " " <> Macro.to_string(expr), padding_size)
  end
  defp code_multiline(expr, padding_size) do
    code_multiline(Macro.to_string(expr), padding_size)
  end

  defp format_binding(binding, width) do
    padding = @counter_padding <> "  "
    padding_size = byte_size(padding)

    result =
      Enum.map_join(binding, "\n" <> padding, fn {var, value} ->
        "#{var} = #{inspect_multiline(value, padding_size, width)}"
      end)

    "\n" <> padding <> result
  end

  defp inspect_multiline(expr, padding_size, width) do
    padding = String.duplicate(" ", padding_size)
    width = if width == :infinity, do: width, else: width - padding_size
    inspect(expr, [pretty: true, width: width])
    |> String.replace("\n", "\n" <> padding)
  end

  defp make_into_lines(reasons, padding) do
    padding <> Enum.join(reasons, "\n" <> padding) <> "\n"
  end

  defp format_sides(struct, formatter, inspect) do
    %{left: left, right: right} = struct

    case format_diff(left, right, formatter) do
      {left, right} ->
        {IO.iodata_to_binary(left), IO.iodata_to_binary(right)}
      nil ->
        {if_value(left, inspect), if_value(right, inspect)}
    end
  end

  defp format_diff(left, right, formatter) do
    if has_value?(left) and has_value?(right) and formatter.(:diff_enabled?, false) do
      if script = edit_script(left, right) do
        colorize_diff(script, formatter, {[], []})
      end
    end
  end

  defp colorize_diff(script, formatter, acc) when is_list(script) do
    Enum.reduce(script, acc, &colorize_diff(&1, formatter, &2))
  end

  defp colorize_diff({:eq, content}, _formatter, {left, right}) do
    {[left | content], [right | content]}
  end

  defp colorize_diff({:del, content}, formatter, {left, right}) do
    format = colorize_format(content, :diff_delete, :diff_delete_whitespace)
    {[left | formatter.(format, content)], right}
  end

  defp colorize_diff({:ins, content}, formatter, {left, right}) do
    format = colorize_format(content, :diff_insert, :diff_insert_whitespace)
    {left, [right | formatter.(format, content)]}
  end

  defp colorize_format(content, normal, whitespace) do
    if String.trim_leading(content) == "", do: whitespace, else: normal
  end

  defp edit_script(left, right) do
    task = Task.async(ExUnit.Diff, :script, [left, right])
    case Task.yield(task, 1_500) || Task.shutdown(task, :brutal_kill) do
      {:ok, script} -> script
      nil -> nil
    end
  end

  defp format_stacktrace([], _case, _test, _color) do
    ""
  end

  defp format_stacktrace(stacktrace, test_case, test, color) do
    extra_info("stacktrace:", color) <>
      Enum.map_join(stacktrace, fn entry ->
        stacktrace_info format_stacktrace_entry(entry, test_case, test), color
      end)
  end

  defp format_stacktrace_entry({test_case, test, _, location}, test_case, test) do
    format_file_line(location[:file], location[:line], " (test)")
  end

  defp format_stacktrace_entry(entry, _test_case, _test) do
    format_stacktrace_entry(entry)
  end

  defp with_location(tags) do
    "#{Path.relative_to_cwd(tags[:file])}:#{tags[:line]}"
  end

  defp failure_header([_], _), do: ""
  defp failure_header(_, i), do: "\n#{@counter_padding}Failure ##{i+1}\n"

  defp with_counter(counter, msg) when counter < 10  do "  #{counter}) #{msg}" end
  defp with_counter(counter, msg) when counter < 100 do  " #{counter}) #{msg}" end
  defp with_counter(counter, msg)                    do   "#{counter}) #{msg}" end

  defp test_case_info(msg, nil),       do: msg <> "failure on setup_all callback, test invalidated\n"
  defp test_case_info(msg, formatter), do: test_case_info(formatter.(:test_case_info, msg), nil)

  defp test_info(msg, nil),       do: msg <> "\n"
  defp test_info(msg, formatter), do: test_info(formatter.(:test_info, msg), nil)

  defp test_location(msg, nil),       do: "     " <> msg <> "\n"
  defp test_location(msg, formatter), do: test_location(formatter.(:location_info, msg), nil)

  defp pad(msg) do
    "     " <> String.replace(msg, "\n", "\n     ") <> "\n"
  end

  defp error_info(msg, nil),       do: pad(msg)
  defp error_info(msg, formatter), do: pad(formatter.(:error_info, msg))

  defp extra_info(msg, nil),       do: pad(msg)
  defp extra_info(msg, formatter), do: pad(formatter.(:extra_info, msg))

  defp stacktrace_info("", _formatter), do: ""
  defp stacktrace_info(msg, nil),       do: "       " <> msg <> "\n"
  defp stacktrace_info(msg, formatter), do: stacktrace_info(formatter.(:stacktrace_info, msg), nil)
end
