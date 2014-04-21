defmodule ExUnit.Formatter do
  @moduledoc """
  This module holds helper functions related to formatting and contains
  documentation about the formatting protocol.

  Formatters are registered at the `ExUnit.EventManager` event manager and
  will be send events by the runner.

  The following events are possible:

  * `{:suite_started, opts}` - The suite has started with the specified
                                 options to the runner.
  * `{:suite_finished, run_us, load_us}` - The suite has finished. `run_us` and
                                             `load_us` are the run and load
                                             times in microseconds respectively.
  * `{:case_started, test_case}` - A test case has started. See
                                     `ExUnit.TestCase` for details.
  * `{:case_finished, test_case}` - A test case has finished. See
                                      `ExUnit.TestCase` for details.
  * `{:test_started, test_case}` - A test case has started. See
                                     `ExUnit.Test` for details.
  * `{:test_finished, test_case}` - A test case has finished. See
                                     `ExUnit.Test` for details.

  """

  @type id :: term
  @type test_case :: ExUnit.TestCase.t
  @type test :: ExUnit.Test.t
  @type run_us :: pos_integer
  @type load_us :: pos_integer | nil

  import Exception, only: [format_stacktrace_entry: 1]

  @label_padding   "      "
  @counter_padding "     "
  @inspect_padding @counter_padding <> @label_padding

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

    ms = run_us + load_us
    "Finished in #{format_us ms} seconds (#{format_us load_us}s on load, #{format_us run_us}s on tests)"
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
  Formats filters used to constain cases to be run.

  ## Examples

    iex> format_filters([run: true, slow: false], :include)
    "Including tags: [run: true, slow: false]"

  """
  @spec format_filters(Keyword.t, atom) :: String.t
  def format_filters(filters, type) do
    case type do
      :include -> "Including tags: #{inspect filters}"
      :exclude -> "Excluding tags: #{inspect filters}"
    end
  end

  @doc """
  Receives a test and formats its failure.
  """
  def format_test_failure(test_case, test, {kind, reason, stack}, counter, width, formatter) do
    test_info(with_counter(counter, "#{test} (#{inspect test_case})"), formatter)
      <> format_kind_reason(kind, reason, width, formatter)
      <> format_stacktrace(stack, test_case, test, formatter)
  end

  @doc """
  Receives a test case and formats its failure.
  """
  def format_test_case_failure(test_case, {kind, reason, stacktrace}, counter, width, formatter) do
    test_case_info(with_counter(counter, "#{inspect test_case}: "), formatter)
      <> format_kind_reason(kind, reason, width, formatter)
      <> format_stacktrace(stacktrace, test_case, nil, formatter)
  end

  defp format_kind_reason(:error, ExUnit.AssertionError[] = record, width, formatter) do
    width = if width == :infinity, do: width, else: width - byte_size(@inspect_padding)

    fields =
      [note: if_value(record.message, &format_message(&1, formatter)),
       code: if_value(record.expr, &macro_multiline(&1, width)),
       lhs:  if_value(record.left,  &inspect_multiline(&1, width)),
       rhs:  if_value(record.right, &inspect_multiline(&1, width))]

    fields
    |> filter_interesting_fields
    |> format_each_reason(formatter)
    |> make_into_lines(@counter_padding)
  end

  defp format_kind_reason(:error, exception, _width, formatter) do
    error_info "** (#{inspect exception.__record__(:name)}) #{exception.message}", formatter
  end

  defp format_kind_reason(kind, reason, _width, formatter) do
    error_info "** (#{kind}) #{inspect(reason)}", formatter
  end

  defp filter_interesting_fields(fields) do
    Enum.filter(fields, fn {_, value} ->
      value != ExUnit.AssertionError.no_value
    end)
  end

  defp format_each_reason(reasons, formatter) do
    Enum.map(reasons, fn {label, value} ->
      format_label(label, formatter) <> value
    end)
  end

  defp if_value(value, fun) do
    if value == ExUnit.AssertionError.no_value do
      value
    else
      fun.(value)
    end
  end

  defp format_label(:note, _formatter) do
    ""
  end

  defp format_label(label, formatter) do
    formatter.(:error_info, String.ljust("#{label}:", byte_size(@label_padding)))
  end

  defp format_message(value, formatter) do
    formatter.(:error_info, value)
  end

  defp macro_multiline(expr, _width) when is_binary(expr) do
    expr
  end

  defp macro_multiline(expr, _width) do
    expr |> Macro.to_string
  end

  defp inspect_multiline(expr, width) do
    expr
    |> inspect(pretty: true, width: width)
    |> String.replace("\n", "\n" <> @inspect_padding)
  end

  defp make_into_lines(reasons, padding) do
    padding <> Enum.join(reasons, "\n" <> padding) <> "\n"
  end

  defp format_stacktrace([{test_case, test, _, location}|_], test_case, test, color) do
    location_info("#{location[:file]}:#{location[:line]}", color)
  end

  defp format_stacktrace([], _case, _test, _color) do
    ""
  end

  defp format_stacktrace(stacktrace, _case, _test, color) do
    location_info("stacktrace:", color) <>
      Enum.map_join(stacktrace, fn(s) -> stacktrace_info format_stacktrace_entry(s), color end)
  end

  defp with_counter(counter, msg) when counter < 10  do "  #{counter}) #{msg}" end
  defp with_counter(counter, msg) when counter < 100 do  " #{counter}) #{msg}" end
  defp with_counter(counter, msg)                    do   "#{counter}) #{msg}" end

  defp test_case_info(msg, nil),      do: msg <> "failure on setup_all/teardown_all callback, tests invalidated\n"
  defp test_case_info(msg, formatter), do: test_case_info(formatter.(:test_case_info, msg), nil)

  defp test_info(msg, nil),      do: msg <> "\n"
  defp test_info(msg, formatter), do: test_info(formatter.(:test_info, msg), nil)

  defp error_info(msg, nil),      do: "     " <> msg <> "\n"
  defp error_info(msg, formatter), do: error_info(formatter.(:error_info, msg), nil)

  defp location_info(msg, nil),      do: "     " <> msg <> "\n"
  defp location_info(msg, formatter), do: location_info(formatter.(:location_info, msg), nil)

  defp stacktrace_info(msg, nil),      do: "       " <> msg <> "\n"
  defp stacktrace_info(msg, formatter), do: stacktrace_info(formatter.(:stacktrace_info, msg), nil)
end
