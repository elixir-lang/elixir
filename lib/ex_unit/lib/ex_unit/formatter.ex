defmodule ExUnit.Formatter do
  @moduledoc """
  This module holds helper functions related to formatting and contains
  documentation about the formatting protocol.

  Formatters are registered at the `ExUnit.EventManager` event manager and
  will be send events by the runner.

  The following events are possible:

  * `{ :suite_started, opts }` - The suite has started with the specified
                                 options to the runner.
  * `{ :suite_finished, run_us, load_us }` - The suite has finished. `run_us` and
                                             `load_us` are the run and load
                                             times in microseconds respectively.
  * `{ :case_started, test_case }` - A test case has started. See
                                     `ExUnit.TestCase` for details.
  * `{ :case_finished, test_case }` - A test case has finished. See
                                      `ExUnit.TestCase` for details.
  * `{ :test_started, test_case }` - A test case has started. See
                                     `ExUnit.Test` for details.
  * `{ :test_finished, test_case }` - A test case has finished. See
                                     `ExUnit.Test` for details.

  """

  @type id :: term
  @type test_case :: ExUnit.TestCase.t
  @type test :: ExUnit.Test.t
  @type run_us :: pos_integer
  @type load_us :: pos_integer | nil

  import Exception, only: [format_stacktrace_entry: 1]

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

  @doc ~S"""
  Receives a test and formats its failure.
  """
  def format_test_failure(test_case, test, { kind, reason, stacktrace }, counter, color) do
    test_info("#{counter}) #{test} (#{inspect test_case})", color) <>
      format_kind_reason(kind, reason, color) <>
      format_stacktrace(stacktrace, test_case, test, color)
  end

  @doc """
  Receives a test case and formats its failure.
  """
  def format_test_case_failure(test_case, { kind, reason, stacktrace }, counter, color) do
    test_case_info("#{counter}) #{inspect test_case}: ", color) <>
      format_kind_reason(kind, reason, color) <>
      format_stacktrace(stacktrace, test_case, nil, color)
  end

  defp format_kind_reason(:error, ExUnit.ExpectationError[] = record, color) do
    prelude   = String.downcase record.prelude
    assertion = record.full_assertion
    max       = max(size(prelude), size(assertion))

    error_info("** (ExUnit.ExpectationError)", color) <>
      if desc = record.expr do
        max = max(max, size("instead got"))
        error_info("  #{pad(prelude, max)}: #{maybe_multiline(desc, max)}", color) <>
        error_info("  #{pad(assertion, max)}: #{maybe_multiline(record.expected, max)}", color) <>
        error_info("  #{pad("instead got", max)}: #{maybe_multiline(record.actual, max)}", color)
      else
        error_info("  #{pad(prelude, max)}: #{maybe_multiline(record.expected, max)}", color) <>
        error_info("  #{pad(assertion, max)}: #{maybe_multiline(record.actual, max)}", color)
      end
  end

  defp format_kind_reason(:error, exception, color) do
    error_info "** (#{inspect exception.__record__(:name)}) #{exception.message}", color
  end

  defp format_kind_reason(kind, reason, color) do
    error_info "** (#{kind}) #{inspect(reason)}", color
  end

  defp format_stacktrace([{ test_case, test, _, location }|_], test_case, test, color) do
    location_info("at #{location[:file]}:#{location[:line]}", color)
  end

  defp format_stacktrace([], _case, _test, _color) do
    ""
  end

  defp format_stacktrace(stacktrace, _case, _test, color) do
    location_info("stacktrace:", color) <>
      Enum.map_join(stacktrace, fn(s) -> stacktrace_info format_stacktrace_entry(s), color end)
  end

  defp pad(binary, max) do
    remaining = max - size(binary)
    if remaining > 0 do
      String.duplicate(" ", remaining) <>  binary
    else
      binary
    end
  end

  defp maybe_multiline(str, max) do
    unless multiline?(str) do
      String.strip(str)
    else
      "\n" <>
      Enum.join((for line <- String.split(str, ~r/\n/), do: String.duplicate(" ", max) <> line ), "\n")
    end
  end

  defp multiline?(<<>>), do: false

  defp multiline?(<<?\n, _ :: binary>>) do
    true
  end

  defp multiline?(<<_, rest :: binary>>) do
    multiline?(rest)
  end

  defp test_case_info(msg, nil),   do: "  " <> msg <> "failure on setup_all/teardown_all callback, tests invalidated\n"
  defp test_case_info(msg, color), do: test_case_info(color.(:test_case_info, msg), nil)

  defp test_info(msg, nil),   do: "  " <> msg <> "\n"
  defp test_info(msg, color), do: test_info(color.(:test_info, msg), nil)

  defp error_info(msg, nil),   do: "     " <> msg <> "\n"
  defp error_info(msg, color), do: error_info(color.(:error_info, msg), nil)

  defp location_info(msg, nil),   do: "     " <> msg <> "\n"
  defp location_info(msg, color), do: location_info(color.(:location_info, msg), nil)

  defp stacktrace_info(msg, nil),   do: "       " <> msg <> "\n"
  defp stacktrace_info(msg, color), do: stacktrace_info(color.(:stacktrace_info, msg), nil)
end
