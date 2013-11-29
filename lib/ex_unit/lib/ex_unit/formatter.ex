defmodule ExUnit.Formatter do
  @moduledoc """
  This module simply defines the callbacks
  expected by an ExUnit.Formatter.
  """

  use Behaviour

  @type id :: term
  @type test_case :: ExUnit.TestCase.t
  @type test :: ExUnit.Test.t
  @type run_us :: pos_integer
  @type load_us :: pos_integer | nil

  defcallback suite_started(opts :: list) :: id
  defcallback suite_finished(id, run_us, load_us) :: non_neg_integer

  defcallback case_started(id, test_case) :: any
  defcallback case_finished(id, test_case) :: any

  defcallback test_started(id, test) :: any
  defcallback test_finished(id, test) :: any

  import Exception, only: [format_stacktrace_entry: 2]

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

  @doc %S"""
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
    format_expectation_error_inner(record, color)
  end

  # Pretty printing code is often ugly. This is no exception.
  defp format_kind_reason(:error, ExUnit.Prop.PropertyError[] = record, color) do
    printer = fn max ->
      shrinks_label = "after shrinks"
      input_label   = "generated input"
      max           = max(max, size(input_label))
      
      output = 
        error_info("  #{pad(shrinks_label, max)}: #{maybe_multiline(inspect(record.shrinks), max)}", color) <>
        error_info("  #{pad(input_label, max)}: #{maybe_multiline(inspect(record.input), max)}", color)
      { output, max }
    end
    case record.wrapped do
      { :error, ExUnit.ExpectationError[] = inner } ->
        error_info("** (ExUnit.ExpectationError in ExUnit.Prop.PropertyError)", color) <>
        format_expectation_error_inner(inner, color, printer)
      { :error, e } when is_exception(e) ->
        string          = "(#{inspect e.__record__(:name)}) #{e.message}"
        exc_label       = "exception"
        { output, max } = printer.(0) 
        error_info("** (ExUnit.Prop.PropertyError)", color) <> output <>
        error_info("  #{pad(exc_label, max)}: #{maybe_multiline(string, max)}", color)
      { kind, reason } ->
        string          = "(#{kind}) #{reason}"
        caught_label    = "caught"
        { output, max } = printer.(0) 
        error_info("** (ExUnit.Prop.PropertyError)", color) <> output <>
        error_info("  #{pad(caught_label, max)}: #{maybe_multiline(string, max)}", color)
    end
  end

  defp format_kind_reason(:error, exception, color) do
    error_info "** (#{inspect exception.__record__(:name)}) #{exception.message}", color
  end

  defp format_kind_reason(kind, reason, color) do
    error_info "** (#{kind}) #{inspect(reason)}", color
  end
  
  defp format_expectation_error_inner(ExUnit.ExpectationError[] = record, color, 
                                      header // &{nil, &1}) do
    prelude   = String.downcase record.prelude
    assertion = record.full_assertion
    max       = max(size(prelude), size(assertion))

    { header_output, max } = header.(max)

    (if header_output, do: header_output, else: error_info("** (ExUnit.ExpectationError)", color)) <>
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

  defp format_stacktrace([{ test_case, test, _, [ file: file, line: line ] }|_], test_case, test, color) do
    location_info("at #{Path.relative_to_cwd(file)}:#{line}", color)
  end

  defp format_stacktrace(stacktrace, _case, _test, color) do
    cwd = System.cwd
    location_info("stacktrace:", color) <>
      Enum.map_join(stacktrace, fn(s) -> stacktrace_info format_stacktrace_entry(s, cwd), color end)
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
      Enum.join((lc line inlist String.split(str, %r/\n/), do: String.duplicate(" ", max) <> line ), "\n")
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
