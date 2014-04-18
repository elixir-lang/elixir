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

  # Width of error counter field
  @prefix_width 5

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
  def format_test_failure(test_case, test, { kind, reason, stack }, counter, terminal) do
    test_info(with_counter(counter, "#{test} (#{inspect test_case})"), terminal)
      <> format_kind_reason(kind, reason, terminal)
      <> format_stacktrace(stack, test_case, test, terminal)
  end

  @doc """
  Receives a test case and formats its failure.
  """
  def format_test_case_failure(test_case, { kind, reason, stacktrace }, counter, terminal) do
    test_case_info(with_counter(counter, "#{inspect test_case}: "), terminal)
      <> format_kind_reason(kind, reason, terminal)
      <> format_stacktrace(stacktrace, test_case, nil, terminal)
  end

  defp format_kind_reason(:error, ExUnit.AssertionError[] = record, terminal) do
    fields =
      [note: if_value(record.message, &format_message(&1, terminal)),
       code: record.expr,
       lhs:  if_value(record.left,  &inspect/1),
       rhs:  if_value(record.right, &inspect/1)]

    fields
    |> filter_interesting_fields
    |> format_each_reason(terminal)
    |> make_into_lines
  end

  defp format_kind_reason(:error, exception, terminal) do
    error_info "** (#{inspect exception.__record__(:name)}) #{exception.message}", terminal
  end

  defp format_kind_reason(kind, reason, terminal) do
    error_info "** (#{kind}) #{inspect(reason)}", terminal
  end

  defp filter_interesting_fields(fields) do
    Enum.filter(fields, fn {_, value} ->
      value != ExUnit.AssertionError.no_value
    end)
  end

  defp format_each_reason(reasons, terminal) do
    label_length =
      reasons
      |> Enum.map(fn {label,_} -> String.length(atom_to_binary(label)) end)
      |> Enum.max
      |> Kernel.+(2)

    Enum.map(reasons, fn {label, value} ->
      format_label(label, label_length, terminal) <> value
    end)
  end

  defp if_value(value, fun) do
    if value == ExUnit.AssertionError.no_value do
      value
    else
      fun.(value)
    end
  end

  defp format_label(:note, _length, _terminal) do
    ""
  end

  defp format_label(label, length, terminal) do
    terminal.(:error_info, String.ljust("#{label}:", length))
  end

  defp format_message(value, terminal) do
    terminal.(:error_info, value)
  end

  defp make_into_lines(reasons) do
    "     " <> Enum.join(reasons, "\n     ") <> "\n"
  end

  defp format_stacktrace([{ test_case, test, _, location }|_], test_case, test, color) do
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
  defp test_case_info(msg, terminal), do: test_case_info(terminal.(:test_case_info, msg), nil)

  defp test_info(msg, nil),      do: msg <> "\n"
  defp test_info(msg, terminal), do: test_info(terminal.(:test_info, msg), nil)

  defp error_info(msg, nil),      do: "     " <> msg <> "\n"
  defp error_info(msg, terminal), do: error_info(terminal.(:error_info, msg), nil)

  defp location_info(msg, nil),      do: "     " <> msg <> "\n"
  defp location_info(msg, terminal), do: location_info(terminal.(:location_info, msg), nil)

  defp stacktrace_info(msg, nil),      do: "       " <> msg <> "\n"
  defp stacktrace_info(msg, terminal), do: stacktrace_info(terminal.(:stacktrace_info, msg), nil)
end
