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

  @prefix_width 5  # width of error counter field

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
  def format_test_failure(test_case, test, { kind, reason, [] }, counter, terminal) do
    [
     test_info(with_counter(counter, "#{test} (#{inspect test_case})"), terminal),
     format_kind_reason(kind, reason, terminal),
    ]
    |> Enum.join
  end

  def format_test_failure(test_case, 
                          test, 
                          krs = { 
                            _kind, 
                            _reason=ExUnit.AssertionError[], 
                            _stacktrace=[{ _, _, _, _location }|_] 
                          }, 
                          counter, 
                          terminal) do
    do_basic_format_test_failure(test_case, test, krs, counter, terminal)
  end


  def format_test_failure(test_case, 
                          test, 
                          krs = {
                            _kind, 
                            _reason, 
                            stacktrace
                          }, 
                          counter, 
                          terminal) do
    do_basic_format_test_failure(test_case, test, krs, counter, terminal) <>
    format_stacktrace(stacktrace, test_case, test, terminal)
  end

  def do_basic_format_test_failure(test_case, 
                                   test, 
                                   { 
                                     kind, 
                                     reason,
                                     [{ _, _, _, location }|_] 
                                   }, 
                                   counter, 
                                   terminal) do

    [ 
      test_info(with_counter(counter, "#{test} (#{inspect test_case})"), terminal),
      location_info("#{location[:file]}:#{location[:line]}", terminal),
      format_kind_reason(kind, reason, terminal)
    ]
    |> Enum.join
  end



  @doc """
  Receives a test case and formats its failure.
  """
  def format_test_case_failure(test_case, { kind, reason, stacktrace }, counter, terminal) do
    [ 
      test_case_info(with_counter(counter, "#{inspect test_case}: "), terminal),
      format_kind_reason(kind, reason, terminal),
      format_stacktrace(stacktrace, test_case, nil, terminal)
    ]
    |> Enum.join
  end

  defp format_kind_reason(:error, ExUnit.AssertionError[] = record, terminal) do
    format_each_reason(interesting_fields_in([
                   { "note", record.message },
                   { "note", operator_message(record.operator) },
                   { "code", record.expr    },
                   { "lhs",  record.left    },
                   { "rhs",  record.right   },
                   { "was",  record.value   }
                ]), terminal)
    |> make_into_lines 
                
  end

  defp format_kind_reason(:error, exception, terminal) do
    error_info "** (#{inspect exception.__record__(:name)}) #{exception.message}", terminal
  end

  defp format_kind_reason(kind, reason, terminal) do
    error_info "** (#{kind}) #{inspect(reason)}", terminal
  end


  defp interesting_fields_in(fields) do
    Enum.filter(fields, fn {_, value} -> value != ExUnit.NoValueSupplied.no_value end)
  end

  defp format_each_reason(reasons, terminal) do
    label_length = (reasons |> Enum.map(fn {label,_} -> String.length(label) end) |> Enum.max) + 2
    Enum.map(reasons, fn {label, value} ->
      display_value = 
        format_and_wrap_value(maybe_inspect(label, value), label_length, terminal)
        |> highlight_value(label, terminal)
      
      ~s{#{format_label(label, label_length, terminal)}} <>
      ~s{#{display_value}} 
    end)
  end

  defp maybe_inspect("code", value), do: value
  defp maybe_inspect("note", value), do: value
  defp maybe_inspect(_,      value), do: value

         
  defp highlight_value(value, "note", terminal) do
    terminal.(:error_info, value)
  end

  defp highlight_value(value, _, _) do
    value
  end

  defp format_label("note", _, _terminal) do
    ""
  end


  defp format_label(label, label_length, nil) do
    String.ljust(label <> ":", label_length)
  end

  defp format_label(label, label_length, terminal) do
    terminal.(:error_info, format_label(label, label_length, nil))
  end


  defp format_and_wrap_value(value, label_length, terminal) do
    indent = label_length + @prefix_width
    width  = terminal.(:width, :please)

    if estimated_length(value) + indent >= width do
      wrap_value(value, indent, width - indent)
    else
      value
    end
  end

  defp estimated_length(value) when is_binary(value), do: String.length(value)
  defp estimated_length(value), do: estimated_length(inspect(value))

  defp wrap_value(value, indent, width) 
  when is_binary(value) do
    leading_spaces = String.duplicate(" ", indent)
    leader = "\n#{leading_spaces}"
    value
    |> String.split("\n")
    |> Enum.flat_map(fn (line) -> force_split_long_lines(line, width) end)
    |> Enum.join(leader)
  end
    
  defp wrap_value(value, indent, width) do
    value
    |> inspect(pretty: true, width: width)
    |> wrap_value(indent, width)

  end

  defp wrap_stacktrace(line, terminal) do
    width  = terminal.(:width, :please)

    if String.length(line) + @prefix_width >= width do
      wrap_stacktrace_line(line, width)
    else
      line
    end
  end

  defp wrap_stacktrace_line(line, width) do
    case Regex.run(~r{(.*)(:\d+:)(.*)}, line) do
    [_, where, line, what ] ->
      what = wrap_value(what, @prefix_width + 6, width - @prefix_width - 6)
      leader = String.duplicate(" ", @prefix_width + 3)
      "#{where}#{line}⤦\n#{leader}#{what}"
    _ ->
      wrap_value(line, @prefix_width, width)
    end
  end

  defp force_split_long_lines(line, max_width) do
    cond do
      String.length(line) > max_width ->
        break_into_smaller_lines(line, max_width)
      true ->
        [line]
    end
  end


  defp break_into_smaller_lines(line, max_width) do
    Regex.scan(~r{(\S*)(?:\s+|$)}, line) 
    |> Enum.reduce([""], fn ([word_and_space, word], [line|lines]) -> 
                              longer_line = line <> word
                              if String.length(longer_line) <= max_width do
                                [line <> word_and_space|lines]
                              else
                                [word_and_space, line | lines]
                              end
                       end)
    |> Enum.map(&String.rstrip/1)
    |> Enum.reverse
  end


  defp make_into_lines(reasons) do
    "     " <> Enum.join(reasons, "\n     ") <> "\n"
  end


  defp format_stacktrace([], _case, _test, _terminal) do
    ""
  end

  defp format_stacktrace(stacktrace, _case, _test, terminal) do
    Enum.map_join(stacktrace, fn(s) -> stacktrace_info(format_stacktrace_entry(s), terminal) end)
  end

  defp with_counter(counter, msg) when counter < 10  do "  #{counter}) #{msg}" end
  defp with_counter(counter, msg) when counter < 100 do  " #{counter}) #{msg}" end
  defp with_counter(counter, msg)                    do   "#{counter}) #{msg}" end

  defp test_case_info(msg, nil),   do: msg <> "failure on setup_all/teardown_all callback, tests invalidated\n"
  defp test_case_info(msg, terminal), do: test_case_info(terminal.(:test_case_info, msg), nil)

  defp test_info(msg, nil),   do: msg <> "\n"
  defp test_info(msg, terminal) do
    msg = format_and_wrap_value(msg, 0, terminal)
    test_info(terminal.(:test_info, msg), nil)
  end

  defp error_info(msg, nil),   do: "     " <> msg <> "\n"
  defp error_info(msg, terminal), do: error_info(terminal.(:error_info, msg), nil)

  defp location_info(msg, nil),   do: "     " <> msg <> "\n"
  defp location_info(msg, terminal), do: location_info(terminal.(:location_info, msg), nil)

  defp stacktrace_info(msg, nil),   do: "       " <> msg <> "\n"
  defp stacktrace_info(msg, terminal) do
    msg = wrap_stacktrace(msg, terminal)
    stacktrace_info(terminal.(:stacktrace_info, msg), nil)
  end

  defp operator_message(operator) do
    if operator ==  ExUnit.NoValueSupplied.no_value do
        operator
    else
        "Comparison (using #{operator}) failed in:"
    end
  end
end
