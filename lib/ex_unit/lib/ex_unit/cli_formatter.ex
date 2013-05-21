defmodule ExUnit.CLIFormatter do
  @moduledoc """
  Formatter responsible for printing raw text
  on the CLI
  """

  @behaviour ExUnit.Formatter
  @timeout 30_000
  use GenServer.Behaviour

  import Exception, only: [format_entry: 2]
  defrecord Config, counter: 0, test_failures: [], case_failures: []

  ## Behaviour

  def suite_started(_opts) do
    { :ok, pid } = :gen_server.start_link(__MODULE__, [], [])
    pid
  end

  def suite_finished(id, run_us, load_us) do
    :gen_server.call(id, { :suite_finished, run_us, load_us }, @timeout)
  end

  def case_started(_id, _test_case) do
    :ok
  end

  def case_finished(id, test_case) do
    :gen_server.cast(id, { :case_finished, test_case })
  end

  def test_started(_id, _test) do
    :ok
  end

  def test_finished(id, test) do
    :gen_server.cast(id, { :test_finished, test })
  end

  ## Callbacks

  def init(_args) do
    { :ok, Config.new }
  end

  def handle_call({ :suite_finished, run_us, load_us }, _from, config) do
    print_suite(config.counter, config.test_failures, config.case_failures, run_us, load_us)
    { :stop, :normal, length(config.test_failures), config }
  end

  def handle_call(_, _, _) do
    super
  end

  def handle_cast({ :test_finished, test = ExUnit.Test[invalid: true] }, config) do
    IO.write invalid("?")
    { :noreply, config.update_counter(&1 + 1).
        update_test_failures([test|&1]) }
  end

  def handle_cast({ :test_finished, ExUnit.Test[failure: nil] }, config) do
    IO.write success(".")
    { :noreply, config.update_counter(&1 + 1) }
  end

  def handle_cast({ :test_finished, test }, config) do
    IO.write failure("F")
    { :noreply, config.update_counter(&1 + 1).
        update_test_failures([test|&1]) }
  end

  def handle_cast({ :case_finished, test_case }, config) do
    if test_case.failure do
      { :noreply, config.update_case_failures([test_case|&1]) }
    else
      { :noreply, config }
    end
  end

  def handle_cast(_, _) do
    super
  end

  defp print_suite(counter, [], [], run_us, load_us) do
    IO.write "\n\n"
    print_time(run_us, load_us)
    IO.puts success("#{counter} tests, 0 failures")
  end

  defp print_suite(counter, test_failures, case_failures, run_us, load_us) do
    IO.write "\n\nFailures:\n\n"
    num_fails = Enum.reduce Enum.reverse(test_failures), 1, print_test_failure(&1, &2, File.cwd!)
    Enum.reduce Enum.reverse(case_failures), num_fails, print_case_failure(&1, &2, File.cwd!)
    num_invalids = Enum.count test_failures, fn test -> test.invalid end

    print_time(run_us, load_us)

    num_fails = num_fails - 1
    message = "#{counter} tests, #{num_fails} failures"
    if num_invalids > 0, do: message = message <>  ", #{num_invalids} invalid"
    cond do
      num_fails > 0    -> IO.puts failure(message)
      num_invalids > 0 -> IO.puts invalid(message)
      true             -> IO.puts success(message)
    end
  end

  defp print_test_failure(ExUnit.Test[failure: nil], acc, _cwd) do
    acc
  end

  defp print_test_failure(ExUnit.Test[case: test_case, name: test, failure: { kind, reason, stacktrace }], acc, cwd) do
    IO.puts "  #{acc}) #{test} (#{inspect test_case.name})"
    print_kind_reason(kind, reason)
    print_stacktrace(stacktrace, test_case.name, test, cwd)
    IO.write "\n"
    acc + 1
  end

  defp print_case_failure(ExUnit.TestCase[name: case_name, failure: { kind, reason, stacktrace }], acc, cwd) do
    IO.puts "  #{acc}) #{inspect case_name}: failure on setup_all/teardown_all callback, tests invalidated."
    print_kind_reason(kind, reason)
    print_stacktrace(stacktrace, case_name, nil, cwd)
    IO.write "\n"
    acc + 1
  end

  defp print_kind_reason(:error, ExUnit.ExpectationError[] = record) do
    prelude  = String.downcase record.prelude
    reason   = record.full_reason
    max      = max(size(prelude), size(reason))

    IO.puts error_info "** (ExUnit.ExpectationError)"

    if desc = record.description do
      IO.puts error_info "  #{pad(prelude, max)}: #{maybe_multiline(desc, max)}"
      IO.puts error_info "  #{pad(reason, max)}: #{maybe_multiline(record.expected, max)}"
      IO.puts error_info "  #{pad("instead got", max)}: #{maybe_multiline(record.actual, max)}"
    else
      IO.puts error_info "  #{pad(prelude, max)}: #{maybe_multiline(record.expected, max)}"
      IO.puts error_info "  #{pad(reason, max)}: #{maybe_multiline(record.actual, max)}"
    end
  end

  defp print_kind_reason(:error, exception) do
    IO.puts error_info "** (#{inspect exception.__record__(:name)}) #{exception.message}"
  end

  defp print_kind_reason(kind, reason) do
    IO.puts error_info "** (#{kind}) #{inspect(reason)}"
  end

  defp print_stacktrace([{ test_case, test, _, [ file: file, line: line ] }|_], test_case, test, cwd) do
    IO.puts location_info "at #{Path.relative_to(file, cwd)}:#{line}"
  end

  defp print_stacktrace(stacktrace, _case, _test, cwd) do
    IO.puts location_info "stacktrace:"
    Enum.each stacktrace, fn(s) -> IO.puts stacktrace_info format_entry(s, cwd) end
  end

  defp print_time(run_us, nil) do
    IO.puts "Finished in #{run_us |> normalize_us |> format_us} seconds"
  end

  defp print_time(run_us, load_us) do
    run_us  = run_us |> normalize_us
    load_us = load_us |> normalize_us

    ms = run_us + load_us
    IO.puts "Finished in #{format_us ms} seconds (#{format_us load_us}s on load, #{format_us run_us}s on tests)"
  end

  defp pad(binary, max) do
    remaining = max - size(binary)
    if remaining > 0 do
      String.duplicate(" ", remaining) <>  binary
    else
      binary
    end
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

  # Print styles

  defp success(msg) do
    IO.ANSI.escape("%{green}" <>  msg)
  end

  defp invalid(msg) do
    IO.ANSI.escape("%{yellow}" <>  msg)
  end

  defp failure(msg) do
    IO.ANSI.escape("%{red}" <>  msg)
  end

  defp error_info(msg) do
    IO.ANSI.escape("%{red}     " <> msg)
  end

  defp location_info(msg) do
    IO.ANSI.escape("%{cyan}     " <> msg)
  end

  defp stacktrace_info(msg) do
    "       " <> msg
  end
end
