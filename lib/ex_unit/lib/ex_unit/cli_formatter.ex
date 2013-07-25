defmodule ExUnit.CLIFormatter do
  @moduledoc """
  Formatter responsible for printing test results to the CLI.
  """

  @behaviour ExUnit.Formatter
  @timeout 30_000
  use GenServer.Behaviour

  import ExUnit.Formatter, only: [format_time: 2, format_test_failure: 4, format_test_case_failure: 4]

  defrecord Config, tests_counter: 0, invalid_counter: 0,
                    test_failures: [], case_failures: [], trace: false, color: true

  ## Behaviour

  def suite_started(opts) do
    { :ok, pid } = :gen_server.start_link(__MODULE__, opts, [])
    pid
  end

  def suite_finished(id, run_us, load_us) do
    :gen_server.call(id, { :suite_finished, run_us, load_us }, @timeout)
  end

  def case_started(id, test_case) do
    :gen_server.cast(id, { :case_started, test_case })
  end

  def case_finished(id, test_case) do
    :gen_server.cast(id, { :case_finished, test_case })
  end

  def test_started(id, test) do
    :gen_server.cast(id, { :test_started, test })
  end

  def test_finished(id, test) do
    :gen_server.cast(id, { :test_finished, test })
  end

  ## Callbacks

  def init(opts) do
    { :ok, Config.new(opts) }
  end

  def handle_call({ :suite_finished, run_us, load_us }, _from, config) do
    print_suite(config.tests_counter, config.invalid_counter,
                config.test_failures, config.case_failures, run_us, load_us, config)
    { :stop, :normal, length(config.test_failures), config }
  end

  def handle_call(reqest, from, config) do
    super(reqest, from, config)
  end

  def handle_cast({ :test_started, ExUnit.Test[] = test }, config) do
    if config.trace, do: IO.write("  * #{trace_test_name test}")
    { :noreply, config }
  end

  def handle_cast({ :test_finished, ExUnit.Test[failure: nil] = test }, config) do
    if config.trace do
      IO.puts success(trace_test_result(test), config)
    else
      IO.write success(".", config)
    end
    { :noreply, config.update_tests_counter(&1 + 1) }
  end

  def handle_cast({ :test_finished, ExUnit.Test[failure: { :invalid, _ }] = test }, config) do
    if config.trace do
      IO.puts invalid(trace_test_result(test), config)
    else
      IO.write invalid("?", config)
    end
    { :noreply, config.update_tests_counter(&1 + 1).
        update_invalid_counter(&1 + 1) }
  end

  def handle_cast({ :test_finished, test }, config) do
    if config.trace do
      IO.puts failure(trace_test_result(test), config)
    else
      IO.write failure("F", config)
    end
    { :noreply, config.update_tests_counter(&1 + 1).
        update_test_failures([test|&1]) }
  end

  def handle_cast({ :case_started, ExUnit.TestCase[name: name] }, config) do
    if config.trace, do: IO.puts("\n#{name}")
    { :noreply, config }
  end

  def handle_cast({ :case_finished, test_case }, config) do
    if test_case.failure do
      { :noreply, config.update_case_failures([test_case|&1]) }
    else
      { :noreply, config }
    end
  end

  def handle_cast(request, config) do
    super(request, config)
  end

  defp trace_test_result(test) do
    "\r  * #{trace_test_name test} (#{trace_test_time(test)})"
  end

  defp trace_test_name(ExUnit.Test[name: name]) do
    case atom_to_binary(name) do
      "test_" <> rest -> rest
      "test " <> rest -> rest
    end
  end

  defp trace_test_time(ExUnit.Test[time: time]) do
    "#{format_us(time)}ms"
  end

  defp format_us(us) do
    us = div(us, 10)
    if us < 10 do
      "0.0#{us}"
    else
      us = div us, 10
      "#{div(us, 10)}.#{rem(us, 10)}"
    end
  end

  defp print_suite(counter, 0, [], [], run_us, load_us, config) do
    IO.write "\n\n"
    IO.puts format_time(run_us, load_us)
    IO.puts success("#{counter} tests, 0 failures", config)
  end

  defp print_suite(counter, num_invalids, test_failures, case_failures, run_us, load_us, config) do
    IO.write "\n\nFailures:\n\n"

    num_fails = Enum.reduce Enum.reverse(test_failures), 0, print_test_failure(&1, &2, File.cwd!, config)
    Enum.reduce Enum.reverse(case_failures), num_fails, print_test_case_failure(&1, &2, File.cwd!, config)

    IO.puts format_time(run_us, load_us)
    message = "#{counter} tests, #{num_fails} failures"

    if num_invalids > 0 do
      message = message <>  ", #{num_invalids} invalid"
    end

    cond do
      num_fails > 0    -> IO.puts failure(message, config)
      num_invalids > 0 -> IO.puts invalid(message, config)
      true             -> IO.puts success(message, config)
    end
  end

  defp print_test_failure(test, acc, cwd, config) do
    IO.puts format_test_failure(test, acc + 1, cwd, formatter(&1, &2, config))
    acc + 1
  end

  defp print_test_case_failure(test_case, acc, cwd, config) do
    IO.puts format_test_case_failure(test_case, acc + 1, cwd, formatter(&1, &2, config))
    acc + 1
  end

  # Color styles

  defp colorize(escape, string, Config[color: color]) do
    IO.ANSI.escape_fragment("%{#{escape}}", color)
      <> string
      <> IO.ANSI.escape_fragment("%{reset}", color)
  end

  defp success(msg, config) do
    colorize("green", msg, config)
  end

  defp invalid(msg, config) do
    colorize("yellow", msg, config)
  end

  defp failure(msg, config) do
    colorize("red", msg, config)
  end

  defp formatter(:error_info, msg, config),    do: colorize("red", msg, config)
  defp formatter(:location_info, msg, config), do: colorize("cyan", msg, config)
  defp formatter(_,  msg, _config),            do: msg
end
