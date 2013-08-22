defmodule ExUnit.CLIFormatter do
  @moduledoc false
  @timeout 30_000
  @behaviour ExUnit.Formatter

  use GenServer.Behaviour

  import ExUnit.Formatter, only: [format_time: 2, format_test_failure: 3, format_test_case_failure: 3]

  defrecord Config, tests_counter: 0, invalids_counter: 0, failures_counter: 0,
                    trace: false, color: true, previous: nil

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

  def handle_call({ :suite_finished, run_us, load_us }, _from, config = Config[]) do
    print_suite(config, run_us, load_us)
    { :stop, :normal, config.failures_counter, config }
  end

  def handle_call(reqest, from, config) do
    super(reqest, from, config)
  end

  def handle_cast({ :test_started, ExUnit.Test[] = test }, config) do
    if config.trace, do: IO.write("  * #{trace_test_name test}")
    { :noreply, config }
  end

  def handle_cast({ :test_finished, ExUnit.Test[failure: nil] = test }, config = Config[]) do
    if config.trace do
      IO.puts success(trace_test_result(test), config)
    else
      IO.write success(".", config)
    end
    { :noreply, config.previous(:ok).update_tests_counter(&(&1 + 1)) }
  end

  def handle_cast({ :test_finished, ExUnit.Test[failure: { :invalid, _ }] = test }, config = Config[]) do
    if config.trace do
      IO.puts invalid(trace_test_result(test), config)
    else
      IO.write invalid("?", config)
    end

    { :noreply, config.previous(:invalid).update_tests_counter(&(&1 + 1))
                      .update_invalids_counter(&(&1 + 1)) }
  end

  def handle_cast({ :test_finished, test }, config) do
    if config.trace do
      IO.puts failure(trace_test_result(test), config)
    end

    config = print_test_failure(test, config)
    { :noreply, config.update_tests_counter(&(&1 + 1)) }
  end

  def handle_cast({ :case_started, ExUnit.TestCase[name: name] }, config) do
    if config.trace do
      IO.puts("\n#{name}")
    end

    { :noreply, config }
  end

  def handle_cast({ :case_finished, test_case }, config) do
    if test_case.failure do
      config = print_test_case_failure(test_case, config)
      { :noreply, config }
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
      rest -> rest
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

  defp print_suite(config = Config[], run_us, load_us) do
    IO.write "\n\n"
    IO.puts format_time(run_us, load_us)

    message = "#{config.tests_counter} tests, #{config.failures_counter} failures"

    if config.invalids_counter > 0 do
      message = message <>  ", #{config.invalids_counter} invalid"
    end

    cond do
      config.failures_counter > 0 -> IO.puts failure(message, config)
      config.invalids_counter > 0 -> IO.puts invalid(message, config)
      true                        -> IO.puts success(message, config)
    end
  end

  defp print_test_failure(test, config) do
    formatted = format_test_failure(test, config.failures_counter + 1, &formatter(&1, &2, config))
    print_any_failure formatted, config
  end

  defp print_test_case_failure(test_case, config) do
    formatted = format_test_case_failure(test_case, config.failures_counter + 1, &formatter(&1, &2, config))
    print_any_failure formatted, config
  end

  defp print_any_failure(formatted, config = Config[]) do
    cond do
      config.trace -> IO.puts ""
      config.previous != :failure -> IO.puts "\n"
      true -> :ok
    end
    IO.puts formatted
    config.update_failures_counter(&(&1 + 1)).previous(:failure)
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
