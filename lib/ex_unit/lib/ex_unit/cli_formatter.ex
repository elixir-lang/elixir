defmodule ExUnit.CLIFormatter do
  @moduledoc false

  use GenEvent.Behaviour

  import ExUnit.Formatter, only: [format_time: 2, format_filters: 2, format_test_failure: 5, format_test_case_failure: 4]

  defrecord Config, tests_counter: 0, invalids_counter: 0, failures_counter: 0,
                    skips_counter: 0, trace: false, color: true, previous: nil,
                    seed: nil

  ## Callbacks

  def init(opts) do
    print_filters(Keyword.take(opts, [:include, :exclude]))
    { :ok, Config.new(opts) }
  end

  def handle_event({ :suite_finished, run_us, load_us }, config = Config[]) do
    print_suite(config, run_us, load_us)
    :remove_handler
  end

  def handle_event({ :test_started, ExUnit.Test[] = test }, config = Config[]) do
    if config.trace, do: IO.write("  * #{trace_test_name test}")
    { :ok, config }
  end

  def handle_event({ :test_finished, ExUnit.Test[state: :passed] = test }, config = Config[]) do
    if config.trace do
      IO.puts success(trace_test_result(test), config)
    else
      IO.write success(".", config)
    end
    { :ok, config.previous(:passed).update_tests_counter(&(&1 + 1)) }
  end

  def handle_event({ :test_finished, ExUnit.Test[state: { :invalid, _ }] = test }, config = Config[]) do
    if config.trace do
      IO.puts invalid(trace_test_result(test), config)
    else
      IO.write invalid("?", config)
    end

    { :ok, config.previous(:invalid).update_tests_counter(&(&1 + 1))
                 .update_invalids_counter(&(&1 + 1)) }
  end

  def handle_event({ :test_finished, ExUnit.Test[state: { :skip, _ }] }, config = Config[]) do
    { :ok, config.previous(:skip).update_skips_counter(&(&1 + 1)) }
  end

  def handle_event({ :test_finished, test }, config = Config[]) do
    if config.trace do
      IO.puts failure(trace_test_result(test), config)
    end

    config = print_test_failure(test, config)
    { :ok, config.update_tests_counter(&(&1 + 1)) }
  end

  def handle_event({ :case_started, ExUnit.TestCase[name: name] }, config = Config[]) do
    if config.trace do
      IO.puts("\n#{inspect name}")
    end

    { :ok, config }
  end

  def handle_event({ :case_finished, test_case }, config = Config[]) do
    if test_case.state != :passed do
      config = print_test_case_failure(test_case, config)
      { :ok, config }
    else
      { :ok, config }
    end
  end

  def handle_event(_, config) do
    { :ok, config }
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

    IO.puts "\nRandomized with seed #{config.seed}"
  end

  defp print_filters([include: [], exclude: []]) do
    :ok
  end

  defp print_filters([include: include, exclude: exclude]) do
    if include != [], do: IO.puts format_filters(include, :include)
    if exclude != [], do: IO.puts format_filters(exclude, :exclude)
    IO.puts("")
    :ok
  end

  defp print_test_failure(ExUnit.Test[name: name, case: mod, state: { :failed, tuple }], config) do
    formatted = format_test_failure(mod, name, tuple, config.failures_counter + 1, &formatter(&1, &2, config))
    print_any_failure formatted, config
  end

  defp print_test_case_failure(ExUnit.TestCase[name: name, state: { :failed, tuple }], config) do
    formatted = format_test_case_failure(name, tuple, config.failures_counter + 1, &formatter(&1, &2, config))
    print_any_failure formatted, config
  end

  defp print_any_failure(formatted, config = Config[]) do
    cond do
      config.trace -> IO.puts ""
      config.previous != :failed -> IO.puts "\n"
      true -> :ok
    end
    IO.puts formatted
    config.update_failures_counter(&(&1 + 1)).previous(:failed)
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
