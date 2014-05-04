defmodule ExUnit.CLIFormatter do
  @moduledoc false

  use GenEvent.Behaviour

  import ExUnit.Formatter, only: [format_time: 2, format_filters: 2, format_test_failure: 5,
                                  format_test_case_failure: 5]

  defrecord Config, tests_counter: 0, invalids_counter: 0, failures_counter: 0,
                    trace: false, seed: nil, color: true, width: :infinity

  ## Callbacks

  def init(opts) do
    print_filters(Keyword.take(opts, [:include, :exclude]))
    {:ok, opts |> Config.new |> add_terminal_width}
  end

  def handle_event({:suite_finished, run_us, load_us}, config) do
    print_suite(config, run_us, load_us)
    :remove_handler
  end

  def handle_event({:test_started, ExUnit.Test[] = test}, config) do
    if config.trace, do: IO.write "  * #{trace_test_name test}"
    {:ok, config}
  end

  def handle_event({:test_finished, ExUnit.Test[state: nil] = test}, config) do
    if config.trace do
      IO.puts success(trace_test_result(test), config)
    else
      IO.write success(".", config)
    end
    {:ok, config.update_tests_counter(&(&1 + 1))}
  end

  def handle_event({:test_finished, ExUnit.Test[state: {:skip, _}] = test}, config) do
    if config.trace, do: IO.puts trace_test_skip(test)
    {:ok, config}
  end

  def handle_event({:test_finished, ExUnit.Test[state: {:invalid, _}] = test}, config) do
    if config.trace do
      IO.puts invalid(trace_test_result(test), config)
    else
      IO.write invalid("?", config)
    end

    {:ok, config.update_tests_counter(&(&1 + 1))
                .update_invalids_counter(&(&1 + 1))}
  end

  def handle_event({:test_finished, ExUnit.Test[state: {:failed, failed}] = test}, config) do
    if config.trace do
      IO.puts failure(trace_test_result(test), config)
    end

    formatted = format_test_failure(test, failed, config.failures_counter + 1,
                                    config.width, &formatter(&1, &2, config))
    print_failure(formatted, config)

    {:ok, config.update_tests_counter(&(&1 + 1))
                .update_failures_counter(&(&1 + 1))}
  end

  def handle_event({:case_started, ExUnit.TestCase[] = test_case}, config) do
    if config.trace do
      IO.puts("\n#{inspect test_case.name}")
    end

    {:ok, config}
  end

  def handle_event({:case_finished, ExUnit.TestCase[state: nil]}, config) do
    {:ok, config}
  end

  def handle_event({:case_finished, ExUnit.TestCase[state: {:failed, failed}] = test_case}, config) do
    formatted = format_test_case_failure(test_case, failed, config.failures_counter + 1,
                                         config.width, &formatter(&1, &2, config))
    print_failure(formatted, config)
    {:ok, config.update_failures_counter(&(&1 + 1))}
  end

  def handle_event(_, config) do
    {:ok, config}
  end

  ## Tracing

  defp trace_test_name(ExUnit.Test[name: name]) do
    case atom_to_binary(name) do
      "test " <> rest -> rest
      rest -> rest
    end
  end

  defp trace_test_time(ExUnit.Test[time: time]) do
    "#{format_us(time)}ms"
  end

  defp trace_test_result(test) do
    "\r  * #{trace_test_name test} (#{trace_test_time(test)})"
  end

  defp trace_test_skip(test) do
    "\r  * #{trace_test_name test} (skipped)"
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

  ## Printing

  defp print_suite(config, run_us, load_us) do
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

  defp print_failure(formatted, config) do
    cond do
      config.trace -> IO.puts ""
      true -> IO.puts "\n"
    end
    IO.write formatted
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
  defp formatter(:extra_info, msg, config),    do: colorize("cyan", msg, config)
  defp formatter(:location_info, msg, config), do: colorize("bright,black", msg, config)
  defp formatter(_,  msg, _config),            do: msg

  defp add_terminal_width(config) do
    case :io.columns do
      {:ok, width} ->
        config.width(max(40, width))
      _ ->
        config
    end
  end
end
