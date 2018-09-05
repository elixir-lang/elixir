defmodule ExUnit.CLIFormatter do
  @moduledoc false
  use GenServer

  import ExUnit.Formatter,
    only: [format_time: 2, format_filters: 2, format_test_failure: 5, format_test_all_failure: 5]

  ## Callbacks

  def init(opts) do
    print_filters(Keyword.take(opts, [:include, :exclude]))

    config = %{
      seed: opts[:seed],
      trace: opts[:trace],
      colors: Keyword.put_new(opts[:colors], :enabled, IO.ANSI.enabled?()),
      width: get_terminal_width(),
      slowest: opts[:slowest],
      test_counter: %{},
      test_timings: [],
      failure_counter: 0,
      skipped_counter: 0,
      excluded_counter: 0,
      invalid_counter: 0
    }

    {:ok, config}
  end

  def handle_cast({:suite_started, _opts}, config) do
    {:noreply, config}
  end

  def handle_cast({:suite_finished, run_us, load_us}, config) do
    print_suite(config, run_us, load_us)
    {:noreply, config}
  end

  def handle_cast({:test_started, %ExUnit.Test{} = test}, config) do
    if config.trace, do: IO.write("  * #{test.name}")
    {:noreply, config}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: nil} = test}, config) do
    if config.trace() do
      IO.puts(success(trace_test_result(test), config))
    else
      IO.write(success(".", config))
    end

    test_counter = update_test_counter(config.test_counter, test)
    test_timings = update_test_timings(config.test_timings, test)
    config = %{config | test_counter: test_counter, test_timings: test_timings}

    {:noreply, config}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {:excluded, _}} = test}, config) do
    if config.trace, do: IO.puts(trace_test_excluded(test))

    test_counter = update_test_counter(config.test_counter, test)
    config = %{config | test_counter: test_counter, excluded_counter: config.excluded_counter + 1}

    {:noreply, config}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {:skipped, _}} = test}, config) do
    if config.trace do
      IO.puts(skipped(trace_test_skipped(test), config))
    else
      IO.write(skipped("*", config))
    end

    test_counter = update_test_counter(config.test_counter, test)
    config = %{config | test_counter: test_counter, skipped_counter: config.skipped_counter + 1}

    {:noreply, config}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {:invalid, _}} = test}, config) do
    if config.trace() do
      IO.puts(invalid(trace_test_result(test), config))
    else
      IO.write(invalid("?", config))
    end

    test_counter = update_test_counter(config.test_counter, test)
    config = %{config | test_counter: test_counter, invalid_counter: config.invalid_counter + 1}

    {:noreply, config}
  end

  def handle_cast({:test_finished, %ExUnit.Test{state: {:failed, failures}} = test}, config) do
    if config.trace() do
      IO.puts(failure(trace_test_result(test), config))
    end

    formatted =
      format_test_failure(
        test,
        failures,
        config.failure_counter + 1,
        config.width,
        &formatter(&1, &2, config)
      )

    print_failure(formatted, config)
    print_logs(test.logs)

    test_counter = update_test_counter(config.test_counter, test)
    test_timings = update_test_timings(config.test_timings, test)
    failure_counter = config.failure_counter + 1

    config = %{
      config
      | test_counter: test_counter,
        test_timings: test_timings,
        failure_counter: failure_counter
    }

    {:noreply, config}
  end

  def handle_cast({:module_started, %ExUnit.TestModule{name: name}}, config) do
    if config.trace() do
      IO.puts("\n#{inspect(name)}")
    end

    {:noreply, config}
  end

  def handle_cast({:module_finished, %ExUnit.TestModule{state: nil}}, config) do
    {:noreply, config}
  end

  def handle_cast(
        {:module_finished, %ExUnit.TestModule{state: {:failed, failures}} = test_module},
        config
      ) do
    tests_length = length(test_module.tests)

    formatted =
      format_test_all_failure(
        test_module,
        failures,
        config.failure_counter + tests_length,
        config.width,
        &formatter(&1, &2, config)
      )

    print_failure(formatted, config)

    test_counter =
      Enum.reduce(test_module.tests, config.test_counter, &update_test_counter(&2, &1))

    failure_counter = config.failure_counter + tests_length
    config = %{config | test_counter: test_counter, failure_counter: failure_counter}

    {:noreply, config}
  end

  def handle_cast(:max_failures_reached, config) do
    "--max-failures reached, aborting test suite"
    |> failure(config)
    |> IO.write()

    {:noreply, config}
  end

  def handle_cast(_, config) do
    {:noreply, config}
  end

  ## Tracing

  defp trace_test_time(%ExUnit.Test{time: time}) do
    "#{format_us(time)}ms"
  end

  defp trace_test_result(test) do
    "\r  * #{test.name} (#{trace_test_time(test)})"
  end

  defp trace_test_excluded(test) do
    "\r  * #{test.name} (excluded)"
  end

  defp trace_test_skipped(test) do
    "\r  * #{test.name} (skipped)"
  end

  defp normalize_us(us) do
    div(us, 1000)
  end

  defp format_us(us) do
    us = div(us, 10)

    if us < 10 do
      "0.0#{us}"
    else
      us = div(us, 10)
      "#{div(us, 10)}.#{rem(us, 10)}"
    end
  end

  defp update_test_counter(test_counter, %{tags: %{test_type: test_type}}) do
    Map.update(test_counter, test_type, 1, &(&1 + 1))
  end

  ## Slowest

  defp format_slowest_total(%{slowest: slowest} = config, run_us) do
    slowest_us =
      config
      |> extract_slowest_tests()
      |> Enum.reduce(0, &(&1.time + &2))

    slowest_time =
      slowest_us
      |> normalize_us()
      |> format_us()

    percentage = Float.round(slowest_us / run_us * 100, 1)

    "Top #{slowest} slowest (#{slowest_time}s), #{percentage}% of total time:\n"
  end

  defp format_slowest_times(config) do
    config
    |> extract_slowest_tests()
    |> Enum.map(&format_slow_test/1)
  end

  defp format_slow_test(%ExUnit.Test{name: name, time: time, module: module}) do
    "  * #{name} (#{format_us(time)}ms) (#{inspect(module)})\n"
  end

  defp extract_slowest_tests(%{slowest: slowest, test_timings: timings} = _config) do
    timings
    |> Enum.sort_by(fn %{time: time} -> -time end)
    |> Enum.take(slowest)
  end

  defp update_test_timings(timings, %ExUnit.Test{} = test) do
    [test | timings]
  end

  ## Printing

  defp print_suite(config, run_us, load_us) do
    IO.write("\n\n")
    IO.puts(format_time(run_us, load_us))

    if config.slowest > 0 do
      IO.write("\n")
      IO.puts(format_slowest_total(config, run_us))
      IO.puts(format_slowest_times(config))
    end

    # singular/plural
    test_type_counts = format_test_type_counts(config)
    failure_pl = pluralize(config.failure_counter, "failure", "failures")

    message =
      "#{test_type_counts}#{config.failure_counter} #{failure_pl}"
      |> if_true(config.excluded_counter > 0, &(&1 <> ", #{config.excluded_counter} excluded"))
      |> if_true(config.invalid_counter > 0, &(&1 <> ", #{config.invalid_counter} invalid"))
      |> if_true(
        config.skipped_counter > 0,
        &(&1 <> ", " <> skipped("#{config.skipped_counter} skipped", config))
      )

    cond do
      config.failure_counter > 0 -> IO.puts(failure(message, config))
      config.invalid_counter > 0 -> IO.puts(invalid(message, config))
      true -> IO.puts(success(message, config))
    end

    IO.puts("\nRandomized with seed #{config.seed}")
  end

  defp if_true(value, false, _fun), do: value
  defp if_true(value, true, fun), do: fun.(value)

  defp print_filters(include: [], exclude: []) do
    :ok
  end

  defp print_filters(include: include, exclude: exclude) do
    if include != [], do: IO.puts(format_filters(include, :include))
    if exclude != [], do: IO.puts(format_filters(exclude, :exclude))
    IO.puts("")
    :ok
  end

  defp print_failure(formatted, config) do
    cond do
      config.trace -> IO.puts("")
      true -> IO.puts("\n")
    end

    IO.puts(formatted)
  end

  defp format_test_type_counts(%{test_counter: test_counter} = _config) do
    Enum.map(test_counter, fn {test_type, count} ->
      type_pluralized = pluralize(count, test_type, ExUnit.plural_rule(test_type |> to_string()))
      "#{count} #{type_pluralized}, "
    end)
  end

  # Color styles

  defp colorize(escape, string, %{colors: colors}) do
    if colors[:enabled] do
      [escape, string, :reset]
      |> IO.ANSI.format_fragment(true)
      |> IO.iodata_to_binary()
    else
      string
    end
  end

  defp success(msg, config) do
    colorize(:green, msg, config)
  end

  defp invalid(msg, config) do
    colorize(:yellow, msg, config)
  end

  defp skipped(msg, config) do
    colorize(:yellow, msg, config)
  end

  defp failure(msg, config) do
    colorize(:red, msg, config)
  end

  defp formatter(:diff_enabled?, _, %{colors: colors}), do: colors[:enabled]

  defp formatter(:error_info, msg, config), do: colorize(:red, msg, config)

  defp formatter(:extra_info, msg, config), do: colorize(:cyan, msg, config)

  defp formatter(:location_info, msg, config), do: colorize([:bright, :black], msg, config)

  defp formatter(:diff_delete, msg, config), do: colorize(:red, msg, config)

  defp formatter(:diff_delete_whitespace, msg, config),
    do: colorize(IO.ANSI.color_background(2, 0, 0), msg, config)

  defp formatter(:diff_insert, msg, config), do: colorize(:green, msg, config)

  defp formatter(:diff_insert_whitespace, msg, config),
    do: colorize(IO.ANSI.color_background(0, 2, 0), msg, config)

  defp formatter(:blame_diff, msg, %{colors: colors} = config) do
    if colors[:enabled] do
      colorize(:red, msg, config)
    else
      "-" <> msg <> "-"
    end
  end

  defp formatter(_, msg, _config), do: msg

  defp pluralize(1, singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  defp get_terminal_width do
    case :io.columns() do
      {:ok, width} -> max(40, width)
      _ -> 80
    end
  end

  defp print_logs(""), do: nil

  defp print_logs(output) do
    indent = "\n     "
    output = String.replace(output, "\n", indent)
    IO.puts(["     The following output was logged:", indent | output])
  end
end
