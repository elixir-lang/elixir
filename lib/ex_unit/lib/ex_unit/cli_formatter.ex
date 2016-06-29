defmodule ExUnit.CLIFormatter do
  @moduledoc false

  use GenEvent

  import ExUnit.Formatter, only: [format_time: 2, format_filters: 2, format_test_failure: 5,
                                  format_test_case_failure: 5]

  ## Callbacks

  def init(opts) do
    print_filters(Keyword.take(opts, [:include, :exclude]))
    config = %{
      seed: opts[:seed],
      trace: opts[:trace],
      colors: Keyword.put_new(opts[:colors], :enabled, IO.ANSI.enabled?),
      width: get_terminal_width(),
      test_counter: %{},
      failure_counter: 0,
      skipped_counter: 0,
      invalid_counter: 0
    }
    {:ok, config}
  end

  def handle_event({:suite_started, _opts}, config) do
    {:ok, config}
  end

  def handle_event({:suite_finished, run_us, load_us}, config) do
    print_suite(config, run_us, load_us)
    :remove_handler
  end

  def handle_event({:test_started, %ExUnit.Test{} = test}, config) do
    if config.trace, do: IO.write "  * #{test.name}"
    {:ok, config}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: nil} = test}, config) do
    if config.trace do
      IO.puts success(trace_test_result(test), config)
    else
      IO.write success(".", config)
    end
    {:ok, %{config | test_counter: update_test_counter(config.test_counter, test)}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:skip, _}} = test}, config) do
    if config.trace, do: IO.puts trace_test_skip(test)
    {:ok, %{config | test_counter: update_test_counter(config.test_counter, test),
                     skipped_counter: config.skipped_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:invalid, _}} = test}, config) do
    if config.trace do
      IO.puts invalid(trace_test_result(test), config)
    else
      IO.write invalid("?", config)
    end

    {:ok, %{config | test_counter: update_test_counter(config.test_counter, test),
                     invalid_counter: config.invalid_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:failed, failures}} = test}, config) do
    if config.trace do
      IO.puts failure(trace_test_result(test), config)
    end

    formatted = format_test_failure(test, failures, config.failure_counter + 1,
                                    config.width, &formatter(&1, &2, config))
    print_failure(formatted, config)
    print_logs(test.logs)

    {:ok, %{config | test_counter: update_test_counter(config.test_counter, test),
                     failure_counter: config.failure_counter + 1}}
  end

  def handle_event({:case_started, %ExUnit.TestCase{name: name}}, config) do
    if config.trace do
      IO.puts("\n#{inspect name}")
    end

    {:ok, config}
  end

  def handle_event({:case_finished, %ExUnit.TestCase{state: nil}}, config) do
    {:ok, config}
  end

  def handle_event({:case_finished, %ExUnit.TestCase{state: {:failed, failures}} = test_case}, config) do
    formatted = format_test_case_failure(test_case, failures, config.failure_counter + 1,
                                         config.width, &formatter(&1, &2, config))
    print_failure(formatted, config)
    {:ok, %{config | failure_counter: config.failure_counter + 1}}
  end

  ## Tracing

  defp trace_test_time(%ExUnit.Test{time: time}) do
    "#{format_us(time)}ms"
  end

  defp trace_test_result(test) do
    "\r  * #{test.name} (#{trace_test_time(test)})"
  end

  defp trace_test_skip(test) do
    "\r  * #{test.name} (skipped)"
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

  defp update_test_counter(test_counter, %{tags: %{type: type}}) do
    Map.update(test_counter, type, 1, &(&1 + 1))
  end

  ## Printing

  defp print_suite(config, run_us, load_us) do
    IO.write "\n\n"
    IO.puts format_time(run_us, load_us)

    # singular/plural
    test_type_counts = format_test_type_counts(config)
    failure_pl = pluralize(config.failure_counter, "failure", "failures")

    message =
      "#{test_type_counts}#{config.failure_counter} #{failure_pl}"
      |> if_true(config.skipped_counter > 0, & &1 <> ", #{config.skipped_counter} skipped")
      |> if_true(config.invalid_counter > 0, & &1 <> ", #{config.invalid_counter} invalid")

    cond do
      config.failure_counter > 0 -> IO.puts failure(message, config)
      config.invalid_counter > 0 -> IO.puts invalid(message, config)
      true                        -> IO.puts success(message, config)
    end

    IO.puts "\nRandomized with seed #{config.seed}"
  end

  defp if_true(value, false, _fun), do: value
  defp if_true(value, true, fun), do: fun.(value)

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
    IO.puts formatted
  end

  defp format_test_type_counts(%{test_counter: test_counter} = _config) do
    Enum.map test_counter, fn {type, count} ->
      type_pluralized = pluralize(count, type, ExUnit.plural_rule(type |> to_string()))
      "#{count} #{type_pluralized}, "
    end
  end

  # Color styles

  defp colorize(escape, string, %{colors: colors}) do
    [escape | string]
    |> IO.ANSI.format(colors[:enabled])
    |> IO.iodata_to_binary
  end

  defp success(msg, config) do
    colorize(:green, msg, config)
  end

  defp invalid(msg, config) do
    colorize(:yellow, msg, config)
  end

  defp failure(msg, config) do
    colorize(:red, msg, config)
  end

  defp formatter(:diff_enabled?, _, %{colors: colors}),
    do: colors[:enabled]

  defp formatter(:error_info, msg, config),
    do: colorize(:red, msg, config)

  defp formatter(:extra_info, msg, config),
    do: colorize(:cyan, msg, config)

  defp formatter(:location_info, msg, config),
    do: colorize([:bright, :black], msg, config)

  defp formatter(:diff_delete, msg, config),
    do: colorize(:red, msg, config)

  defp formatter(:diff_insert, msg, config),
    do: colorize(:green, msg, config)

  defp formatter(_,  msg, _config),
    do: msg

  defp pluralize(1, singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  defp get_terminal_width do
    case :io.columns do
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
