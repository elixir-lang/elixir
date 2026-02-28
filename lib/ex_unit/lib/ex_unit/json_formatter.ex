# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule ExUnit.JSONFormatter do
  @moduledoc """
  A formatter that outputs test results as a single JSON document.

  This formatter is useful for CI systems, editors, and tools that
  consume test results programmatically. It outputs a single JSON
  document to standard output when the test suite finishes.

  ## Usage

  You can use this formatter via the `--formatter` flag:

      $ mix test --formatter ExUnit.JSONFormatter

  Or configure it directly in your `test_helper.exs`:

      ExUnit.configure(formatters: [ExUnit.JSONFormatter])

  > #### Tip {: .tip}
  >
  > You may want to use `--no-color` when using this formatter to
  > avoid ANSI escape codes in error messages from other output.

  ## Output format

  The output is a JSON object with the following keys:

    * `"seed"` - the random seed used for test ordering
    * `"summary"` - an object with test counts and duration
    * `"tests"` - an array of test result objects
    * `"module_failures"` - an array of module-level failures (from `setup_all`)

  Each test object contains:

    * `"module"` - the test module name
    * `"name"` - the test name
    * `"file"` - the test file path (relative to project root)
    * `"line"` - the line number
    * `"state"` - one of `"passed"`, `"failed"`, `"skipped"`, `"excluded"`, `"invalid"`
    * `"time_us"` - the test duration in microseconds
    * `"tags"` - user-defined tags (internal ExUnit tags are filtered out)
    * `"failures"` - an array of failure details (empty for passing tests)

  Each failure object contains:

    * `"kind"` - the failure kind (`"assertion"`, `"error"`, `"exit"`, `"throw"`)
    * `"message"` - the formatted error message
    * `"stacktrace"` - an array of stacktrace frame objects
    * `"assertion"` - (only for assertion failures) an object with
      `"left"`, `"right"`, and `"expr"` keys

  Tests are sorted deterministically by `{file, line, name}` regardless
  of execution order.
  """

  @moduledoc since: "1.20.0"

  use GenServer

  # Internal ExUnit tag keys to exclude from the "tags" field.
  @internal_tag_keys [
    :registered,
    :file,
    :line,
    :describe,
    :describe_line,
    :async,
    :module,
    :test,
    :test_type
  ]

  @doc false
  def init(opts) do
    config = %{
      seed: opts[:seed],
      tests: [],
      modules: []
    }

    {:ok, config}
  end

  @doc false
  def handle_cast({:suite_started, _opts}, config) do
    {:noreply, config}
  end

  def handle_cast({:test_finished, %ExUnit.Test{} = test}, config) do
    {:noreply, %{config | tests: [test | config.tests]}}
  end

  def handle_cast({:module_finished, %ExUnit.TestModule{state: {:failed, _}} = module}, config) do
    {:noreply, %{config | modules: [module | config.modules]}}
  end

  def handle_cast({:module_finished, _module}, config) do
    {:noreply, config}
  end

  def handle_cast({:suite_finished, times_us}, config) do
    tests = config.tests |> Enum.reverse() |> sort_tests()
    summary = build_summary(tests, times_us)

    document = %{
      seed: config.seed,
      summary: summary,
      tests: Enum.map(tests, &encode_test/1),
      module_failures: config.modules |> Enum.reverse() |> Enum.map(&encode_module_failure/1)
    }

    IO.write(:json.encode(document))
    {:noreply, config}
  end

  def handle_cast({:sigquit, _current}, config) do
    tests = config.tests |> Enum.reverse() |> sort_tests()
    summary = build_summary(tests, %{run: 0})

    document = %{
      seed: config.seed,
      summary: Map.put(summary, :aborted, true),
      tests: Enum.map(tests, &encode_test/1),
      module_failures: config.modules |> Enum.reverse() |> Enum.map(&encode_module_failure/1)
    }

    IO.write(:json.encode(document))
    {:noreply, config}
  end

  def handle_cast(_event, config) do
    {:noreply, config}
  end

  ## Encoding

  defp encode_test(%ExUnit.Test{} = test) do
    %{
      module: inspect(test.module),
      name: to_string(test.name),
      file: relative_path(test.tags[:file]),
      line: test.tags[:line],
      state: encode_state(test.state),
      time_us: test.time,
      tags: encode_tags(test.tags),
      failures: encode_failures(test.state)
    }
  end

  defp encode_module_failure(%ExUnit.TestModule{} = module) do
    %{
      module: inspect(module.name),
      file: relative_path(module.file),
      failures: encode_failures(module.state)
    }
  end

  defp encode_state(nil), do: "passed"
  defp encode_state({:failed, _}), do: "failed"
  defp encode_state({:skipped, _}), do: "skipped"
  defp encode_state({:excluded, _}), do: "excluded"
  defp encode_state({:invalid, _}), do: "invalid"

  defp encode_tags(tags) when is_map(tags) do
    tags
    |> Enum.reject(&internal_tag?/1)
    |> Map.new(fn {key, value} -> {to_string(key), encode_tag_value(value)} end)
  end

  defp encode_tags(_), do: %{}

  defp internal_tag?({_key, :ex_unit_no_meaningful_value}), do: true
  defp internal_tag?({key, _value}) when key in @internal_tag_keys, do: true

  defp internal_tag?({key, _value}) when is_atom(key) do
    key |> Atom.to_string() |> String.starts_with?("ex_")
  end

  defp internal_tag?(_), do: false

  # NOTE: is_boolean/1 must come before is_atom/1 since true/false are atoms
  defp encode_tag_value(value) when is_boolean(value), do: value
  defp encode_tag_value(value) when is_atom(value), do: to_string(value)
  defp encode_tag_value(value) when is_binary(value), do: value
  defp encode_tag_value(value) when is_number(value), do: value
  defp encode_tag_value(value) when is_list(value), do: Enum.map(value, &encode_tag_value/1)

  defp encode_tag_value(value) when is_map(value),
    do: Map.new(value, fn {k, v} -> {to_string(k), encode_tag_value(v)} end)

  defp encode_tag_value(value), do: inspect(value)

  defp encode_failures({:failed, failures}) when is_list(failures) do
    Enum.map(failures, &encode_failure/1)
  end

  defp encode_failures(_), do: []

  defp encode_failure({kind, error, stacktrace}) do
    base = %{
      kind: encode_failure_kind(kind, error),
      message: format_error_message(error),
      stacktrace: encode_stacktrace(stacktrace)
    }

    maybe_add_assertion(base, error)
  end

  defp encode_failure_kind(_kind, %ExUnit.AssertionError{}), do: "assertion"
  defp encode_failure_kind(:error, _), do: "error"
  defp encode_failure_kind(:exit, _), do: "exit"
  defp encode_failure_kind(:throw, _), do: "throw"
  defp encode_failure_kind(kind, _), do: inspect(kind)

  defp format_error_message(error) when is_exception(error), do: Exception.message(error)
  defp format_error_message(error), do: inspect(error)

  defp maybe_add_assertion(base, %ExUnit.AssertionError{} = error) do
    assertion = %{
      left: inspect_value(error.left),
      right: inspect_value(error.right),
      expr: format_expr(error.expr)
    }

    Map.put(base, :assertion, assertion)
  end

  defp maybe_add_assertion(base, _), do: base

  defp inspect_value(value) do
    no_value = ExUnit.AssertionError.no_value()
    if value == no_value, do: nil, else: inspect(value)
  end

  defp format_expr(nil), do: nil
  defp format_expr(expr), do: Macro.to_string(expr)

  defp encode_stacktrace(stacktrace) when is_list(stacktrace) do
    Enum.map(stacktrace, &encode_stacktrace_frame/1)
  end

  defp encode_stacktrace(_), do: []

  defp encode_stacktrace_frame({module, function, arity, location}) do
    %{
      module: inspect(module),
      function: to_string(function),
      arity: normalize_arity(arity),
      file: location_file(location),
      line: location[:line]
    }
  end

  defp encode_stacktrace_frame(_),
    do: %{module: nil, function: nil, arity: nil, file: nil, line: nil}

  defp normalize_arity(arity) when is_integer(arity), do: arity
  defp normalize_arity(args) when is_list(args), do: length(args)
  defp normalize_arity(_), do: nil

  defp location_file(location) when is_list(location) do
    case Keyword.get(location, :file) do
      nil -> nil
      file -> relative_path(to_string(file))
    end
  end

  defp location_file(_), do: nil

  ## Helpers

  defp build_summary(tests, times_us) do
    counts =
      Enum.reduce(tests, %{passed: 0, failed: 0, skipped: 0, excluded: 0, invalid: 0}, fn test,
                                                                                          acc ->
        increment_count(acc, encode_state(test.state))
      end)

    %{
      total: length(tests),
      passed: counts.passed,
      failed: counts.failed,
      skipped: counts.skipped,
      excluded: counts.excluded,
      invalid: counts.invalid,
      duration_us: extract_duration(times_us)
    }
  end

  defp increment_count(acc, "passed"), do: Map.update!(acc, :passed, &(&1 + 1))
  defp increment_count(acc, "failed"), do: Map.update!(acc, :failed, &(&1 + 1))
  defp increment_count(acc, "skipped"), do: Map.update!(acc, :skipped, &(&1 + 1))
  defp increment_count(acc, "excluded"), do: Map.update!(acc, :excluded, &(&1 + 1))
  defp increment_count(acc, "invalid"), do: Map.update!(acc, :invalid, &(&1 + 1))

  defp extract_duration(%{run: run}) when is_integer(run), do: run
  defp extract_duration(_), do: 0

  defp sort_tests(tests) do
    Enum.sort_by(tests, fn test ->
      {test.tags[:file] || "", test.tags[:line] || 0, to_string(test.name)}
    end)
  end

  defp relative_path(nil), do: nil
  defp relative_path(path) when is_binary(path), do: Path.relative_to_cwd(path)
  defp relative_path(path), do: to_string(path)
end
