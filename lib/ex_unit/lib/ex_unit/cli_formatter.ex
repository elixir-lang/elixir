defmodule ExUnit.CLIFormatter do
  @moduledoc """
  Formatter responsible for printing raw text
  on the CLI
  """

  @behaviour ExUnit.Formatter
  @timeout 30_000
  use GenServer.Behaviour

  import Exception, only: [format_entry: 1]
  defrecord Config, counter: 0, failures: []

  ## Behaviour

  def suite_started(_opts) do
    { :ok, pid } = :gen_server.start_link(__MODULE__, [], [])
    pid
  end

  def suite_finished(id) do
    :gen_server.call(id, :suite_finished, @timeout)
  end

  def case_started(_id, _test_case) do
    :ok
  end

  def case_finished(_id, _test_case) do
    :ok
  end

  def test_started(_id, _test_case, _test) do
    :ok
  end

  def test_finished(id, test_case, test, result) do
    :gen_server.cast(id, { :test_finished, test_case, test, result })
  end

  ## Callbacks

  def init(_args) do
    { :ok, Config.new }
  end

  def handle_call(:suite_finished, _from, config) do
    IO.write "\n\n"
    Enum.reduce Enum.reverse(config.failures), 1, print_failure(&1, &2)
    failures_count = length(config.failures)
    IO.puts "#{config.counter} tests, #{failures_count} failures."
    { :stop, :normal, failures_count, config }
  end

  def handle_call(_, _, _) do
    super
  end

  def handle_cast({ :test_finished, _test_case, _test, nil }, config) do
    IO.write "."
    { :noreply, config.update_counter(&1 + 1) }
  end

  def handle_cast({ :test_finished, test_case, test, failure }, config) do
    IO.write "F"
    { :noreply, config.update_counter(&1 + 1).
        update_failures([{test_case, test, failure}|&1]) }
  end

  def handle_cast(_, _) do
    super
  end

  defp print_failure({test_case, test, { kind, reason, stacktrace }}, acc) do
    IO.puts "#{acc}) #{test} (#{inspect test_case})"
    print_kind_reason(kind, reason)
    print_stacktrace(stacktrace, test_case, test)
    IO.write "\n"
    acc + 1
  end

  defp print_kind_reason(:error, record) when is_record(record, ExUnit.ExpectationError) do
    left  = String.downcase record.prelude
    right = "to " <> if(record.negation, do: "not ", else: "") <> "be #{record.reason}"
    max   = max(size(left), size(right))

    IO.puts "  ** (ExUnit.ExpectationError)"
    IO.puts "     #{pad(left, max)}: #{inspect record.expected}"
    IO.puts "     #{pad(right, max)}: #{inspect record.actual}"
  end

  defp print_kind_reason(:error, exception) do
    IO.puts "  ** (#{inspect exception.__record__(:name)}) #{exception.message}"
  end

  defp print_kind_reason(kind, reason) do
    IO.puts "  ** (#{kind}) #{inspect(reason)}"
  end

  defp print_stacktrace([{ test_case, test, _, [ file: file, line: line ] }|_], test_case, test) do
    IO.puts "  at #{file}:#{line}"
  end

  defp print_stacktrace(stacktrace, _case, _test) do
    IO.puts "  stacktrace:"
    Enum.each stacktrace, fn(s) -> IO.puts "    #{format_entry(s)}" end
  end

  defp pad(binary, max) do
    remaining = max - size(binary)
    if remaining > 0 do
      String.duplicate(" ", remaining) <>  binary
    else
      binary
    end
  end
end