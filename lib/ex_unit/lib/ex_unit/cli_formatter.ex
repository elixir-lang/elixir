defmodule ExUnit.CLIFormatter do
  @moduledoc """
  Formatter responsible for printing raw text
  on the CLI
  """

  @behaviour ExUnit.Formatter
  @timeout 30_000
  use GenServer.Behaviour

  import Exception, only: [format_entry: 2]
  defrecord Config, counter: 0, failures: []

  ## Behaviour

  def suite_started(_opts) do
    { :ok, pid } = :gen_server.start_link(__MODULE__, [], [])
    pid
  end

  def suite_finished(id, ms) do
    :gen_server.call(id, { :suite_finished, ms }, @timeout)
  end

  def case_started(_id, _test_case) do
    :ok
  end

  def case_finished(_id, _test_case) do
    :ok
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

  def handle_call({ :suite_finished, ms }, _from, config) do
    print_suite(config.counter, config.failures, ms)
    { :stop, :normal, length(config.failures), config }
  end

  def handle_call(_, _, _) do
    super
  end

  def handle_cast({ :test_finished, ExUnit.Test[failure: nil] }, config) do
    IO.write success(".")
    { :noreply, config.update_counter(&1 + 1) }
  end

  def handle_cast({ :test_finished, test }, config) do
    IO.write failure("F")
    { :noreply, config.update_counter(&1 + 1).
        update_failures([test|&1]) }
  end

  def handle_cast(_, _) do
    super
  end

  defp print_suite(counter, [], ms) do
    IO.write "\n\n"
    IO.puts "Finished in #{format_ms ms} seconds"
    IO.puts success("#{counter} tests, 0 failures")
  end

  defp print_suite(counter, failures, ms) do
    IO.write "\n\nFailures:\n\n"
    Enum.reduce Enum.reverse(failures), 1, print_failure(&1, &2, File.cwd!)
    IO.puts "Finished in #{format_ms ms} seconds"
    IO.puts failure("#{counter} tests, #{length(failures)} failures")
  end

  defp print_failure(ExUnit.Test[case: test_case, name: test, failure: { kind, reason, stacktrace }], acc, cwd) do
    IO.puts "  #{acc}) #{test} (#{inspect test_case})"
    print_kind_reason(kind, reason)
    print_stacktrace(stacktrace, test_case, test, cwd)
    IO.write "\n"
    acc + 1
  end

  defp print_kind_reason(:error, record) when is_record(record, ExUnit.ExpectationError) do
    left  = String.downcase record.prelude
    right = "to " <> if(record.negation, do: "not ", else: "") <> record.reason
    instead_prelude = record.instead_prelude
    max   = max(size(left), size(right))

    IO.puts error_info "** (ExUnit.ExpectationError)"
    IO.puts error_info "  #{pad(left, max)}: #{maybe_multiline(record.expected, max)}"
    IO.puts error_info "  #{pad(right, max)}: #{maybe_multiline(record.actual, max)}"

    unless nil?(record.instead) do
      IO.puts error_info "  #{pad(instead_prelude, max)}: #{maybe_multiline(inspect(record.instead), max)}"
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

  defp pad(binary, max) do
    remaining = max - size(binary)
    if remaining > 0 do
      String.duplicate(" ", remaining) <>  binary
    else
      binary
    end
  end

  defp format_ms(ms) do
    if ms < 100000 do
      "0.0#{div(ms, 10000)}"
    else
      ms = div ms, 100000
      "#{div(ms, 10)}.#{rem(ms, 10)}"
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