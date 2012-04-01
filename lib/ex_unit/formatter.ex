defmodule ExUnit.Formatter do
  use GenServer.Behavior

  defrecord Config, counter: 0, failures: []

  import Exception, only: [format_stacktrace: 1]

  def start do
    { :ok, pid } = Erlang.gen_server.start_link(__MODULE__, [], [])
    pid
  end

  def init(_args) do
    { :ok, Config.new }
  end

  def handle_call({:each, _test_case, _test, nil }, _from, config) do
    IO.print "."
    { :reply, :ok, config.increment_counter }
  end

  def handle_call({:each, test_case, test, failure }, _from, config) do
    IO.print "F"
    { :reply, :ok, config.increment_counter.
      prepend_failures([{test_case, test, failure}]) }
  end

  def handle_call({:each_case, _test_case}, _from, config) do
    { :reply, :ok, config }
  end

  def handle_call(:finish, _from, config) do
    IO.print "\n\n"
    Enum.reduce List.reverse(config.failures), 1, print_failure(&1, &2)
    failures_count = length(config.failures)
    IO.puts "#{config.counter} tests, #{failures_count} failures."
    { :reply, failures_count, config }
  end

  defp print_failure({test_case, test, { kind, reason, stacktrace }}, acc) do
    IO.puts "#{acc}) #{test} (#{inspect test_case})"
    IO.puts "  ** #{format_catch(kind, reason)}\n  stacktrace:"
    Enum.each filter_stacktrace(stacktrace), fn(s, do: IO.puts "    #{format_stacktrace(s)}")
    IO.print "\n"
    acc + 1
  end

  defp format_catch(:error, exception) do
    "(#{inspect exception.__record__(:name)}) #{exception.message}"
  end

  defp format_catch(kind, reason) do
    "(#{kind}) #{inspect(reason)}"
  end

  defp filter_stacktrace([{ ExUnit.Assertions, _, _, _ }|t]), do: filter_stacktrace(t)
  defp filter_stacktrace([h|t]), do: [h|filter_stacktrace(t)]
  defp filter_stacktrace([]), do: []
end