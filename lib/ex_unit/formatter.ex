defmodule ExUnit::Formatter do
  import Exception, only: [format_stacktrace: 1]

  defrecord Config, counter: 0, failures: []

  def start do
    %{ :ok, pid } = Erlang.gen_server.start_link(__MODULE__, [], [])
    pid
  end

  def init(_args) do
    %{ :ok, Config.new }
  end

  def handle_call(%{:each, _test_case, _test, nil }, _from, config) do
    IO.print "."
    %{ :reply, :ok, config.increment_counter }
  end

  def handle_call(%{:each, test_case, test, failure }, _from, config) do
    IO.print "F"
    %{ :reply, :ok, config.increment_counter.
      prepend_failures([%{test_case, test, failure}]) }
  end

  def handle_call(%{:each_case, _test_case}, _from, config) do
    %{ :reply, :ok, config }
  end

  def handle_call(:finish, _from, config) do
    IO.print "\n\n"
    Enum.foldl List.reverse(config.failures), 1, fn(x, acc) { print_failure(x, acc) }
    failures_count = length(config.failures)
    IO.puts "#{config.counter} tests, #{failures_count} failures."
    %{ :reply, failures_count, config }
  end

  def handle_call(_request, _from, config) do
    %{ :reply, :undef, config }
  end

  def handle_info(_msg, config) do
    %{ :noreply, config }
  end

  def handle_cast(_msg, config) do
    %{ :noreply, config }
  end

  def terminate(reason, config) do
    IO.puts "[FATAL] ExUnit::Formatter crashed:\n#{reason}"
    IO.puts "[FATAL] ExUnit::Formatter snapshot:\n#{config}"
    :ok
  end

  def code_change(_old, config, _extra) do
    %{ :ok, config }
  end

  defp print_failure(%{test_case, test, %{ kind, reason, stacktrace }}, acc) do
    IO.puts "#{acc}) #{test} (#{test_case})"
    IO.puts "  ** #{format_catch(kind, reason)}\n  stacktrace:"
    Enum.each stacktrace, fn(s){ IO.puts "    #{format_stacktrace(s)}" }
    IO.print "\n"
    acc + 1
  end

  defp format_catch(:error, exception) do
    "(#{exception.__record__}) #{exception.message}"
  end

  defp format_catch(kind, reason) do
    "(#{kind}) #{inspect(reason)}"
  end
end