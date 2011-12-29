module ExUnit::Formatter do
  defrecord Config, counter: 0, failures: []

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
    List.foldl config.failures, 1, fn(x, acc) { print_failure(x, acc) }
    failures_count = length(config.failures)
    IO.puts "#{integer_to_binary(config.counter)} tests, #{integer_to_binary(failures_count)} failures."
    { :reply, failures_count, config }
  end

  def handle_call(_request, _from, config) do
    { :reply, :undef, config }
  end

  def handle_info(_msg, config) do
    { :noreply, config }
  end

  def handle_cast(_msg, config) do
    { :noreply, config }
  end

  def terminate(reason, config) do
    IO.puts "[FATAL] ExUnit::Formatter crashed:\n#{reason}"
    IO.puts "[FATAL] ExUnit::Formatter snapshot:\n#{config}"
    :ok
  end

  def code_change(_old, config, _extra) do
    { :ok, config }
  end

  private

  def integer_to_binary(int) do
    list_to_binary(integer_to_list(int))
  end

  def print_failure({test_case, test, { kind, reason, stacktrace }}, acc) do
    IO.puts "#{integer_to_binary(acc)}) #{atom_to_binary(test, :utf8)} (#{atom_to_binary(test_case, :utf8)})"
    IO.puts "  #{atom_to_binary(kind, :utf8)} #{Elixir::Formatter.format_catch(kind, reason)}\n  stacktrace:"
    List.each stacktrace, fn(s){ IO.puts "    #{Elixir::Formatter.format_stacktrace(s)}" }
    IO.print "\n"
    acc + 1
  end
end