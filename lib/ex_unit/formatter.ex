module ExUnit::Formatter

defrecord Config, counter: 0, failures: []

def start do
  { :ok, pid } = Erlang.gen_server.start_link(__MODULE__, [], [])
  pid
end

def init(_args) do
  { :ok, ExUnit::Formatter::Config.new }
end

def handle_call({:each, _test_case, _test, nil }, _from, config) do
  Erlang.io.format "."
  { :reply, :ok, config.increment_counter }
end

def handle_call({:each, test_case, test, failure }, _from, config) do
  Erlang.io.format "F"
  { :reply, :ok, config.increment_counter.
    prepend_failures([{test_case, test, failure}]) }
end

def handle_call({:each_case, _test_case}, _from, config) do
  { :reply, :ok, config }
end

def handle_call(:finish, _from, config) do
  Erlang.io.format "\n"
  List.foldl config.failures, 1, fn(x, acc) { print_failure(x, acc) }
  Erlang.io.format "#{integer_to_binary(config.counter)} tests, #{integer_to_binary(length(config.failures))} failures.\n"
  { :reply, :ok, config }
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

def print_failure({test_case, test, failure}, acc) do
  Erlang.io.format "#{integer_to_binary(acc)}) #{atom_to_binary(test, :utf8)} (#{atom_to_binary(test_case, :utf8)})\n"
  Erlang.io.format '~p~n', [failure]
  acc + 1
end