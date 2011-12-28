# TODO: Make this a genserver

module ExUnit::Formatter

defrecord Config, counter: 0, failures: []

def start do
  Config.new
end

def each(config, _test_case, _test, nil) do
  Erlang.io.format "."
  config.counter(config.counter + 1)
end

def each(config, test_case, test, failure) do
  Erlang.io.format "F"
  config.counter(config.counter + 1).
    prepend_failures([{test_case, test, failure}])
end

def each_case(config, _test_case) do
  config
end

def finish(config) do
  Erlang.io.format "\n"
  List.foldl config.failures, 1, fn(x, acc) { print_failure(x, acc) }
  Erlang.io.format "#{integer_to_binary(config.counter)} tests, #{integer_to_binary(length(config.failures))} failures."
end

private

def integer_to_binary(int) do
  list_to_binary(integer_to_list(int))
end

def print_failure({test_case, test, failure}, acc) do
  Erlang.io.format "#{integer_to_binary(acc)}) #{atom_to_binary(test, :utf8)}(#{atom_to_binary(test_case, :utf8)})\n"
  Erlang.io.format '~p~n', [failure]
  acc + 1
end