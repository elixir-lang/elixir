defrecord ExUnit::Runner::Config, formatter: ExUnit::Formatter, cases: [], max_cases: 4, taken_cases: 0, sync_cases: []

defmodule ExUnit::Runner do
  # The runner entry point. At first, it will simply spawn cases and start
  # looping expecting messages. When all the cases are spawned and finished,
  # we start running the sync cases. When sync cases finish, tell the formatter
  # we finished and exit.
  def start(config) do
    if config.cases == [] do
      if config.taken_cases > 0 do
        do_loop config
      elsif: config.sync_cases == []
        call_formatter config, :finish
      else:
        do_loop spawn_sync_cases(config)
      end
    else:
      do_loop spawn_cases(config)
    end
  end

  # Loop expecting messages to be sent to the formatter. Whenever a test
  # case has finished executing, decrease the taken cases counter and
  # attempt to spawn new ones.
  #
  # TODO Add timeout.
  def do_loop(config) do
    receive do
    match: { pid, :each, { test_case, test, final } }
      call_formatter config, { :each, test_case, test, final }
      do_loop config
    match: { pid, :each_case, test_case }
      call_formatter config, { :each_case, test_case }
      start config.increment_taken_cases(-1)
    end
  end

  # Spawn the maximum possible of cases according to the max_cases value.
  # If any of the cases are set to run synchronously, put them in a list
  # of tests that will be executed at the end.
  def spawn_cases(config) do
    case config.cases do
    match: [test_case|t]
      if config.taken_cases < config.max_cases do
        spawn_case test_case
        spawn_cases config.increment_taken_cases.cases(t)
      else:
        config
      end
    match: []
      config
    end
  end

  # After all cases were run, it is time to run the asynchronous ones.
  def spawn_sync_cases(config) do
    [test_case|t] = config.sync_cases
    spawn_case test_case
    config.sync_cases(t)
  end

  ## Private

  defp call_formatter(config, message) do
    Erlang.gen_server.call(config.formatter, message)
  end

  # Run each test case in its own process.
  defp spawn_case(test_case) do
    pid = self()
    spawn_link fn(do: run_tests(pid, test_case, tests_for(test_case)))
  end

  # For each instanciated object, dispatch each test in it.
  defp run_tests(pid, test_case, [test|t]) do
    final = try do
      # test_case.setup(test)

      partial = try do
        apply test_case, test, []
        nil
      rescue: error1
        { :error, error1, Code.stacktrace }
      catch: kind1, error1
        { kind1, error1, Code.stacktrace }
      end

      # test_case.teardown(test)
      partial
    rescue: error2
      { :error, error2, Code.stacktrace }
    catch: kind2, error2
      { kind2, error2, Code.stacktrace }
    end

    pid <- { self(), :each, { test_case, test, final } }
    run_tests(pid, test_case, t)
  end

  # When all tests in a testcase were run, notify the runner.
  defp run_tests(pid, test_case, []) do
    pid <- { self(), :each_case, test_case }
  end

  # Retrieves test functions from the module.
  defp tests_for(mod) do
    exports = mod.__info__(:exports)
    tests_for exports, []
  end

  defp tests_for([{function,0}|t], acc) do
    list = atom_to_list(function)
    if match?('test_' ++ _, list) || match?('test ' ++ _, list) do
      tests_for t, [function|acc]
    else:
      tests_for t, acc
    end
  end

  defp tests_for([_|t], acc), do: tests_for t, acc
  defp tests_for([], acc),    do: List.reverse(acc)
end