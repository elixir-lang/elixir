defmodule ExUnit.Runner do
  @moduledoc false

  defrecord Config, formatter: ExUnit.Formatter, async_cases: [],
    max_cases: 4, taken_cases: 0, sync_cases: []

  # The runner entry point. At first, it will simply spawn async cases
  # and expect messages as the cases finish. When all async cases are
  # spawned and finished, we start running the sync cases. When sync
  # cases finish, tell the formatter we finished and exit.
  def start(config) do
    if config.async_cases == [] do
      cond do
        config.taken_cases > 0 ->
          do_loop config
        config.sync_cases == [] ->
          call_formatter config, :finish
        true ->
          do_loop spawn_sync_cases(config)
      end
    else
      do_loop spawn_async_cases(config)
    end
  end

  # Loop expecting messages from the spawned cases. Whenever a test
  # case has finished executing, decrease the taken cases counter and
  # attempt to spawn new ones.
  defp do_loop(config) do
    receive do
      { pid, :each, { test_case, test, final } } ->
        call_formatter config, { :each, test_case, test, final }
        do_loop config
      { pid, :each_case, test_case } ->
        call_formatter config, { :each_case, test_case }
        start config.increment_taken_cases(-1)
    end
  end

  # Spawn the maximum possible of cases according to the max_cases value.
  defp spawn_async_cases(config) do
    case config.async_cases do
      [test_case|t] ->
        if config.taken_cases < config.max_cases do
          spawn_case test_case
          spawn_async_cases config.increment_taken_cases.async_cases(t)
        else
          config
        end
      [] ->
        config
    end
  end

  # After all cases were run, it is time to run the asynchronous ones.
  def spawn_sync_cases(config) do
    [test_case|t] = config.sync_cases
    spawn_case test_case
    config.sync_cases(t)
  end

  # Spawn each test case in a new process.
  defp spawn_case(test_case) do
    pid = self
    spawn_link fn -> run_tests(pid, test_case) end
  end

  defp run_tests(pid, test_case) do
    try do
      tests = tests_for(test_case)
      test_case.setup_all
      Enum.each tests, run_test(pid, test_case, &1)
      test_case.teardown_all
    after
      pid <- { self, :each_case, test_case }
    end
  end

  defp run_test(pid, test_case, test) do
    final = try do
      _ = test_case.setup(test)

      partial = try do
        apply test_case, test, []
        nil
      rescue
        error1 ->
          { :error, error1, System.stacktrace }
      catch
        kind1, error1 ->
          { kind1, error1, System.stacktrace }
      end

      _ = test_case.teardown(test)
      partial
    rescue
      error2 ->
        { :error, error2, System.stacktrace }
    catch
      kind2, error2 ->
        { kind2, error2, System.stacktrace }
    end

    pid <- { self, :each, { test_case, test, final } }
  end

  defp call_formatter(config, message) do
    Erlang.gen_server.call(config.formatter, message)
  end

  # Retrieves test functions from the module.
  defp tests_for(mod) do
    exports = mod.__info__(:functions)
    lc { function, 0 } inlist exports, is_test?(atom_to_list(function)), do: function
  end

  defp is_test?('test_' ++ _), do: true
  defp is_test?('test ' ++ _), do: true
  defp is_test?(_)           , do: false
end