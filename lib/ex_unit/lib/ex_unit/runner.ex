defmodule ExUnit.Runner do
  @moduledoc false

  defrecord Config, formatter: ExUnit.CLIFormatter, formatter_id: nil,
                    max_cases: 4, taken_cases: 0, async_cases: [], sync_cases: []

  def run(async, sync, opts) do
    config = Config[max_cases: :erlang.system_info(:schedulers_online)]
    config = config.update(opts)

    { ms, config } =
      :timer.tc fn ->
        loop config.async_cases(async).sync_cases(sync).
               formatter_id(config.formatter.suite_started(opts))
      end

    config.formatter.suite_finished(config.formatter_id, ms)
  end

  defp loop(Config[] = config) do
    available = config.max_cases - config.taken_cases

    cond do
      # No cases available, wait for one
      available <= 0 ->
        wait_until_available config

      # Slots are available, start with async cases
      tuple = take_async_cases(config, available) ->
        { config, cases } = tuple
        spawn_cases(config, cases)

      # No more async cases, wait for them to finish
      config.taken_cases > 0 ->
        wait_until_available config

      # So we can start all sync cases
      tuple = take_sync_cases(config) ->
        { config, cases } = tuple
        spawn_cases(config, cases)

      # No more cases, we are done!
      true ->
        config
    end
  end

  # Loop expecting messages from the spawned cases. Whenever
  # a test case has finished executing, decrease the taken
  # cases counter and attempt to spawn new ones.
  defp wait_until_available(config) do
    receive do
      { _pid, :test_finished, test } ->
        config.formatter.test_finished(config.formatter_id, test)
        wait_until_available config
      { _pid, :case_finished, test_case } ->
        config.formatter.case_finished(config.formatter_id, test_case)
        loop config.update_taken_cases(&1-1)
    end
  end

  defp spawn_cases(config, cases) do
    Enum.each cases, spawn_case(config, &1)
    loop config.update_taken_cases(&1+length(cases))
  end

  defp spawn_case(config, test_case) do
    pid = self()
    spawn_link fn ->
      run_tests(config, pid, test_case)
    end
  end

  defp run_tests(config, pid, case_name) do
    test_case = ExUnit.TestCase[name: case_name]
    config.formatter.case_started(config.formatter_id, test_case)

    self_pid = self
    { case_pid, case_ref } = Process.spawn_monitor fn ->
      { test_case, context } = try do
        context = case_name.__exunit__(:setup_all, [case: test_case])
        { test_case, context }
      rescue
        error ->
          { test_case.failure({ :error, error, filtered_stacktrace }), nil }
      catch
        kind, error ->
          { test_case.failure({ kind, error, filtered_stacktrace }), nil }
      end

      tests = tests_for(case_name)
      if test_case.failure do
        Enum.each tests, fn test_name ->
          test = ExUnit.Test[name: test_name, case: test_case, invalid: true]
          pid <- { self, :test_finished, test }
        end
        self_pid <- { self, :case_finished, test_case }

      else
        Enum.each tests, run_test(config, pid, test_case, &1, context)

        test_case = try do
          case_name.__exunit__(:teardown_all, context)
          test_case
        rescue
          error ->
            test_case.failure { :error, error, filtered_stacktrace }
        catch
          kind, error ->
            test_case.failure { kind, error, filtered_stacktrace }
        end

        self_pid <- { self, :case_finished, test_case }
      end
    end

    receive do
      { ^case_pid, :case_finished, test_case } ->
        pid <- { case_pid, :case_finished, test_case }
      { :DOWN, ^case_ref, :process, ^case_pid, { error, stacktrace } } ->
        test_case = test_case.failure { :EXIT, error, filter_stacktrace(stacktrace) }
        pid <- { case_pid, :case_finished, test_case }
    end
  end

  defp run_test(config, pid, test_case, test_name, context) do
    test = ExUnit.Test[name: test_name, case: test_case]
    ExUnit.TestCase[name: case_name] = test_case
    config.formatter.test_started(config.formatter_id, test)

    # Run test in a new process so that we can trap exits for a single test
    self_pid = self
    { test_pid, test_ref } = Process.spawn_monitor fn ->
      test = try do
        context = case_name.__exunit__(:setup, Keyword.put(context, :test, test))

        test = try do
          apply case_name, test_name, [context]
          test
        rescue
          error1 ->
            test.failure { :error, error1, filtered_stacktrace }
        catch
          kind1, error1 ->
            test.failure { kind1, error1, filtered_stacktrace }
        end

        case_name.__exunit__(:teardown, Keyword.put(context, :test, test))
        test
      rescue
        error2 ->
          test.failure { :error, error2, filtered_stacktrace }
      catch
        kind2, error2 ->
          test.failure { kind2, error2, filtered_stacktrace }
      end

      self_pid <- { self, :test_finished, test }
    end

    receive do
      { ^test_pid, :test_finished, test } ->
        pid <- { test_pid, :test_finished, test }
      { :DOWN, ^test_ref, :process, ^test_pid, { error, stacktrace } } ->
        test = test.failure { :EXIT, error, filter_stacktrace(stacktrace) }
        pid <- { test_pid, :test_finished, test }
    end
  end

  ## Helpers

  defp take_async_cases(Config[] = config, count) do
    case config.async_cases do
      [] -> nil
      cases ->
        { response, remaining } = Enum.split(cases, count)
        { config.async_cases(remaining), response }
    end
  end

  defp take_sync_cases(Config[] = config) do
    case config.sync_cases do
      [h|t] -> { config.sync_cases(t), [h] }
      []    -> nil
    end
  end

  defp tests_for(mod) do
    exports = mod.__info__(:functions)

    lc { function, 0 } inlist exports, is_test?(atom_to_list(function)) do
      IO.puts "Test function #{inspect mod}.#{function} with arity 0 is no longer supported. Use the test macro instead."
    end

    lc { function, 1 } inlist exports, is_test?(atom_to_list(function)) do
      function
    end
  end

  defp is_test?('test_' ++ _), do: true
  defp is_test?('test ' ++ _), do: true
  defp is_test?(_)           , do: false

  defp filtered_stacktrace, do: filter_stacktrace(System.stacktrace)

  # Assertions can pop-up in the middle of the stack
  defp filter_stacktrace([{ ExUnit.Assertions, _, _, _ }|t]), do: filter_stacktrace(t)

  # As soon as we see a Runner, it is time to ignore the stacktrace
  defp filter_stacktrace([{ ExUnit.Runner, _, _, _ }|_]), do: []

  # All other cases
  defp filter_stacktrace([h|t]), do: [h|filter_stacktrace(t)]
  defp filter_stacktrace([]), do: []
end
