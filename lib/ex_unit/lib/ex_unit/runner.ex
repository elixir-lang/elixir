defmodule ExUnit.Runner do
  @moduledoc false

  defrecord Config, formatter: ExUnit.CLIFormatter, max_cases: 4, taken_cases: 0

  def run(opts) do
    config = ExUnit.Runner.Config.new opts
    config.formatter.suite_started
    loop config
  end

  defp loop(config) do
    available = config.max_cases - config.taken_cases

    cond do
      # No cases available, wait for one
      available <= 0 ->
        wait_until_available config

      # Slots are available, start with async cases
      cases = ExUnit.Server.take_async_cases(available) ->
        spawn_cases(config, cases)

      # No more async cases, wait for them to finish
      config.taken_cases > 0 ->
        wait_until_available config

      # So we can start all sync cases
      cases = ExUnit.Server.take_sync_cases ->
        spawn_cases(config, cases)

      # No more cases, we are done!
      true ->
        config.formatter.suite_finished
    end
  end

  # Loop expecting messages from the spawned cases. Whenever
  # a test case has finished executing, decrease the taken
  # cases counter and attempt to spawn new ones.
  defp wait_until_available(config) do
    receive do
      { pid, :test_finished, { test_case, test, final } } ->
        config.formatter.test_finished(test_case, test, final)
        wait_until_available config
      { pid, :case_finished, test_case } ->
        config.formatter.case_finished(test_case)
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
      ExUnit.Server.run_after_spawn
      run_tests(config, pid, test_case)
    end
  end

  defp run_tests(config, pid, test_case) do
    config.formatter.case_started(test_case)

    try do
      tests = tests_for(test_case)
      context = run_setup_all(test_case)
      Enum.each tests, run_test(config, pid, test_case, context, &1)
      run_teardown_all(test_case, context)
    after
      pid <- { self, :case_finished, test_case }
    end
  end

  defp run_test(config, pid, test_case, setup_context, test) do
    config.formatter.test_started(test_case, test)

    final = try do
      context = run_setup(test_case, setup_context, test)

      partial = try do
        apply test_case, test, []
        nil
      rescue
        error1 ->
          { :error, error1, filtered_stacktrace }
      catch
        kind1, error1 ->
          { kind1, error1, filtered_stacktrace }
      end

      run_teardown(test_case, context, test)
      partial
    rescue
      error2 ->
        { :error, error2, filtered_stacktrace }
    catch
      kind2, error2 ->
        { kind2, error2, filtered_stacktrace }
    end

    pid <- { self, :test_finished, { test_case, test, final } }
  end

  defp run_setup_all(test_case) do
    cond do
      function_exported?(test_case, :setup_all, 0) ->
        test_case.setup_all
      true ->
        []
    end
  end

  defp run_setup(test_case, context, test) do
    cond do
      function_exported?(test_case, :setup, 2) ->
        test_case.setup(context, test)
      function_exported?(test_case, :setup, 1) ->
        test_case.setup(context)
      function_exported?(test_case, :setup, 0) ->
        test_case.setup
      true ->
        context
    end
  end

  defp run_teardown(test_case, context, test) do
    cond do
      function_exported?(test_case, :teardown, 2) ->
        test_case.teardown(context, test)
      function_exported?(test_case, :teardown, 1) ->
        test_case.teardown(context)
      function_exported?(test_case, :teardown, 0) ->
        test_case.teardown
      true ->
        context
    end
  end

  defp run_teardown_all(test_case, context) do
    cond do
      function_exported?(test_case, :teardown_all, 1) ->
        test_case.teardown_all(context)
      function_exported?(test_case, :teardown_all, 0) ->
        test_case.teardown_all
      true ->
        context
    end
  end

  ## Helpers

  defp tests_for(mod) do
    exports = mod.__info__(:functions)
    lc { function, 0 } inlist exports, is_test?(atom_to_list(function)), do: function
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