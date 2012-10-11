defmodule ExUnit.Runner do
  @moduledoc false

  defrecord Config, formatter: ExUnit.Formatter, max_cases: 4, taken_cases: 0

  def loop(config) do
    available = config.max_cases - config.taken_cases

    cond do
      # No cases available, wait for one
      available <= 0 ->
        wait_until_available config

      # Slots are available, start with async cases
      cases = ExUnit.Server.take_async_cases(available) ->
        spawn_cases(cases, config)

      # No more async cases, wait for them to finish
      config.taken_cases > 0 ->
        wait_until_available config

      # So we can start all sync cases
      cases = ExUnit.Server.take_sync_cases ->
        spawn_cases(cases, config)

      # No more cases, we are done!
      true ->
        call_formatter config, :finish
    end
  end

  # Loop expecting messages from the spawned cases. Whenever
  # a test case has finished executing, decrease the taken
  # cases counter and attempt to spawn new ones.
  defp wait_until_available(config) do
    receive do
      { pid, :each, { test_case, test, final } } ->
        call_formatter config, { :each, test_case, test, final }
        wait_until_available config
      { pid, :each_case, test_case } ->
        call_formatter config, { :each_case, test_case }
        loop config.increment_taken_cases(-1)
    end
  end

  defp spawn_cases(cases, config) do
    Enum.each cases, spawn_case(&1)
    loop config.increment_taken_cases(length(cases))
  end

  defp spawn_case(test_case) do
    pid = self()
    spawn_link fn ->
      ExUnit.Server.run_after_spawn
      run_tests(pid, test_case)
    end
  end

  defp run_tests(pid, test_case) do
    try do
      tests = tests_for(test_case)
      context = run_setup_all(test_case)
      Enum.each tests, run_test(pid, test_case, context, &1)
      run_teardown_all(test_case, context)
    after
      pid <- { self, :each_case, test_case }
    end
  end

  defp run_test(pid, test_case, setup_context, test) do
    final = try do
      context = run_setup(test_case, setup_context, test)

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

      run_teardown(test_case, context, test)
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

  defp call_formatter(config, message) do
    :gen_server.call(config.formatter, message)
  end

  defp tests_for(mod) do
    exports = mod.__info__(:functions)
    lc { function, 0 } inlist exports, is_test?(atom_to_list(function)), do: function
  end

  defp is_test?('test_' ++ _), do: true
  defp is_test?('test ' ++ _), do: true
  defp is_test?(_)           , do: false
end