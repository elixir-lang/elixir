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