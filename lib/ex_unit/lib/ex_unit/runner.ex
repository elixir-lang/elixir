defmodule ExUnit.Runner do
  @moduledoc false

  alias ExUnit.EventManager, as: EM

  def run(async, sync, opts, load_us) do
    {opts, config} = configure(opts)

    :erlang.system_flag(:backtrace_depth,
                        Keyword.fetch!(opts, :stacktrace_depth))

    {run_us, _} =
      :timer.tc fn ->
        EM.suite_started(config.manager, opts)
        loop %{config | sync_cases: shuffle(config, sync),
                        async_cases: shuffle(config, async)}
      end

    EM.suite_finished(config.manager, run_us, load_us)
    EM.call(config.manager, ExUnit.RunnerStats, :stop, :infinity)
  end

  def configure(opts) do
    opts = normalize_opts(opts)

    {:ok, pid} = EM.start_link
    formatters = [ExUnit.RunnerStats|opts[:formatters]]
    Enum.each formatters, &(:ok = EM.add_handler(pid, &1, opts))

    config = %{
      async_cases: [],
      capture_log: opts[:capture_log],
      exclude: opts[:exclude],
      include: opts[:include],
      manager: pid,
      max_cases: opts[:max_cases],
      seed: opts[:seed],
      sync_cases: [],
      taken_cases: 0,
      timeout: opts[:timeout],
      trace: opts[:trace]
     }

    {opts, config}
  end

  defp normalize_opts(opts) do
    {include, exclude} = ExUnit.Filters.normalize(opts[:include], opts[:exclude])

    opts
    |> Keyword.put(:exclude, exclude)
    |> Keyword.put(:include, include)
    |> Keyword.put(:max_cases, max_cases(opts))
    |> Keyword.put_new(:seed, :os.timestamp |> elem(2))
  end

  defp max_cases(opts) do
    cond do
      opts[:trace]           -> 1
      max = opts[:max_cases] -> max
      true                   -> :erlang.system_info(:schedulers_online)
    end
  end

  defp loop(config) do
    available = config.max_cases - config.taken_cases

    cond do
      # No cases available, wait for one
      available <= 0 ->
        wait_until_available config

      # Slots are available, start with async cases
      tuple = take_async_cases(config, available) ->
        {config, cases} = tuple
        spawn_cases(config, cases)

      # No more async cases, wait for them to finish
      config.taken_cases > 0 ->
        wait_until_available config

      # So we can start all sync cases
      tuple = take_sync_cases(config) ->
        {config, cases} = tuple
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
      {_pid, :case_finished, _test_case} ->
        loop %{config | taken_cases: config.taken_cases - 1}
    end
  end

  defp spawn_cases(config, cases) do
    pid = self()

    Enum.each cases, fn case_name ->
      spawn_link fn ->
        run_case(config, pid, case_name)
      end
    end

    loop %{config | taken_cases: config.taken_cases + length(cases)}
  end

  defp run_case(config, pid, case_name) do
    test_case = case_name.__ex_unit__(:case)
    EM.case_started(config.manager, test_case)

    # Prepare tests, selecting which ones should
    # run and which ones were skipped.
    tests = prepare_tests(config, test_case.tests)

    {test_case, pending} =
      if Enum.all?(tests, &(&1.state)) do
        {test_case, tests}
      else
        spawn_case(config, test_case, tests)
      end

    # Run the pending tests. We don't actually spawn those
    # tests but we do send the notifications to formatter.
    Enum.each pending, &run_test(config, &1, [])
    EM.case_finished(config.manager, test_case)
    send pid, {self, :case_finished, test_case}
  end

  defp prepare_tests(config, tests) do
    tests   = shuffle(config, tests)
    include = config.include
    exclude = config.exclude

    for test <- tests do
      tags = Map.merge(test.tags, %{test: test.name, case: test.case})
      case ExUnit.Filters.eval(include, exclude, tags, tests) do
        :ok           -> %{test | tags: tags}
        {:error, msg} -> %{test | state: {:skip, msg}}
      end
    end
  end

  defp spawn_case(config, test_case, tests) do
    parent = self

    {case_pid, case_ref} =
      spawn_monitor(fn ->
        ExUnit.OnExitHandler.register(self)

        case exec_case_setup(test_case) do
          {:ok, test_case, context} ->
            Enum.each tests, &run_test(config, &1, context)
            send parent, {self, :case_finished, test_case, []}

          {:error, test_case} ->
            failed_tests = Enum.map tests, & %{&1 | state: {:invalid, test_case}}
            send parent, {self, :case_finished, test_case, failed_tests}
        end

        exit(:shutdown)
      end)

    {test_case, pending} =
      receive do
        {^case_pid, :case_finished, test_case, tests} ->
          receive do
            {:DOWN, ^case_ref, :process, ^case_pid, _} -> :ok
          end
          {test_case, tests}
        {:DOWN, ^case_ref, :process, ^case_pid, error} ->
          test_case = %{test_case | state: failed({:EXIT, case_pid}, error, [])}
          {test_case, []}
      end

    {exec_on_exit(test_case, case_pid), pending}
  end

  defp exec_case_setup(%ExUnit.TestCase{name: case_name} = test_case) do
    {:ok, context} = case_name.__ex_unit__(:setup_all, %{case: case_name})
    {:ok, test_case, context}
  catch
    kind, error ->
      failed = failed(kind, error, pruned_stacktrace())
      {:error, %{test_case | state: failed}}
  end

  defp run_test(true, config, test, context) do
    run_test([], config, test, context)
  end

  defp run_test(false, config, test, context) do
    spawn_test(config, test, context)
  end

  defp run_test(opts, config, test, context) do
    ref = make_ref()
    try do
      ExUnit.CaptureLog.capture_log(opts, fn ->
        send self(), {ref, spawn_test(config, test, context)}
      end)
    catch
      :exit, :noproc ->
        message =
          "could not run test, it uses @tag :capture_log" <>
          " but the :logger application is not running"
        %{test | state: failed(:error, RuntimeError.exception(message), [])}
    else
      logged ->
        receive do
          {^ref, test} -> %{test | logs: logged}
        end
    end
  end

  defp run_test(config, %{tags: tags} = test, context) do
    EM.test_started(config.manager, test)

    test = 
      if is_nil(test.state) do
        capture_log? = Map.get(tags, :capture_log, config.capture_log)
        run_test(capture_log?, config, test, Map.merge(tags, context))
      else
        test
      end

    EM.test_finished(config.manager, test)
  end

  defp spawn_test(config, test, context) do
    parent = self()

    {test_pid, test_ref} =
      spawn_monitor(fn ->
        ExUnit.OnExitHandler.register(self)

        {us, test} =
          :timer.tc(fn ->
            case exec_test_setup(test, context) do
              {:ok, test} ->
                exec_test(test)
              {:error, test} ->
                test
            end
          end)

        send parent, {self, :test_finished, %{test | time: us}}
        exit(:shutdown)
      end)

    timeout = get_timeout(test.tags, config)

    test =
      receive do
        {^test_pid, :test_finished, test} ->
          receive do
            {:DOWN, ^test_ref, :process, ^test_pid, _} -> :ok
          end
          test
        {:DOWN, ^test_ref, :process, ^test_pid, error} ->
          %{test | state: failed({:EXIT, test_pid}, error, [])}
      after
        timeout ->
          stacktrace =
            try do
              Process.info(test_pid, :current_stacktrace)
            catch
              _, _ -> []
            else
              {:current_stacktrace, stacktrace} -> stacktrace
            end
          Process.exit(test_pid, :kill)
          Process.demonitor(test_ref, [:flush])
          %{test | state: failed(:error, %ExUnit.TimeoutError{timeout: timeout}, stacktrace)}
      end

    exec_on_exit(test, test_pid)
  end

  defp exec_test_setup(%ExUnit.Test{case: case} = test, context) do
    {:ok, context} = case.__ex_unit__(:setup, context)
    {:ok, %{test | tags: context}}
  catch
    kind, error ->
      {:error, %{test | state: failed(kind, error, pruned_stacktrace())}}
  end

  defp exec_test(%ExUnit.Test{case: case, name: name, tags: context} = test) do
    apply(case, name, [context])
    test
  catch
    kind, error ->
      %{test | state: failed(kind, error, pruned_stacktrace())}
  end

  defp exec_on_exit(test_or_case, pid) do
    case ExUnit.OnExitHandler.run(pid) do
      :ok ->
        test_or_case
      {kind, reason, stack} ->
        state = test_or_case.state || failed(kind, reason, prune_stacktrace(stack))
        %{test_or_case | state: state}
    end
  end

  ## Helpers

  defp get_timeout(tags, config) do
    if config.trace do
      :infinity
    else
      Map.get(tags, :timeout, config.timeout)
    end
  end

  defp shuffle(%{seed: 0}, list) do
    Enum.reverse(list)
  end

  defp shuffle(%{seed: seed}, list) do
    _ = :rand.seed(:exsplus, {3172, 9814, seed})
    Enum.shuffle(list)
  end

  defp take_async_cases(config, count) do
    case config.async_cases do
      [] -> nil
      cases ->
        {response, remaining} = Enum.split(cases, count)
        {%{config | async_cases: remaining}, response}
    end
  end

  defp take_sync_cases(config) do
    case config.sync_cases do
      [h|t] -> {%{config | sync_cases: t}, [h]}
      []    -> nil
    end
  end

  defp failed(:error, %ExUnit.MultiError{errors: errors}, _stack) do
    {:failed,
     Enum.map(errors, fn {kind, reason, stack} ->
       {kind, Exception.normalize(kind, reason), prune_stacktrace(stack)}
     end)}
  end

  defp failed(kind, reason, stack) do
    {:failed, [{kind, Exception.normalize(kind, reason), stack}]}
  end

  defp pruned_stacktrace, do: prune_stacktrace(System.stacktrace)

  # Assertions can pop-up in the middle of the stack
  defp prune_stacktrace([{ExUnit.Assertions, _, _, _}|t]), do: prune_stacktrace(t)

  # As soon as we see a Runner, it is time to ignore the stacktrace
  defp prune_stacktrace([{ExUnit.Runner, _, _, _}|_]), do: []

  # All other cases
  defp prune_stacktrace([h|t]), do: [h|prune_stacktrace(t)]
  defp prune_stacktrace([]), do: []
end
