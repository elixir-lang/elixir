defmodule ExUnit.Runner do
  @moduledoc false

  alias ExUnit.EventManager, as: EM

  @rand_algorithm :exs1024

  def run(opts, load_us) when (is_integer(load_us) or is_nil(load_us)) and is_list(opts) do
    opts = normalize_opts(opts)
    {:ok, manager} = EM.start_link()
    {:ok, stats_pid} = EM.add_handler(manager, ExUnit.RunnerStats, opts)
    config = configure(opts, manager, self(), stats_pid)

    :erlang.system_flag(:backtrace_depth, Keyword.fetch!(opts, :stacktrace_depth))

    {run_us, _} =
      :timer.tc(fn ->
        EM.suite_started(config.manager, opts)
        loop(config, :async, 0)
      end)

    EM.suite_finished(config.manager, run_us, load_us)
    stats = ExUnit.RunnerStats.stats(stats_pid)
    EM.stop(config.manager)
    after_suite_callbacks = Application.fetch_env!(:ex_unit, :after_suite)
    Enum.each(after_suite_callbacks, fn callback -> callback.(stats) end)
    stats
  end

  defp configure(opts, manager, runner_pid, stats_pid) do
    Enum.each(opts[:formatters], &EM.add_handler(manager, &1, opts))

    %{
      capture_log: opts[:capture_log],
      exclude: opts[:exclude],
      include: opts[:include],
      manager: manager,
      max_cases: opts[:max_cases],
      only_test_ids: opts[:only_test_ids],
      runner_pid: runner_pid,
      seed: opts[:seed],
      stats_pid: stats_pid,
      timeout: opts[:timeout],
      trace: opts[:trace]
    }
  end

  defp normalize_opts(opts) do
    {include, exclude} = ExUnit.Filters.normalize(opts[:include], opts[:exclude])

    opts
    |> Keyword.put(:exclude, exclude)
    |> Keyword.put(:include, include)
  end

  defp loop(config, :async, taken) do
    available = config.max_cases - taken

    cond do
      # No modules available, wait for one
      available <= 0 ->
        wait_until_available(config, :async, taken)

      # Slots are available, start with async modules
      modules = ExUnit.Server.take_async_modules(available) ->
        spawn_modules(config, modules, :async, taken)

      true ->
        modules = ExUnit.Server.take_sync_modules()
        loop(config, modules, taken)
    end
  end

  defp loop(config, modules, taken) do
    case modules do
      _ when taken > 0 ->
        wait_until_available(config, modules, taken)

      # So we can start all sync modules
      [head | tail] ->
        spawn_modules(config, [head], tail, taken)

      # No more modules, we are done!
      [] ->
        config
    end
  end

  # Loop expecting messages from the spawned modules. Whenever
  # a module has finished executing, decrease the taken modules
  # counter and attempt to spawn new ones.
  defp wait_until_available(config, modules, taken) do
    receive do
      {_pid, :module_finished, _test_case} ->
        loop(config, modules, taken - 1)
    end
  end

  defp spawn_modules(config, modules, modules_remaining, taken) do
    Enum.each(modules, fn module ->
      spawn_link(fn ->
        run_module(config, module)
      end)
    end)

    loop(config, modules_remaining, taken + length(modules))
  end

  defp run_module(config, module) do
    test_module = module.__ex_unit__()
    EM.module_started(config.manager, test_module)

    # Prepare tests, selecting which ones should
    # run and which ones were skipped.
    tests = prepare_tests(config, test_module.tests)

    {test_module, pending, finished_tests} =
      if Enum.all?(tests, & &1.state) do
        # The pending tests here aren't actually run, so they're already
        # "finished"
        {test_module, tests, tests}
      else
        spawn_module(config, test_module, tests)
      end

    # Run the pending tests. We don't actually spawn those
    # tests but we do send the notifications to formatter.
    Enum.each(pending, &run_test(config, &1, []))
    test_module = %{test_module | tests: finished_tests}
    EM.module_finished(config.manager, test_module)
    send(config.runner_pid, {self(), :module_finished, test_module})
  end

  defp prepare_tests(config, tests) do
    tests = shuffle(config, tests)
    include = config.include
    exclude = config.exclude
    test_ids = config.only_test_ids

    for test <- tests, include_test?(test_ids, test) do
      tags = Map.merge(test.tags, %{test: test.name, module: test.module})

      case ExUnit.Filters.eval(include, exclude, tags, tests) do
        :ok -> %{test | tags: tags}
        excluded_or_skipped -> %{test | state: excluded_or_skipped}
      end
    end
  end

  defp include_test?(nil, _test), do: true

  defp include_test?(test_ids, test) do
    MapSet.member?(test_ids, {test.module, test.name})
  end

  defp spawn_module(config, test_module, tests) do
    parent_pid = self()
    timeout = get_timeout(config, %{})

    {module_pid, module_ref} =
      spawn_monitor(fn ->
        ExUnit.OnExitHandler.register(self())

        case exec_module_setup(test_module) do
          {:ok, test_module, context} ->
            finished_tests = Enum.map(tests, &run_test(config, &1, context))
            send(parent_pid, {self(), :module_finished, test_module, [], finished_tests})

          {:error, test_module} ->
            failed_tests = Enum.map(tests, &%{&1 | state: {:invalid, test_module}})
            send(parent_pid, {self(), :module_finished, test_module, failed_tests, []})
        end

        exit(:shutdown)
      end)

    {test_module, pending, finished_tests} =
      receive do
        {^module_pid, :module_finished, test_module, failed_tests, finished_tests} ->
          Process.demonitor(module_ref, [:flush])
          {test_module, failed_tests, finished_tests}

        {:DOWN, ^module_ref, :process, ^module_pid, error} ->
          test_module = %{test_module | state: failed({:EXIT, module_pid}, error, [])}
          {test_module, [], []}
      end

    {exec_on_exit(test_module, module_pid, timeout), pending, finished_tests}
  end

  defp exec_module_setup(%ExUnit.TestModule{name: module} = test_module) do
    {:ok, test_module, module.__ex_unit__(:setup_all, %{module: module, case: module})}
  catch
    kind, error ->
      failed = failed(kind, error, prune_stacktrace(__STACKTRACE__))
      {:error, %{test_module | state: failed}}
  end

  defp run_test_with_capture_log(true, config, test, context) do
    run_test_with_capture_log([], config, test, context)
  end

  defp run_test_with_capture_log(false, config, test, context) do
    spawn_test(config, test, context)
  end

  defp run_test_with_capture_log(capture_log_opts, config, test, context) do
    ref = make_ref()

    try do
      ExUnit.CaptureLog.capture_log(capture_log_opts, fn ->
        send(self(), {ref, spawn_test(config, test, context)})
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
        capture_log = Map.get(tags, :capture_log, config.capture_log)
        run_test_with_capture_log(capture_log, config, test, Map.merge(tags, context))
      else
        test
      end

    EM.test_finished(config.manager, test)
    test
  end

  defp spawn_test(config, test, context) do
    parent_pid = self()
    timeout = get_timeout(config, test.tags)

    {test_pid, test_ref} =
      spawn_monitor(fn ->
        ExUnit.OnExitHandler.register(self())

        generate_test_seed(config, test)

        {us, test} =
          :timer.tc(fn ->
            case exec_test_setup(test, context) do
              {:ok, test} ->
                exec_test(test)

              {:error, test} ->
                test
            end
          end)

        send(parent_pid, {self(), :test_finished, %{test | time: us}})
        exit(:shutdown)
      end)

    test = receive_test_reply(test, test_pid, test_ref, timeout)

    exec_on_exit(test, test_pid, timeout)
  end

  defp receive_test_reply(test, test_pid, test_ref, timeout) do
    receive do
      {^test_pid, :test_finished, test} ->
        Process.demonitor(test_ref, [:flush])
        test

      {:DOWN, ^test_ref, :process, ^test_pid, error} ->
        %{test | state: failed({:EXIT, test_pid}, error, [])}
    after
      timeout ->
        case Process.info(test_pid, :current_stacktrace) do
          {:current_stacktrace, stacktrace} ->
            Process.demonitor(test_ref, [:flush])
            Process.exit(test_pid, :kill)

            exception =
              ExUnit.TimeoutError.exception(
                timeout: timeout,
                type: Atom.to_string(test.tags.test_type)
              )

            %{test | state: failed(:error, exception, stacktrace)}

          nil ->
            receive_test_reply(test, test_pid, test_ref, timeout)
        end
    end
  end

  defp exec_test_setup(%ExUnit.Test{module: module} = test, context) do
    {:ok, %{test | tags: module.__ex_unit__(:setup, context)}}
  catch
    kind, error ->
      {:error, %{test | state: failed(kind, error, prune_stacktrace(__STACKTRACE__))}}
  end

  defp exec_test(%ExUnit.Test{module: module, name: name, tags: context} = test) do
    apply(module, name, [context])
    test
  catch
    kind, error ->
      %{test | state: failed(kind, error, prune_stacktrace(__STACKTRACE__))}
  end

  defp exec_on_exit(test_or_case, pid, timeout) do
    case ExUnit.OnExitHandler.run(pid, timeout) do
      :ok ->
        test_or_case

      {kind, reason, stack} ->
        state = test_or_case.state || failed(kind, reason, prune_stacktrace(stack))
        %{test_or_case | state: state}
    end
  end

  ## Helpers

  defp generate_test_seed(%{seed: seed}, %ExUnit.Test{module: module, name: name}) do
    :rand.seed(@rand_algorithm, {:erlang.phash2(module), :erlang.phash2(name), seed})
  end

  defp get_timeout(config, tags) do
    if config.trace() do
      :infinity
    else
      Map.get(tags, :timeout, config.timeout)
    end
  end

  defp shuffle(%{seed: 0}, list) do
    Enum.reverse(list)
  end

  defp shuffle(%{seed: seed}, list) do
    _ = :rand.seed(@rand_algorithm, {seed, seed, seed})
    Enum.shuffle(list)
  end

  defp failed(:error, %ExUnit.MultiError{errors: errors}, _stack) do
    errors =
      Enum.map(errors, fn {kind, reason, stack} ->
        {kind, Exception.normalize(kind, reason, stack), prune_stacktrace(stack)}
      end)

    {:failed, errors}
  end

  defp failed(kind, reason, stack) do
    {:failed, [{kind, Exception.normalize(kind, reason, stack), stack}]}
  end

  # Assertions can pop-up in the middle of the stack
  defp prune_stacktrace([{ExUnit.Assertions, _, _, _} | t]), do: prune_stacktrace(t)

  # As soon as we see a Runner, it is time to ignore the stacktrace
  defp prune_stacktrace([{ExUnit.Runner, _, _, _} | _]), do: []

  # All other cases
  defp prune_stacktrace([h | t]), do: [h | prune_stacktrace(t)]
  defp prune_stacktrace([]), do: []
end
