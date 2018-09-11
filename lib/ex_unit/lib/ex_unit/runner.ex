defmodule ExUnit.Runner do
  @moduledoc false

  alias ExUnit.EventManager, as: EM

  @rand_algorithm :exs1024
  @not_executed_msg "not executed due to maximum number of failures exceeded"

  def run(opts, load_us) do
    {:ok, manager} = EM.start_link()
    {:ok, stat_sup} = EM.add_handler(manager, ExUnit.RunnerStats, opts)
    ExUnit.OnExitHandler.start_failure_counter(manager)
    {opts, config} = configure(manager, opts)
    config = Map.put(config, :stat_sup, stat_sup)
    :erlang.system_flag(:backtrace_depth, Keyword.fetch!(opts, :stacktrace_depth))

    {run_us, _} =
      :timer.tc(fn ->
        EM.suite_started(config.manager, opts)
        loop(config, 0)
      end)

    EM.suite_finished(config.manager, run_us, load_us)
    stats = ExUnit.RunnerStats.stats(stat_sup)
    EM.stop(config.manager)
    after_suite_callbacks = Application.fetch_env!(:ex_unit, :after_suite)
    Enum.each(after_suite_callbacks, fn callback -> callback.(stats) end)
    stats
  end

  defp configure(manager, opts) do
    opts = normalize_opts(opts)
    Enum.each(opts[:formatters], &EM.add_handler(manager, &1, opts))

    config = %{
      capture_log: opts[:capture_log],
      exclude: opts[:exclude],
      include: opts[:include],
      manager: manager,
      max_cases: opts[:max_cases],
      max_failures: opts[:max_failures],
      only_test_ids: opts[:only_test_ids],
      seed: opts[:seed],
      modules: :async,
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
  end

  defp loop(%{modules: :async} = config, taken) do
    available = config.max_cases - taken

    cond do
      # No modules available, wait for one
      available <= 0 ->
        wait_until_available(config, taken)

      # Slots are available, start with async modules
      modules = ExUnit.Server.take_async_modules(available) ->
        spawn_modules(modules, self(), taken, config)

      true ->
        modules = ExUnit.Server.take_sync_modules()
        loop(%{config | modules: modules}, taken)
    end
  end

  defp loop(%{modules: modules} = config, taken) do
    case modules do
      _ when taken > 0 ->
        wait_until_available(config, taken)

      # So we can start all sync modules
      [head | tail] ->
        spawn_modules([head], self(), taken, %{config | modules: tail})

      # No more modules, we are done!
      [] ->
        config
    end
  end

  # Loop expecting messages from the spawned modules. Whenever
  # a module has finished executing, decrease the taken modules
  # counter and attempt to spawn new ones.
  defp wait_until_available(config, taken) do
    receive do
      {_pid, :module_finished, _test_case} ->
        loop(config, taken - 1)
    end
  end

  defp spawn_modules(modules, parent_pid, taken, config) do
    Enum.each(modules, fn module ->
      spawn_link(fn ->
        run_module(module, parent_pid, config)
      end)
    end)

    loop(config, taken + length(modules))
  end

  defp run_module(module, parent_pid, config) do
    test_module = module.__ex_unit__()
    EM.module_started(config.manager, test_module)

    # Prepare tests, selecting which ones should be run or skipped
    tests = prepare_tests(config, test_module.tests)

    {test_module, pending, finished_tests} =
      if Enum.all?(tests, & &1.state) do
        # The pending tests here aren't actually run, so they're already
        # "finished"
        {test_module, tests, tests}
      else
        spawn_module(test_module, tests, config)
      end

    # Run the pending tests. We don't actually spawn those
    # tests but we do send the notifications to formatter.
    Enum.each(pending, &run_test(config, &1, []))
    test_module = %{test_module | tests: finished_tests}
    EM.module_finished(config.manager, test_module)

    # message loop/2
    send(parent_pid, {self(), :module_finished, test_module})
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

  defp spawn_module(test_module, tests, config) do
    parent_pid = self()
    timeout = get_timeout(%{}, config)

    {module_pid, module_ref} = spawn_module_monitor(test_module, parent_pid, tests, config)
    options = %{pid: module_pid, parent_pid: parent_pid, config: config}

    {test_module, pending, finished_tests} =
      receive do
        {^module_pid, :module_finished, test_module, failed_tests, finished_tests} ->
          process_failure(test_module, options)
          Process.demonitor(module_ref, [:flush])
          {test_module, failed_tests, finished_tests}

        {:DOWN, ^module_ref, :process, ^module_pid, :max_failures_exceeded} ->
          test_module = %{test_module | state: {:not_executed, @not_executed_msg}}
          process_failure(test_module, options)
          {test_module, [], []}

        {:DOWN, ^module_ref, :process, ^module_pid, error} ->
          test_module = %{test_module | state: failed({:EXIT, module_pid}, error, [])}
          process_failure(test_module, options)
          {test_module, [], []}
      end

    {exec_on_exit(test_module, module_pid, timeout), pending, finished_tests}
  end

  defp spawn_module_monitor(test_module, parent_pid, tests, config) do
    spawn_monitor(fn ->
      ExUnit.OnExitHandler.register(self(), config.manager)

      if max_failures_exceeded?(config.manager, config.max_failures) do
        test_module = %{test_module | state: {:not_executed, @not_executed_msg}}
        not_executed_tests = Enum.map(tests, &%{&1 | state: {:not_executed, @not_executed_msg}})
        send(parent_pid, {self(), :module_finished, test_module, not_executed_tests, []})
      else
        case exec_module_setup(test_module) do
          {:ok, test_module, context} ->
            finished_tests = Enum.map(tests, &run_test(config, &1, context))
            send(parent_pid, {self(), :module_finished, test_module, [], finished_tests})

          {:error, test_module} ->
            failed_tests = Enum.map(tests, &%{&1 | state: {:invalid, test_module}})
            send(parent_pid, {self(), :module_finished, test_module, failed_tests, []})
        end
      end

      exit(:shutdown)
    end)
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
    spawn_test(test, context, config)
  end

  defp run_test_with_capture_log(opts, config, test, context) do
    ref = make_ref()

    try do
      ExUnit.CaptureLog.capture_log(opts, fn ->
        send(self(), {ref, spawn_test(test, context, config)})
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
        run_test_with_capture_log(capture_log?, config, test, Map.merge(tags, context))
      else
        test
      end

    EM.test_finished(config.manager, test)
    test
  end

  defp spawn_test(test, context, config) do
    parent_pid = self()
    timeout = get_timeout(test.tags, config)

    {test_pid, test_ref} = spawn_test_monitor(test, parent_pid, context, config)

    receive_test_reply_options = %{
      pid: test_pid,
      ref: test_ref,
      parent_pid: parent_pid,
      timeout: timeout,
      config: config
    }

    test =
      case receive_test_reply(test, receive_test_reply_options) do
        {:ok, test} ->
          test

        {:error, {:max_failures_exceeded, test}} ->
          test
      end

    exec_on_exit(test, test_pid, timeout)
  end

  defp spawn_test_monitor(test, parent_pid, context, config) do
    spawn_monitor(fn ->
      test_pid = self()
      generate_test_seed(test, config)

      ExUnit.OnExitHandler.register(test_pid, config.manager)

      {time, test} =
        :timer.tc(fn ->
          if max_failures_exceeded?(config.manager, config.max_failures) do
            %{test | state: {:not_executed, @not_executed_msg}, time: 0}
          else
            case exec_test_setup(test, context) do
              {:ok, test} ->
                exec_test(test)

              {:error, test} ->
                # %{test | state: {:invalid, test}}
                test
            end
          end
        end)

      send(parent_pid, {test_pid, :test_finished, %{test | time: time}})
      exit(:shutdown)
    end)
  end

  defp generate_test_seed(%ExUnit.Test{module: module, name: name}, %{seed: seed}) do
    :rand.seed(@rand_algorithm, {:erlang.phash2(module), :erlang.phash2(name), seed})
  end

  defp receive_test_reply(
         test,
         %{pid: test_pid, ref: test_ref, timeout: timeout} = options
       ) do
    receive do
      {^test_pid, :test_finished, test} ->
        result = process_failure(test, options)
        Process.demonitor(test_ref, [:flush])
        result

      {:DOWN, ^test_ref, :process, ^test_pid, :max_failures_exceeded} ->
        test = %{test | state: {:not_executed, @not_executed_msg}}
        {:error, {:max_failures_exceeded, test}}

      {:DOWN, ^test_ref, :process, ^test_pid, error} ->
        test = %{test | state: failed({:EXIT, test_pid}, error, [])}
        process_failure(test, options)
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

            test = %{test | state: failed(:error, exception, stacktrace)}
            process_failure(test, options)

          nil ->
            receive_test_reply(test, options)
        end
    end
  end

  defp process_failure(
         %ExUnit.TestModule{state: {tag, _}, tests: tests} = test_module,
         %{pid: module_pid, parent_pid: parent_pid, config: config} = _options
       )
       when tag in [:failed, :invalid] do
    failure_counter = increment_failure_counter(config.manager, test_module, length(tests))

    if max_failures_exceeded?(failure_counter, config.max_failures) do
      terminate_all_running_processes(config.manager, parent_pid, module_pid)
      {:error, {:max_failures_exceeded, test_module}}
    else
      {:ok, test_module}
    end
  end

  defp process_failure(%ExUnit.TestModule{} = test_module, _options) do
    {:ok, test_module}
  end

  defp process_failure(
         %ExUnit.Test{state: {tag, _}} = test,
         %{pid: test_pid, parent_pid: parent_pid, config: config} = _options
       )
       when tag in [:failed, :invalid] do
    failure_counter = increment_failure_counter(config.manager, test)

    if max_failures_exceeded?(failure_counter, config.max_failures) do
      terminate_all_running_processes(config.manager, parent_pid, test_pid)
      {:error, {:max_failures_exceeded, test}}
    else
      {:ok, test}
    end
  end

  defp process_failure(%ExUnit.Test{} = test, _options) do
    {:ok, test}
  end

  # Stops all running tests in the suite
  defp terminate_all_running_processes(manager, parent_pid, current_pid) do
    for registered_pid when registered_pid not in [parent_pid, current_pid] <-
          ExUnit.OnExitHandler.get_registered_pids(manager) do
      if Process.alive?(registered_pid) do
        Process.exit(registered_pid, :max_failures_exceeded)
      end
    end
  end

  defp get_failure_counter(manager),
    do: ExUnit.OnExitHandler.get_failure_counter(manager)

  defp increment_failure_counter(manager, test, increment \\ 1)

  defp increment_failure_counter(manager, %ExUnit.Test{state: {tag, _}}, increment)
       when is_integer(increment) and increment >= 1 and tag in [:failed, :invalid],
       do: ExUnit.OnExitHandler.increment_failure_counter(manager, increment)

  defp increment_failure_counter(manager, %ExUnit.TestModule{state: {tag, _}}, increment)
       when is_integer(increment) and increment >= 1 and tag in [:failed, :invalid, :not_executed],
       do: ExUnit.OnExitHandler.increment_failure_counter(manager, increment)

  defp increment_failure_counter(manager, _, _increment),
    do: get_failure_counter(manager)

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
    if ExUnit.OnExitHandler.registered?(pid) do
      case ExUnit.OnExitHandler.run(pid, timeout) do
        :ok ->
          test_or_case

        {kind, reason, stack} ->
          state = test_or_case.state || failed(kind, reason, prune_stacktrace(stack))
          %{test_or_case | state: state}
      end
    else
      test_or_case
    end
  end

  ## Helpers

  defp max_failures_exceeded?(failure_counter, max_failures)
       when is_integer(failure_counter) and failure_counter >= 0 do
    is_integer(max_failures) and failure_counter > max_failures
  end

  defp max_failures_exceeded?(manager, max_failures) do
    is_integer(max_failures) and get_failure_counter(manager) > max_failures
  end

  defp get_timeout(tags, config) do
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
