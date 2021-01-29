defmodule ExUnit.Runner do
  @moduledoc false

  alias ExUnit.EventManager, as: EM

  @rand_algorithm :exs1024
  @current_key __MODULE__

  def run(opts, load_us) when (is_integer(load_us) or is_nil(load_us)) and is_list(opts) do
    runner = self()
    id = {__MODULE__, runner}

    try do
      # It may fail on Windows, so we ignore the result.
      _ =
        System.trap_signal(:sigquit, id, fn ->
          ref = Process.monitor(runner)
          send(runner, {ref, self(), :sigquit})

          receive do
            ^ref -> :ok
            {:DOWN, ^ref, _, _, _} -> :ok
          after
            5_000 -> :ok
          end

          Process.demonitor(ref, [:flush])
          :ok
        end)

      run_with_trap(opts, load_us)
    after
      System.untrap_signal(:sigquit, id)
    end
  end

  defp run_with_trap(opts, load_us) do
    opts = normalize_opts(opts)
    {:ok, manager} = EM.start_link()
    {:ok, stats_pid} = EM.add_handler(manager, ExUnit.RunnerStats, opts)
    config = configure(opts, manager, self(), stats_pid)
    :erlang.system_flag(:backtrace_depth, Keyword.fetch!(opts, :stacktrace_depth))

    start_time = System.monotonic_time()
    EM.suite_started(config.manager, opts)
    async_stop_time = loop(config, :async, %{}, false)
    stop_time = System.monotonic_time()

    if max_failures_reached?(config) do
      EM.max_failures_reached(config.manager)
    end

    async_us =
      async_stop_time &&
        System.convert_time_unit(async_stop_time - start_time, :native, :microsecond)

    run_us = System.convert_time_unit(stop_time - start_time, :native, :microsecond)
    times_us = %{async: async_us, load: load_us, run: run_us}
    EM.suite_finished(config.manager, times_us)

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
      max_failures: opts[:max_failures],
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

  defp loop(config, :async, running, async_once?) do
    available = config.max_cases - map_size(running)

    cond do
      # No modules available, wait for one
      available <= 0 ->
        wait_until_available(config, :async, running, async_once?)

      # Slots are available, start with async modules
      modules = ExUnit.Server.take_async_modules(available) ->
        spawn_modules(config, modules, :async, running, true)

      true ->
        modules = ExUnit.Server.take_sync_modules()
        loop(config, modules, running, async_once?)
    end
  end

  defp loop(config, modules, running, async_stop_time) do
    case modules do
      _ when running != %{} ->
        wait_until_available(config, modules, running, async_stop_time)

      # So we can start all sync modules
      [head | tail] ->
        spawn_modules(config, [head], tail, running, async_stop_time(async_stop_time))

      # No more modules, we are done!
      [] ->
        async_stop_time(async_stop_time)
    end
  end

  defp async_stop_time(false = _async_once?), do: nil
  defp async_stop_time(true = _async_once?), do: System.monotonic_time()
  defp async_stop_time(async_stop_time), do: async_stop_time

  # Loop expecting down messages from the spawned modules.
  #
  # We first look at the sigquit signal because we don't want
  # to spawn new test cases when we know we will have to handle
  # sigquit next.
  #
  # Otherwise, whenever a module has finished executing, update
  # the runnig modules and attempt to spawn new ones.
  defp wait_until_available(config, modules, running, async_timing) do
    receive do
      {ref, pid, :sigquit} ->
        sigquit(config, ref, pid, running)
    after
      0 ->
        receive do
          {ref, pid, :sigquit} ->
            sigquit(config, ref, pid, running)

          {:DOWN, ref, _, _, _} when is_map_key(running, ref) ->
            loop(config, modules, Map.delete(running, ref), async_timing)
        end
    end
  end

  defp spawn_modules(config, [], modules_remaining, running, async_timing) do
    loop(config, modules_remaining, running, async_timing)
  end

  defp spawn_modules(config, [module | modules], modules_remaining, running, async_timing) do
    if max_failures_reached?(config) do
      loop(config, modules_remaining, running, async_timing)
    else
      {pid, ref} = spawn_monitor(fn -> run_module(config, module) end)
      spawn_modules(config, modules, modules_remaining, Map.put(running, ref, pid), async_timing)
    end
  end

  ## stacktrace

  # Assertions can pop-up in the middle of the stack
  def prune_stacktrace([{ExUnit.Assertions, _, _, _} | t]), do: prune_stacktrace(t)

  # As soon as we see a Runner, it is time to ignore the stacktrace
  def prune_stacktrace([{ExUnit.Runner, _, _, _} | _]), do: []

  # All other cases
  def prune_stacktrace([h | t]), do: [h | prune_stacktrace(t)]
  def prune_stacktrace([]), do: []

  ## sigquit

  defp sigquit(config, ref, pid, running) do
    # Stop all child processes from running and get their current state.
    # We need to stop these processes because they may invoke the event
    # manager and we must stop the event manager to guarantee the sigquit
    # data has been flushed.
    current =
      Enum.map(running, fn {ref, pid} ->
        current = safe_pdict_current(pid)
        Process.exit(pid, :shutdown)

        receive do
          {:DOWN, ^ref, _, _, _} -> current
        end
      end)

    EM.sigquit(config.manager, Enum.reject(current, &is_nil/1))
    EM.stop(config.manager)

    # Reply to the event manager and wait until it shuts down the VM.
    send(pid, ref)
    Process.sleep(:infinity)
  end

  defp safe_pdict_current(pid) do
    with {:dictionary, dictionary} <- Process.info(pid, :dictionary),
         {@current_key, current} <- List.keyfind(dictionary, @current_key, 0),
         do: current
  rescue
    _ -> nil
  end

  ## Running modules

  defp run_module(config, module) do
    test_module = module.__ex_unit__()
    EM.module_started(config.manager, test_module)

    # Prepare tests, selecting which ones should be run or skipped
    tests = prepare_tests(config, test_module.tests)
    {excluded_and_skipped_tests, to_run_tests} = Enum.split_with(tests, & &1.state)

    for excluded_or_skipped_test <- excluded_and_skipped_tests do
      EM.test_started(config.manager, excluded_or_skipped_test)
      EM.test_finished(config.manager, excluded_or_skipped_test)
    end

    {test_module, invalid_tests, finished_tests} = run_module(config, test_module, to_run_tests)

    pending_tests =
      case process_max_failures(config, test_module) do
        :no ->
          invalid_tests

        {:reached, n} ->
          Enum.take(invalid_tests, n)

        :surpassed ->
          nil
      end

    # If pending_tests is [], EM.module_finished is still called.
    # Only if process_max_failures/2 returns :surpassed it is not.
    if pending_tests do
      for pending_test <- pending_tests do
        EM.test_started(config.manager, pending_test)
        EM.test_finished(config.manager, pending_test)
      end

      test_module = %{test_module | tests: Enum.reverse(finished_tests, pending_tests)}
      EM.module_finished(config.manager, test_module)
    end
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

  defp include_test?(test_ids, test) do
    test_ids == nil or MapSet.member?(test_ids, {test.module, test.name})
  end

  defp run_module(_config, test_module, []) do
    {test_module, [], []}
  end

  defp run_module(config, test_module, tests) do
    {module_pid, module_ref} = run_setup_all(test_module, self())

    {test_module, invalid_tests, finished_tests} =
      receive do
        {^module_pid, :setup_all, {:ok, context}} ->
          finished_tests =
            if max_failures_reached?(config), do: [], else: run_tests(config, tests, context)

          :ok = exit_setup_all(module_pid, module_ref)
          {test_module, [], finished_tests}

        {^module_pid, :setup_all, {:error, test_module}} ->
          invalid_tests = Enum.map(tests, &%{&1 | state: {:invalid, test_module}})
          :ok = exit_setup_all(module_pid, module_ref)
          {test_module, invalid_tests, []}

        {:DOWN, ^module_ref, :process, ^module_pid, error} ->
          test_module = %{test_module | state: failed({:EXIT, module_pid}, error, [])}
          {test_module, [], []}
      end

    timeout = get_timeout(config, %{})
    {exec_on_exit(test_module, module_pid, timeout), invalid_tests, finished_tests}
  end

  defp run_setup_all(%ExUnit.TestModule{name: module} = test_module, parent_pid) do
    Process.put(@current_key, test_module)

    spawn_monitor(fn ->
      ExUnit.OnExitHandler.register(self())

      result =
        try do
          {:ok, module.__ex_unit__(:setup_all, %{module: module, case: module})}
        catch
          kind, error ->
            failed = failed(kind, error, prune_stacktrace(__STACKTRACE__))
            {:error, %{test_module | state: failed}}
        end

      send(parent_pid, {self(), :setup_all, result})

      # We keep the process alive so all of its resources
      # stay alive until we run all tests in this case.
      ref = Process.monitor(parent_pid)

      receive do
        {^parent_pid, :exit} -> :ok
        {:DOWN, ^ref, _, _, _} -> :ok
      end
    end)
  end

  defp exit_setup_all(pid, ref) do
    send(pid, {self(), :exit})

    receive do
      {:DOWN, ^ref, _, _, _} -> :ok
    end
  end

  defp run_tests(config, tests, context) do
    Enum.reduce_while(tests, [], fn test, acc ->
      Process.put(@current_key, test)

      case run_test(config, test, context) do
        {:ok, test} -> {:cont, [test | acc]}
        :max_failures_reached -> {:halt, acc}
      end
    end)
  end

  defp run_test(config, %{tags: tags} = test, context) do
    EM.test_started(config.manager, test)

    context = maybe_create_tmp_dir(test, context, tags)
    capture_log = Map.get(tags, :capture_log, config.capture_log)
    test = run_test_with_capture_log(capture_log, config, test, Map.merge(tags, context))

    case process_max_failures(config, test) do
      :no ->
        EM.test_finished(config.manager, test)
        {:ok, test}

      {:reached, 1} ->
        EM.test_finished(config.manager, test)
        :max_failures_reached

      :surpassed ->
        :max_failures_reached
    end
  end

  defp maybe_create_tmp_dir(test, context, %{tmp_dir: true}) do
    create_tmp_dir!(test, "", context)
  end

  defp maybe_create_tmp_dir(test, context, %{tmp_dir: path}) when is_binary(path) do
    create_tmp_dir!(test, path, context)
  end

  defp maybe_create_tmp_dir(_, context, %{tmp_dir: false}) do
    context
  end

  defp maybe_create_tmp_dir(_, _, %{tmp_dir: other}) do
    raise ArgumentError, "expected :tmp_dir to be a boolean or a string, got: #{inspect(other)}"
  end

  defp maybe_create_tmp_dir(_, context, _) do
    context
  end

  defp create_tmp_dir!(test, extra_path, context) do
    module = escape_path(inspect(test.module))
    name = escape_path(to_string(test.name))
    path = Path.join(["tmp", module, name, extra_path])
    File.rm_rf!(path)
    File.mkdir_p!(path)
    Map.put(context, :tmp_dir, path)
  end

  @escape Enum.map(' [~#%&*{}\\:<>?/+|"]', &<<&1::utf8>>)

  defp escape_path(path) do
    String.replace(path, @escape, "-")
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

  defp spawn_test(config, test, context) do
    parent_pid = self()
    timeout = get_timeout(config, test.tags)
    {test_pid, test_ref} = spawn_test_monitor(config, test, parent_pid, context)
    test = receive_test_reply(test, test_pid, test_ref, timeout)
    exec_on_exit(test, test_pid, timeout)
  end

  defp spawn_test_monitor(config, test, parent_pid, context) do
    spawn_monitor(fn ->
      ExUnit.OnExitHandler.register(self())
      generate_test_seed(config, test)

      {time, test} =
        :timer.tc(fn ->
          case exec_test_setup(test, context) do
            {:ok, test} -> exec_test(test)
            {:error, test} -> test
          end
        end)

      send(parent_pid, {self(), :test_finished, %{test | time: time}})
      exit(:shutdown)
    end)
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

  defp process_max_failures(%{max_failures: :infinity}, _), do: :no

  defp process_max_failures(config, %ExUnit.TestModule{state: {:failed, _}, tests: tests}) do
    process_max_failures(config.stats_pid, config.max_failures, length(tests))
  end

  defp process_max_failures(config, %ExUnit.Test{state: {:failed, _}}) do
    process_max_failures(config.stats_pid, config.max_failures, 1)
  end

  defp process_max_failures(config, _test_module_or_test) do
    if max_failures_reached?(config), do: :surpassed, else: :no
  end

  defp process_max_failures(stats_pid, max_failures, bump) do
    previous = ExUnit.RunnerStats.increment_failure_counter(stats_pid, bump)

    cond do
      previous >= max_failures -> :surpassed
      previous + bump < max_failures -> :no
      true -> {:reached, max_failures - previous}
    end
  end

  defp max_failures_reached?(%{stats_pid: stats_pid, max_failures: max_failures}) do
    max_failures != :infinity and
      ExUnit.RunnerStats.get_failure_counter(stats_pid) >= max_failures
  end

  defp get_timeout(config, tags) do
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
end
