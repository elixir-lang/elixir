defmodule ExUnit.Runner do
  @moduledoc false

  alias ExUnit.EventManager, as: EM
  @stop_timeout 30_000

  defrecord Config, max_cases: 4, taken_cases: 0, async_cases: [], seed: nil,
                    sync_cases: [], include: [], exclude: [], manager: []

  def run(async, sync, opts, load_us) do
    opts = normalize_opts(opts)

    { :ok, pid } = EM.start_link
    formatters = [ExUnit.RunnerStats|opts[:formatters]]
    Enum.each formatters, &(:ok = EM.add_handler(pid, &1, opts))

    config = Config[manager: pid].update(opts)

    { run_us, _ } =
      :timer.tc fn ->
        EM.suite_started(config.manager, opts)
        loop config.async_cases(shuffle(config, async)).sync_cases(shuffle(config, sync))
      end

    EM.suite_finished(config.manager, run_us, load_us)
    EM.call(config.manager, ExUnit.RunnerStats, :stop, @stop_timeout)
  end

  defp normalize_opts(opts) do
    opts =
      if opts[:trace] do
        Keyword.put_new(opts, :max_cases, 1)
      else
        Keyword.put(opts, :trace, false)
      end

    { include, exclude } = ExUnit.Filters.normalize(opts[:include], opts[:exclude])

    opts
    |> Keyword.put(:exclude, exclude)
    |> Keyword.put(:include, include)
    |> Keyword.put_new(:color, IO.ANSI.terminal?)
    |> Keyword.put_new(:max_cases, :erlang.system_info(:schedulers_online))
    |> Keyword.put_new(:seed, :erlang.now |> elem(2))
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
      { _pid, :case_finished, _test_case } ->
        loop config.update_taken_cases(&(&1-1))
    end
  end

  defp spawn_cases(config, cases) do
    pid = self()

    Enum.each cases, fn case_name ->
      spawn_link fn ->
        run_case(config, pid, case_name)
      end
    end

    loop config.update_taken_cases(&(&1+length(cases)))
  end

  defp run_case(config, pid, case_name) do
    ExUnit.TestCase[] = test_case = case_name.__ex_unit__(:case)
    EM.case_started(config.manager, test_case)

    # Prepare tests, selecting which ones should
    # run and which ones were skipped.
    tests = prepare_tests(config, test_case.tests)

    { test_case, failed } =
      if Enum.all?(tests, &(&1.state)) do
        { test_case.state(:passed), tests }
      else
        spawn_case(config, test_case, tests)
      end

    # Run the failed tests. We don't actually spawn those tests
    # but we do send the notifications to formatter and other
    # entities involved.
    Enum.each failed, &run_test(config, &1, [])
    EM.case_finished(config.manager, test_case)
    send pid, { self, :case_finished, test_case }
  end

  defp prepare_tests(config, tests) do
    tests   = shuffle(config, tests)
    include = config.include
    exclude = config.exclude

    for test <- tests do
      tags = Keyword.put(test.tags, :test, test.name)
      case ExUnit.Filters.eval(include, exclude, tags) do
        :ok             -> test.tags(tags)
        { :error, tag } -> test.state({ :skip, "due to #{tag} filter" })
      end
    end
  end

  defp spawn_case(config, test_case, tests) do
    self_pid = self

    { case_pid, case_ref } =
      Process.spawn_monitor(fn ->
        case exec_case_setup(test_case) do
          { :ok, { test_case, context } } ->
            Enum.each(tests, &run_test(config, &1, context))
            test_case = exec_case_teardown(test_case, context)
            send self_pid, { self, :case_finished, test_case, [] }

          { :error, test_case } ->
            failed_tests = Enum.map(tests, &(&1.state({ :invalid, test_case })))
            send self_pid, { self, :case_finished, test_case, failed_tests }
        end
      end)

    receive do
      { ^case_pid, :case_finished, test_case, tests } ->
        { test_case, tests }
      { :DOWN, ^case_ref, :process, ^case_pid, error } ->
        { test_case.state({ :failed, { :EXIT, error, [] } }), [] }
    end
  end

  defp exec_case_setup(ExUnit.TestCase[name: case_name] = test_case) do
    { :ok, context } = case_name.__ex_unit__(:setup_all, [case: case_name])
    { :ok, { test_case.state(:passed), context } }
  catch
    kind, error ->
      { :error, test_case.state({ :failed, { kind, Exception.normalize(kind, error), pruned_stacktrace } }) }
  end

  defp exec_case_teardown(ExUnit.TestCase[name: case_name] = test_case, context) do
    case_name.__ex_unit__(:teardown_all, context)
    test_case
  catch
    kind, error ->
      test_case.state { :failed, { kind, Exception.normalize(kind, error), pruned_stacktrace } }
  end

  defp run_test(config, test, context) do
    EM.test_started(config.manager, test)

    if nil?(test.state) do
      test = spawn_test(config, test, Keyword.merge(test.tags, context))
    end

    EM.test_finished(config.manager, test)
  end

  defp spawn_test(_config, test, context) do
    self_pid = self

    { test_pid, test_ref } =
      Process.spawn_monitor(fn ->
        { us, test } =
          :timer.tc(fn ->
            case exec_test_setup(test, context) do
              { :ok, { test, context } } ->
                if nil?(test.state) do
                  test = exec_test(test, context)
                end
                exec_test_teardown(test, context)
              { :error, test } ->
                test
            end
          end)

        send self_pid, { self, :test_finished, test.time(us) }
      end)

    receive do
      { ^test_pid, :test_finished, test } ->
        test
      { :DOWN, ^test_ref, :process, ^test_pid, error } ->
        test.state { :failed, { :EXIT, error, [] } }
    end
  end

  defp exec_test_setup(ExUnit.Test[] = test, context) do
    { :ok, context } = test.case.__ex_unit__(:setup, context)
    { :ok, { test, context } }
  catch
    kind2, error2 ->
      { :error, test.state({ :failed, { kind2, Exception.normalize(kind2, error2), pruned_stacktrace } }) }
  end

  defp exec_test(ExUnit.Test[] = test, context) do
    apply(test.case, test.name, [context])
    test.state(:passed)
  catch
    kind, error ->
      test.state { :failed, { kind, Exception.normalize(kind, error), pruned_stacktrace } }
  end

  defp exec_test_teardown(ExUnit.Test[] = test, context) do
    { :ok, _context } = test.case.__ex_unit__(:teardown, context)
    test
  catch
    kind, error ->
      if nil?(test.state) do
        test.state { :failed, { kind, Exception.normalize(kind, error), pruned_stacktrace } }
      else
        test
      end
  end

  ## Helpers

  defp shuffle(Config[seed: seed], list) do
    :random.seed(3172, 9814, seed)
    Enum.shuffle(list)
  end

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

  defp pruned_stacktrace, do: prune_stacktrace(System.stacktrace)

  # Assertions can pop-up in the middle of the stack
  defp prune_stacktrace([{ ExUnit.Assertions, _, _, _ }|t]), do: prune_stacktrace(t)

  # As soon as we see a Runner, it is time to ignore the stacktrace
  defp prune_stacktrace([{ ExUnit.Runner, _, _, _ }|_]), do: []

  # All other cases
  defp prune_stacktrace([h|t]), do: [h|prune_stacktrace(t)]
  defp prune_stacktrace([]), do: []
end

