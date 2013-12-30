defmodule ExUnit.Runner do
  @moduledoc false

  defrecord Config, formatter: ExUnit.CLIFormatter, formatter_id: nil,
                    max_cases: 4, taken_cases: 0, async_cases: [],
                    sync_cases: [], include: [], exclude: []

  def run(async, sync, opts, load_us) do
    opts   = normalize_opts(opts)
    config = Config[max_cases: :erlang.system_info(:schedulers_online)]
    config = combine_filters(config.update(opts))

    { run_us, config } =
      :timer.tc fn ->
        loop config.async_cases(async).sync_cases(sync).
               formatter_id(config.formatter.suite_started(opts))
      end

    config.formatter.suite_finished(config.formatter_id, run_us, load_us)
  end

  defp normalize_opts(opts) do
    if opts[:trace] do
      Keyword.put_new(opts, :max_cases, 1)
    else
      Keyword.put(opts, :trace, false)
    end
  end

  defp combine_filters(config) do
    config.update_include(&group_by_key/1).update_exclude(&group_by_key/1)
  end

  defp group_by_key(dict) do
    Enum.reduce dict, [], fn { key, value }, acc ->
      Dict.update acc, key, [value], &[value|&1]
    end
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
    Enum.each cases, &spawn_case(config, &1)
    loop config.update_taken_cases(&(&1+length(cases)))
  end

  defp spawn_case(config, test_case) do
    pid = self()
    spawn_link fn ->
      run_test_case(config, pid, test_case)
    end
  end

  defp run_test_case(config, pid, case_name) do
    ExUnit.TestCase[] = test_case = case_name.__ex_unit__(:case)
    config.formatter.case_started(config.formatter_id, test_case)

    self_pid = self
    { case_pid, case_ref } = Process.spawn_monitor fn ->
      { test_case, context } = try do
        { :ok, context } = case_name.__ex_unit__(:setup_all, [case: case_name])
        { test_case.state(:passed), context }
      catch
        kind, error ->
          { test_case.state({ :failed, { kind, Exception.normalize(kind, error), pruned_stacktrace } }), nil }
      end

      tests = test_case.tests

      case test_case.state do
        :passed ->
          Enum.each tests, &run_test(config, &1, context)

          test_case = try do
            case_name.__ex_unit__(:teardown_all, context)
            test_case
          catch
            kind, error ->
              test_case.state { :failed, { kind, Exception.normalize(kind, error), pruned_stacktrace } }
          end

          self_pid <- { self, :case_finished, test_case, [] }
        { :failed, _ } ->
          tests = Enum.map tests, fn test -> test.state({ :invalid, test_case }) end
          self_pid <- { self, :case_finished, test_case, tests }
      end
    end

    receive do
      { ^case_pid, :case_finished, test_case, tests } ->
        Enum.map tests, &config.formatter.test_finished(config.formatter_id, &1)
        config.formatter.case_finished(config.formatter_id, test_case)
        pid <- { case_pid, :case_finished, test_case }
      { :DOWN, ^case_ref, :process, ^case_pid, { error, stacktrace } } ->
        test_case = test_case.state { :failed, { :EXIT, error, prune_stacktrace(stacktrace) } }
        config.formatter.case_finished(config.formatter_id, test_case)
        pid <- { case_pid, :case_finished, test_case }
    end
  end

  defp run_test(config, test, context) do
    config.formatter.test_started(config.formatter_id, test)

    test =
      case ExUnit.Filters.eval(config.include, config.exclude, test.tags) do
        :ok ->
          spawn_test(config, test, context)
        { :error, tag } ->
          skip_test(test, tag)
      end

    config.formatter.test_finished(config.formatter_id, test)
  end

  defp spawn_test(_config, test, context) do
    case_name = test.case

    # Run test in a new process so that we can trap exits for a single test
    self_pid = self
    { test_pid, test_ref } = Process.spawn_monitor(fn ->

      { us, test } = :timer.tc(fn ->
        { test, context } = exec_setup(test, context)
        if nil?(test.state) do
          test = exec_test(test, context)
        end
        exec_teardown(test, context)
      end)

      self_pid <- { self, :test_finished, test.time(us) }
    end)

    receive do
      { ^test_pid, :test_finished, test } ->
        test
      { :DOWN, ^test_ref, :process, ^test_pid, { error, stacktrace } } ->
        test.state { :failed, { :EXIT, error, prune_stacktrace(stacktrace) } }
    end
  end

  defp skip_test(test, mismatch) do
    test.state { :skip, "due to #{mismatch} filter" }
  end

  defp exec_setup(ExUnit.Test[] = test, context) do
    context = context |> Keyword.merge(test.tags) |> Keyword.put(:test, test.name)
    { :ok, context } = test.case.__ex_unit__(:setup, context)
    { test, context }
  catch
    kind2, error2 ->
      { test.state({ :failed, { kind2, Exception.normalize(kind2, error2), pruned_stacktrace } }), context }
  end

  defp exec_test(ExUnit.Test[] = test, context) do
    apply(test.case, test.name, [context])
    test.state(:passed)
  catch
    kind, error ->
      test.state { :failed, { kind, Exception.normalize(kind, error), pruned_stacktrace } }
  end

  defp exec_teardown(ExUnit.Test[] = test, context) do
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
