defmodule ExUnit.ProcessAssertions do
  @moduledoc """
  Assertions for testing processes.
  """

  defmacro assert_processes_started(launch_fun, lookup_list) do
    quote do
      check_processes(unquote(launch_fun), unquote(lookup_list), nil, :pid)
    end
  end

  defmacro refute_processes_started(launch_fun, lookup_list) do
    quote do
      check_processes(unquote(launch_fun), unquote(lookup_list), nil, nil)
    end
  end

  defmacro assert_processes_stopped(launch_fun, lookup_list) do
    quote do
      check_processes_stopped(unquote(launch_fun), unquote(lookup_list))
    end
  end

  defmacro refute_processes_stopped(launch_fun, lookup_list) do
    quote do
      check_processes_survived(unquote(launch_fun), unquote(lookup_list))
    end
  end

  defmacro assert_processes_survived(launch_fun, lookup_list) do
    quote do
      check_processes_survived(unquote(launch_fun), unquote(lookup_list))
    end
  end

  defmacro refute_processes_survived(launch_fun, lookup_list) do
    quote do
      check_processes_stopped(unquote(launch_fun), unquote(lookup_list))
    end
  end

  @timeout 1500
  @task_timeout 1000

  def check_processes(launch_fun, lookup_list, check_before, check_after) do
    before_check = check_lookup_list(lookup_list, check_before, :before)

    result_of_launch_fun = launch_fun.()

    after_check = check_lookup_list(lookup_list, check_after, :after)

    (List.wrap(before_check) ++ List.wrap(after_check))
    |> collect_errors()
    |> result_interpretation(result_of_launch_fun)
  end

  def check_processes_stopped(launch_fun, lookup_list) do
    before_check = check_lookup_list(lookup_list, :pid, :before)

    before_errors = collect_errors(before_check)

    if Enum.empty?(before_errors) do
      waiting_to_stop =
        before_check
        |> Enum.map(fn {:ok, name, pid} -> {name, pid} end)
        |> Task.async_stream(
          __MODULE__,
          :await_stop,
          [:after],
          timeout: @timeout
        )

      result_of_launch_fun = launch_fun.()

      after_check =
        waiting_to_stop
        |> Enum.to_list()
        |> Enum.map(fn {:ok, result} -> result end)

      (List.wrap(before_check) ++ List.wrap(after_check))
      |> collect_errors()
      |> result_interpretation(result_of_launch_fun)
    else
      result_interpretation(before_errors, nil)
    end
  end

  def await_stop({name, pid}, step) do
    Process.monitor(pid)

    receive do
      {_, ref, :process, _object, reason} ->
        Process.demonitor(ref)
        {:ok, name, pid, ref, reason}
    after
      @task_timeout ->
        {:error, "[#{step}] Process :#{stringify(name)} expected to be stopped but still alive"}
    end
  end

  def check_processes_survived(launch_fun, lookup_list) do
    before_found = check_lookup_list(lookup_list, :pid, :before)

    result_of_launch_fun = launch_fun.()

    after_found = check_lookup_list(lookup_list, :pid, :after)

    collected_errors = collect_errors(List.wrap(before_found) ++ List.wrap(after_found))

    if Enum.empty?(collected_errors) do
      equality_errors =
        before_found
        |> Enum.zip(after_found)
        |> Enum.map(fn {before_checked, after_checked} ->
          ensure_equal(before_checked, after_checked)
        end)
        |> collect_errors()

      result_interpretation(
        List.wrap(collected_errors) ++ List.wrap(equality_errors),
        result_of_launch_fun
      )
    else
      result_interpretation(collected_errors, nil)
    end
  end

  defp check_lookup_list(lookup_list, expected, step) do
    names = check_list(lookup_list, expected, step, :names, :waiting_process)

    registry = check_list(lookup_list, expected, step, :registry, :waiting_registry)

    (List.wrap(names) ++ List.wrap(registry))
    |> List.flatten()
  end

  defp check_list(lookup_list, expected, step, key, method) do
    lookup_list
    |> Keyword.get(key, [])
    |> Task.async_stream(__MODULE__, method, [expected, step])
    |> Enum.to_list()
    |> Enum.map(fn {:ok, result} -> result end)
  end

  defp ensure_equal({:ok, name, pid}, {:ok, name, pid}) when is_pid(pid), do: {:ok, name}

  defp ensure_equal({:ok, name, b_pid}, {:ok, name, a_pid})
       when is_pid(b_pid) and is_pid(a_pid) do
    {:error, "Expected that #{stringify(name)} have the same pids, 
          but before calling launch function was #{b_pid} and after #{a_pid}"}
  end

  defp collect_errors(results_list) do
    results_list
    |> Enum.filter(&match?({:error, _}, &1))
    |> Enum.map(fn {:error, value} -> value end)
  end

  def waiting_process(
        process_name,
        expected,
        step,
        start_time \\ :os.system_time(:millisecond)
      )
      when is_atom(process_name) do
    timeout_reached = :os.system_time(:millisecond) - start_time > @timeout

    case {Process.whereis(process_name), expected, timeout_reached} do
      {pid, :pid, _} when is_pid(pid) ->
        {:ok, process_name, pid}

      {nil, nil, _} ->
        {:ok, process_name, nil}

      {_, :pid, true} ->
        {:error, error_not_found(process_name, step)}

      {_, nil, true} ->
        {:error, error_found(process_name, step)}

      {_, _, _} ->
        :timer.sleep(10)
        waiting_process(process_name, expected, step, start_time)
    end
  end

  def waiting_registry({name, ids} = registry_pair, expected, step)
      when is_tuple(registry_pair) and is_list(ids) do
    ids
    |> Enum.map(fn id -> {name, id} end)
    |> Task.async_stream(__MODULE__, :waiting_registry, [expected, step])
    |> Enum.to_list()
    |> Enum.map(fn {:ok, result} -> result end)
  end

  def waiting_registry(
        {name, id} = registry_pair,
        expected,
        step,
        start_time \\ :os.system_time(:millisecond)
      )
      when is_tuple(registry_pair) do
    timeout_reached = :os.system_time(:millisecond) - start_time > @timeout

    case {Registry.lookup(name, id), expected, timeout_reached} do
      {[{pid, nil}], :pid, _} when is_pid(pid) ->
        {:ok, registry_pair, pid}

      {[], nil, _} ->
        {:ok, registry_pair, nil}

      {_, :pid, true} ->
        {:error, error_not_found(name, id, step)}

      {_, nil, true} ->
        {:error, error_found(name, id, step)}

      {_, _, _} ->
        :timer.sleep(500)
        waiting_registry(registry_pair, expected, step, start_time)
    end
  end

  defp result_interpretation(errors_list, result) do
    case errors_list do
      [] ->
        result

      errors_list ->
        raise_errors(errors_list)
    end
  end

  defp error_found(process_name, step) do
    "[#{step}] Process :#{process_name} expected to not be started"
  end

  defp error_found(name, id, step) do
    "[#{step}] Process :#{name} with id #{id} expected to not be started"
  end

  defp error_not_found(process_name, step) do
    "[#{step}] Process :#{process_name} expected to be started, but not found"
  end

  defp error_not_found(name, id, step) do
    "[#{step}] Process :#{name} with id #{id} expected to be started, but not found"
  end

  defp raise_errors(errors_list) do
    errors = Enum.join(errors_list, ", \n")
    raise "Some processes in incorrect state:\n #{errors}"
  end

  defp stringify([]), do: 'none'
  defp stringify({atom, id}), do: "#{atom}: #{id}"
  defp stringify(atom), do: "#{atom}"
end
