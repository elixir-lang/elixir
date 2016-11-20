defmodule ExUnit.OnExitHandler do
  @moduledoc false
  @name __MODULE__

  def start_link do
    Agent.start_link(fn -> %{} end, name: @name)
  end

  @spec register(pid) :: :ok
  def register(pid) when is_pid(pid) do
    Agent.update(@name, &Map.put(&1, pid, []))
  end

  @spec add(pid, term, (()-> term)) :: :ok | :error
  def add(pid, name_or_ref, callback) when is_pid(pid) and is_function(callback, 0) do
    Agent.get_and_update(@name, fn map ->
      if entries = Map.get(map, pid) do
        entries = List.keystore(entries, name_or_ref, 0, {name_or_ref, callback})
        {:ok, Map.put(map, pid, entries)}
      else
        {:error, map}
      end
    end)
  end

  @spec run(pid, timeout) :: :ok | {Exception.kind, term, Exception.stacktrace}
  def run(pid, timeout) when is_pid(pid) do
    callbacks = Agent.get_and_update(@name, &Map.pop(&1, pid, []))
    exec_on_exit_callbacks(Enum.reverse(callbacks), timeout)
  end

  defp exec_on_exit_callbacks(callbacks, timeout) do
    {runner_pid, runner_monitor, state} =
      Enum.reduce(callbacks, {nil, nil, nil}, &exec_on_exit_callback(&1, timeout, &2))

    if is_pid(runner_pid) and Process.alive?(runner_pid) do
      send(runner_pid, :shutdown)
      receive do
        {:DOWN, ^runner_monitor, :process, ^runner_pid, _error} -> :ok
      end
    end

    state || :ok
  end

  defp exec_on_exit_callback({_name_or_ref, callback}, timeout, {runner_pid, runner_monitor, state}) do
    {runner_pid, runner_monitor} = ensure_alive_callback_runner(runner_pid, runner_monitor)
    send(runner_pid, {:run, self(), callback})
    receive_runner_reply(runner_pid, runner_monitor, state, timeout)
  end

  defp receive_runner_reply(runner_pid, runner_monitor, state, timeout) do
    receive do
      {^runner_pid, nil} ->
        {runner_pid, runner_monitor, state}
      {^runner_pid, error} ->
        {runner_pid, runner_monitor, state || error}
      {:DOWN, ^runner_monitor, :process, ^runner_pid, error} ->
        {nil, nil, state || {{:EXIT, runner_pid}, error, []}}
    after
      timeout ->
        case Process.info(runner_pid, :current_stacktrace) do
          {:current_stacktrace, stacktrace} ->
            Process.exit(runner_pid, :kill)
            receive do
              {:DOWN, ^runner_monitor, :process, ^runner_pid, _} -> :ok
            end
            exception = ExUnit.TimeoutError.exception(timeout: timeout, type: :on_exit)
            {nil, nil, state || {:error, exception, stacktrace}}
          nil ->
            receive_runner_reply(runner_pid, runner_monitor, state, timeout)
        end
    end
  end

  ## Runner

  @doc false
  def on_exit_runner_loop do
    receive do
      {:run, from, fun} ->
        send(from, {self(), exec_callback(fun)})
        on_exit_runner_loop()
      :shutdown ->
        :ok
    end
  end

  defp ensure_alive_callback_runner(nil, nil) do
    spawn_monitor(__MODULE__, :on_exit_runner_loop, [])
  end

  defp ensure_alive_callback_runner(runner_pid, runner_monitor) do
    {runner_pid, runner_monitor}
  end

  defp exec_callback(callback) do
    callback.()
    nil
  catch
    kind, error ->
      {kind, error, System.stacktrace}
  end
end
