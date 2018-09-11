defmodule ExUnit.OnExitHandler do
  @moduledoc false

  @name __MODULE__
  @ets_opts [:public, :named_table, read_concurrency: true, write_concurrency: true]

  # ETS column numbers
  @pid 1
  @supervisor 3
  @on_exit 4

  use Agent

  @spec start_link(keyword()) :: {:ok, pid}
  def start_link(_opts) do
    Agent.start_link(fn -> :ets.new(@name, @ets_opts) end, name: @name)
  end

  @spec register(pid, term) :: :ok
  def register(pid, manager) when is_pid(pid) do
    :ets.insert(@name, {pid, manager, nil, []})
  end

  @spec registered?(pid) :: boolean
  def registered?(pid) when is_pid(pid) do
    case :ets.lookup(@name, pid) do
      [] ->
        false

      _ ->
        true
    end
  end

  @spec get_registered_pids(term) :: list()
  def get_registered_pids(manager) do
    :ets.match(@name, {:"$#{@pid}", manager, :_, :_, :_})
    |> List.flatten()
  end

  @spec add(pid, term, (() -> term)) :: :ok | :error
  def add(pid, name_or_ref, callback) when is_pid(pid) and is_function(callback, 0) do
    try do
      :ets.lookup_element(@name, pid, @on_exit)
    rescue
      _ -> :error
    else
      entries ->
        entries = List.keystore(entries, name_or_ref, 0, {name_or_ref, callback})
        true = :ets.update_element(@name, pid, {@on_exit, entries})
        :ok
    end
  end

  @spec get_supervisor(pid) :: {:ok, pid | nil} | :error
  def get_supervisor(pid) when is_pid(pid) do
    try do
      {:ok, :ets.lookup_element(@name, pid, @supervisor)}
    rescue
      _ -> :error
    end
  end

  @spec put_supervisor(pid, pid) :: :ok | :error
  def put_supervisor(pid, sup) when is_pid(pid) and is_pid(sup) do
    case :ets.update_element(@name, pid, {@supervisor, sup}) do
      true -> :ok
      false -> :error
    end
  end

  @spec run(pid, timeout) :: :ok | {Exception.kind(), term, Exception.stacktrace()}
  def run(pid, timeout) when is_pid(pid) do
    [{^pid, _manager, sup, callbacks}] = :ets.take(@name, pid)
    error = terminate_supervisor(sup, timeout)
    exec_on_exit_callbacks(Enum.reverse(callbacks), timeout, error)
  end

  @spec start_failure_counter({pid, pid}) :: :ok
  def start_failure_counter(manager) do
    :ets.insert(@name, {{:failure_counter, manager}, 0})
  end

  @spec increment_failure_counter({pid, pid}, pos_integer) :: pos_integer()
  def increment_failure_counter(manager, increment \\ 1)
      when is_integer(increment) and increment >= 1 do
    :ets.update_counter(@name, {:failure_counter, manager}, increment)
  end

  @spec get_failure_counter({pid, pid}) :: non_neg_integer()
  def get_failure_counter(manager) do
    [{{:failure_counter, _manager}, counter}] = :ets.lookup(@name, {:failure_counter, manager})
    counter
  end

  defp terminate_supervisor(nil, _timeout), do: nil

  defp terminate_supervisor(sup, timeout) do
    ref = Process.monitor(sup)

    receive do
      {:DOWN, ^ref, _, _, _} -> nil
    after
      timeout ->
        {:error, ExUnit.TimeoutError.exception(timeout: timeout, type: "supervisor shutdown"), []}
    end
  end

  defp exec_on_exit_callbacks(callbacks, timeout, error) do
    {runner_pid, runner_monitor, error} =
      Enum.reduce(callbacks, {nil, nil, error}, &exec_on_exit_callback(&1, timeout, &2))

    if is_pid(runner_pid) and Process.alive?(runner_pid) do
      Process.exit(runner_pid, :shutdown)

      receive do
        {:DOWN, ^runner_monitor, :process, ^runner_pid, _error} -> :ok
      end
    end

    error || :ok
  end

  defp exec_on_exit_callback({_name_or_ref, callback}, timeout, runner) do
    {runner_pid, runner_monitor, error} = runner
    {runner_pid, runner_monitor} = ensure_alive_callback_runner(runner_pid, runner_monitor)
    send(runner_pid, {:run, self(), callback})
    receive_runner_reply(runner_pid, runner_monitor, error, timeout)
  end

  defp receive_runner_reply(runner_pid, runner_monitor, error, timeout) do
    receive do
      {^runner_pid, reason} ->
        {runner_pid, runner_monitor, error || reason}

      {:DOWN, ^runner_monitor, :process, ^runner_pid, reason} ->
        {nil, nil, error || {{:EXIT, runner_pid}, reason, []}}
    after
      timeout ->
        case Process.info(runner_pid, :current_stacktrace) do
          {:current_stacktrace, stacktrace} ->
            Process.exit(runner_pid, :kill)

            receive do
              {:DOWN, ^runner_monitor, :process, ^runner_pid, _} -> :ok
            end

            exception = ExUnit.TimeoutError.exception(timeout: timeout, type: "on_exit callback")
            {nil, nil, error || {:error, exception, stacktrace}}

          nil ->
            receive_runner_reply(runner_pid, runner_monitor, error, timeout)
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
      {kind, error, __STACKTRACE__}
  end
end
