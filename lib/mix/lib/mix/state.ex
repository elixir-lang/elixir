defmodule Mix.State do
  @moduledoc false
  @name __MODULE__
  @timeout :infinity

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def lock(key, fun) do
    try do
      GenServer.call(@name, {:lock, key}, @timeout)
      fun.()
    after
      GenServer.call(@name, {:unlock, key}, @timeout)
    end
  end

  ## ETS state storage (mutable, not cleared ion tests)

  def fetch(key) do
    case :ets.lookup(@name, key) do
      [{^key, value}] -> {:ok, value}
      [] -> :error
    end
  end

  def get(key, default \\ nil) do
    case :ets.lookup(@name, key) do
      [{^key, value}] -> value
      [] -> default
    end
  end

  def put(key, value) do
    :ets.insert(@name, {key, value})
    :ok
  end

  def update(key, fun) do
    :ets.insert(@name, {key, fun.(:ets.lookup_element(@name, key, 2))})
    :ok
  end

  ## Persistent term cache (persistent, cleared in tests)

  def read_cache(key) do
    :persistent_term.get({__MODULE__, key}, nil)
  end

  def write_cache(key, value) do
    :persistent_term.put({__MODULE__, key}, value)
    value
  end

  def delete_cache(key) do
    :persistent_term.erase({__MODULE__, key})
  end

  def clear_cache do
    for {{__MODULE__, _} = key, _value} <- :persistent_term.get() do
      :persistent_term.erase(key)
    end
  end

  ## Callbacks

  @impl true
  def init(:ok) do
    table = :ets.new(@name, [:public, :set, :named_table, read_concurrency: true])

    :ets.insert(table,
      shell: Mix.Shell.IO,
      env: from_env("MIX_ENV", :dev),
      target: from_env("MIX_TARGET", :host),
      scm: [Mix.SCM.Git, Mix.SCM.Path]
    )

    {:ok, {%{}, %{}}}
  end

  defp from_env(varname, default) do
    case System.get_env(varname) do
      nil -> default
      "" -> default
      value -> String.to_atom(value)
    end
  end

  @impl true
  def handle_call({:lock, key}, {pid, _} = from, {key_to_waiting, pid_to_key}) do
    key_to_waiting =
      case key_to_waiting do
        %{^key => {locked, waiting}} ->
          Map.put(key_to_waiting, key, {locked, :queue.in(from, waiting)})

        %{} ->
          go!(from)
          Map.put(key_to_waiting, key, {pid, :queue.new()})
      end

    ref = Process.monitor(pid)
    {:noreply, {key_to_waiting, Map.put(pid_to_key, pid, {key, ref})}}
  end

  @impl true
  def handle_call({:unlock, key}, {pid, _}, {key_to_waiting, pid_to_key}) do
    {{^key, ref}, pid_to_key} = Map.pop(pid_to_key, pid)
    Process.demonitor(ref, [:flush])
    {:reply, :ok, {unlock(key_to_waiting, pid_to_key, key), pid_to_key}}
  end

  @impl true
  def handle_info({:DOWN, ref, _type, pid, _reason}, {key_to_waiting, pid_to_key}) do
    {{key, ^ref}, pid_to_key} = Map.pop(pid_to_key, pid)

    key_to_waiting =
      case key_to_waiting do
        %{^key => {^pid, _}} ->
          unlock(key_to_waiting, pid_to_key, key)

        %{^key => {locked, waiting}} ->
          Map.put(key_to_waiting, key, {locked, List.keydelete(waiting, pid, 0)})
      end

    {:noreply, {key_to_waiting, pid_to_key}}
  end

  defp unlock(key_to_waiting, pid_to_key, key) do
    %{^key => {_locked, waiting}} = key_to_waiting

    case :queue.out(waiting) do
      {{:value, {pid, _} = from}, waiting} ->
        # Assert that we still know this PID
        _ = Map.fetch!(pid_to_key, pid)
        go!(from)
        Map.put(key_to_waiting, key, {pid, waiting})

      {:empty, _waiting} ->
        Map.delete(key_to_waiting, key)
    end
  end

  defp go!(from), do: GenServer.reply(from, :ok)
end
