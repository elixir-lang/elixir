defmodule ExUnit.Server do
  @moduledoc false

  @timeout 30_000
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ## Before run API

  def start_load() do
    GenServer.cast(__MODULE__, :start_load)
  end

  def add_async_case(name) do
    GenServer.cast(__MODULE__, {:add_async_case, name})
  end

  def add_sync_case(name) do
    GenServer.cast(__MODULE__, {:add_sync_case, name})
  end

  ## After run API

  def start_run() do
    GenServer.call(__MODULE__, :start_run, @timeout)
  end

  ## Capture Device API

  def capture_device(device, pid) do
    GenServer.call(__MODULE__, {:capture_device, device, pid})
  end

  def release_device(ref) do
    GenServer.call(__MODULE__, {:release_device, ref})
  end

  def log_capture_on(pid) do
    GenServer.call(__MODULE__, {:log_capture_on, pid})
  end

  def log_capture_off(ref) do
    GenServer.call(__MODULE__, {:log_capture_off, ref})
  end

  ## Callbacks

  def init(:ok) do
    {:ok, %{
      async_cases: %{},
      sync_cases: %{},
      start_load: :os.timestamp,
      captured_devices: {%{}, %{}},
      log_captures: %{},
      log_status: nil
    }}
  end

  def handle_call(:start_run, _from, config) do
    load_us =
      if start_load = config.start_load do
        :timer.now_diff(:os.timestamp, start_load)
      end

    {:reply,
      {Map.keys(config.async_cases), Map.keys(config.sync_cases), load_us},
      %{config | async_cases: %{}, sync_cases: %{}, start_load: nil}}
  end

  def handle_call({:capture_device, name, pid}, _from, config) do
    {names, refs} = config.captured_devices
    if Map.has_key?(names, name) do
      {:reply, {:error, :already_captured}, config}
    else
      orig_pid = Process.whereis(name)
      Process.unregister(name)
      Process.register(pid, name)
      ref = Process.monitor(pid)
      refs = Map.put(refs, ref, {name, orig_pid})
      names = Map.put(names, name, true)
      {:reply, {:ok, ref}, %{config | captured_devices: {names, refs}}}
    end
  end

  def handle_call({:release_device, ref}, _from, config) do
    config = release_device(ref, config)
    {:reply, :ok, config}
  end

  def handle_call({:log_capture_on, pid}, _from, config) do
    ref  = Process.monitor(pid)
    refs = Map.put(config.log_captures, ref, true)

    if map_size(refs) == 1 do
      status = Logger.remove_backend(:console)
      {:reply, ref, %{config | log_captures: refs, log_status: status}}
    else
      {:reply, ref, %{config | log_captures: refs}}
    end
  end

  def handle_call({:log_capture_off, ref}, _from, config) do
    Process.demonitor(ref, [:flush])
    config = remove_log_capture(ref, config)
    {:reply, :ok, config}
  end

  def handle_cast(:start_load, config) do
    {:noreply,
      %{config | start_load: :os.timestamp}}
  end

  def handle_cast({:add_async_case, name}, config) do
    {:noreply,
      %{config | async_cases: Map.put(config.async_cases, name, true)}}
  end

  def handle_cast({:add_sync_case, name}, config) do
    {:noreply,
      %{config | sync_cases: Map.put(config.sync_cases, name, true)}}
  end

  def handle_info({:DOWN, ref, _, _, _}, config) do
    config = remove_log_capture(ref, config)
    config = release_device(ref, config)
    {:noreply, config}
  end

  def handle_info(msg, state) do
    super(msg, state)
  end

  defp release_device(ref, %{captured_devices: {names, refs}} = config) do
    case Map.pop(refs, ref) do
      {{name, pid}, refs} ->
        names = Map.delete(names, name)
        Process.demonitor(ref, [:flush])
        try do
          try do
            Process.unregister(name)
          after
            Process.register(pid, name)
          end
        rescue
          ArgumentError -> nil
        end
        %{config | captured_devices: {names, refs}}
      {nil, _refs} -> config
    end
  end

  defp remove_log_capture(ref, %{log_captures: refs} = config) do
    if Map.has_key?(refs, ref) do
      refs = Map.delete(refs, ref)
      maybe_add_console(refs, config.log_status)
      %{config | log_captures: refs}
    else
      config
    end
  end

  defp maybe_add_console(refs, status) do
    if status == :ok and map_size(refs) == 0 do
      Logger.add_backend(:console, flush: true)
    end
  end
end
