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

  def add_device(device) do
    GenServer.call(__MODULE__, {:add_device, device})
  end

  def remove_device(device) do
    GenServer.call(__MODULE__, {:remove_device, device})
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
      async_cases: HashSet.new,
      sync_cases: HashSet.new,
      start_load: :os.timestamp,
      captured_devices: HashSet.new,
      log_captures: HashSet.new,
      log_status: nil
    }}
  end

  def handle_call(:start_run, _from, config) do
    load_us =
      if start_load = config.start_load do
        :timer.now_diff(:os.timestamp, start_load)
      end

    {:reply,
      {config.async_cases, config.sync_cases, load_us},
      %{config | async_cases: HashSet.new, sync_cases: HashSet.new, start_load: nil}}
  end

  def handle_call({:add_device, device}, _from, config) do
    {:reply,
      not(device in config.captured_devices),
      %{config | captured_devices: Set.put(config.captured_devices, device)}}
  end

  def handle_call({:remove_device, device}, _from, config) do
    {:reply, :ok,
      %{config | captured_devices: Set.delete(config.captured_devices, device)}}
  end

  def handle_call({:log_capture_on, pid}, _from, config) do
    ref  = Process.monitor(pid)
    refs = HashSet.put(config.log_captures, ref)

    if HashSet.size(refs) == 1 do
      status = Logger.remove_backend(:console)
      {:reply, {:ok, ref}, %{config | log_captures: refs, log_status: status}}
    else
      {:reply, {:ok, ref}, %{config | log_captures: refs}}
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
      %{config | async_cases: Set.put(config.async_cases, name)}}
  end

  def handle_cast({:add_sync_case, name}, config) do
    {:noreply,
      %{config | sync_cases: Set.put(config.sync_cases, name)}}
  end

  def handle_info({:DOWN, ref, _, _, _}, config) do
    config = remove_log_capture(ref, config)
    {:noreply, config}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp remove_log_capture(ref, %{log_captures: refs} = config) do
    if ref in refs do
      refs = HashSet.delete(refs, ref)
      maybe_add_console(refs, config.log_status)
      %{config | log_captures: refs}
    else
      config
    end
  end

  defp maybe_add_console(refs, status) do
    if status == :ok and HashSet.size(refs) == 0 do
      Logger.add_backend(:console, flush: true)
    end
  end
end
