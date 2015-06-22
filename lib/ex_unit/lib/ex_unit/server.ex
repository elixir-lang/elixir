defmodule ExUnit.Server do
  @moduledoc false

  @timeout 30_000
  use GenServer

  alias Logger.Backends.Console

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

  def log_capture_on(pid, args) do
    GenServer.call(__MODULE__, {:log_capture_on, pid, args})
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
      log_status: nil,
      log_captures: %{}
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

  def handle_call({:log_capture_on, pid, args}, _from, config) do
    ref = Process.monitor(pid)
    try do
      GenEvent.add_mon_handler(Logger, {Console, ref}, args)
    catch
      :exit, :noproc ->
        Process.demonitor(ref, [:flush])
        {:reply, {:error, :no_logger}, config}
    else
      :ok ->
        case Map.put(config.log_captures, ref, :ok) do
          refs when map_size(refs) > 1 ->
            {:reply, {:ok, ref}, %{config | log_captures: refs}}
          refs ->
            status = Logger.remove_backend(:console)
            {:reply, {:ok, ref}, %{config | log_captures: refs, log_status: status}}
        end
    end
  end

  def handle_call({:log_capture_off, ref}, _from, config) do
    Process.demonitor(ref, [:flush])
    case Map.pop(config.log_captures, ref) do
      {:ok, refs} ->
        maybe_add_console(refs, config.log_status)
        {:reply, remove_capture(ref), %{config | log_captures: refs}}
      {other, refs} ->
        maybe_add_console(refs, config.log_status)
        {:reply, {:error, other}, %{config | log_captures: refs}}
    end
  end

  def handle_call(request, from, config) do
    super(request, from, config)
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

  def handle_cast(request, config) do
    super(request, config)
  end

  def handle_info({:DOWN, ref, _, _, _}, config) do
    refs = Map.delete(config.log_captures, ref)
    maybe_add_console(refs, config.log_status)
    remove_capture(ref)
    {:noreply, %{config | log_captures: refs}}
  end

  def handle_info({:gen_event_EXIT, _, :normal}, config) do
    {:noreply, config}
  end

  def handle_info({:gen_event_EXIT, {Console, ref}, reason}, config) do
    refs = Map.put(config.log_captures, ref, reason)
    maybe_add_console(refs, config.log_status)
    {:noreply, %{config | log_captures: refs}}
  end

  def handle_info(msg, state) do
    super(msg, state)
  end

  defp remove_capture(ref) do
    case GenEvent.remove_handler(Logger, {Console, ref}, nil) do
      {:error, :not_found} ->
        receive do
          {:gen_event_EXIT, {Console, ^ref}, reason} ->
            {:error, reason}
        after
          0 -> {:error, :not_found}
        end
      :ok -> :ok
    end
  end

  defp maybe_add_console(refs, status) do
    if status === :ok and map_size(refs) == 0 do
      Logger.add_backend(:console, flush: true)
    end
  end
end
