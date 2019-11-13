defmodule ExUnit.CaptureServer do
  @moduledoc false
  @compile {:no_warn_undefined, Logger}
  @timeout 30000
  @name __MODULE__

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def device_capture_on(device, encoding, input) do
    GenServer.call(@name, {:device_capture_on, device, encoding, input}, @timeout)
  end

  def device_output(ref) do
    GenServer.call(@name, {:device_output, ref}, @timeout)
  end

  def device_capture_off(ref) do
    GenServer.call(@name, {:device_capture_off, ref}, @timeout)
  end

  def log_capture_on(pid) do
    GenServer.call(@name, {:log_capture_on, pid}, @timeout)
  end

  def log_capture_off(ref) do
    GenServer.call(@name, {:log_capture_off, ref}, @timeout)
  end

  ## Callbacks

  def init(:ok) do
    state = %{
      devices: %{},
      log_captures: %{},
      log_status: nil
    }

    {:ok, state}
  end

  def handle_call({:device_capture_on, name, encoding, input}, _from, %{devices: devices} = config) do
    {:ok, pid} = StringIO.open(input, encoding: encoding)

    if Map.has_key?(devices, name) do
      orig_pid = Process.whereis(name)
      try do
        Process.unregister(name)
      rescue
        ArgumentError ->
          nil
      end
      Process.register(pid, name)
      ref = Process.monitor(pid)
      devices = Map.put(devices, ref, {orig_pid, name})
      {:reply, {:ok, ref}, %{config | devices: devices}}
    else
      orig_pid = Process.whereis(name)
      try do
        Process.unregister(name)
      rescue
        ArgumentError ->
          nil
      end
      Process.register(pid, name)
      ref = Process.monitor(pid)
      devices = Map.put(devices, ref, {orig_pid, name})
      devices = Map.put(devices, :pid, pid)
      {:reply, {:ok, ref}, %{config | devices: devices}}
    end
  end

  def handle_call({:device_output, _ref}, _from, %{devices: %{pid: pid}} = config) do
    {_, output} = StringIO.contents(pid)
    {:reply, output, config}
  end

  def handle_call({:device_capture_off, ref}, _from, config) do
    config = release_device(ref, config)
    {:reply, :ok, config}
  end

  def handle_call({:log_capture_on, pid}, _from, config) do
    ref = Process.monitor(pid)
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

  def handle_info({:DOWN, ref, _, _, _}, config) do
    config = remove_log_capture(ref, config)
    config = release_device(ref, config)
    {:noreply, config}
  end

  defp release_device(ref, %{devices: devices} = config) do
    try do
      StringIO.close(devices.pid)
    rescue
      _ ->
        nil
    end

    case Map.get(devices, ref) do
      {pid, name} ->
        try do
          Process.unregister(name)
        rescue
          ArgumentError ->
            nil
        end
        Process.register(pid, name)
        devices = Map.delete(devices, ref)
        devices = Map.delete(devices, :pid)
        %{config | devices: devices}

      _ ->
        config
    end
  end

  defp remove_log_capture(ref, %{log_captures: refs} = config) do
    case Map.pop(refs, ref, false) do
      {true, refs} ->
        maybe_add_console(refs, config.log_status)
        %{config | log_captures: refs}

      {false, _refs} ->
        config
    end
  end

  defp maybe_add_console(refs, status) do
    if status == :ok and map_size(refs) == 0 do
      Logger.add_backend(:console, flush: true)
    end
  end
end
