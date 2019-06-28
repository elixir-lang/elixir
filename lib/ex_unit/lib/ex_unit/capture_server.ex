defmodule ExUnit.CaptureServer do
  @moduledoc false
  @compile {:no_warn_undefined, Logger}
  @timeout 30000
  @name __MODULE__

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def device_capture_on(device, pid) do
    GenServer.call(@name, {:device_capture_on, device, pid}, @timeout)
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
      devices: {%{}, %{}},
      log_captures: %{},
      log_status: nil
    }

    {:ok, state}
  end

  def handle_call({:device_capture_on, name, pid}, _from, config) do
    {names, refs} = config.devices

    if Map.has_key?(names, name) do
      {:reply, {:error, :already_captured}, config}
    else
      orig_pid = Process.whereis(name)
      Process.unregister(name)
      Process.register(pid, name)
      ref = Process.monitor(pid)
      refs = Map.put(refs, ref, {name, orig_pid})
      names = Map.put(names, name, true)
      {:reply, {:ok, ref}, %{config | devices: {names, refs}}}
    end
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

  defp release_device(ref, %{devices: {names, refs}} = config) do
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

        %{config | devices: {names, refs}}

      {nil, _refs} ->
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
