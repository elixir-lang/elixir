defmodule ExUnit.CaptureServer do
  @moduledoc false
  @compile {:no_warn_undefined, Logger}
  @timeout 30000
  @name __MODULE__

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def device_capture_on(name, encoding, input) do
    GenServer.call(@name, {:device_capture_on, name, encoding, input}, @timeout)
  end

  def device_output(name, ref) do
    GenServer.call(@name, {:device_output, name, ref}, @timeout)
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

  def handle_call({:device_capture_on, name, encoding, input}, {caller, _}, config) do
    capture_device(name, encoding, input, config, caller)
  end

  def handle_call({:device_output, name, ref}, _from, config) do
    device = Map.fetch!(config.devices, name)
    {_, output} = StringIO.contents(device.pid)
    total = byte_size(output)
    offset = Map.fetch!(device.refs, ref)
    output_size = total - offset
    {:reply, binary_part(output, offset, output_size), config}
  end

  def handle_call({:device_capture_off, ref}, _from, config) do
    Process.demonitor(ref, [:flush])
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

  defp capture_device(name, encoding, "", %{devices: devices} = config, caller)
       when is_map_key(devices, name) do
    case Map.fetch!(devices, name) do
      %{encoding: ^encoding} = device ->
        {_, output} = StringIO.contents(device.pid)
        ref = Process.monitor(caller)
        {:reply, {:ok, ref}, put_in(config.devices[name].refs[ref], byte_size(output))}

      %{encoding: other_encoding} ->
        {:reply, {:error, {:changed_encoding, other_encoding}}, config}
    end
  end

  defp capture_device(name, _, _, %{devices: devices} = config, _)
       when is_map_key(devices, name) do
    {:reply, {:error, :input_on_already_captured_device}, config}
  end

  defp capture_device(name, encoding, input, config, caller) do
    {:ok, pid} = StringIO.open(input, encoding: encoding)
    original_pid = Process.whereis(name)

    try do
      Process.unregister(name)
      Process.register(pid, name)
    rescue
      ArgumentError ->
        {:reply, {:error, :no_device}, config}
    else
      _ ->
        ref = Process.monitor(caller)
        device = %{original_pid: original_pid, pid: pid, refs: %{ref => 0}, encoding: encoding}
        {:reply, {:ok, ref}, put_in(config.devices[name], device)}
    end
  end

  defp release_device(ref, %{devices: devices} = config) do
    case Enum.find(devices, fn {_, device} -> device.refs[ref] end) do
      {name, device} ->
        case Enum.reject(device.refs, &(elem(&1, 0) == ref)) do
          [] ->
            revert_device_to_original_pid(name, device.original_pid)
            close_string_io(device.pid)
            %{config | devices: Map.delete(devices, name)}

          refs ->
            put_in(config.devices[name].refs, Map.new(refs))
        end

      _ ->
        config
    end
  end

  defp revert_device_to_original_pid(name, pid) do
    Process.unregister(name)
  rescue
    ArgumentError -> nil
  after
    Process.register(pid, name)
  end

  defp close_string_io(pid) do
    StringIO.close(pid)
  rescue
    ArgumentError -> nil
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
