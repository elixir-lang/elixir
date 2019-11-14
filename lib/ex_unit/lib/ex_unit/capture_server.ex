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

  def handle_call({:device_capture_on, name, encoding, input}, {caller, _}, config) do
    capture_device(name, encoding, input, config, caller)
  end

  def handle_call({:device_output, ref}, _from, %{devices: devices} = config) do
    {_, device} = device_by_ref(devices, ref)
    {_, output} = StringIO.contents(device.pid)
    total = byte_size(output)
    offset = offset_by_ref(device, ref)
    output_size = total - offset
    {:reply, binary_part(output, offset, output_size), config}
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

  defp capture_device(name, encoding, "", %{devices: devices} = config, caller)
       when is_map_key(devices, name) do
    case Map.get(devices, name) do
      %{encoding: ^encoding} = device ->
        {_, output} = StringIO.contents(device.pid)
        ref = Process.monitor(caller)
        refs = [{ref, byte_size(output)} | device.refs]
        devices = %{devices | name => %{device | refs: refs}}
        {:reply, {:ok, ref}, %{config | devices: devices}}

      %{encoding: other_encoding} ->
        {:reply, {:error, {:changed_encoding, other_encoding}}, config}
    end
  end

  defp capture_device(name, _, _, %{devices: devices} = config, _)
       when is_map_key(devices, name) do
    {:reply, {:error, :input_on_already_captured_device}, config}
  end

  defp capture_device(name, encoding, input, %{devices: devices} = config, caller) do
    {:ok, pid} = StringIO.open(input, encoding: encoding)
    orig_pid = Process.whereis(name)

    Process.unregister(name)
    Process.register(pid, name)

    _ = Process.monitor(pid)
    ref = Process.monitor(caller)

    device = %{orig_pid: orig_pid, pid: pid, refs: [{ref, 0}], encoding: encoding}
    devices = Map.put(devices, name, device)
    {:reply, {:ok, ref}, %{config | devices: devices}}
  rescue
    ArgumentError ->
      {:reply, {:error, :no_device}, config}
  end

  defp release_device(ref, %{devices: devices} = config) do
    case device_by_ref(devices, ref) do
      {name, device_info} ->
        case Enum.reject(device_info.refs, &refs_equal?(&1, ref)) do
          [] ->
            try do
              try do
                Process.unregister(name)
              after
                Process.register(device_info.orig_pid, name)
                StringIO.close(device_info.pid)
                Process.demonitor(ref)
              end
            rescue
              ArgumentError ->
                nil
            end

            %{config | devices: Map.delete(devices, name)}

          refs ->
            device_info = %{device_info | refs: refs}
            devices = Map.put(devices, name, device_info)
            %{config | devices: devices}
        end

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

  defp device_by_ref(devices, ref) do
    Enum.find(devices, fn {_, device} -> offset_by_ref(device, ref) end)
  end

  defp offset_by_ref(device, ref) do
    case Enum.find(device.refs, &refs_equal?(&1, ref)) do
      {_, offset} -> offset
      _ -> nil
    end
  end

  defp refs_equal?({ref, _}, ref), do: true
  defp refs_equal?(_, _), do: false
end
