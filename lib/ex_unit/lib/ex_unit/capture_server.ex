defmodule ExUnit.CaptureServer do
  @moduledoc false
  @compile {:no_warn_undefined, Logger}
  @timeout :infinity
  @name __MODULE__
  @ets __MODULE__

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

  def log_capture_on(pid, string_io, opts) do
    GenServer.call(@name, {:log_capture_on, pid, string_io, opts}, @timeout)
  end

  def log_capture_off(ref) do
    GenServer.call(@name, {:log_capture_off, ref}, @timeout)
  end

  ## Callbacks

  @impl true
  def init(:ok) do
    :ets.new(@name, [:named_table, :public, :set])

    state = %{
      devices: %{},
      log_captures: %{},
      log_status: :error
    }

    {:ok, state}
  end

  @impl true
  def handle_call(call, from, state)

  def handle_call({:device_capture_on, name, encoding, input}, {caller, _}, config) do
    capture_device(name, encoding, input, config, caller)
  end

  def handle_call({:device_output, name, ref}, _from, config) do
    device = Map.fetch!(config.devices, name)
    {_, output} = StringIO.contents(device.pid)
    total = byte_size(output)
    {_pid, offset} = Map.fetch!(device.refs, ref)
    output_size = total - offset
    {:reply, binary_part(output, offset, output_size), config}
  end

  def handle_call({:device_capture_off, ref}, _from, config) do
    {:reply, :ok, release_device(ref, config)}
  end

  def handle_call({:log_capture_on, pid, string_io, opts}, _from, config) do
    ref = Process.monitor(pid)
    refs = Map.put(config.log_captures, ref, true)

    {level, opts} = Keyword.pop(opts, :level)
    {formatter_mod, formatter_config} = Logger.default_formatter(opts)
    true = :ets.insert(@ets, {ref, string_io, level || :all, formatter_mod, formatter_config})

    if map_size(refs) == 1 do
      :ok = :logger.add_handler(@name, __MODULE__, %{})

      status =
        with {:ok, config} <- :logger.get_handler_config(:default),
             :ok <- :logger.remove_handler(:default) do
          {:ok, config}
        else
          _ -> :error
        end

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

  @impl true
  def handle_info({:DOWN, ref, _, _, _}, config) do
    config = remove_log_capture(ref, config)
    config = release_device(ref, config)
    {:noreply, config}
  end

  defp capture_device(name, encoding, input, config, caller) do
    case config.devices do
      %{^name => device} ->
        dead_refs = for {ref, {pid, _}} <- device.refs, not Process.alive?(pid), do: ref

        case dead_refs do
          [] ->
            capture_existing_device(name, encoding, input, config, caller)

          _ ->
            config = Enum.reduce(dead_refs, config, &release_device/2)
            capture_device(name, encoding, input, config, caller)
        end

      %{} ->
        capture_new_device(name, encoding, input, config, caller)
    end
  end

  defp capture_existing_device(name, encoding, input, config, caller) do
    case Map.fetch!(config.devices, name) do
      %{input?: input?} when input? or input != "" ->
        {:reply, {:error, :input_on_already_captured_device}, config}

      %{encoding: ^encoding} = device ->
        {_, output} = StringIO.contents(device.pid)
        ref = Process.monitor(caller)
        config = put_in(config.devices[name].refs[ref], {caller, byte_size(output)})
        {:reply, {:ok, ref}, config}

      %{encoding: other_encoding} ->
        {:reply, {:error, {:changed_encoding, other_encoding}}, config}
    end
  end

  defp capture_new_device(name, encoding, input, config, caller) do
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

        device = %{
          original_pid: original_pid,
          pid: pid,
          refs: %{ref => {caller, 0}},
          encoding: encoding,
          input?: input != ""
        }

        {:reply, {:ok, ref}, put_in(config.devices[name], device)}
    end
  end

  defp release_device(ref, %{devices: devices} = config) do
    Process.demonitor(ref, [:flush])

    case Enum.find(devices, fn {_, device} -> Map.has_key?(device.refs, ref) end) do
      {name, device} ->
        case Map.delete(device.refs, ref) do
          refs when map_size(refs) == 0 ->
            revert_device_to_original_pid(name, device.original_pid)
            close_string_io(device.pid)
            %{config | devices: Map.delete(devices, name)}

          refs ->
            put_in(config.devices[name].refs, refs)
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
    true = :ets.delete(@ets, ref)

    case Map.pop(refs, ref, false) do
      {true, refs} ->
        maybe_revert_to_default_handler(refs, config.log_status)
        %{config | log_captures: refs}

      {false, _refs} ->
        config
    end
  end

  defp maybe_revert_to_default_handler(refs, status) when map_size(refs) == 0 do
    :logger.remove_handler(@name)

    with {:ok, %{module: module} = config} <- status do
      :logger.add_handler(:default, module, config)
    end
  end

  defp maybe_revert_to_default_handler(_refs, _config) do
    :ok
  end

  ## :logger handler callback.

  def log(event, _config) do
    for {_ref, string_io, level, formatter_mod, formatter_config} <- :ets.tab2list(@ets),
        :logger.compare_levels(event.level, level) in [:gt, :eq] do
      chardata = formatter_mod.format(event, formatter_config)
      # There is a race condition where the capture_log is removed
      # but another process is attempting to log to string io device
      # that no longer exists, so we wrap it in try/catch.
      try do
        IO.write(string_io, chardata)
      rescue
        _ -> :ok
      end
    end

    :ok
  end
end
