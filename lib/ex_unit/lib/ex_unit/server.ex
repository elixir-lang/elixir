defmodule ExUnit.Server do
  @moduledoc false

  @timeout 30_000
  use GenServer.Behaviour

  defrecord Config, async_cases: [], sync_cases: [], start_load: nil, captured_devices: HashSet.new

  def start_link() do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, :ok, [])
  end

  ## Before run API

  def start_load() do
    :gen_server.cast(__MODULE__, :start_load)
  end

  def add_async_case(name) do
    :gen_server.cast(__MODULE__, { :add_async_case, name })
  end

  def add_sync_case(name) do
    :gen_server.cast(__MODULE__, { :add_sync_case, name })
  end

  ## After run API

  def start_run() do
    :gen_server.call(__MODULE__, :start_run, @timeout)
  end

  ## Capture Device API

  def add_device(device) do
    :gen_server.call(__MODULE__, { :add_device, device })
  end

  def remove_device(device) do
    :gen_server.call(__MODULE__, { :remove_device, device })
  end

  ## Callbacks

  def init(:ok) do
    { :ok, Config[] }
  end

  def handle_call(:start_run, _from, config) do
    load_us =
      if start_load = config.start_load do
        :timer.now_diff(:os.timestamp, start_load)
      end

    { :reply,
      { config.async_cases, config.sync_cases, load_us },
      config.async_cases([]).sync_cases([]).start_load(nil) }
  end

  def handle_call({ :add_device, device }, _from, config) do
    { :reply,
      not(device in config.captured_devices),
      config.update_captured_devices(&Set.put(&1, device)) }
  end

  def handle_call({ :remove_device, device }, _from, config) do
    { :reply, :ok, config.update_captured_devices(&Set.delete(&1, device)) }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast(:start_load, config) do
    { :noreply, config.start_load(:os.timestamp) }
  end

  def handle_cast({:add_async_case, name}, config) do
    { :noreply, config.update_async_cases &[name|&1] }
  end

  def handle_cast({:add_sync_case, name}, config) do
    { :noreply, config.update_sync_cases &[name|&1] }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end
