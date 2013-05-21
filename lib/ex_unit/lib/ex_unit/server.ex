defmodule ExUnit.Server do
  @moduledoc false

  @timeout 30_000
  use GenServer.Behaviour

  defrecord Config, options: [], async_cases: [], sync_cases: [],
                    start_load: nil

  def start_link(options) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
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

  def merge_options(options) do
    :gen_server.cast(__MODULE__, { :merge_options, options })
  end

  ## After run API

  def start_run() do
    :gen_server.call(__MODULE__, :start_run, @timeout)
  end

  ## Callbacks

  def init(options) do
    { :ok, Config[options: options] }
  end

  def handle_call(:start_run, _from, config) do
    load_us =
      if start_load = config.start_load do
        :timer.now_diff(:os.timestamp, start_load)
      end

    { :reply,
      { config.async_cases, config.sync_cases, config.options, load_us },
      config.async_cases([]).sync_cases([]).start_load(nil) }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast(:start_load, config) do
    { :noreply, config.start_load(:os.timestamp) }
  end

  def handle_cast({:add_async_case, name}, config) do
    { :noreply, config.update_async_cases [name|&1] }
  end

  def handle_cast({:add_sync_case, name}, config) do
    { :noreply, config.update_sync_cases [name|&1] }
  end

  def handle_cast({:merge_options, options}, config) do
    { :noreply, config.update_options(&1 |> Keyword.merge(options)) }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end
