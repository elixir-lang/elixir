defmodule ExUnit.Server do
  @moduledoc false

  @timeout 30_000
  use GenServer.Behaviour

  defrecord Config, options: [], async_cases: [], sync_cases: []

  def start_link(options) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
  end

  ## Before run API

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

  def options do
    :gen_server.call(__MODULE__, :options, @timeout)
  end

  def cases do
    :gen_server.call(__MODULE__, :cases, @timeout)
  end

  ## Callbacks

  def init(options) do
    { :ok, Config[options: options] }
  end

  def handle_call(:options, _from, config) do
    { :reply, config.options, config }
  end

  def handle_call(:cases, _from, config) do
    { :reply,
      { config.async_cases, config.sync_cases },
      config.async_cases([]).sync_cases([]) }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
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
