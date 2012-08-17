defmodule ExUnit.Server do
  @moduledoc false

  use GenServer.Behaviour

  defrecord Config, options: [], async_cases: [], sync_cases: []

  def start_link do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, [], [])
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

  def options do
    :gen_server.call(__MODULE__, :options)
  end

  ## Callbacks

  def init(_args) do
    { :ok, Config.new }
  end

  def handle_call(:options, _from, config) do
    options = Keyword.merge config.options,
      async_cases: Enum.reverse(config.async_cases),
      sync_cases:  Enum.reverse(config.sync_cases)
    { :reply, options, config }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast({:add_async_case, name}, config) do
    { :noreply, config.prepend_async_cases [name] }
  end

  def handle_cast({:add_sync_case, name}, config) do
    { :noreply, config.prepend_sync_cases [name] }
  end

  def handle_cast({:merge_options, options}, config) do
    { :noreply, config.merge_options(options) }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end
