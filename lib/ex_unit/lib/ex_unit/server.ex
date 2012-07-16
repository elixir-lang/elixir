defrecord ExUnit.Server.Config, options: [], async_cases: [], sync_cases: []

defmodule ExUnit.Server do
  @moduledoc false

  use GenServer.Behavior

  def start_link do
    Erlang.gen_server.start_link({:local, :exunit_server}, __MODULE__, [], [])
  end

  def add_async_case(name) do
    check fn -> Erlang.gen_server.call(:exunit_server, { :add_async_case, name }) end
  end

  def add_sync_case(name) do
    check fn -> Erlang.gen_server.call(:exunit_server, { :add_sync_case, name }) end
  end

  def merge_options(options) do
    check fn -> Erlang.gen_server.call(:exunit_server, { :merge_options, options }) end
  end

  def options do
    Erlang.gen_server.call(:exunit_server, :options)
  end

  ## Callbacks

  def init(_args) do
    { :ok, ExUnit.Server.Config.new }
  end

  def handle_call({:add_async_case, name}, _from, config) do
    { :reply, :ok, config.prepend_async_cases [name] }
  end

  def handle_call({:add_sync_case, name}, _from, config) do
    { :reply, :ok, config.prepend_sync_cases [name] }
  end

  def handle_call({:merge_options, options}, _from, config) do
    { :reply, :ok, config.merge_options(options) }
  end

  def handle_call(:options, _from, config) do
    options = Keyword.merge config.options,
      async_cases: List.reverse(config.async_cases),
      sync_cases:  List.reverse(config.sync_cases)
    { :reply, options, config }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  defp check(function) do
    try do
      function.()
    catch
      :exit, { :noproc, _ } ->
        exit "ExUnit.Server is not running. Are you sure you used exunit from command line?"
    end
  end
end
