defrecord Elixir.Server.Config, argv: [], loaded: [], at_exit: [], compiler_options: []

defmodule Elixir.Server do
  @moduledoc false

  use GenServer.Behavior

  def start_link do
    { :ok, _ } = Erlang.gen_server.start_link({:local, :elixir_code_server}, __MODULE__, [], [])
  end

  def init(_args) do
    { :ok, Elixir.Server.Config.new }
  end

  def handle_call({:loaded, path}, _from, config) do
    { :reply, :ok, config.prepend_loaded [path] }
  end

  def handle_call({:at_exit, fun}, _from, config) do
    { :reply, :ok, config.prepend_at_exit [fun] }
  end

  def handle_call({:argv, argv}, _from, config) do
    { :reply, :ok, config.argv(argv) }
  end

  def handle_call({:compiler_options, record}, _from, config) do
    { :reply, :ok, config.compiler_options(record) }
  end

  def handle_call(:loaded, _from, config) do
    { :reply, config.loaded, config }
  end

  def handle_call(:at_exit, _from, config) do
    { :reply, config.at_exit, config }
  end

  def handle_call(:argv, _from, config) do
    { :reply, config.argv, config }
  end

  def handle_call(:compiler_options, _from, config) do
    { :reply, config.compiler_options, config }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end
end