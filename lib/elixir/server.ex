module Elixir::Server

defrecord Config, argv: [], loaded: []

def start_link do
  { :ok, _ } = Erlang.gen_server.start_link({:local, :elixir_code_server}, __MODULE__, [], [])
end

def init(_args) do
  { :ok, Elixir::Server::Config.new }
end

def handle_call({:loaded, path}, _from, config) do
  { :reply, :ok, config.prepend_loaded [path] }
end

def handle_call({:argv, argv}, _from, config) do
  { :reply, :ok, config.argv(argv) }
end

def handle_call(:loaded, _from, config) do
  { :reply, config.loaded, config }
end

def handle_call(:argv, _from, config) do
  { :reply, config.argv, config }
end

def handle_call(_request, _from, config) do
  { :reply, :undef, config }
end

def handle_info(_msg, config) do
  { :noreply, config }
end

def handle_cast(_msg, config) do
  { :noreply, config }
end

def terminate(reason, config) do
  IO.puts "[FATAL] Code::Server crashed:\n#{reason}"
  IO.puts "[FATAL] Code::Server snapshot:\n#{config}"
  :ok
end

def code_change(_old, config, _extra) do
  { :ok, config }
end