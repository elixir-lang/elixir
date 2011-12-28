module ExUnit::Server
  
defrecord Config, options: [], cases: []

def start_link do
  { :ok, _ } = Erlang.gen_server.start_link({:local, :exunit_server}, __MODULE__, [], [])
end

def add_case(name) do
  check fn { Erlang.gen_server.call(:exunit_server, { :add_case, name }) }
end

def merge_options(options) do
  check fn { Erlang.gen_server.call(:exunit_server, { :merge_options, options }) }
end

def cases do
  List.reverse Erlang.gen_server.call(:exunit_server, :cases)
end

def options do
  Erlang.gen_server.call(:exunit_server, :options)
end

## Callbacks

def init(_args) do
  { :ok, ExUnit::Server::Config.new }
end

def handle_call({:add_case, name}, _from, config) do
  { :reply, :ok, config.prepend_cases [name] }
end

def handle_call({:merge_options, options}, _from, config) do
  { :reply, :ok, config.options Orddict.merge(config.options, options) }
end

def handle_call(:cases, _from, config) do
  { :reply, config.cases, config }
end

def handle_call(:options, _from, config) do
  { :reply, config.options, config }
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
  IO.puts "[FATAL] ExUnit::Server crashed:\n#{reason}"
  IO.puts "[FATAL] ExUnit::Server snapshot:\n#{config}"
  :ok
end

def code_change(_old, config, _extra) do
  { :ok, config }
end

private

def check(function) do
  try do
    function.()
  catch: { :exit, { :noproc, _ }, _ }
    exit "ExUnit::Server is not running. Are you sure you used exunit from command line?"
  end
end