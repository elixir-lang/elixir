% elixir: cache

module Code::Server
  proto GenServer

  def start(basepath, basefiles)
    paths  = [File.expand_path(basepath)]
    loaded = [File.expand_path(file) for file in basefiles]
    dict   = { 'paths: paths, 'loaded: loaded, 'argv: [] }
    self.start({'local, 'elixir_code_server}, dict, [])
  end

  callbacks

  def init(state)
    { 'ok, state }
  end

  def handle_call({'push_path, path}, _from, state)
    unless_included state, path, -> state['paths] + [path]
  end

  def handle_call({'unshift_path, path}, _from, state)
    unless_included state, path, -> [path|state['paths]]
  end

  def handle_call({'delete_path, path}, _from, state)
    { 'reply, 'ok, state.set('paths, state['paths].delete(path)) }
  end

  def handle_call({'loaded, path}, _from, state)
    { 'reply, 'ok, state.append('loaded, path) }
  end

  def handle_call({'argv, argv}, _from, state)
    { 'reply, 'ok, state.set('argv, argv) }
  end

  % Read values from state
  ['paths, 'loaded, 'argv].each do (arg)
    module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
def handle_call('#{arg}, _from, state)
  { 'reply, state['#{arg}], state }
end
~~
  end

  def handle_call(_request, _from, state)
    { 'reply, 'undef, state }
  end

  def handle_info(_msg, state)
    { 'no_reply, state }
  end

  def handle_cast(_msg, state)
    { 'no_reply, state }
  end

  def terminate(reason, state)
    IO.puts "[FATAL] Code::Server crashed:\n#{reason}"
    IO.puts "[FATAL] Code::Server snapshot:\n#{state}"
    Erlang.init.stop(1) % Shut everything
  end

  def code_change(_old, state, _extra)
    { 'ok, state }
  end

  private

  def unless_included(state, path, function)
    if state['paths].include?(path)
      { 'reply, 'ok, state }
    else
      { 'reply, 'ok, state.set('paths, function()) }
    end
  end
end