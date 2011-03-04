module Code::Server
  proto GenServer

  def start(basepath, basefiles)
    path  = [File.expand_path(basepath)]
    files = [File.expand_path(file) for file in basefiles]
    self.start({'local, 'elixir_code_server}, {path, files}, [])
  end

  callbacks

  def init(state)
    { 'ok, state }
  end

  def handle_call({'push_path, path}, _from, {paths,loaded})
    unless_included path, paths, loaded, -> paths + [path]
  end

  def handle_call({'unshift_path, path}, _from, {paths,loaded})
    unless_included path, paths, loaded, -> [path|paths]
  end

  def handle_call({'delete_path, path}, _from, {paths,loaded})
    { 'reply, 'ok, { paths.delete(path), loaded } }
  end

  def handle_call('paths, _from, {paths, loaded})
    { 'reply, paths, { paths, loaded} }
  end

  def handle_call({'push_loaded, path}, _from, {paths,loaded})
    { 'reply, 'ok, { paths, [path|loaded] } }
  end

  def handle_call('loaded, _from, {paths, loaded})
    { 'reply, loaded, { paths, loaded} }
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

  def unless_included(path, paths, loaded, function)
    final = if paths.include?(path) then paths else function() end
    { 'reply, 'ok, {final, loaded} }
  end
end