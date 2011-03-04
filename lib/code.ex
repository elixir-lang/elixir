module Code
  module Server
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

  def unshift_path(path)
    server_call { 'unshift_path, File.expand_path(path).to_bin }
  end

  def push_path(path)
    server_call { 'push_path, File.expand_path(path).to_bin }
  end

  def delete_path(path)
    server_call { 'delete_path, File.expand_path(path).to_bin }
  end

  def paths
    raw_paths.map -> (p) String.new p
  end

  def loaded
    raw_loaded.map -> (l) String.new l
  end

  % Requires a file in the load paths. Returns true if the file was loaded,
  % false if the file was already loaded and raises an error if the file
  % could not be found in any load path.
  def require(path)
    paths = raw_paths
    loaded = raw_loaded

    first = require_in_paths(path, paths, loaded)
    if first == []
      second = require_in_paths(path + ".ex", paths, loaded)
      if second == [] then self.error { 'enoent, path } else second end
    else
      first
    end
  end

  % Requires a file given by the path. If the file does
  % not exist, an error is raised.
  def require_file(path)
    fullpath = File.expand_path(path).to_bin

    case Erlang.file.read_file(fullpath)
    match { 'ok, binary }
      server_call { 'push_loaded, fullpath }
      Erlang.elixir.eval(binary.to_char_list, [], fullpath.to_char_list)
    match other
      self.error other
    end
  end

  protected

  def process_argv(files)
    files.each do (file)
      require_file file
    end

    % Regardless, always halt at the end.
    Erlang.halt()
  end

  private

  def require_in_paths(path, [h|t], loaded)
    fullpath = File.expand_path(path, h).to_bin

    if File.is_file?(fullpath)
      if loaded.include?(fullpath)
        false
      else
        require_file(fullpath)
        true
      end
    else
      require_in_paths(path, t, loaded)
    end
  end

  def require_in_paths(_, [], _)
    []
  end

  def raw_paths
    server_call 'paths
  end

  def raw_loaded
    server_call 'loaded
  end

  def server_call(args)
    GenServer.call('elixir_code_server, args)
  end
end