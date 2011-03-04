module Code
  def argv
    server_call 'argv
  end

  def version
    "0.1.0"
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
      server_call { 'loaded, fullpath }
      Erlang.elixir.eval(binary.to_char_list, [], fullpath.to_char_list)
    match other
      self.error other
    end
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