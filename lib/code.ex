module Code
  module Server
    proto GenServer

    callbacks

    def init(state)
      { 'ok, state }
    end

    def handle_call({'push_path, path}, _from, {paths,loaded})
      { 'reply, 'ok, { paths + [path], loaded } }
    end

    def handle_call({'unshift_path, path}, _from, {paths,loaded})
      { 'reply, 'ok, { [path|paths], loaded } }
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
  end

  def unshift_path(path)
    GenServer.call('elixir_code_server, { 'unshift_path, File.expand_path(path) })
  end

  def paths
    GenServer.call('elixir_code_server, 'paths)
  end

  def require(path)
    Erlang.elixir.require_file(path.to_char_list)
  end
end