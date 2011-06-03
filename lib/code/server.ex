module Code::Server
  def start_link()
    { 'ok, _ } = GenServer.start_link({'local, 'elixir_code_server}, #Code::Server::Instance(), [])
  end

  module Instance
    def __bound__()
      @('argv: [], 'loaded: [])
    end

    def init
      { 'ok, self }
    end

    def handle_call({'loaded, path}, _from)
      { 'reply, 'ok, @('loaded, [path|@loaded]) }
    end

    def handle_call({'argv, argv}, _from)
      { 'reply, 'ok, @('argv, argv) }
    end

    def handle_call('loaded, _from)
      { 'reply, @loaded, self }
    end

    def handle_call('argv, _from)
      { 'reply, @argv, self }
    end

    def handle_call(_request, _from)
      { 'reply, 'undef, self }
    end

    def handle_info(_msg)
      { 'noreply, self }
    end

    def handle_cast(_msg)
      { 'noreply, self }
    end

    def terminate(reason)
      IO.puts "[FATAL] Code::Server crashed:\n#{reason}"
      IO.puts "[FATAL] Code::Server snapshot:\n#{self}"
      'ok
    end

    def code_change(_old, _extra)
      { 'ok, self }
    end
  end
end