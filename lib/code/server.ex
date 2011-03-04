module Code::Server
  object State
    attr_reader ['paths, 'loaded, 'argv]

    def constructor(basepath, basefiles)
      paths  = [File.expand_path(basepath)]
      loaded = [File.expand_path(file) for file in basefiles]
      { 'paths: paths, 'loaded: loaded, 'argv: [] }
    end

    def push_path(path)
      unless_included path, -> @paths + [path]
    end

    def unshift_path(path)
      unless_included path, -> [path|@paths]
    end

    def delete_path(path)
      self.set_ivar 'paths, @paths.delete(path)
    end

    def loaded(path)
      self.set_ivar 'loaded, [path|@loaded]
    end

    def argv(argv)
      self.set_ivar 'argv, argv
    end

    protected

    def unless_included(path, function)
      if @paths.include?(path)
        self
      else
        self.set_ivar('paths, function())
      end
    end
  end

  proto GenServer

  def start(basepath, basefiles)
    self.start({'local, 'elixir_code_server}, Code::Server::State.new(basepath, basefiles), [])
  end

  callbacks

  def init(state)
    { 'ok, state }
  end

  % Define the following callbacks expecting one argument and dispatching them to state.
  ['push_path, 'unshift_path, 'delete_path, 'argv, 'loaded].each do (arg)
    module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
def handle_call({'#{arg}, arg}, _from, state)
  { 'reply, 'ok, state.#{arg}(arg) }
end
~~
  end

  % Read values from state
  ['paths, 'loaded, 'argv].each do (arg)
    module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
def handle_call('#{arg}, _from, state)
  { 'reply, state.#{arg}, state }
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
end