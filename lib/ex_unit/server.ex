% elixir: cache

module ExUnit::Server
  proto GenServer

  def start(options)
    state = {
      'options: options,
      'cases: []
    }
    self.start_link({'local, 'exunit_server}, state, [])
  end

  def add_case(name)
    try
      GenServer.call('exunit_server, { 'add_case, name })
    catch 'exit: { 'noproc, _ }
      self.exit "ExUnit::Server is not running. Are you sure you invoked ExUnit.configure()?"
    end
  end

  def cases
    GenServer.call('exunit_server, 'cases)
  end

  callbacks

  def init(state)
    { 'ok, state }
  end

  def handle_call({'add_case, name}, _from, state)
    { 'reply, 'ok, state.append('cases, name) }
  end

  def handle_call('cases, _from, state)
    { 'reply, state['cases], state }
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
    IO.puts "[FATAL] ExUnit::Server crashed:\n#{reason}"
    IO.puts "[FATAL] ExUnit::Server snapshot:\n#{state}"
    Erlang.init.stop(1) % Shut everything
  end

  def code_change(_old, state, _extra)
    { 'ok, state }
  end
end