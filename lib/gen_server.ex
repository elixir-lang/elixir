% elixir: cache

% Handles GenServer creation and communication.
module GenServer
  delegate ['call/2, 'call/3, 'cast/2], 'to: "Erlang.gen_server"

  def start(state, options := [])
    Erlang.gen_server.start('elixir_gen_server, state, options)
  end

  def start(name, state, options)
    Erlang.gen_server.start(name, 'elixir_gen_server, state, options)
  end

  def start_link(state, options := [])
    Erlang.gen_server.start_link('elixir_gen_server, state, options)
  end

  def start_link(name, state, options)
    Erlang.gen_server.start_link(name, 'elixir_gen_server, state, options)
  end
end