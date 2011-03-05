% elixir: cache

module GenServer
  module Mixin
    delegate ['call/2, 'call/3, 'cast/2], 'to: "Erlang.gen_server"

    protected

    % Callback that defines the registered module as a gen_server
    def __added_as_proto__(base)
      base.define_behavior('gen_server)
    end
  end

  protected

  def start(state)
    start(state, [])
  end

  def start(state, options)
    Erlang.gen_server.start(self.__callbacks_module__, state, options)
  end

  def start(name, state, options)
    Erlang.gen_server.start(name, self.__callbacks_module__, state, options)
  end

  def start_link(state)
    start_link(state, [])
  end

  def start_link(state, options)
    Erlang.gen_server.start_link(self.__callbacks_module__, state, options)
  end

  def start_link(name, state, options)
    Erlang.gen_server.start_link(name, self.__callbacks_module__, state, options)
  end

  delegate ['call/2, 'call/3, 'cast/2], 'to: "Erlang.gen_server"
end