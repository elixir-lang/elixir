% elixir: cache

% Handles GenServer creation and communication.
%
% ## Mixin methods
%
% GenServer exposes as mixin methods all the methods available in
% the Erlang gen_server module to communicate with a GenServer.
%
% ## Proto methods
%
% The GenServer can also be included in your object in order to create a new
% GenServer. When included, this module adds start/1, start/2, start/3 and
% start_link/1, start_link/2, start_link/3 as protected methods to the object
% to bootstrap the GenServer. However, the communication still happens through
% the Mixin methods.
%
% ## Delegator
%
% A delegator allows you to pass an object to a GenServer which wil be invoked as callback.
% The main difference to Erlang's gen server is that no state is ever passed as argument,
% because the state is in the object itself.
module GenServer
  % Starts a new GenServer that delegates all calls to the underlying object instance.
  module Delegator
    def start(state)
      start(state, [])
    end

    def start(state, options)
      Erlang.gen_server.start('elixir_gen_server, state, options)
    end

    def start(name, state, options)
      Erlang.gen_server.start(name, 'elixir_gen_server, state, options)
    end

    def start_link(state)
      start_link(state, [])
    end

    def start_link(state, options)
      Erlang.gen_server.start_link('elixir_gen_server, state, options)
    end

    def start_link(name, state, options)
      Erlang.gen_server.start_link(name, 'elixir_gen_server, state, options)
    end
  end

  % GenServer exposed API.
  module Mixin
    delegate ['start/3, 'start/4, 'start_link/3, 'start_link/4,
      'call/2, 'call/3, 'cast/2], 'to: "Erlang.gen_server"

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
end