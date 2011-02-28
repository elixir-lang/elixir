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

  def start_link(args)
    start_link(args, [])
  end

  def start_link(args, options)
    Erlang.gen_server.start_link(self.__callbacks_module__, args, options)
  end

  def start_link(name, args, options)
    Erlang.gen_server.start_link(name, self.__callbacks_module__, args, options)
  end

  delegate ['call/2, 'call/3, 'cast/2], 'to: "Erlang.gen_server"
end