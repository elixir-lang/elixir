object Process
  module Mixin
    % Returns the pid for the current process.
    % This method is also aliased as 'current.
    def self
      Erlang.self
    end
    alias_local 'self, 'current, 0

    % Spawn the given function.
    def spawn(function)
      Erlang.spawn function
    end

    % Spawn the given function using proc_lib.
    def spawn!(function)
      Erlang.proc_lib.spawn function
    end

    % Spawn as link the given function.
    def spawn_link(function)
      Erlang.spawn_link function
    end

    % Spawn as link the given function using proc_lib.
    def spawn_link!(function)
      Erlang.proc_lib.spawn_link function
    end

    def flag(key, value)
      Erlang.process_flag(key, value)
    end

    def link(process)
      Erlang.link(process)
    end

    def unregister(name)
      Erlang.unregister(name)
    end

    def registered
      Erlang.registered
    end
  end

  % Sends a message to the Pid. This is the equivalent to Erlang's !.
  % This method is also aliased as send.
  def <-(message)
    Erlang.send(self, message)
  end
  alias_local '<-, 'send, 1

  def inspect
    [_|t] = Erlang.pid_to_list(self)
    ($"<Process " + t).to_bin
  end

  def register(name)
    Erlang.register(name, self)
  end

  def exit(reason)
    Erlang.exit(self, reason)
  end

  def to_s
    inspect
  end
end