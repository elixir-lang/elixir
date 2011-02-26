object Process
  module Mixin
    % Returns the pid for the current process.
    % This method is also aliased as 'current.
    def self
      Erlang.self
    end
    alias_local 'self, 'current, 0

    % TODO Optimize for modules
    def spawn(object, method, args)
      Erlang.spawn -> object.__send__(method, args)
    end
  end

  % Sends a message to the Pid. This is the equivalent to Erlang's !.
  % This method is also aliased as send.
  def <-(message)
    Erlang.send(self, message)
  end
  alias_local '"<-", 'send, 1

  def inspect
    [_|t] = Erlang.pid_to_list(self)
    String.new $"<Process " + t
  end

  def to_s
    inspect
  end
end