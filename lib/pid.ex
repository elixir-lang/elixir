object Pid
  module Mixin
    def self
      Erlang.self
    end

    def spawn(object, method, args)
      Erlang.spawn -> object.send(method, args)
    end
  end

  def <-(message)
    
  end

  def <<-(message)
    
  end
end