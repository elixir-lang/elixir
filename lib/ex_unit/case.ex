Code.require "ex_unit/assertions"

module ExUnit::Case
  proto ExUnit::Assertions

  def __tests__
    regexp = ~r(_test$)
    self.__public_proto_methods__.foldl [], do ({name, _arity}, acc)
      if regexp.match?(name)
        [name|acc]
      else
        acc
      end
    end
  end
end