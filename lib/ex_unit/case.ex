Code.require "ex_unit/assertions"

module ExUnit::Case
  proto ExUnit::Assertions

  def __tests__
    regexp = ~r(_test$)
    [name for {name, _} in self.__public_proto_methods__, regexp.match?(name)]
  end
end
