Code.require "ex_unit/assertions"

module ExUnit::Case
  proto ExUnit::Assertions

  def __tests__
    regexp = ~r(_test$)
    method_names = self.__public_proto_methods__.map -> ({name, _arity}) name
    method_names.select -> (name) regexp.match?(name)
  end
end
