object ExUnit::Case
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