module UnboundMethod
  attr_reader ['owner, 'name, 'arity]

  def __bound__(owner, name, arity)
    @('owner: owner, 'name: name, 'arity: arity)
  end

  def bind(object)
    #Method(object, @owner, name, arity)
  end

  def apply_to(object, args)
    Erlang.apply(@owner, @name, [object|args])
  end
end