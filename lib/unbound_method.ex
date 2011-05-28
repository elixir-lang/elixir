object UnboundMethod
  attr_reader ['owner, 'name, 'arity]

  def initialize(owner, name, arity)
    @('owner: owner, 'name: name, 'arity: arity)
  end

  def bind(object)
    Method.new(object, @owner, name, arity)
  end
end