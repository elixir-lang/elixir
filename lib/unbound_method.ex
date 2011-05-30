object UnboundMethod
  attr_reader ['owner, 'name, 'arity]

  def initialize(owner, name, arity)
    @('owner: owner, 'name: name, 'arity: arity)
  end

  def bind(object)
    Method.new(object, @owner, name, arity)
  end

  def apply_to(object, args)
    Erlang.elixir_dispatch.owner_dispatch(@owner, object, @name, args)
  end
end