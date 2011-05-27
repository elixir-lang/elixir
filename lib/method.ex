object Method
  attr_reader ['owner, 'name, 'arity]

  def initialize(owner, name, arity)
    @('owner: owner, 'name: name, 'arity: arity)
  end
end