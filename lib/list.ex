object List
  def +(another)
    Erlang.lists.append(self, another)
  end

  def map(function)
    Erlang.lists.map(function, self)
  end

  def join(string)
    strings = map -> (x) x.to_s.to_list
    String.new Erlang.string.join(strings, string.to_list)
  end

  def to_list
    self
  end

  def length
    Erlang.length(self)
  end
end