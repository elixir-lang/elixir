object List
  def +(another)
    Erlang.lists.append(self, another)
  end

  def append(another)
    Erlang.lists.append(self, [another])
  end

  def foldl(acc, function)
    Erlang.lists.foldl(function, acc, self)
  end

  def join(string)
    joined = foldl [], -> (x, acc) acc.append(x)
    Erlang.lists.concat(Erlang.lists.sublist(joined, joined.length - 1))
  end

  def to_list
    self
  end

  def length
    Erlang.length(self)
  end
end