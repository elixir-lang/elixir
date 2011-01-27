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
    list = string.to_list
    joined = foldl [], -> (x, acc) acc.append(x.to_list).append(list)
    sublist = Erlang.lists.sublist(joined, joined.length - 1)
    String.new Erlang.lists.flatten(sublist)
  end

  def to_list
    self
  end

  def length
    Erlang.length(self)
  end
end