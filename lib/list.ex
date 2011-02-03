object List
  def +(another)
    Erlang.lists.append(self, another)
  end

  def foldl(acc, function)
    Erlang.lists.foldl(function, acc, self)
  end

  def map(function)
    Erlang.lists.map(function, self)
  end

  def join(string)
    strings = map -> (x) x.to_s.to_char_list
    String.new Erlang.string.join(strings, string.to_char_list)
  end

  def to_list
    self
  end

  % Simply returns self. This method does not ensure the current
  % list is really a char list.
  def to_char_list
    self
  end
  alias_local 'to_char_list, 'to_cl, 0

  def length
    Erlang.erlang.length(self)
  end
end