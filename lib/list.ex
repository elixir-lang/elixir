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

  def member?(item)
    Erlang.lists.member(item, self)
  end
  alias_local 'member?, 'include?, 1

  def filter(function)
    Erlang.lists.filter(function, self)
  end
  alias_local 'filter, 'select, 1

  def delete(item)
    Erlang.lists.delete(item, self)
  end

  def join(string)
    strings = map -> (x) x.to_s.to_char_list
    String.new Erlang.string.join(strings, string.to_char_list)
  end

  def to_list
    self
  end

  def to_bin
    Erlang.list_to_binary(self)
  end

  def to_char_list
    self
  end

  def length
    Erlang.length(self)
  end
end