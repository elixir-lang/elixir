object List
  % Returns a new list with the contents of the
  % current list and the other list.
  %
  % ## Examples
  %
  %     [1,2,3] + [4,5,6] %=> [1,2,3,4,5,6]
  %     [1,2,3] + [1,2,3] %=> [1,2,3,1,2,3]
  %
  def +(another)
    Erlang.lists.append(self, another)
  end

  def foldl(acc, function)
    Erlang.lists.foldl(function, acc, self)
  end

  def foldr(acc, function)
    Erlang.lists.foldr(function, acc, self)
  end

  def map(function)
    Erlang.lists.map(function, self)
  end

  def each(function)
    Erlang.lists.foreach(function, self)
  end

  % Returns true if the given item exists in the array.
  %
  % ## Examples
  %
  %     [1,2,3].member?(1) %=> true
  %     [1,2,3].include?(4) %=> false
  %
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

  def inspect
    strings = map -> (x) x.inspect.to_char_list
    "[#{String.new Erlang.string.join(strings, [$,, $\s])}]"
  end

  def to_s
    inspect
  end

  def length
    Erlang.length(self)
  end
end