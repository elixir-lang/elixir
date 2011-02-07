object List
  % Returns a new list with the contents of the
  % current list and the other list.
  %
  % ## Examples
  %
  %     [1,2,3] + [4,5,6] % => [1,2,3,4,5,6]
  %     [1,2,3] + [1,2,3] % => [1,2,3,1,2,3]
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

  % Calls the function once for each element in the list.
  %
  % Returns a new list containing the values returned by the function.
  %
  %     [1,2,3].map -> (x) x + 1 % => [2,3,4]
  %
  def map(function)
    Erlang.lists.map(function, self)
  end
  alias_local 'map, 'collect, 1

  % Calls function once for each element in the list, passing that
  % element as a parameter.
  %
  % Returns self.
  %
  % ## Examples
  %
  %     [1,2,3].each -> (x) do_something(x)
  %     [1,2,3].each do (x)
  %       do_something_else(x)
  %     end
  %
  def each(function)
    Erlang.lists.foreach(function, self)
    self
  end

  % Returns the head of the list:
  %
  % ## Examples
  %
  %     [1,2].head % => 1
  %     [].head    % => []
  %
  def head
    if self
      Erlang.hd(self)
    else
      self
    end
  end

  % Returns the tail of the list:
  %
  % ## Examples
  %
  %     [1,2].tail % => [2]
  %     [].tail    % => []
  %
  def tail
    if self
      Erlang.tl(self)
    else
      self
    end
  end

  % Returns true if the given item exists in the array.
  %
  % ## Examples
  %
  %     [1,2,3].member?(1) % => true
  %     [1,2,3].include?(4) % => false
  %
  def member?(item)
    Erlang.lists.member(item, self)
  end
  alias_local 'member?, 'include?, 1

  def filter(function)
    Erlang.lists.filter(function, self)
  end
  alias_local 'filter, 'select, 1

  % Deletes an item from the list. If there is more than one
  % occurence of the item in the list just the first one is deleted.
  %
  % ## Examples
  %
  %     [1,2,3].delete(2)   % => [1,3]
  %     [1,2,1,3].delete(1) % => [2,1,3]
  %
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