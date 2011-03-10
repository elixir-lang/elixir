% elixir: cache

object List
  % Returns true if all items in the list evaluates to true according the given function.
  %
  % ## Examples
  %
  %     [1,2,3].all? -> (i) i % 2 == 0  % => false
  %     [2,4,6].all? -> (i) i % 2 == 0  % => true
  %
  def all?(function)
    Erlang.lists.all(function, self)
  end

  % Push a new element to the list.
  %
  % ## Examples
  %
  %     [1,2,3].push 4    % => [1,2,3,4]
  %     [1,2,3].append 4  % => [1,2,3,4]
  %
  def push(item)
    Erlang.lists.append(self, [item])
  end
  alias_local 'push, 'append, 1

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

  % Retrieves an item from the list. Negative indexes are allowed
  % and they retrieve the element in the reverse order. Out of bound
  % indexes raises 'function_clause error.
  %
  % ## Examples
  %
  %     [1,2,3][0] % => 1
  %     [1,2,3][1] % => 2
  %     [1,2,3][2] % => 3
  %     [1,2,3][3] % => Raises 'function_clause error
  %
  %     [1,2,3][-1] % => 3
  %     [1,2,3][-2] % => 2
  %     [1,2,3][-3] % => 1
  %     [1,2,3][-43] % => Raises 'function_clause error
  %
  def [](number)
    if number < 0
      Erlang.lists.nth(length + number + 1, self)
    else
      Erlang.lists.nth(number + 1, self)
    end
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

  % Returns the head of the list. Raises 'badarg error if the list
  % is empty.
  %
  % ## Examples
  %
  %     [1,2].head % => 1
  %     [].head    % => []
  %
  def head
    Erlang.hd(self)
  end

  % Flattens the given list. If the list being flattened is made of lists
  % one level deep, use flatten! instead as it is optimized for such cases.
  %
  % ## Examples
  %
  %     [[1],[[2]],3].flatten  % => [1,2,3]
  %
  def flatten
    Erlang.lists.flatten(self)
  end

  % Flattens a list of lists one level deep. If one of the elements of the list
  % is not a list, raises an error.
  %
  % ## Examples
  %
  %     [[1],[2],[3]].flatten!    % => [1,2,3]
  %     [[1],[[2]],[3]].flatten!  % => [1,[2],3]
  %
  def flatten!
    Erlang.lists.append(self)
  end

  % Returns the tail of the list. Raises 'badarg error if the list
  % is empty.
  %
  % ## Examples
  %
  %     [1,2].tail % => [2]
  %
  def tail
    Erlang.tl(self)
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

  % Returns a list with items matching the given filter function
  %
  % ## Examples
  %
  %     [1,2,3].filter -> (x) x / 2 == 1          % => [2]
  %     [1,2,3,4].filter -> (x) [3,4].include?(x) % => [3,4]
  %
  def filter(function)
    Erlang.lists.filter(function, self)
  end
  alias_local 'filter, 'select, 1

  def reverse
    Erlang.lists.reverse(self)
  end

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
    tail = list_tail(self)
    if tail == []
      "[#{inspect_join(self)}]"
    else
      "[#{inspect_join(copy_without_tail(self, []))}|#{tail}]"
    end
  end

  def to_s
    inspect
  end

  % Returns the list length. Also aliased to size.
  %
  % ## Examples
  %
  %     [1,2,3].length % => 3
  %     [].size        % => 0
  %
  def length
    Erlang.length(self)
  end
  alias_local 'length, 'size, 0

  % Returns if the list is proper.
  %
  % ## Examples
  %
  %     [1,2].proper?    % => true
  %     [1|[2]].proper?  % => true
  %     [1|2].proper?    % => false
  %
  def proper?
    list_tail(self) == []
  end

  private

  def inspect_join(list)
    strings = list.map -> (x) x.inspect.to_char_list
    String.new Erlang.string.join(strings, [$,])
  end

  def list_tail([_|t]) list_tail(t); end
  def list_tail([]) []; end
  def list_tail(object) object; end

  def copy_without_tail([h|t], acc) copy_without_tail(t, [h|acc]); end
  def copy_without_tail(_, acc) acc.reverse; end
end
