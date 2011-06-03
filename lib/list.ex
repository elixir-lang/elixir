module List
  module Instance
    % Returns true if all items in the list evaluates to true according the given function.
    %
    % ## Examples
    %
    %     [1,2,3].all? -> (i) i rem 2 == 0  % => false
    %     [2,4,6].all? -> (i) i rem 2 == 0  % => true
    %
    def all?(function)
      Erlang.lists.all(function, self)
    end

    % Push a new element to the list.
    %
    % ## Examples
    %
    %     [1,2,3].push 4    % => [1,2,3,4]
    %
    def push(item)
      Erlang.lists.append(self, [item])
    end

    % Searches the list for a tuple whose nth element compares equal to key.
    % Returns the tuple if such a tuple is found, otherwise false. The list
    % needs necessarily to have only tuples and n is 0..(tuple.size - 1).
    %
    % ## Examples
    %
    %     ['foo/1, 'bar/2].keyfind('foo, 0)  % => {'foo, 1}
    %     ['foo/1, 'bar/2].keyfind('baz, 0)  % => false
    %
    def keyfind(key, n)
      Erlang.lists.keyfind(key, n + 1, self)
    end

    % Returns true if the list is empty.
    def empty?
      self == []
    end

    % "Zips" two lists of equal length into one list of two-tuples, where the
    % first element of each tuple is taken from the first list and the second
    % element is taken from corresponding element in the second list.
    %
    % Raises an error if list sizes does not match.
    %
    % ## Examples
    %
    %     ['foo, 'bar].zip [1,2]  % => [{'foo,1}, {'bar,2}]
    %
    def zip(list)
      Erlang.lists.zip(self, list)
    end

    % Does the opposite of `zip`.
    %
    % ## Examples
    %
    %     {['foo, 'bar], [1,2]} = [{'foo, 1}, {'bar, 2}].unzip
    %
    def unzip
      Erlang.lists.unzip(self)
    end

    % Combine the elements of two lists of equal length into one list using
    % a function.
    %
    % Raises an error if list sizes does not match.
    %
    % ## Examples
    %
    %    [5,7,9] = [1,2,3].zipwith([4,5,6], -> (x,y) x + y)
    %
    def zipwith(list, function)
      Erlang.lists.zipwith(function, self, list)
    end

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

    % Combine all elements of the lists by applying the given function, starting
    % with the given accumulator. The list is traversed from the left.
    %
    % ## Examples
    %
    %    [1,2,3].foldl(0, -> (e, acc) e + acc)                % => 6
    %    ["foo", "bar", "baz"].foldl("", -> (e, acc) e + acc) % => "bazbarfoo"
    %
    def foldl(acc, function)
      Erlang.lists.foldl(function, acc, self)
    end

    % Combine all elements of the lists by applying the given function, starting
    % with the given accumulator. The list is traversed from the right.
    %
    % ## Examples
    %
    %    [1,2,3].foldl(0, -> (e, acc) e + acc)            % => 6
    %    ["foo", "bar", "baz"].foldr("", -> (e, acc) e + acc) % => "foobarbaz"
    %
    def foldr(acc, function)
      Erlang.lists.foldr(function, acc, self)
    end

    % Retrieves an item from the list. Negative indexes are allowed
    % and they retrieve the element in the reverse order.
    %
    % ## Examples
    %
    %     [1,2,3][0] % => 1
    %     [1,2,3][1] % => 2
    %     [1,2,3][2] % => 3
    %     [1,2,3][3] % => nil
    %
    %     [1,2,3][-1] % => 3
    %     [1,2,3][-2] % => 2
    %     [1,2,3][-3] % => 1
    %     [1,2,3][-43] % => nil
    %
    def [](number)
      if number < 0
        brackets(-1 * (1 + number), Erlang.lists.reverse(self))
      else
        brackets(number, self)
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

    % Receives a list of lists and flatten them one level deep. If one of the
    % elements of the list is not a list, raises an error. This has much better
    % performance than the original flatten.
    %
    % ## Examples
    %
    %     [[1],[2],[3]].flatten_lists    % => [1,2,3]
    %     [[1],[[2]],[3]].flatten_lists  % => [1,[2],3]
    %
    def flatten_lists
      Erlang.lists.append(self)
    end

    % Removes all duplicated elements from a list.
    def uniq
      uniq(self, [])
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

    % Returns a list with its elements in reverse order.
    %
    % ## Examples
    %
    %    [1,2,3].reverse % => [3,2,1]
    %
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

    % Deletes all item from the list matching the given argument.
    %
    % ## Examples
    %
    %     [1,2,1,3].delete(1) % => [3,2]
    %
    def delete_all(item)
      Erlang.sets.to_list(Erlang.sets.del_element(item, Erlang.sets.from_list(self)))
    end

    % Returns a string created by converting each items of the list
    % to a string, separated by the given string
    %
    % ## Examples
    %
    %    [1,2,3].join(",")      % => "1,2,3"
    %    ['foo, 'bar].join("_") % => "foo_bar"
    %
    def join(string)
      strings = map -> (x) x.to_s.to_char_list
      Erlang.string.join(strings, string.to_char_list).to_bin
    end

    % Returns the sorted list
    %
    % ## Examples
    %
    %    [4,1,3,2,4].sort      % => [1,2,3,4,4]
    %    ["foo", "bar", "baz"] % => ["bar", "baz", "foo"]
    %
    def sort
      Erlang.lists.sort(self)
    end

    % Takes elements from the list while the function returns true.
    %
    % ## Examples
    %
    %    [1,2,3,4,5].takewhile(-> (x) x < 3) % => [1,2]
    %
    def takewhile(function)
      Erlang.lists.takewhile(function, self)
    end

    % Split the list into two list, where the first contains N elements
    % and the second the rest.
    %
    % Raise an error if position is out of bound.
    %
    % ## Examples
    %
    %    [1,2,3,4,5].split(3) % => {[1,2,3], [4,5]}
    def split(n)
      Erlang.lists.split(n, self)
    end

    % Returns a new list with item inserted at position n.
    %
    % Raise an error if position is out of bound.
    %
    % ## Examples
    %
    %    [1,2,3,4,5].insert(0, 2) % => [1,2,0,3,4,5]
    %
    def insert(item, n)
      {h,t} = split(n)
      h + [item] + t
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

    def brackets(0, [h|_])
      h
    end

    def brackets(_, [])
      nil
    end

    def brackets(n, [_|t]) when n > 0
      brackets(n - 1, t)
    end

    def uniq([h|t], acc)
      case Erlang.lists.member(h, acc)
      match true
        uniq(t, acc)
      match false
        uniq(t, [h|acc])
      end
    end

    def uniq([], acc)
      Erlang.lists.reverse(acc)
    end

    def inspect_join(list)
      strings = list.map -> (x) x.inspect.to_char_list
      Erlang.string.join(strings, [$,]).to_bin
    end

    def list_tail([_|t]) list_tail(t); end
    def list_tail([]) []; end
    def list_tail(object) object; end

    def copy_without_tail([h|t], acc) copy_without_tail(t, [h|acc]); end
    def copy_without_tail(_, acc) acc.reverse; end
  end
end
