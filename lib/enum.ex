defprotocol Enum.Iterator do
  @moduledoc """
  This is the protocol used by the `Enum` module.
  Usually, when you invoke a function in the module `Enum`,
  the first argument passed to `Enum` is a collection which
  is forwarded to this protocol in order to retrieve information
  on how to iterate the collection. That said, when:

      Enum.map [1,2,3], &1 * 2

  Is invoked, it invokes Enum.Iterator.iterator([1,2,3])
  which returns all the information required by Enum.
  Read each function documentation below for more information.
  """

  @only [List, Record]

  @doc """
  Iteration in Elixir happens with the help of a iterator
  function. Every time this function is called, it must
  return a tuple with two elements. The first element
  is the next item and the second can be any Elixir term
  which the function is going to receive as argument the
  next time it is invoked.

  When there are no more items to be iterated, the function
  must return the atom `:stop`.

  In order to retrieve this iterator function, Elixir invokes
  `Enum.Iterator.iterator(collection)` which should return a
  tuple with two elements: the first element is the iterator
  function and the second is the first step of iteration.

  As an example, here is the implementation of iterator for lists:

      def iterator(list),   do: { iterate(&1), iterate(list) }
      defp iterate([h|t]),  do: { h, t }
      defp iterate([]),     do: :stop

  """
  def iterator(collection)

  @doc """
  This function is responsible to receive a collection and
  convert it to a list. It is called directly by `Enum.to_list`.
  An implementation may choose to raise an error if it cannot
  be converted to list.
  """
  def to_list(collection)
end

defprotocol Enum.OrdIterator do
  @moduledoc """
  This protocol is invoked by some functions in Enum which
  requires ordering in order to function correctly. For
  instance, `Enum.split_with/2`, `Enum.take_while` all rely
  on this protocol
  """

  @only [List, Record]

  @doc """
  Must return a tuple under the same conditions as
  `Enum.Iterator.iterator`.
  """
  def iterator(collection)

  @doc """
  On each step, the iterator function returned by `iterator/1`
  returns a tuple with two elements. This function receives
  those two elements and must return a list back.

  This is used in order to quicky return a list from any point
  during iteration. For example, consider the function `Enum.drop`.
  `Enum.drop collection, 3` should drop 3 items and return a list
  back. While we could loop over the remaining items to get a list
  back, this function is invoked allowing us to get a result
  back without a need to loop the remaining items.
  """
  def to_list(current, next)
end

defmodule Enum do
  require Enum.Iterator, as: I
  require Enum.OrdIterator, as: O

  @moduledoc """
  Provides a set of algorithms that enumerate over collections according to the
  `Enum.Iterator` protocol. Most of the functions in this module have two
  flavours. If a given collection implements the mentioned protocol (like
  list, for instance), you can do

      Enum.map [1,2,3], fn(x, do: x * 2)

  Depending on the type of the collection, the user-provided function will
  accept a certain type of argument. For dicts, the argument is always a
  `{ key, value }` tuple.
  """

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns true if
  each invocation returns true as well, otherwise it shirt-circuits and returns
  false.

  ## Examples

      Enum.all? [2,4,6], fn(x, do: rem(x, 2) == 0)
      #=> true

      Enum.all? [2,3,4], fn(x, do: rem(x, 2) == 0)
      #=> false

  If no function is given, it defaults to checking if
  all items in the collection evaluate to true.

      Enum.all? [1,2,3]   #=> true
      Enum.all? [1,nil,3] #=> false

  """
  def all?(collection, fun // fn(x, do: x)) do
    { iterator, pointer } = I.iterator(collection)
    do_all?(pointer, iterator, fun)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns true if
  at least one invocation returns true. Returns false otherwise.

  ## Examples

      Enum.any? [2,4,6], fn(x, do: rem(x, 2) == 1)
      #=> false

      Enum.any? [2,3,4], fn(x, do: rem(x, 2) == 1)
      #=> true

  If no function is given, it defaults to checking if
  at least one item in the collection evaluates to true.

      Enum.any? [false,false,false] #=> false
      Enum.any? [false,true,false]  #=> true

  """
  def any?(collection, fun // fn(x, do: x)) do
    { iterator, pointer } = I.iterator(collection)
    do_any?(pointer, iterator, fun)
  end

  @doc """
  Drops the first `count` items from the collection. Expects an ordered
  collection.

  ## Examples

      Enum.drop [1,2,3], 2  #=> [3]
      Enum.drop [1,2,3], 10 #=> []
      Enum.drop [1,2,3], 0  #=> [1,2,3]

  """
  def drop(collection, count) do
    elem split(collection, count), 2
  end

  @doc """
  Drops items at the beginning of `collection` while `fun` returns true.
  Expects an ordered collection.

  ## Examples

      Enum.drop_while [1,2,3,4,5], fn(x, do: x < 3)
      #=> [3,4,5]
  """
  def drop_while(collection, fun) do
    { iterator, pointer } = O.iterator(collection)
    module = O.__impl_for__!(collection)
    do_drop_while(pointer, iterator, fun, module)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`.
  Returns the `collection` itself.

  ## Examples

      Enum.each ['some', 'example'], fn(x, do: IO.puts x)

  """
  def each(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_each(pointer, iterator, fun)
    collection
  end

  @doc """
  Returns a list of all the entries in the collection.

  ## Examples

      Enum.to_list [1,2,3] #=> [1,2,3]

  """
  def to_list(collection) when is_list(collection) do
    collection
  end

  def to_list(collection) do
    I.to_list(collection)
  end

  @doc """
  Returns true if the collection is empty, otherwise false.

  ## Examples

      Enum.empty? []      #=> true
      Enum.empty? [1,2,3] #=> false

  """
  def empty?(collection) when is_list(collection) do
    collection == []
  end

  def empty?(collection) do
    { _iterator, pointer } = I.iterator(collection)
    pointer === :stop
  end

  @doc """
  Filters the collection, i.e. returns only those elements
  for which `fun` returns true.

  ## Examples

      Enum.filter [1, 2, 3], fn(x, do: rem(x, 2) == 0)
      #=> [2]

  """
  def filter(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_filter(pointer, iterator, fun)
  end

  @doc """
  Filters the collection and maps its values in one pass.

  ## Examples

      Enum.filter_map [1, 2, 3], fn(x, do: rem(x, 2) == 0), &1 * 2
      #=> [4]

  """
  def filter_map(collection, filter, mapper) do
    { iterator, pointer } = I.iterator(collection)
    do_filter_map(pointer, iterator, filter, mapper)
  end

  @doc """
  Returns the first item for which `fun` returns a truthy value. If no such
  item is found, returns `ifnone`.

  ## Examples

      Enum.find [2,4,6], fn(x, do: rem(x, 2) == 1)
      # => nil

      Enum.find [2,4,6], 0, fn(x, do: rem(x, 2) == 1)
      # => 0

      Enum.find [2,3,4], fn(x, do: rem(x, 2) == 1)
      # => 3

  """
  def find(collection, ifnone // nil, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_find(pointer, iterator, ifnone, fun)
  end

  @doc """
  Similar to find, but returns the value of the function
  invocation instead of the element itself.

    ## Examples

        Enum.find_value [2,4,6], fn(x, do: rem(x, 2) == 1)
        # => nil

        Enum.find_value [2,4,6], 0, fn(x, do: rem(x, 2) == 1)
        # => 0

        Enum.find_value [2,3,4], fn(x, do: rem(x, 2) == 1)
        # => true

  """
  def find_value(collection, ifnone // nil, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_find_value(pointer, iterator, ifnone, fun)
  end

  @doc """
  Joins the given `collection` according to `joiner`.
  Joiner can be either a binary or a list and the
  result will be of the same type as joiner. If
  joiner is not passed at all, it defaults to an
  empty binary.

  All items in the collection must be convertible
  to binary, otherwise an error is raised.

  ## Examples

      Enum.join([1,2,3])        #=> "123"
      Enum.join([1,2,3], " = ") #=> "1 = 2 = 3"
      Enum.join([1,2,3], ' = ') #=> '1 = 2 = 3'

  """
  def join(collection, joiner // "") do
    { iterator, pointer } = I.iterator(collection)
    join(iterator, pointer, joiner)
  end

  defp join(iterator, collection, joiner) when is_list(joiner) do
    binary_to_list join(iterator, collection, list_to_binary(joiner))
  end

  defp join(iterator, pointer, joiner) do
    do_join(pointer, iterator, joiner, nil)
  end

  @doc """
  Finds the first item in `collection` of tuples where the element
  `position` in the tuple is equal to `key`. If none is found,
  returns `default` (which defaults to nil).

  Usage for dicts is limited and is discouraged. Use Enum.find instead.

  ## Examples

      list = [{:a,1},{:b,2},{:a,3}]
      Enum.keyfind list, :a, 1 #=> {:a, 1}
      Enum.keyfind list, 3, 2  #=> {:a, 3}

  """
  def keyfind(collection, key, position, default // nil)

  def keyfind(collection, key, position, default) when is_list(collection) do
    :lists.keyfind(key, position, collection) || default
  end

  def keyfind(collection, key, position, default) do
    { iterator, pointer } = I.iterator(collection)
    do_keyfind(pointer, iterator, key, position, default)
  end

  @doc """
  Returns a new collection, where each item is the result
  of invoking `fun` on each corresponding item of `collection`.

  For dicts, the function accepts a key-value tuple and it should return a
  tuple as well.

  ## Examples

      Enum.map [1, 2, 3], fn(x, do: x * 2)
      #=> [2, 4, 6]

      Enum.map [a: 1, b: 2], fn({k, v}, do: { k, -v })
      #=> [a: -1, b: -2]

  """
  def map(collection, fun) when is_list(collection) do
    lc item in collection, do: fun.(item)
  end

  def map(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_map(pointer, iterator, fun)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`
  while also keeping an accumulator. Returns a tuple where
  the first element is the mapped collection and the second
  one is the final accumulator.

  For dicts, the first tuple element has to be a { key, value }
  tuple itself.

  ## Examples

      Enum.map_reduce [1, 2, 3], 0, fn(x, acc, do: { x * 2, x + acc })
      #=> { [2, 4, 6], 6 }

  """
  def map_reduce(collection, acc, f) when is_list(collection) do
    :lists.mapfoldl(f, acc, collection)
  end

  def map_reduce(collection, acc, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_map_reduce(pointer, iterator, [], acc, fun)
  end

  @doc """
  Partitions `collection` into two where the first one contains elements
  for which `fun` returns a truthy value, and the second one -- for which `fun`
  returns false or nil.

  ## Examples

      Enum.partition [1, 2, 3], fn(x, do: rem(x, 2) == 0)
      #=> { [2], [1,3] }

  """
  def partition(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_partition(pointer, iterator, fun, [], [])
  end

  @doc """
  Invokes `fun` for each element in the collection passing the accumulator
  `acc` and the element as arguments. The return value is stored in `acc`.
  Returns the accumulator.

  ## Examples

      Enum.reduce [1, 2, 3], 0, fn(x, acc, do: x + acc)
      #=> 6

  """
  def reduce(collection, acc, fun) when is_list(collection) do
    :lists.foldl(fun, acc, collection)
  end

  def reduce(collection, acc, fun) do
    { iterator, pointer } = I.iterator(collection)
    do_reduce(pointer, iterator, acc, fun)
  end

  @doc """
  Splits the enumerable into two collections, leaving `count` elements in the
  first one. Expects an ordered collection.

  ## Examples

      Enum.split [1,2,3], 2  #=> { [1,2], [3] }
      Enum.split [1,2,3], 10 #=> { [1,2,3], [] }
      Enum.split [1,2,3], 0  #=> { [], [1,2,3] }

  """
  def split(collection, count) when count >= 0 do
    { iterator, pointer } = O.iterator(collection)
    module = O.__impl_for__!(collection)
    do_split(pointer, iterator, count, [], module)
  end

  @doc """
  Splits `collection` at the first element, for which `fun` returns true.
  Expects an ordered collection.

  ## Examples

      Enum.split_with [1,2,3,4], fn(x) -> x == 2 end
      #=> { [1], [2, 3, 4] }
  """
  def split_with(collection, fun) do
    { iterator, pointer } = O.iterator(collection)
    module = O.__impl_for__!(collection)
    do_split_with(pointer, iterator, fun, [], module)
  end

  @doc """
  Takes the first `count` items from the collection. Expects an ordered
  collection.

  ## Examples

      Enum.take [1,2,3], 2  #=> [1,2]
      Enum.take [1,2,3], 10 #=> [1,2,3]
      Enum.take [1,2,3], 0  #=> []

  """
  def take(collection, count) do
    elem split(collection, count), 1
  end

  @doc """
  Takes the items at the beginning of `collection` while `fun` returns true.
  Expects an ordered collection.

  ## Examples

      Enum.take_while [1,2,3], fn(x, do: x < 3)
      #=> [1, 2]

  """
  def take_while(collection, fun // fn(x, do: x)) do
    { iterator, pointer } = O.iterator(collection)
    do_take_while(pointer, iterator, fun)
  end

  @doc """
  Iterates the given function n times, passing values from 1
  to n.

  ## Examples

      Enum.times 3, fn(x, do: IO.puts x)
      1
      2
      3

  """
  def times(times, function) when times >= 0 do
    case is_function(function, 0) do
    match: true
      do_times_0(times, 1, function)
    else:
      do_times_1(times, 1, function)
    end
    times
  end

  @doc """
  Iterates the given function n times, passing values from 1
  to n. Also has an accumulator similar to reduce to store the
  value between computations.

  ## Examples

      Enum.times 5, 0, fn(x, acc, do: acc + x)
      #=> 15

  """
  def times(times, acc, function) when times >= 0 do
    do_times_2(times, 1, function, acc)
  end

  ## Implementations

  ## all?

  defp do_all?({ h, next }, iterator, fun) do
    case fun.(h) do
    match: x when x == false or x == nil
      false
    else:
      do_all?(iterator.(next), iterator, fun)
    end
  end

  defp do_all?(:stop, _, _) do
    true
  end

  ## any?

  defp do_any?({ h, next }, iterator, fun) do
    case fun.(h) do
    match: x when x == false or x == nil
      do_any?(iterator.(next), iterator, fun)
    else:
      true
    end
  end

  defp do_any?(:stop, _, _) do
    false
  end

  ## drop_while

  defp do_drop_while({ h, next }, iterator, fun, module) do
    case fun.(h) do
    match: x when x == false or x == nil
      module.to_list(h, next)
    else:
      do_drop_while(iterator.(next), iterator, fun, module)
    end
  end

  defp do_drop_while(:stop, _, _, _) do
    []
  end

  ## find

  defp do_find({ h, next }, iterator, ifnone, fun) do
    case fun.(h) do
    match: x when x == false or x == nil
      do_find(iterator.(next), iterator, ifnone, fun)
    else:
      h
    end
  end

  defp do_find(:stop, _, ifnone, _) do
    ifnone
  end

  ## find_value

  defp do_find_value({ h, next }, iterator, ifnone, fun) do
    case fun.(h) do
    match: x when x == false or x == nil
      do_find_value(iterator.(next), iterator, ifnone, fun)
    match: other
      other
    end
  end

  defp do_find_value(:stop, _, ifnone, _) do
    ifnone
  end

  ## each

  defp do_each({ h, next }, iterator, fun) do
    fun.(h)
    do_each(iterator.(next), iterator, fun)
  end

  defp do_each(:stop, _, _) do
    []
  end

  ## filter

  defp do_filter({ h, next }, iterator, fun) do
    case fun.(h) do
    match: x when x == false or x == nil
      do_filter(iterator.(next), iterator, fun)
    else:
      [h|do_filter(iterator.(next), iterator, fun)]
    end
  end

  defp do_filter(:stop, _, _) do
    []
  end

  ## filter_map

  defp do_filter_map({ h, next }, iterator, filter, mapper) do
    case filter.(h) do
    match: x when x == false or x == nil
      do_filter_map(iterator.(next), iterator, filter, mapper)
    else:
      [mapper.(h)|do_filter_map(iterator.(next), iterator, filter, mapper)]
    end
  end

  defp do_filter_map(:stop, _, _, _) do
    []
  end

  ## reduce

  defp do_reduce({ h, next }, iterator, acc, fun) do
    do_reduce(iterator.(next), iterator, fun.(h, acc), fun)
  end

  defp do_reduce(:stop, _, acc, _) do
    acc
  end

  ## split_with

  defp do_split_with({ h, next }, iterator, fun, acc, module) do
    case fun.(h) do
    match: x when x == false or x == nil
      do_split_with(iterator.(next), iterator, fun, [h|acc], module)
    else:
      { List.reverse(acc), module.to_list(h, next) }
    end
  end

  defp do_split_with(:stop, _, _, acc, _module) do
    { List.reverse(acc), [] }
  end

  ## join

  # The first item is simply stringified unless ...
  defp do_join({ h, next }, iterator, joiner, nil) do
    do_join(iterator.(next), iterator, joiner, to_binary(h))
  end

  # The first item is :stop, then we return an empty string;
  defp do_join(:stop, _, _joiner, nil) do
    ""
  end

  # All other items are concatenated to acc, by first adding the joiner;
  defp do_join({ h, next }, iterator, joiner, acc) do
    acc = << acc | :binary, joiner | :binary, to_binary(h) | :binary >>
    do_join(iterator.(next), iterator, joiner, acc)
  end

  # Until we have to stop iteration, then we return acc.
  defp do_join(:stop, _, _joiner, acc) do
    acc
  end

  ## keyfind

  defp do_keyfind({ h, _next }, _iterator, key, position, _ifnone) when elem(h, position) == key do
    h
  end

  defp do_keyfind({ _h, next }, iterator, key, position, ifnone) do
    do_keyfind(iterator.(next), iterator, key, position, ifnone)
  end

  defp do_keyfind(:stop, _, _, _, ifnone) do
    ifnone
  end

  ## map

  defp do_map({ h, next }, iterator, fun) do
    [fun.(h)|do_map(iterator.(next), iterator, fun)]
  end

  defp do_map(:stop, _, _) do
    []
  end

  ## map_reduce

  defp do_map_reduce({ h, next }, iterator, list_acc, acc, f) do
    { result, acc } = f.(h, acc)
    do_map_reduce(iterator.(next), iterator, [result|list_acc], acc, f)
  end

  defp do_map_reduce(:stop, _, list_acc, acc, _f) do
    { List.reverse(list_acc), acc }
  end

  ## partition

  defp do_partition({ h, next }, iterator, fun, acc1, acc2) do
    case fun.(h) do
    match: x when x == false or x == nil
      do_partition(iterator.(next), iterator, fun, acc1, [h|acc2])
    else:
      do_partition(iterator.(next), iterator, fun, [h|acc1], acc2)
    end
  end

  defp do_partition(:stop, _, _, acc1, acc2) do
    { List.reverse(acc1), List.reverse(acc2) }
  end

  ## split

  defp do_split({ h, next }, iterator, counter, acc, module) when counter > 0 do
    do_split(iterator.(next), iterator, counter - 1, [h|acc], module)
  end

  defp do_split({ h, next }, _iterator, 0, acc, module) do
    { List.reverse(acc), module.to_list(h, next) }
  end

  defp do_split(:stop, _, _, acc, _module) do
    { List.reverse(acc), [] }
  end

  ## take_while

  defp do_take_while({ h, next }, iterator, fun) do
    case fun.(h) do
    match: x when x == false or x == nil
      []
    else:
      [h|do_take_while(iterator.(next), iterator, fun)]
    end
  end

  defp do_take_while(:stop, _, _) do
    []
  end

  ## times

  defp do_times_0(limit, counter, _function) when counter > limit do
  end

  defp do_times_0(limit, counter, function) do
    function.()
    do_times_0(limit, 1 + counter, function)
  end

  defp do_times_1(limit, counter, _function) when counter > limit do
  end

  defp do_times_1(limit, counter, function) do
    function.(counter)
    do_times_1(limit, 1 + counter, function)
  end

  defp do_times_2(limit, counter, _function, acc) when counter > limit do
    acc
  end

  defp do_times_2(limit, counter, function, acc) do
    new_acc = function.(counter, acc)
    do_times_2(limit, 1 + counter, function, new_acc)
  end
end

defimpl Enum.Iterator, for: List do
  def iterator(list),   do: { iterate(&1), iterate(list) }
  def to_list(list),    do: list

  defp iterate([h|t]),  do: { h, t }
  defp iterate([]),     do: :stop # The :stop atom is the end of the iteration.
end

defimpl Enum.OrdIterator, for: List do
  def iterator(list) do
    Enum.Iterator.List.iterator(list)
  end

  def to_list(h, next), do: [h|next]
end

defimpl Enum.Iterator, for: HashDict.Record do
  def iterator(dict),   do: Enum.Iterator.List.iterator(to_list(dict))
  def to_list(dict),    do: Dict.HashDict.Record.to_list(dict)
end

defimpl Enum.Iterator, for: Orddict.Record do
  def iterator(dict),   do: Enum.Iterator.List.iterator(to_list(dict))
  def to_list(dict),    do: Dict.Orddict.Record.to_list(dict)
end

defimpl Enum.OrdIterator, for: Orddict.Record do
  def iterator(dict) do
    Enum.Iterator.Orddict.Record.iterator(dict)
  end

  def to_list(h, next), do: [h|next]
end
