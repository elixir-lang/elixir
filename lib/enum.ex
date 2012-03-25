defprotocol Enum.Iterator, [iterator(collection)], only: [List, Record], as: I

defmodule Enum do
  @moduledoc """
  Evalutes the items in the given collection according to the
  Enum.Iterator protocol. Most functions in this module
  will automatically retrieve the protocol given the collection
  and iterator, for example:

      Enum.map [1,2,3], fun(x, do: x * 2)

  However, one can use their own iteration function for any
  collection by passing the iterator function as the first
  argument:

      Enum.map my_iteration_function, [1,2,3], fun(x, do: x * 2)

  ## The protocol

  When `Enum.map` is invoked without the iterator function,
  it invokes `Enum.Iterator.iterator(collection)` with the
  given collection in order to retrieve the default iterator
  for that collection. You can implement the protocol for any
  data type you wish. Elixir ships with a default iterator
  for lists, implemented as follow:

      defimpl Enum.Iterator, for: List do
        def iterator(list), do: { iterate(&1), iterate(list) }

        defp iterate([h|t]) do
          { h, t }
        end

        defp iterate([]) do
          :stop
        end
      end

  The :stop is a special Elixir token that
  marks when iteration should finish.
  """

  @doc """
  Invokes the given `fun` for each item in the `collection`
  checking if all results evalutes to true. If any does not,
  abort and return false. Otherwise, true.

  ## Examples

      Enum.all? [2,4,6], fn(x, do: rem(x, 2) == 0)
      #=> true

      Enum.all? [2,3,4], fn(x, do: rem(x, 2) == 0)
      #=> false

  If no function is given, it defaults to checking if
  all items in the collection evalutes to true.

      Enum.all? [1,2,3]   #=> true
      Enum.all? [1,nil,3] #=> false

  """
  def all?(collection, fun // fn(x, do: x)) do
    { iterator, pointer } = I.iterator(collection)
    all?(iterator, pointer, fun)
  end

  def all?(iterator, pointer, fun) do
    do_all?(pointer, iterator, fun)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`
  checking if any of the results returns true. If one does,
  aborts and returns true. If not, returns false.

  ## Examples

      Enum.any? [2,4,6], fn(x, do: rem(x, 2) == 1)
      #=> false

      Enum.any? [2,3,4], fn(x, do: rem(x, 2) == 1)
      #=> true

  If no function is given, it defaults to checking if
  any item in the collection evalutes to true.

      Enum.any? [false,false,false] #=> false
      Enum.any? [false,true,false]  #=> true

  """
  def any?(collection, fun // fn(x, do: x)) do
    { iterator, pointer } = I.iterator(collection)
    any?(iterator, pointer, fun)
  end

  def any?(iterator, pointer, fun) do
    do_any?(pointer, iterator, fun)
  end

  @doc """
  Drops the first *count* items from the collection.

  ## Examples

      Enum.drop [1,2,3], 2  #=> [3]
      Enum.drop [1,2,3], 10 #=> []
      Enum.drop [1,2,3], 0  #=> [1,2,3]

  """
  def drop(collection, count) do
    { iterator, pointer } = I.iterator(collection)
    drop(iterator, pointer, count)
  end

  def drop(iterator, pointer, count) do
    elem split(iterator, pointer, count), 2
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`.
  Returns the `collection` itself.

  ## Examples

      Enum.each ['some', 'example'], fn(x, do: IO.puts x)

  """
  def each(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    each(iterator, pointer, fun)
    collection
  end

  def each(iterator, pointer, fun) do
    do_each(pointer, iterator, fun)
    pointer
  end

  @doc """
  Returns all the entries in the collection. It is the equivalent
  to calling map with an identify function.

  ## Examples

      Enum.entries [1,2,3] #=> [1,2,3]

  """
  def entries(collection) when is_list(collection) do
    collection
  end

  def entries(collection) do
    map(collection, fn(x) -> x end)
  end

  @doc """
  Returns if the collection is empty or not.

  ## Examples

      Enum.empty? [] #=> true
      Enum.empty? [1,2,3] #=> false

  """
  def empty?(collection) when is_list(collection) do
    collection == []
  end

  def empty?(collection) do
    { iterator, pointer } = I.iterator(collection)
    empty?(iterator, pointer)
  end

  def empty?(_iterator, pointer) do
    pointer == :stop
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`.
  Returns only the items the function evalutes to true.

  ## Examples

      Enum.filter [1, 2, 3], fn(x, do: rem(x, 2) == 0)
      #=> [2]

  """
  def filter(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    filter(iterator, pointer, fun)
  end

  def filter(iterator, pointer, fun) do
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
    filter_map(iterator, pointer, filter, mapper)
  end

  def filter_map(iterator, pointer, filter, mapper) do
    do_filter_map(pointer, iterator, filter, mapper)
  end

  @doc """
  Invokes the `fun` for each item in collection
  and returns the first the function returns a truthy
  value. If no item is found, returns `ifnone`.

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
    find(iterator, pointer, ifnone, fun)
  end

  def find(iterator, pointer, ifnone, fun) do
    do_find(pointer, iterator, ifnone, fun)
  end

  @doc """
  Similar to find, but returns the value of the function
    invocation instead of the element iterated.

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
    find_value(iterator, pointer, ifnone, fun)
  end

  def find_value(iterator, pointer, ifnone, fun) do
    do_find_value(pointer, iterator, ifnone, fun)
  end

  @doc """
  Join the given `collection` according to `joiner`.
  Joiner can be either a binary or a list and the
  result will be of the same type of joiner. If
  joiner is not passed at all, it defaults to an
  empty binary.

  All items in the collection must be convertable
  to binary, otherwise an error is raised.

  ## Examples

      Enum.join([1,2,3]) => "123"
      Enum.join([1,2,3], " = ") #=> "1 = 2 = 3"
      Enum.join([1,2,3], ' = ') #=> '1 = 2 = 3'

  """
  def join(collection, joiner // "") do
    { iterator, pointer } = I.iterator(collection)
    join(iterator, pointer, joiner)
  end

  def join(iterator, collection, joiner) when is_list(joiner) do
    binary_to_list join(iterator, collection, list_to_binary(joiner))
  end

  def join(iterator, pointer, joiner) do
    do_join(pointer, iterator, joiner, nil)
  end

  @doc """
  Finds the first item in collection of tuples where the element
    `position` in the tuple is equal to `key`. If none is found,
    returns `default` (which defaults to nil).

    ## Examples

        list = [{:a,1},{:b,2},{:a,3}]
        Enum.keyfind list, :a, 1 #=> {:a, 1}

  """
  def keyfind(collection, key, position, default) when is_list(collection) do
    :lists.keyfind(key, position, collection) || default
  end

  def keyfind(collection, key, position, default // nil) do
    { iterator, pointer } = I.iterator(collection)
    keyfind(iterator, pointer, key, position, default)
  end

  def keyfind(iterator, pointer, key, position, default) do
    do_keyfind(pointer, iterator, key, position, default)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`.
    Returns the result of all function calls.

    ## Examples

        Enum.map [1, 2, 3], fn(x, do: x * 2)
        #=> [2, 4, 6]

  """
  def map(collection, fun) when is_list(collection) do
    lc item in collection, do: fun.(item)
  end

  def map(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    map(iterator, pointer, fun)
  end

  def map(iterator, pointer, fun) do
    do_map(pointer, iterator, fun)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`
  while also keeping an accumulator. Returns a tuple where
  the first element is the iterated collection and the second
  one is the final accumulator.

  ## Examples

      Enum.map_reduce [1, 2, 3], 0, fn(x, acc, do: { x * 2, x + acc })
      #=> { [2, 4, 6], 6 }

  """
  def map_reduce(collection, acc, f) when is_list(collection) do
    :lists.mapfoldl(f, acc, collection)
  end

  def map_reduce(collection, acc, fun) do
    { iterator, pointer } = I.iterator(collection)
    map_reduce(iterator, pointer, acc, fun)
  end

  def map_reduce(iterator, pointer, acc, fun) do
    do_map_reduce(pointer, iterator, acc, fun)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`
  partitioning it in two lists.

  ## Examples

      Enum.partition [1, 2, 3], fn(x, do: rem(x, 2) == 0)
      #=> { [2], [1,3] }

  """
  def partition(collection, fun) do
    { iterator, pointer } = I.iterator(collection)
    partition(iterator, pointer, fun)
  end

  def partition(iterator, pointer, fun) do
    do_partition(pointer, iterator, fun, [], [])
  end

  @doc """
  Iterates the collection passing an accumulator as parameter.
  Returns the accumulator.

  ## Examples

      Enum.reduce [1, 2, 3], 0, fn(x, acc, do: x + acc)
      #=> 6

  """
  def reduce(collection, acc, f) when is_list(collection) do
    :lists.foldl(f, acc, collection)
  end

  def reduce(collection, acc, f) do
    { iterator, pointer } = I.iterator(collection)
    reduce(iterator, pointer, acc, f)
  end

  def reduce(iterator, pointer, acc, f) do
    do_reduce(pointer, iterator, acc, f)
  end

  @doc """
  Splits the enumerable in the given counter.

  ## Examples

      Enum.split [1,2,3], 2  #=> { [1,2], [3] }
      Enum.split [1,2,3], 10 #=> { [1,2,3], [] }
      Enum.split [1,2,3], 0  #=> { [], [1,2,3] }

  """
  def split(collection, count) do
    { iterator, pointer } = I.iterator(collection)
    split(iterator, pointer, count)
  end

  def split(iterator, pointer, count) when count >= 0 do
    do_split(pointer, iterator, count, [])
  end

  @doc """
  Takes the first *count* items from the collection.

  ## Examples

      Enum.take [1,2,3], 2  #=> [1,2]
      Enum.take [1,2,3], 10 #=> [1,2,3]
      Enum.take [1,2,3], 0  #=> []

  """
  def take(collection, count) do
    { iterator, pointer } = I.iterator(collection)
    take(iterator, pointer, count)
  end

  def take(iterator, pointer, count) do
    elem split(iterator, pointer, count), 1
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
  to n. Also has an accumulator similar to fold to store the
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
    match: false
      false
    match: nil
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
    match: false
      do_any?(iterator.(next), iterator, fun)
    match: nil
      do_any?(iterator.(next), iterator, fun)
    else:
      true
    end
  end

  defp do_any?(:stop, _, _) do
    false
  end

  ## find

  defp do_find({ h, next }, iterator, ifnone, fun) do
    case fun.(h) do
    match: false
      do_find(iterator.(next), iterator, ifnone, fun)
    match: nil
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
    match: false
      do_find_value(iterator.(next), iterator, ifnone, fun)
    match: nil
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
    match: false
      do_filter(iterator.(next), iterator, fun)
    match: nil
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
    match: false
      do_filter_map(iterator.(next), iterator, filter, mapper)
    match: nil
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

  defp do_map_reduce({ h, next }, iterator, acc, f) do
    { result, acc } = f.(h, acc)
    { rest, acc }   = do_map_reduce(iterator.(next), iterator, acc, f)
    { [result|rest], acc }
  end

  defp do_map_reduce(:stop, _, acc, _f) do
    { [], acc }
  end

  ## partition

  defp do_partition({ h, next }, iterator, fun, acc1, acc2) do
    case fun.(h) do
    match: false
      do_partition(iterator.(next), iterator, fun, acc1, [h|acc2])
    match: nil
      do_partition(iterator.(next), iterator, fun, acc1, [h|acc2])
    else:
      do_partition(iterator.(next), iterator, fun, [h|acc1], acc2)
    end
  end

  defp do_partition(:stop, _, _, acc1, acc2) do
    { :lists.reverse(acc1), :lists.reverse(acc2) }
  end

  ## split

  defp do_split({ h, next }, iterator, counter, acc) when counter > 0 do
    do_split(iterator.(next), iterator, counter - 1, [h|acc])
  end

  defp do_split({ h, next }, _iterator, 0, acc) when is_list(next) do
    { List.reverse(acc), [h|next] }
  end

  defp do_split({ h, next }, iterator, 0, acc) do
    { List.reverse(acc), [h|map(iterator, next, fn(x) -> x end)] }
  end

  defp do_split(:stop, _, _, acc) do
    { List.reverse(acc), [] }
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
  def iterator(list), do: { iterate(&1), iterate(list) }

  defp iterate([h|t]) do
    { h, t }
  end

  defp iterate([]) do
    :stop
  end
end
