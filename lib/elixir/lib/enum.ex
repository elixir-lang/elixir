defprotocol Enum.Iterator do
  @moduledoc """
  This is the protocol used by the `Enum` module.
  Usually, when you invoke a function in the module `Enum`,
  the first argument passed to `Enum` is a collection which
  is forwarded to this protocol in order to retrieve information
  on how to iterate the collection. That said, when:

      Enum.map [1,2,3], &1 * 2

  Is invoked, it invokes `Enum.Iterator.iterator([1,2,3])`
  which returns all the information required by Enum.
  Read each function documentation below for more information.
  """

  @only [List, Record, Function]

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

  ## Iterating lists

  If a data structure needs to be converted to a list in order
  to be iterated, the iterator function can simply return the
  list and the Enum module will be able to take over the list
  and retrieve the proper iterator function.
  """
  def iterator(collection)

  @doc """
  The function used to retrieve the collection size.
  """
  def count(collection)
end

defmodule Enum do
  alias Enum.Iterator, as: I

  @moduledoc """
  Provides a set of algorithms that enumerate over collections according to the
  `Enum.Iterator` protocol. Most of the functions in this module have two
  flavours. If a given collection implements the mentioned protocol (like
  list, for instance), you can do:

      Enum.map [1,2,3], fn(x) -> x * 2 end

  Depending on the type of the collection, the user-provided function will
  accept a certain type of argument. For dicts, the argument is always a
  `{ key, value }` tuple.
  """

  @type t :: Enum.Iterator.t
  @type element :: any

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns true if
  each invocation returns true as well, otherwise it short-circuits and returns
  false.

  ## Examples

      Enum.all? [2,4,6], fn(x) -> rem(x, 2) == 0 end
      #=> true

      Enum.all? [2,3,4], fn(x) -> rem(x, 2) == 0 end
      #=> false

  If no function is given, it defaults to checking if
  all items in the collection evaluate to true.

      Enum.all? [1,2,3]   #=> true
      Enum.all? [1,nil,3] #=> false

  """
  @spec all?(t) :: boolean
  @spec all?(t, (element -> boolean)) :: boolean

  def all?(collection, fun // fn(x) -> x end)

  def all?(collection, fun) when is_list(collection) do
    do_all?(collection, fun)
  end

  def all?(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_all?(pointer, iterator, fun)
      list when is_list(list) ->
        do_all?(list, fun)
    end
  end

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns true if
  at least one invocation returns true. Returns false otherwise.

  ## Examples

      Enum.any? [2,4,6], fn(x) -> rem(x, 2) == 1 end
      #=> false

      Enum.any? [2,3,4], fn(x) -> rem(x, 2) == 1 end
      #=> true

  If no function is given, it defaults to checking if
  at least one item in the collection evaluates to true.

      Enum.any? [false,false,false] #=> false
      Enum.any? [false,true,false]  #=> true

  """
  @spec any?(t) :: boolean
  @spec any?(t, (element -> boolean)) :: boolean

  def any?(collection, fun // fn(x) -> x end)

  def any?(collection, fun) when is_list(collection) do
    do_any?(collection, fun)
  end

  def any?(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_any?(pointer, iterator, fun)
      list when is_list(list) ->
        do_any?(list, fun)
    end
  end

  @doc """
  Finds the element at the given index (zero-based).
  Raises out of bounds error in case the given position
  is outside the range of the collection.

  Expects an ordered collection.

    ## Examples

        Enum.at! [2,4,6], 0 #=> 2
        Enum.at! [2,4,6], 2 #=> 6
        Enum.at! [2,4,6], 4 #=> raises Enum.OutOfBoundsError

  """
  @spec at!(t, non_neg_integer) :: element | no_return
  def at!(collection, n) when is_list(collection) and n >= 0 do
    do_at!(collection, n)
  end

  def at!(collection, n) when n >= 0 do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_at!(pointer, iterator, n)
      list when is_list(list) ->
        do_at!(list, n)
    end
  end

  @doc """
  Returns the collection size.

  ## Examples

      Enum.count [1,2,3] #=> 3

  """
  @spec count(t) :: non_neg_integer
  def count(collection) do
    I.count(collection)
  end

  @doc """
  Counts for how many items the function returns true.
  """
  @spec count(t, (element -> boolean)) :: non_neg_integer
  def count(collection, fun) when is_list(collection) do
    do_count(collection, fun)
  end

  def count(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_count(pointer, iterator, fun)
      list when is_list(list) ->
        do_count(list, fun)
    end
  end

  @doc """
  Drops the first `count` items from the collection.
  Expects an ordered collection.

  ## Examples

      Enum.drop [1,2,3], 2  #=> [3]
      Enum.drop [1,2,3], 10 #=> []
      Enum.drop [1,2,3], 0  #=> [1,2,3]

  """
  @spec drop(t, integer) :: list
  def drop(collection, count) when is_list(collection) and count >= 0 do
    do_drop(collection, count)
  end

  def drop(collection, count) when count >= 0 do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_drop(pointer, iterator, count)
      list when is_list(list) ->
        do_drop(list, count)
    end
  end

  def drop(collection, count) when count < 0 do
    { list, count } = iterate_and_count(collection, count)
    drop(list, count)
  end

  @doc """
  Drops items at the beginning of `collection` while `fun` returns true.
  Expects an ordered collection.

  ## Examples

      Enum.drop_while [1,2,3,4,5], fn(x) -> x < 3 end
      #=> [3,4,5]
  """
  @spec drop_while(t, (element -> boolean)) :: list
  def drop_while(collection, fun) when is_list(collection) do
    do_drop_while(collection, fun)
  end

  def drop_while(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_drop_while(pointer, iterator, fun)
      list when is_list(list) ->
        do_drop_while(list, fun)
    end
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`.
  Returns the `collection` itself.

  ## Examples

      Enum.each ['some', 'example'], fn(x) -> IO.puts x end

  """
  @spec each(t, (element -> any)) :: :ok
  def each(collection, fun) when is_list(collection) do
    :lists.foreach(fun, collection)
    :ok
  end

  def each(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_each(pointer, iterator, fun)
        :ok
      list when is_list(list) ->
        each(list, fun)
    end
  end

  @doc """
  Returns true if the collection is empty, otherwise false.

  ## Examples

      Enum.empty? []      #=> true
      Enum.empty? [1,2,3] #=> false

  """
  @spec empty?(t) :: boolean
  def empty?(collection) when is_list(collection) do
    collection == []
  end

  def empty?(collection) do
    case I.iterator(collection) do
      { _iterator, pointer }  -> pointer == :stop
      list when is_list(list) -> list == []
    end
  end

  @doc """
  Filters the collection, i.e. returns only those elements
  for which `fun` returns true.

  ## Examples

      Enum.filter [1, 2, 3], fn(x) -> rem(x, 2) == 0 end
      #=> [2]

  """
  @spec filter(t, (element -> boolean)) :: list
  def filter(collection, fun) when is_list(collection) do
    lc item inlist collection, fun.(item), do: item
  end

  def filter(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer }  ->
        do_filter(pointer, iterator, fun)
      list when is_list(list) ->
        filter(list, fun)
    end
  end

  @doc """
  Filters the collection and maps its values in one pass.

  ## Examples

      Enum.filter_map [1, 2, 3], fn(x) -> rem(x, 2) == 0 end, &1 * 2
      #=> [4]

  """
  @spec filter_map(t, (element -> boolean), (element -> element)) :: list
  def filter_map(collection, filter, mapper) when is_list(collection) do
    lc item inlist collection, filter.(item), do: mapper.(item)
  end

  def filter_map(collection, filter, mapper) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_filter_map(pointer, iterator, filter, mapper)
      list when is_list(list) ->
        filter_map(list, filter, mapper)
    end
  end

  @doc """
  Returns the first item for which `fun` returns a truthy value. If no such
  item is found, returns `ifnone`.

  ## Examples

      Enum.find [2,4,6], fn(x) -> rem(x, 2) == 1 end
      #=> nil

      Enum.find [2,4,6], 0, fn(x) -> rem(x, 2) == 1 end
      #=> 0

      Enum.find [2,3,4], fn(x) -> rem(x, 2) == 1 end
      #=> 3

  """
  @spec find(t, (element -> any)) :: element | :nil
  @spec find(t, any, (element -> any)) :: element | :nil

  def find(collection, ifnone // nil, fun)

  def find(collection, ifnone, fun) when is_list(collection) do
    do_find(collection, ifnone, fun)
  end

  def find(collection, ifnone, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_find(pointer, iterator, ifnone, fun)
      list when is_list(list) ->
        do_find(list, ifnone, fun)
    end
  end

  @doc """
  Similar to find, but returns the value of the function
  invocation instead of the element itself.

  ## Examples

      Enum.find_value [2,4,6], fn(x) -> rem(x, 2) == 1 end
      #=> nil

      Enum.find_value [2,3,4], fn(x) -> rem(x, 2) == 1 end
      #=> true

  """
  @spec find_value(t, (element -> any)) :: any | :nil
  @spec find_value(t, any, (element -> any)) :: any | :nil

  def find_value(collection, ifnone // nil, fun)

  def find_value(collection, ifnone, fun) when is_list(collection) do
    do_find_value(collection, ifnone, fun)
  end

  def find_value(collection, ifnone, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_find_value(pointer, iterator, ifnone, fun)
      list when is_list(list) ->
        do_find_value(list, ifnone, fun)
    end
  end

  @doc """
  Similar to find, but returns the index (count starts with 0)
  of the item instead of the element itself.

  Expects an ordered collection.

  ## Examples

      Enum.find_index [2,4,6], fn(x) -> rem(x, 2) == 1 end
      #=> nil

      Enum.find_index [2,3,4], fn(x) -> rem(x, 2) == 1 end
      #=> 2

  """
  @spec find_index(t, (element -> any)) :: non_neg_integer | :nil
  def find_index(collection, fun) when is_list(collection) do
    do_find_index(collection, 0, fun)
  end

  def find_index(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_find_index(pointer, iterator, 0, fun)
      list when is_list(list) ->
        do_find_index(list, 0, fun)
    end
  end

  @doc """
  Returns the first item in the collection or nil otherwise.

  ## Examples

      Enum.first []      #=> nil
      Enum.first [1,2,3] #=> 1

  """
  @spec first(t) :: :nil | element
  def first([]),    do: nil
  def first([h|_]), do: h

  def first(collection) do
    case I.iterator(collection) do
      { _iterator, { h, _ } } -> h
      { _iterator, :stop }    -> nil
      list when is_list(list) -> first(list)
    end
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
  @spec join(t) :: String.t
  @spec join(t, String.t | char_list) :: String.t | char_list
  def join(collection, joiner // "")

  def join(collection, joiner) when is_list(joiner) do
    binary_to_list join(collection, list_to_binary(joiner))
  end

  def join(collection, joiner) when is_list(collection) and is_binary(joiner) do
    do_join(collection, joiner, nil)
  end

  def join(collection, joiner) when is_binary(joiner) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_join(pointer, iterator, joiner, nil)
      list when is_list(list) ->
        do_join(list, joiner, nil)
    end
  end

  @doc """
  Returns a new collection, where each item is the result
  of invoking `fun` on each corresponding item of `collection`.
  For dicts, the function accepts a key-value tuple.

  ## Examples

      Enum.map [1, 2, 3], fn(x) -> x * 2 end
      #=> [2, 4, 6]

      Enum.map [a: 1, b: 2], fn({k, v}) -> { k, -v } end
      #=> [a: -1, b: -2]

  """
  @spec map(t, (element -> any)) :: list
  def map(collection, fun) when is_list(collection) do
    lc item inlist collection, do: fun.(item)
  end

  def map(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer }  ->
        do_map(pointer, iterator, fun)
      list when is_list(list) ->
        map(list, fun)
    end
  end

  @doc """
  Maps and joins the given `collection` in one pass.
  Joiner can be either a binary or a list and the
  result will be of the same type as joiner. If
  joiner is not passed at all, it defaults to an
  empty binary.

  All items in the collection must be convertible
  to binary, otherwise an error is raised.

  ## Examples

      Enum.map_join([1,2,3], &1 * 2)        #=> "246"
      Enum.map_join([1,2,3], " = ", &1 * 2) #=> "2 = 4 = 6"
      Enum.map_join([1,2,3], ' = ', &1 * 2) #=> '2 = 4 = 6'

  """
  @spec map_join(t, (element -> any)) :: String.t
  @spec map_join(t, String.t | char_list, (element -> any)) :: String.t | char_list
  def map_join(collection, joiner // "", mapper)

  def map_join(collection, joiner, mapper) when is_list(joiner) do
    binary_to_list map_join(collection, list_to_binary(joiner), mapper)
  end

  def map_join(collection, joiner, mapper) when is_list(collection) and is_binary(joiner) do
    do_map_join(collection, mapper, joiner, nil)
  end

  def map_join(collection, joiner, mapper) when is_binary(joiner) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_map_join(pointer, iterator, mapper, joiner, nil)
      list when is_list(list) ->
        do_map_join(list, mapper, joiner, nil)
    end
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`
  while also keeping an accumulator. Returns a tuple where
  the first element is the mapped collection and the second
  one is the final accumulator.

  For dicts, the first tuple element has to be a { key, value }
  tuple itself.

  ## Examples

      Enum.map_reduce [1, 2, 3], 0, fn(x, acc) -> { x * 2, x + acc } end
      #=> { [2, 4, 6], 6 }

  """
  @spec map_reduce(t, any, (element, any -> any)) :: any
  def map_reduce(collection, acc, f) when is_list(collection) do
    :lists.mapfoldl(f, acc, collection)
  end

  def map_reduce(collection, acc, fun) do
    case I.iterator(collection) do
      { iterator, pointer }  ->
        do_map_reduce(pointer, iterator, [], acc, fun)
      list when is_list(list) ->
        map_reduce(list, acc, fun)
    end
  end

  @doc """
  Partitions `collection` into two where the first one contains elements
  for which `fun` returns a truthy value, and the second one -- for which `fun`
  returns false or nil.

  ## Examples

      Enum.partition [1, 2, 3], fn(x) -> rem(x, 2) == 0 end
      #=> { [2], [1,3] }

  """
  @spec partition(t, (element -> any)) :: {list, list}
  def partition(collection, fun) when is_list(collection) do
    do_partition(collection, fun, [], [])
  end

  def partition(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_partition(pointer, iterator, fun, [], [])
      list when is_list(list) ->
        do_partition(list, fun, [], [])
    end
  end

  @doc """
  Invokes `fun` for each element in the collection passing that element and the
  accumulator `acc` as arguments. `fun`'s return value is stored in `acc`.
  Returns the accumulator.

  ## Examples

      Enum.reduce [1, 2, 3], 0, fn(x, acc) -> x + acc end
      #=> 6

  """
  @spec reduce(t, any, (element, any -> any)) :: any
  def reduce(collection, acc, fun) when is_list(collection) do
    :lists.foldl(fun, acc, collection)
  end

  def reduce(collection, acc, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_reduce(pointer, iterator, acc, fun)
      list when is_list(list) ->
        reduce(list, acc, fun)
    end
  end

  @doc """
  Reverses the collection.

  ## Examples

      Enum.reverse [1, 2, 3]
      #=> [3, 2, 1]

  """
  @spec reverse(t) :: list
  def reverse(collection) when is_list(collection) do
    :lists.reverse(collection)
  end

  def reverse(collection) do
    case I.iterator(collection) do
      { iterator, pointer }   -> do_reverse(pointer, iterator, [])
      list when is_list(list) -> reverse(list)
    end
  end

  @doc """
  Sorts the collection using the merge sort algorithm.

  ## Examples

      Enum.sort [3,2,1] #=> [1,2,3]

  """
  @spec sort(t) :: list
  def sort(collection) when is_list(collection) do
    :lists.sort(collection)
  end

  def sort(collection) do
    case I.iterator(collection) do
      { iterator, pointer }  ->
        do_sort(pointer, iterator, &1 <= &2)
      list when is_list(list) ->
        sort(list)
    end
  end

  @doc """
  Sorts the collection using the merge sort algorithm.

  ## Examples

      Enum.sort [3,2,1], &1 > &2 #=> [1,2,3]

  """
  @spec sort(t, (element, element -> boolean)) :: list
  def sort(collection, fun) when is_list(collection) do
    :lists.sort(fun, collection)
  end

  def sort(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer }  ->
        do_sort(pointer, iterator, fun)
      list when is_list(list) ->
        sort(list, fun)
    end
  end

  @doc """
  Splits the enumerable into two collections, leaving `count`
  elements in the first one. If `count` is a negative number,
  it starts couting from the back to the beginning of the
  collection.

  Be aware that a negative `count` implies the collection
  will be iterate twice. One to calculate the position and
  another one to do the actual splitting.

  ## Examples

      Enum.split [1,2,3], 2  #=> { [1,2], [3] }
      Enum.split [1,2,3], 10 #=> { [1,2,3], [] }
      Enum.split [1,2,3], 0  #=> { [], [1,2,3] }
      Enum.split [1,2,3], -1 #=> { [1,2], [3] }
      Enum.split [1,2,3], -5 #=> { [], [1,2,3] }

  """
  @spec split(t, integer) :: {list, list}
  def split(collection, count) when is_list(collection) and count >= 0 do
    do_split(collection, count, [])
  end

  def split(collection, count) when count >= 0 do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_split(pointer, iterator, count, [])
      list when is_list(list) ->
        do_split(list, count, [])
    end
  end

  def split(collection, count) when count < 0 do
    { list, count } = iterate_and_count(collection, count)
    split(list, count)
  end

  @doc """
  Splits `collection` at the first element, for which `fun` returns true.
  Expects an ordered collection.

  ## Examples

      Enum.split_while [1,2,3,4], fn x -> x == 2 end
      #=> { [1], [2, 3, 4] }
  """
  @spec split_while(t, (element -> boolean)) :: {list, list}
  def split_while(collection, fun) when is_list(collection) do
    do_split_while(collection, fun, [])
  end

  def split_while(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_split_while(pointer, iterator, fun, [])
      list when is_list(list) ->
        do_split_while(list, fun, [])
    end
  end

  @doc """
  Takes the first `count` items from the collection. Expects an ordered
  collection.

  ## Examples

      Enum.take [1,2,3], 2  #=> [1,2]
      Enum.take [1,2,3], 10 #=> [1,2,3]
      Enum.take [1,2,3], 0  #=> []

  """
  @spec take(t, integer) :: list
  def take(collection, count) when is_list(collection) and count >= 0 do
    do_take(collection, count)
  end

  def take(collection, count) when count >= 0 do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_take(pointer, iterator, count)
      list when is_list(list) ->
        do_take(list, count)
    end
  end

  def take(collection, count) when count < 0 do
    { list, count } = iterate_and_count(collection, count)
    take(list, count)
  end

  @doc """
  Takes the items at the beginning of `collection` while `fun` returns true.
  Expects an ordered collection.

  ## Examples

      Enum.take_while [1,2,3], fn(x) -> x < 3 end
      #=> [1, 2]

  """
  @spec take_while(t, (element -> boolean)) :: list
  def take_while(collection, fun) when is_list(collection) do
    do_take_while(collection, fun)
  end

  def take_while(collection, fun) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_take_while(pointer, iterator, fun)
      list when is_list(list) ->
        do_take_while(list, fun)
    end
  end

  @doc """
  Iterates the enumerable removing all duplicated items.

  ## Examples

      Enum.uniq [1,2,3,2,1]
      #=> [1, 2, 3]

  """
  @spec uniq(t) :: list
  def uniq(collection) when is_list(collection) do
    do_uniq(collection, [])
  end

  def uniq(collection) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        do_uniq(pointer, iterator, [])
      list when is_list(list) ->
        do_uniq(list, [])
    end
  end

  @doc """
  Zips corresponding elements from two collections into one list
  of tuples. The number of elements in the resulting list is
  dictated by the first enum. In case the second list is shorter,
  values are filled with nil.
  """
  @spec zip(t, t) :: [{any, any}]
  def zip(coll1, coll2) when is_list(coll1) do
    do_zip(coll1, iterator(coll2))
  end

  def zip(coll1, coll2) do
    case I.iterator(coll1) do
      { iterator, pointer } ->
        do_zip(pointer, iterator, iterator(coll2))
      list when is_list(list) ->
        do_zip(list, iterator(coll2))
    end
  end

  ## Helpers

  defp iterator(collection) when is_list(collection), do: collection
  defp iterator(collection), do: I.iterator(collection)

  defp to_list({ h, next }, iterator) do
    [h|to_list(iterator.(next), iterator)]
  end

  defp to_list(:stop, _) do
    []
  end

  defp iterate_and_count(collection, count) do
    { list, total_items } = do_iterate_and_count(collection)
    { list, max(0, total_items - abs(count)) }
  end

  defp do_iterate_and_count(collection) when is_list(collection) do
    { collection, length(collection) }
  end

  defp do_iterate_and_count(collection) do
    case I.iterator(collection) do
      { iterator, pointer } ->
        reducer = fn(x, acc) -> { x, acc + 1 } end
        do_map_reduce(pointer, iterator, [], 0, reducer)
      list when is_list(list) ->
        do_iterate_and_count(list)
    end
  end

  ## Implementations

  ## all?

  defp do_all?([h|t], fun) do
    if fun.(h) do
      do_all?(t, fun)
    else
      false
    end
  end

  defp do_all?([], _) do
    true
  end

  defp do_all?({ h, next }, iterator, fun) do
    if fun.(h) do
      do_all?(iterator.(next), iterator, fun)
    else
      false
    end
  end

  defp do_all?(:stop, _, _) do
    true
  end

  ## any?

  defp do_any?([h|t], fun) do
    if fun.(h) do
      true
    else
      do_any?(t, fun)
    end
  end

  defp do_any?([], _) do
    false
  end

  defp do_any?({ h, next }, iterator, fun) do
    if fun.(h) do
      true
    else
      do_any?(iterator.(next), iterator, fun)
    end
  end

  defp do_any?(:stop, _, _) do
    false
  end

  ## at!

  defp do_at!([h|_], 0), do: h
  defp do_at!([_|t], n), do: do_at!(t, n - 1)
  defp do_at!([], _),    do: raise Enum.OutOfBoundsError

  defp do_at!({ h, _next }, _iterator, 0), do: h
  defp do_at!({ _, next }, iterator, n),   do: do_at!(iterator.(next), iterator, n - 1)
  defp do_at!(:stop, _iterator, _),        do: raise Enum.OutOfBoundsError

  ## count

  defp do_count([h|t], fun) do
    if fun.(h) do
      1 + do_count(t, fun)
    else
      do_count(t, fun)
    end
  end

  defp do_count([], _) do
    0
  end

  defp do_count({ h, next }, iterator, fun) do
    if fun.(h) do
      1 + do_count(iterator.(next), iterator, fun)
    else
      do_count(iterator.(next), iterator, fun)
    end
  end

  defp do_count(:stop, _, _) do
    0
  end

  ## drop

  defp do_drop([_|t], counter) when counter > 0 do
    do_drop(t, counter - 1)
  end

  defp do_drop(list, 0) do
    list
  end

  defp do_drop([], _) do
    []
  end

  defp do_drop({ _, next }, iterator, counter) when counter > 0 do
    do_drop(iterator.(next), iterator, counter - 1)
  end

  defp do_drop(extra, iterator, 0) do
    to_list(extra, iterator)
  end

  defp do_drop(:stop, _, _) do
    []
  end

  ## drop_while

  defp do_drop_while([h|t], fun) do
    if fun.(h) do
      do_drop_while(t, fun)
    else
      [h|t]
    end
  end

  defp do_drop_while([], _) do
    []
  end

  defp do_drop_while({ h, next } = extra, iterator, fun) do
    if fun.(h) do
      do_drop_while(iterator.(next), iterator, fun)
    else
      to_list(extra, iterator)
    end
  end

  defp do_drop_while(:stop, _, _) do
    []
  end

  ## find

  defp do_find([h|t], ifnone, fun) do
    if fun.(h) do
      h
    else
      do_find(t, ifnone, fun)
    end
  end

  defp do_find([], ifnone, _) do
    ifnone
  end

  defp do_find({ h, next }, iterator, ifnone, fun) do
    if fun.(h) do
      h
    else
      do_find(iterator.(next), iterator, ifnone, fun)
    end
  end

  defp do_find(:stop, _, ifnone, _) do
    ifnone
  end

  ## find_index

  defp do_find_index([h|t], counter, fun) do
    if fun.(h) do
      counter
    else
      do_find_index(t, counter + 1, fun)
    end
  end

  defp do_find_index([], _, _) do
    nil
  end

  defp do_find_index({ h, next }, iterator, counter, fun) do
    if fun.(h) do
      counter
    else
      do_find_index(iterator.(next), iterator, counter + 1, fun)
    end
  end

  defp do_find_index(:stop, _, _, _) do
    nil
  end

  ## find_value

  defp do_find_value([h|t], ifnone, fun) do
    fun.(h) || do_find_value(t, ifnone, fun)
  end

  defp do_find_value([], ifnone, _) do
    ifnone
  end

  defp do_find_value({ h, next }, iterator, ifnone, fun) do
    fun.(h) || do_find_value(iterator.(next), iterator, ifnone, fun)
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
    if fun.(h) do
      [h|do_filter(iterator.(next), iterator, fun)]
    else
      do_filter(iterator.(next), iterator, fun)
    end
  end

  defp do_filter(:stop, _, _) do
    []
  end

  ## filter_map

  defp do_filter_map({ h, next }, iterator, filter, mapper) do
    if filter.(h) do
      [mapper.(h)|do_filter_map(iterator.(next), iterator, filter, mapper)]
    else
      do_filter_map(iterator.(next), iterator, filter, mapper)
    end
  end

  defp do_filter_map(:stop, _, _, _) do
    []
  end

  ## join

  defp do_join([h|t], joiner, nil) do
    do_join(t, joiner, to_binary(h))
  end

  defp do_join([h|t], joiner, acc) do
    acc = << acc :: binary, joiner :: binary, to_binary(h) :: binary >>
    do_join(t, joiner, acc)
  end

  defp do_join([], _joiner, acc) do
    acc || ""
  end

  defp do_join({ h, next }, iterator, joiner, nil) do
    do_join(iterator.(next), iterator, joiner, to_binary(h))
  end

  defp do_join({ h, next }, iterator, joiner, acc) do
    acc = << acc :: binary, joiner :: binary, to_binary(h) :: binary >>
    do_join(iterator.(next), iterator, joiner, acc)
  end

  defp do_join(:stop, _, _joiner, acc) do
    acc || ""
  end

  ## map

  defp do_map({ h, next }, iterator, fun) do
    [fun.(h)|do_map(iterator.(next), iterator, fun)]
  end

  defp do_map(:stop, _, _) do
    []
  end

  ## map join

  defp do_map_join([h|t], mapper, joiner, nil) do
    do_map_join(t, mapper, joiner, to_binary(mapper.(h)))
  end

  defp do_map_join([h|t], mapper, joiner, acc) do
    acc = << acc :: binary, joiner :: binary, to_binary(mapper.(h)) :: binary >>
    do_map_join(t, mapper, joiner, acc)
  end

  defp do_map_join([], _mapper, _joiner, acc) do
    acc || ""
  end

  defp do_map_join({ h, next }, iterator, mapper, joiner, nil) do
    do_map_join(iterator.(next), iterator, mapper, joiner, to_binary(mapper.(h)))
  end

  defp do_map_join({ h, next }, iterator, mapper, joiner, acc) do
    acc = << acc :: binary, joiner :: binary, to_binary(mapper.(h)) :: binary >>
    do_map_join(iterator.(next), iterator, mapper, joiner, acc)
  end

  defp do_map_join(:stop, _, _mapper, _joiner, acc) do
    acc || ""
  end

  ## map_reduce

  defp do_map_reduce({ h, next }, iterator, list_acc, acc, f) do
    { result, acc } = f.(h, acc)
    do_map_reduce(iterator.(next), iterator, [result|list_acc], acc, f)
  end

  defp do_map_reduce(:stop, _, list_acc, acc, _f) do
    { :lists.reverse(list_acc), acc }
  end

  ## partition

  defp do_partition([h|t], fun, acc1, acc2) do
    if fun.(h) do
      do_partition(t, fun, [h|acc1], acc2)
    else
      do_partition(t, fun, acc1, [h|acc2])
    end
  end

  defp do_partition([], _, acc1, acc2) do
    { :lists.reverse(acc1), :lists.reverse(acc2) }
  end

  defp do_partition({ h, next }, iterator, fun, acc1, acc2) do
    if fun.(h) do
      do_partition(iterator.(next), iterator, fun, [h|acc1], acc2)
    else
      do_partition(iterator.(next), iterator, fun, acc1, [h|acc2])
    end
  end

  defp do_partition(:stop, _, _, acc1, acc2) do
    { :lists.reverse(acc1), :lists.reverse(acc2) }
  end

  ## reduce

  defp do_reduce({ h, next }, iterator, acc, fun) do
    do_reduce(iterator.(next), iterator, fun.(h, acc), fun)
  end

  defp do_reduce(:stop, _, acc, _) do
    acc
  end

  ## reverse

  defp do_reverse({ h, next }, iterator, acc) do
    do_reverse(iterator.(next), iterator, [h|acc])
  end

  defp do_reverse(:stop, _, acc) do
    acc
  end

  ## sort

  defp do_sort(extra, iterator, fun) do
    case sort_take(extra, iterator, 2, []) do
      { [y, x], next } -> sort_split(y, x, next, iterator, fun, [], [], fun.(x, y))
      { other, _ } -> other
    end
  end


  defp sort_take({ h, next }, iterator, counter, acc) when counter > 0 do
    sort_take(iterator.(next), iterator, counter - 1, [h|acc])
  end

  defp sort_take(extra, _iterator, 0, acc) do
    { acc, extra }
  end

  defp sort_take(:stop, _, _, acc) do
    { acc, :stop }
  end


  defp sort_split(y, x, { z, next }, iterator, fun, r, rs, bool) do
    cond do
      fun.(y, z) == bool ->
        sort_split(z, y, iterator.(next), iterator, fun, [x | r], rs, bool)
      fun.(x, z) == bool ->
        sort_split(y, z, iterator.(next), iterator, fun, [x | r], rs, bool)
      r == [] ->
        sort_split(y, x, iterator.(next), iterator, fun, [z], rs, bool)
      true ->
        sort_split_pivot(y, x, iterator.(next), iterator, fun, r, rs, z, bool)
    end
  end

  defp sort_split(y, x, :stop, _iterator, fun, r, rs, bool) do
    sort_merge([[y, x | r] | rs], fun, bool)
  end

  defp sort_split_pivot(y, x, { z, next }, iterator, fun, r, rs, s, bool) do
    cond do
      fun.(y, z) == bool ->
        sort_split_pivot(z, y, iterator.(next), iterator, fun, [x | r], rs, s, bool)
      fun.(x, z) == bool ->
        sort_split_pivot(y, z, iterator.(next), iterator, fun, [x | r], rs, s, bool)
      fun.(s, z) == bool ->
        sort_split(z, s, iterator.(next), iterator, fun, [], [[y, x | r] | rs], bool)
      true ->
        sort_split(s, z, iterator.(next), iterator, fun, [], [[y, x | r] | rs], bool)
    end
  end

  defp sort_split_pivot(y, x, :stop, _iterator, fun, r, rs, s, bool) do
    sort_merge([[s], [[y, x | r] | rs]], fun, bool)
  end


  defp sort_merge(list, fun, true), do:
    reverse_sort_merge(list, [], fun, true)

  defp sort_merge(list, fun, false), do:
    sort_merge(list, [], fun, false)


  defp sort_merge([t1, [h2 | t2] | l], acc, fun, true), do:
    sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, false) | acc], fun, true)

  defp sort_merge([[h2 | t2], t1 | l], acc, fun, false), do:
    sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, false) | acc], fun, false)

  defp sort_merge([l], [], _fun, _bool), do: l

  defp sort_merge([l], acc, fun, bool), do:
    reverse_sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp sort_merge([], acc, fun, bool), do:
    reverse_sort_merge(acc, [], fun, bool)


  defp reverse_sort_merge([[h2 | t2], t1 | l], acc, fun, true), do:
    reverse_sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, true) | acc], fun, true)

  defp reverse_sort_merge([t1, [h2 | t2] | l], acc, fun, false), do:
    reverse_sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, true) | acc], fun, false)

  defp reverse_sort_merge([l], acc, fun, bool), do:
    sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp reverse_sort_merge([], acc, fun, bool), do:
    sort_merge(acc, [], fun, bool)


  defp sort_merge_1([h1 | t1], h2, t2, m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge_2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge_1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge_1([], h2, t2, m, _fun, _bool), do:
    :lists.reverse(t2, [h2 | m])


  defp sort_merge_2(h1, t1, [h2 | t2], m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge_2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge_1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge_2(h1, t1, [], m, _fun, _bool), do:
    :lists.reverse(t1, [h1 | m])

  ## split

  defp do_split([h|t], counter, acc) when counter > 0 do
    do_split(t, counter - 1, [h|acc])
  end

  defp do_split(list, 0, acc) do
    { :lists.reverse(acc), list }
  end

  defp do_split([], _, acc) do
    { :lists.reverse(acc), [] }
  end

  defp do_split({ h, next }, iterator, counter, acc) when counter > 0 do
    do_split(iterator.(next), iterator, counter - 1, [h|acc])
  end

  defp do_split(extra, iterator, 0, acc) do
    { :lists.reverse(acc), to_list(extra, iterator) }
  end

  defp do_split(:stop, _, _, acc) do
    { :lists.reverse(acc), [] }
  end

  ## split_while

  defp do_split_while([h|t], fun, acc) do
    if fun.(h) do
      do_split_while(t, fun, [h|acc])
    else
      { :lists.reverse(acc), [h|t] }
    end
  end

  defp do_split_while([], _, acc) do
    { :lists.reverse(acc), [] }
  end

  defp do_split_while({ h, next } = extra, iterator, fun, acc) do
    if fun.(h) do
      do_split_while(iterator.(next), iterator, fun, [h|acc])
    else
      { :lists.reverse(acc), to_list(extra, iterator) }
    end
  end

  defp do_split_while(:stop, _, _, acc) do
    { :lists.reverse(acc), [] }
  end

  ## take

  defp do_take([h|t], counter) when counter > 0 do
    [h|do_take(t, counter - 1)]
  end

  defp do_take(_list, 0) do
    []
  end

  defp do_take([], _) do
    []
  end

  defp do_take({ h, next }, iterator, counter) when counter > 0 do
    [h|do_take(iterator.(next), iterator, counter - 1)]
  end

  defp do_take(_extra, _iterator, 0) do
    []
  end

  defp do_take(:stop, _, _) do
    []
  end

  ## take_while

  defp do_take_while([h|t], fun) do
    if fun.(h) do
      [h|do_take_while(t, fun)]
    else
      []
    end
  end

  defp do_take_while([], _) do
    []
  end

  defp do_take_while({ h, next }, iterator, fun) do
    if fun.(h) do
      [h|do_take_while(iterator.(next), iterator, fun)]
    else
      []
    end
  end

  defp do_take_while(:stop, _, _) do
    []
  end

  ## uniq

  defp do_uniq([h|t], acc) do
    case :lists.member(h, acc) do
      true  -> do_uniq(t, acc)
      false -> [h|do_uniq(t, [h|acc])]
    end
  end

  defp do_uniq([], _acc) do
    []
  end

  defp do_uniq({ h, next }, iterator, acc) do
    case :lists.member(h, acc) do
      true  -> do_uniq(iterator.(next), iterator, acc)
      false -> [h|do_uniq(iterator.(next), iterator, [h|acc])]
    end
  end

  defp do_uniq(:stop, _, _acc) do
    []
  end

  ## zip

  defp do_zip([h1|next1], other) do
    { h2, next2 } = do_zip_next(other)
    [{ h1, h2 }|do_zip(next1, next2)]
  end

  defp do_zip([], _) do
    []
  end

  defp do_zip({ h1, next1 }, iterator, other) do
    { h2, next2 } = do_zip_next(other)
    [{ h1, h2 }|do_zip(iterator.(next1), iterator, next2)]
  end

  defp do_zip(:stop, _, _) do
    []
  end

  defp do_zip_next([h|t]), do: { h, t }
  defp do_zip_next([]),    do: { nil, [] }

  defp do_zip_next({ iterator, { h, next } }) do
    { h, { iterator, iterator.(next) } }
  end

  defp do_zip_next({ _iterator, :stop } = i) do
    { nil, i }
  end
end

defimpl Enum.Iterator, for: List do
  def iterator(list),               do: list
  def count(list),                  do: length(list)
end

defimpl Enum.Iterator, for: Function do
  def iterator(function) do
    function.()
  end

  def count(function) do
    { function, first } = function.()
    do_count(first, function, 0)
  end

  defp do_count({ _, next }, function, acc) do
    do_count(function.(next), function, acc + 1)
  end

  defp do_count(:stop, _, acc) do
    acc
  end
end
