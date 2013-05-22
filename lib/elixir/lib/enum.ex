defprotocol Enum.Iterator do
  @moduledoc """
  This is the protocol used by the `Enum` module.

  Usually, when you invoke a function in the module `Enum`, the first argument
  passed to it is a collection which is forwarded to this protocol in order to
  retrieve information on how to iterate the collection.

  For example, in the expression

      Enum.map([1,2,3], &1 * 2)

  `Enum.map` invokes `Enum.Iterator.iterator([1,2,3])` to retrieve the iterator
  function that will drive the iteration process.
  """

  @only [List, Record, Function]

  def reduce(collection, acc, fun)

  @doc """
  The function used to check if a value exists within the collection.
  """
  def member?(collection, value)

  @doc """
  The function used to retrieve the collection's size.
  """
  def count(collection)
end

defmodule Enum do
  alias Enum.Iterator, as: I

  import Kernel, except: [max: 2, min: 2]

  @moduledoc """
  Provides a set of algorithms that enumerate over collections according to the
  `Enum.Iterator` protocol. Most of the functions in this module have two
  flavours. If a given collection implements the mentioned protocol (like
  list, for instance), you can do:

      Enum.map([1,2,3], fn(x) -> x * 2 end)

  Depending on the type of the collection, the user-provided function will
  accept a certain type of argument. For dicts, the argument is always a
  `{ key, value }` tuple.
  """

  @type t :: Enum.Iterator.t
  @type element :: any
  @type index :: non_neg_integer
  @type default :: any

  @doc """
  Checks if the `value` exists within the `collection`.

  ## Examples

      iex> Enum.member?(1..10, 5)
      true
      iex> Enum.member?([:a, :b, :c], :d)
      false

  """
  @spec member?(t, element) :: boolean
  def member?(collection, value) do
    I.member?(collection, value)
  end

  @doc """
  Returns the collection size.

  ## Examples

      iex> Enum.count([1,2,3])
      3

  """
  @spec count(t) :: non_neg_integer
  def count(collection) do
    I.count(collection)
  end

  @doc """
  Counts for how many items the function returns true.
  """
  @spec count(t, (element -> as_boolean(term))) :: non_neg_integer
  def count(collection, fun) when is_list(collection) do
    folder = fn(x, acc) -> if fun.(x), do: acc+1, else: acc end
    :lists.foldl(folder, 0, collection)
  end

  def count(collection, fun) do
    I.reduce(collection, 0, fn(entry, acc) ->
      if fun.(entry), do: acc + 1, else: acc
    end)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns true if
  each invocation returns true as well, otherwise it short-circuits and returns
  false.

  ## Examples

      iex> Enum.all?([2,4,6], fn(x) -> rem(x, 2) == 0 end)
      true

      iex> Enum.all?([2,3,4], fn(x) -> rem(x, 2) == 0 end)
      false

  If no function is given, it defaults to checking if
  all items in the collection evaluate to true.

      iex> Enum.all?([1,2,3])
      true
      iex> Enum.all?([1,nil,3])
      false

  """
  @spec all?(t) :: boolean
  @spec all?(t, (element -> as_boolean(term))) :: boolean

  def all?(collection, fun // fn(x) -> x end)

  def all?(collection, fun) when is_list(collection) do
    do_all?(collection, fun)
  end

  def all?(collection, fun) do
    I.reduce(collection, nil, fn(entry, _) ->
      if fun.(entry), do: nil, else: throw(:enum_all?)
    end)
    true
  catch
    :enum_all? -> false
  end

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns true if
  at least one invocation returns true. Returns false otherwise.

  ## Examples

      iex> Enum.any?([2,4,6], fn(x) -> rem(x, 2) == 1 end)
      false

      iex> Enum.any?([2,3,4], fn(x) -> rem(x, 2) == 1 end)
      true

  If no function is given, it defaults to checking if
  at least one item in the collection evaluates to true.

      iex> Enum.any?([false,false,false])
      false
      iex> Enum.any?([false,true,false])
      true

  """
  @spec any?(t) :: boolean
  @spec any?(t, (element -> as_boolean(term))) :: boolean

  def any?(collection, fun // fn(x) -> x end)

  def any?(collection, fun) when is_list(collection) do
    do_any?(collection, fun)
  end

  def any?(collection, fun) do
    I.reduce(collection, nil, fn(entry, _) ->
      if fun.(entry), do: throw(:enum_any?), else: nil
    end)
    false
  catch
    :enum_any? -> true
  end

  @doc """
  Finds the element at the given index (zero-based).
  Raises out of bounds error in case the given position
  is outside the range of the collection.

  Expects an ordered collection.

    ## Examples

        iex> Enum.at([2,4,6], 0)
        2
        iex> Enum.at([2,4,6], 2)
        6
        iex> Enum.at([2,4,6], 4)
        nil
        iex> Enum.at([2,4,6], 4, :none)
        :none

  """
  @spec at(t, index) :: element | nil
  @spec at(t, index, default) :: element | default
  def at(collection, n, default // nil) when n >= 0 do
    case fetch(collection, n) do
      { :ok, h } -> h
      :error     -> default
    end
  end

  @doc """
  Drops the first `count` items from the collection.
  Expects an ordered collection.

  ## Examples

      iex> Enum.drop([1,2,3], 2)
      [3]
      iex> Enum.drop([1,2,3], 10)
      []
      iex> Enum.drop([1,2,3], 0)
      [1,2,3]

  """
  @spec drop(t, integer) :: list
  def drop(collection, count) when is_list(collection) and count >= 0 do
    do_drop(collection, count)
  end

  def drop(collection, count) when count >= 0 do
    { list, _ } = I.reduce(collection, { [], count }, fn(entry, { list, count }) ->
      if count > 0 do
        { [], count - 1}
      else
        { [entry|list], count }
      end
    end)
    :lists.reverse(list)
  end

  def drop(collection, count) when count < 0 do
    { list, count } = iterate_and_count(collection, count)
    drop(list, count)
  end

  @doc """
  Drops items at the beginning of `collection` while `fun` returns true.
  Expects an ordered collection.

  ## Examples

      iex> Enum.drop_while([1,2,3,4,5], fn(x) -> x < 3 end)
      [3,4,5]
  """
  @spec drop_while(t, (element -> as_boolean(term))) :: list
  def drop_while(collection, fun) when is_list(collection) do
    do_drop_while(collection, fun)
  end

  def drop_while(collection, fun) do
    I.reduce(collection, [], fn(entry, acc) ->
      if fun.(entry), do: [], else: [entry|acc]
    end) |> :lists.reverse
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`.
  Returns the `collection` itself. `fun` can take two parameters,
  in which case the second parameter will be the iteration index.

  ## Examples

      Enum.each(['some', 'example'], fn(x) -> IO.puts x end)

  """
  @spec each(t, (element -> any) | (element, index -> any)) :: :ok
  def each(collection, fun) when is_list(collection) do
    cond do
      is_function(fun, 1) -> :lists.foreach(fun, collection)
      is_function(fun, 2) -> :lists.foldl(fn(h, idx) -> fun.(h, idx); idx + 1 end, 0, collection)
    end

    :ok
  end

  def each(collection, fun) do
    cond do
      is_function(fun, 1) ->
        I.reduce(collection, nil, fn(entry, _) ->
          fun.(entry)
          nil
        end)

      is_function(fun, 2) ->
        I.reduce(collection, 0, fn(entry, idx) ->
          fun.(entry, idx)
          idx + 1
        end)
    end

    :ok
  end

  @doc """
  Returns true if the collection is empty, otherwise false.

  ## Examples

      iex> Enum.empty?([])
      true
      iex> Enum.empty?([1,2,3])
      false

  """
  @spec empty?(t) :: boolean
  def empty?(collection) when is_list(collection) do
    collection == []
  end

  def empty?(collection) do
    I.reduce(collection, nil, fn(_, _) -> throw(:enum_empty?) end)
    true
  catch
    :enum_empty? -> false
  end

  @doc """
  Finds the element at the given index (zero-based).
  Returns `{ :ok, element }` if found, otherwise `:error`.

  Expects an ordered collection.

    ## Examples

        iex> Enum.fetch([2,4,6], 0)
        { :ok, 2 }
        iex> Enum.fetch([2,4,6], 2)
        { :ok, 6 }
        iex> Enum.fetch([2,4,6], 4)
        :error

  """
  @spec fetch(t, index) :: { :ok, element } | :error
  def fetch(collection, n) when is_list(collection) and n >= 0 do
    do_fetch(collection, n)
  end

  def fetch(collection, n) when n >= 0 do
    I.reduce(collection, 0, fn(entry, acc) ->
      if acc == n do
        throw({ :enum_fetch, entry })
      else
        acc + 1
      end
    end)

    :error
  catch
    { :enum_fetch, entry } -> { :ok, entry }
  end

  @doc """
  Finds the element at the given index (zero-based).
  Raises out of bounds error in case the given position
  is outside the range of the collection.

  ## Examples

      iex> Enum.fetch!([2,4,6], 0)
      2
      iex> Enum.fetch!([2,4,6], 2)
      6
      iex> Enum.fetch!([2,4,6], 4)
      ** (Enum.OutOfBoundsError) out of bounds error

  """
  @spec fetch!(t, index) :: element | no_return
  def fetch!(collection, n) when n >= 0 do
    case fetch(collection, n) do
      { :ok, h } -> h
      :error     -> raise Enum.OutOfBoundsError
    end
  end

  @doc false
  def at!(collection, n) when n >= 0 do
    IO.write "[WARNING] Enum.at! is deprecated, please use Enum.fetch! instead\n#{Exception.format_stacktrace}"
    fetch!(collection, n)
  end

  @doc """
  Filters the collection, i.e. returns only those elements
  for which `fun` returns true.

  ## Examples

      iex> Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [2]

  """
  @spec filter(t, (element -> as_boolean(term))) :: list
  def filter(collection, fun) when is_list(collection) do
    lc item inlist collection, fun.(item), do: item
  end

  def filter(collection, fun) do
    I.reduce(collection, [], fn(entry, acc) ->
      if fun.(entry), do: [entry|acc], else: acc
    end) |> :lists.reverse
  end

  @doc """
  Filters the collection and maps its values in one pass.

  ## Examples

      iex> Enum.filter_map([1, 2, 3], fn(x) -> rem(x, 2) == 0 end, &1 * 2)
      [4]

  """
  @spec filter_map(t, (element -> as_boolean(term)), (element -> element)) :: list
  def filter_map(collection, filter, mapper) when is_list(collection) do
    lc item inlist collection, filter.(item), do: mapper.(item)
  end

  def filter_map(collection, filter, mapper) do
    I.reduce(collection, [], fn(entry, acc) ->
      if filter.(entry), do: [mapper.(entry)|acc], else: acc
    end) |> :lists.reverse
  end

  @doc """
  Returns the first item for which `fun` returns a truthy value. If no such
  item is found, returns `ifnone`.

  ## Examples

      iex> Enum.find([2,4,6], fn(x) -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find([2,4,6], 0, fn(x) -> rem(x, 2) == 1 end)
      0

      iex> Enum.find([2,3,4], fn(x) -> rem(x, 2) == 1 end)
      3

  """
  @spec find(t, (element -> any)) :: element | nil
  @spec find(t, default, (element -> any)) :: element | default

  def find(collection, ifnone // nil, fun)

  def find(collection, ifnone, fun) when is_list(collection) do
    do_find(collection, ifnone, fun)
  end

  def find(collection, ifnone, fun) do
    I.reduce(collection, nil, fn(entry, _) ->
      if fun.(entry), do: throw({ :enum_find, entry })
    end)
    ifnone
  catch
    { :enum_find, entry } -> entry
  end

  @doc """
  Similar to find, but returns the value of the function
  invocation instead of the element itself.

  ## Examples

      iex> Enum.find_value([2,4,6], fn(x) -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find_value([2,3,4], fn(x) -> rem(x, 2) == 1 end)
      true

  """
  @spec find_value(t, (element -> any)) :: any | :nil
  @spec find_value(t, any, (element -> any)) :: any | :nil

  def find_value(collection, ifnone // nil, fun)

  def find_value(collection, ifnone, fun) when is_list(collection) do
    do_find_value(collection, ifnone, fun)
  end

  def find_value(collection, ifnone, fun) do
    I.reduce(collection, nil, fn(entry, _) ->
      fun_entry = fun.(entry)
      if fun_entry, do: throw({ :enum_find, fun_entry })
    end)
    ifnone
  catch
    { :enum_find, entry } -> entry
  end

  @doc """
  Similar to find, but returns the index (count starts with 0)
  of the item instead of the element itself.

  Expects an ordered collection.

  ## Examples

      iex> Enum.find_index([2,4,6], fn(x) -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find_index([2,3,4], fn(x) -> rem(x, 2) == 1 end)
      1

  """
  @spec find_index(t, (element -> any)) :: index | :nil
  def find_index(collection, fun) when is_list(collection) do
    do_find_index(collection, 0, fun)
  end

  def find_index(collection, fun) do
    I.reduce(collection, 0, fn(entry, acc) ->
      if fun.(entry), do: throw({ :enum_find_index, acc }), else: acc + 1
    end)
    nil
  catch
    { :enum_find_index, idx } -> idx
  end

  @doc """
  Returns the first item in the collection or nil otherwise.

  ## Examples

      iex> Enum.first([])
      nil
      iex> Enum.first([1,2,3])
      1

  """
  @spec first(t) :: :nil | element
  def first([]),    do: nil
  def first([h|_]), do: h

  def first(collection) do
    I.reduce(collection, nil, fn(entry, _) ->
      throw({ :enum_first, entry})
    end)
  catch
    { :enum_first, entry } -> entry
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

      iex> Enum.join([1,2,3])
      "123"
      iex> Enum.join([1,2,3], " = ")
      "1 = 2 = 3"
      iex> Enum.join([1,2,3], ' = ')
      '1 = 2 = 3'

  """
  @spec join(t) :: String.t
  @spec join(t, String.t | char_list) :: String.t | char_list
  def join(collection, joiner // "")

  def join(collection, joiner) when is_list(joiner) do
    :unicode.characters_to_list join(collection, :unicode.characters_to_binary(joiner))
  end

  def join(collection, joiner) when is_list(collection) and is_binary(joiner) do
    do_join(collection, joiner, nil)
  end

  def join(collection, joiner) when is_binary(joiner) do
    I.reduce(collection, nil, fn
      entry, nil -> to_binary(entry)
      entry, acc -> acc <> joiner <> to_binary(entry)
    end)
  end

  @doc """
  Returns a new collection, where each item is the result
  of invoking `fun` on each corresponding item of `collection`.
  `fun` can take two parameters, in which case the second parameter
  will be the iteration index.

  For dicts, the function accepts a key-value tuple.

  ## Examples

      iex> Enum.map([1, 2, 3], fn(x) -> x * 2 end)
      [2, 4, 6]

      iex> Enum.map([a: 1, b: 2], fn({k, v}) -> { k, -v } end)
      [a: -1, b: -2]

  """
  @spec map(t, (element -> any) | (element, index -> any)) :: list
  def map(collection, fun) when is_list(collection) do
    cond do
      is_function(fun, 1) ->
        lc item inlist collection, do: fun.(item)
      is_function(fun, 2) ->
        mapper = fn(h, idx) -> { fun.(h, idx), idx + 1 } end
        { list, _ } = :lists.mapfoldl(mapper, 0, collection)
        list
    end
  end

  def map(collection, fun) do
    I.reduce(collection, [], fn(entry, acc) ->
      [fun.(entry)|acc]
    end) |> :lists.reverse
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

      iex> Enum.map_join([1,2,3], &1 * 2)
      "246"
      iex> Enum.map_join([1,2,3], " = ", &1 * 2)
      "2 = 4 = 6"
      iex> Enum.map_join([1,2,3], ' = ', &1 * 2)
      '2 = 4 = 6'

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
    I.reduce(collection, nil, fn
      entry, nil -> to_binary(mapper.(entry))
      entry, acc -> acc <> joiner <> to_binary(mapper.(entry))
    end)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`
  while also keeping an accumulator. Returns a tuple where
  the first element is the mapped collection and the second
  one is the final accumulator.

  For dicts, the first tuple element has to be a { key, value }
  tuple itself.

  ## Examples

      iex> Enum.map_reduce([1, 2, 3], 0, fn(x, acc) -> { x * 2, x + acc } end)
      { [2, 4, 6], 6 }

  """
  @spec map_reduce(t, any, (element, any -> any)) :: any
  def map_reduce(collection, acc, f) when is_list(collection) do
    :lists.mapfoldl(f, acc, collection)
  end

  def map_reduce(collection, acc, fun) do
    { list, acc } = I.reduce(collection, { [], acc }, fn(entry, { list, acc }) ->
      { new_entry, acc } = fun.(entry, acc)
      { [new_entry|list], acc }
    end)
    { :lists.reverse(list), acc }
  end

  @doc """
  Partitions `collection` into two where the first one contains elements
  for which `fun` returns a truthy value, and the second one -- for which `fun`
  returns false or nil.

  ## Examples

      iex> Enum.partition([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      { [2], [1,3] }

  """
  @spec partition(t, (element -> any)) :: {list, list}
  def partition(collection, fun) when is_list(collection) do
    do_partition(collection, fun, [], [])
  end

  def partition(collection, fun) do
    { acc1, acc2 } = I.reduce(collection, { [], [] }, fn(entry, { acc1, acc2 }) ->
      if fun.(entry) do
        { [entry|acc1], acc2 }
      else
        { acc1, [entry|acc2] }
      end
    end)

    { :lists.reverse(acc1), :lists.reverse(acc2) }
  end

  @doc """
  Invokes `fun` for each element in the collection passing that element and the
  accumulator `acc` as arguments. `fun`'s return value is stored in `acc`.
  Returns the accumulator.

  ## Examples

      iex> Enum.reduce([1, 2, 3], 0, fn(x, acc) -> x + acc end)
      6

  """
  @spec reduce(t, any, (element, any -> any)) :: any
  def reduce(collection, acc, fun) when is_list(collection) do
    :lists.foldl(fun, acc, collection)
  end

  def reduce(collection, acc, fun) do
    I.reduce(collection, acc, fun)
  end

  @doc """
  Reverses the collection.

  ## Examples

      iex> Enum.reverse([1, 2, 3])
      [3, 2, 1]

  """
  @spec reverse(t) :: list
  def reverse(collection) when is_list(collection) do
    :lists.reverse(collection)
  end

  def reverse(collection) do
    I.reduce(collection, [], fn(entry, acc) ->
      [entry|acc]
    end)
  end

  @doc """
  Returns a sorted list of collection elements. Uses the merge sort algorithm.

  ## Examples

      iex> Enum.sort([3,2,1])
      [1,2,3]

  """
  @spec sort(t) :: list
  def sort(collection) when is_list(collection) do
    :lists.sort(collection)
  end

  def sort(collection) do
    fun = &1 <= &2
    I.reduce(collection, [], sort_reducer(&1, &2, fun)) |>
      sort_terminator(fun)
  end

  @doc """
  Returns a sorted list of collection elements. Uses the merge sort algorithm.

  ## Examples

      iex> Enum.sort([1,2,3], &1 > &2)
      [3,2,1]

  """
  @spec sort(t, (element, element -> boolean)) :: list
  def sort(collection, fun) when is_list(collection) do
    :lists.sort(fun, collection)
  end

  def sort(collection, fun) do
    I.reduce(collection, [], sort_reducer(&1, &2, fun)) |>
      sort_terminator(fun)
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

      iex> Enum.split([1,2,3], 2)
      { [1,2], [3] }
      iex> Enum.split([1,2,3], 10)
      { [1,2,3], [] }
      iex> Enum.split([1,2,3], 0)
      { [], [1,2,3] }
      iex> Enum.split([1,2,3], -1)
      { [1,2], [3] }
      iex> Enum.split([1,2,3], -5)
      { [], [1,2,3] }

  """
  @spec split(t, integer) :: {list, list}
  def split(collection, count) when is_list(collection) and count >= 0 do
    do_split(collection, count, [])
  end

  def split(collection, count) when count >= 0 do
    { _, list1, list2 } =
      I.reduce(collection, { count, [], [] }, fn(entry, { counter, acc1, acc2 }) ->
        if counter > 0 do
          { counter - 1, [entry|acc1], acc2 }
        else
          { counter, acc1, [entry|acc2] }
        end
      end)

    { :lists.reverse(list1), :lists.reverse(list2) }
  end

  def split(collection, count) when count < 0 do
    { list, count } = iterate_and_count(collection, count)
    split(list, count)
  end

  @doc """
  Splits `collection` in two while `fun` returns true.

  ## Examples

      iex> Enum.split_while([1,2,3,4], fn(x) -> x < 3 end)
      { [1, 2], [3, 4] }
  """
  @spec split_while(t, (element -> as_boolean(term))) :: {list, list}
  def split_while(collection, fun) when is_list(collection) do
    do_split_while(collection, fun, [])
  end

  def split_while(collection, fun) do
    { _, list1, list2 } =
      I.reduce(collection, { false, [], [] }, fn(entry, { split, acc1, acc2 }) ->
        cond do
          split ->
            { true, acc1, [entry|acc2] }
          fun.(entry) ->
            { false, [entry|acc1], acc2 }
          true ->
            { true, acc1, [entry|acc2] }
          end
      end)

    { :lists.reverse(list1), :lists.reverse(list2) }
  end

  @doc """
  Takes the first `count` items from the collection. Expects an ordered
  collection.

  ## Examples

      iex> Enum.take([1,2,3], 2)
      [1,2]
      iex> Enum.take([1,2,3], 10)
      [1,2,3]
      iex> Enum.take([1,2,3], 0)
      []

  """
  @spec take(t, integer) :: list

  def take(_collection, 0) do
    []
  end

  def take(collection, count) when is_list(collection) and count > 0 do
    do_take(collection, count)
  end

  def take(collection, count) when count > 0 do
    { list, _ } = I.reduce(collection, { [], count }, fn(entry, { list, count }) ->
      if count > 0 do
        { [entry|list], count - 1 }
      else
        throw({ :enum_take, list })
      end
    end)
    :lists.reverse(list)
  catch
    { :enum_take, list } -> :lists.reverse(list)
  end

  def take(collection, count) when count < 0 do
    { list, count } = iterate_and_count(collection, count)
    take(list, count)
  end

  @doc """
  Takes the items at the beginning of `collection` while `fun` returns true.
  Expects an ordered collection.

  ## Examples

      iex> Enum.take_while([1,2,3], fn(x) -> x < 3 end)
      [1, 2]

  """
  @spec take_while(t, (element -> as_boolean(term))) :: list
  def take_while(collection, fun) when is_list(collection) do
    do_take_while(collection, fun)
  end

  def take_while(collection, fun) do
    I.reduce(collection, [], fn(entry, list) ->
      if fun.(entry) do
        [entry|list]
      else
        throw({ :enum_take, list })
      end
    end) |> :lists.reverse
  catch
    { :enum_take, list } -> :lists.reverse(list)
  end

  @doc """
  Convert `collection` to a list.

  ## Examples

      iex> Enum.to_list(1 .. 3)
      [1, 2, 3]

  """
  @spec to_list(t) :: [term]
  def to_list(collection) when is_list collection do
    collection
  end

  def to_list(collection) do
    I.reduce(collection, [], fn(entry, acc) ->
      [entry|acc]
    end) |> :lists.reverse
  end

  @doc """
  Iterates the enumerable removing all duplicated items.

  ## Examples

      iex> Enum.uniq([1,2,3,2,1])
      [1, 2, 3]

      iex> Enum.uniq([{1,:x}, {2,:y}, {1,:z}], fn {x,_} -> x end)
      [{1,:x}, {2,:y}]

  """
  @spec uniq(t) :: list
  @spec uniq(t, (element -> term)) :: boolean
  def uniq(collection, fun // fn x -> x end)

  def uniq(collection, fun) when is_list(collection) do
    do_uniq(collection, [], fun)
  end

  def uniq(collection, fun) do
    { list, _ } = I.reduce(collection, { [], [] }, fn(entry, { acc, fun_acc }) ->
      fun_entry = fun.(entry)
      if :lists.member(fun_entry, fun_acc) do
        { acc, fun_acc }
      else
        { [entry|acc], [fun_entry|fun_acc] }
      end
    end)

    :lists.reverse(list)
  end

  @doc """
  Zips corresponding elements from two collections into one list
  of tuples. The number of elements in the resulting list is
  dictated by the first enum. In case the second list is shorter,
  values are filled with nil.
  """
  @spec zip(t, t) :: [{any, any}]
  def zip(coll1, coll2) when is_list(coll1) and is_list(coll2) do
    do_zip(coll1, coll2)
  end

  def zip(coll1, coll2) do
    list = to_list(coll1)
    { zipped, rest } = I.reduce(coll2, { [], list }, fn
      entry, { acc, [h|t] } -> { [{h, entry}|acc], t }
      _,     { acc, [] }    -> throw { :enum_zip, acc }
    end)

    list = :lists.foldl(fn(x, acc) -> [{ x, nil }|acc] end, zipped, rest)
    :lists.reverse(list)
  catch
    { :enum_zip, rest } -> :lists.reverse(rest)
  end

  @doc """
  Returns the maximum value.
  Raises empty error in case the collection is empty.

  ## Examples

      iex> Enum.max([1,2,3])
      3

  """
  @spec max(t) :: element | no_return
  def max(collection) when is_list(collection) do
    if collection == [], do: raise Enum.EmptyError
    :lists.max(collection)
  end

  def max(collection) do
    result = I.reduce(collection, :first, fn
      entry, :first ->
        { :reduce, entry }
      entry, { :reduce, max } ->
        if entry > max, do: { :reduce, entry }, else: { :reduce, max }
    end)

    case result do
      :first                                         -> raise Enum.EmptyError
      { :reduce, max } -> max
    end
  end


  @doc """
  Returns the maximum value.
  Raises empty error in case the collection is empty.

  ## Examples

      iex> Enum.max(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "aaa"

  """
  @spec max(t, (element -> any)) :: element | no_return
  def max(collection, fun) when is_list(collection) do
    do_max_first(collection, fun)
  end

  def max(collection, fun) do
    result = I.reduce(collection, :first, fn
      entry, :first ->
        { :reduce, entry, fun.(entry) }
      entry, { :reduce, _, fun_max } = old ->
        fun_entry = fun.(entry)
        if fun_entry > fun_max, do: { :reduce, entry, fun_entry }, else: old
    end)

    case result do
      :first                -> raise Enum.EmptyError
      { :reduce, entry, _ } -> entry
    end
  end

  @doc """
  Returns the manimum value.
  Raises empty error in case the collection is empty.

  ## Examples

      iex> Enum.min([1,2,3])
      1

  """
  @spec min(t) :: element | no_return
  def min(collection) when is_list(collection) do
    if collection == [], do: raise Enum.EmptyError
    :lists.min(collection)
  end

  def min(collection) do
    result = I.reduce(collection, :first, fn
      entry, :first ->
        { :reduce, entry }
      entry, { :reduce, min } ->
        if entry < min, do: { :reduce, entry }, else: { :reduce, min }
    end)

    case result do
      :first           -> raise Enum.EmptyError
      { :reduce, min } -> min
    end
  end

  @doc """
  Returns the manimum value.
  Raises empty error in case the collection is empty.

  ## Examples

      iex> Enum.min(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "a"

  """
  @spec min(t, (element -> any)) :: element | no_return
  def min(collection, fun) when is_list(collection) do
    do_min_first(collection, fun)
  end

  def min(collection, fun) do
    result = I.reduce(collection, :first, fn
      entry, :first ->
        { :reduce, entry, fun.(entry) }
      entry, { :reduce, _, fun_min } = old ->
        fun_entry = fun.(entry)
        if fun_entry < fun_min, do: { :reduce, entry, fun_entry }, else: old
    end)

    case result do
      :first                -> raise Enum.EmptyError
      { :reduce, entry, _ } -> entry
    end
  end

  ## Helpers

  defp iterate_and_count(collection, count) do
    { list, total_items } = do_iterate_and_count(collection)
    { list, Kernel.max(0, total_items - abs(count)) }
  end

  defp do_iterate_and_count(collection) when is_list(collection) do
    { collection, length(collection) }
  end

  defp do_iterate_and_count(collection) do
    reducer = fn(x, acc) -> { x, acc + 1 } end
    map_reduce(collection, 0, reducer)
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

  ## fetch

  defp do_fetch([h|_], 0), do: { :ok, h }
  defp do_fetch([_|t], n), do: do_fetch(t, n - 1)
  defp do_fetch([], _),    do: :error

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

  ## find_value

  defp do_find_value([h|t], ifnone, fun) do
    fun.(h) || do_find_value(t, ifnone, fun)
  end

  defp do_find_value([], ifnone, _) do
    ifnone
  end

  ## join

  defp do_join([h|t], joiner, nil) do
    do_join(t, joiner, to_binary(h))
  end

  defp do_join([h|t], joiner, acc) do
    acc = acc <> joiner <> to_binary(h)
    do_join(t, joiner, acc)
  end

  defp do_join([], _joiner, acc) do
    acc || ""
  end

  ## map join

  defp do_map_join([h|t], mapper, joiner, nil) do
    do_map_join(t, mapper, joiner, to_binary(mapper.(h)))
  end

  defp do_map_join([h|t], mapper, joiner, acc) do
    acc = acc <> joiner <> to_binary(mapper.(h))
    do_map_join(t, mapper, joiner, acc)
  end

  defp do_map_join([], _mapper, _joiner, acc) do
    acc || ""
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

  ## sort

  defp sort_reducer(entry, { :split, y, x, r, rs, bool }, fun) do
    cond do
      fun.(y, entry) == bool ->
        { :split, entry, y, [x|r], rs, bool }
      fun.(x, entry) == bool ->
        { :split, y, entry, [x|r], rs, bool }
      r == [] ->
        { :split, y, x, [entry], rs, bool }
      true ->
        { :pivot, y, x, r, rs, entry, bool }
    end
  end

  defp sort_reducer(entry, { :pivot, y, x, r, rs, s, bool }, fun) do
    cond do
      fun.(y, entry) == bool ->
        { :pivot, entry, y, [x | r], rs, s, bool }
      fun.(x, entry) == bool ->
        { :pivot, y, entry, [x | r], rs, s, bool }
      fun.(s, entry) == bool ->
        { :split, entry, s, [], [[y, x | r] | rs], bool }
      true ->
        { :split, s, entry, [], [[y, x | r] | rs], bool }
    end
  end

  defp sort_reducer(entry, [x], fun) do
    { :split, entry, x, [], [], fun.(x, entry) }
  end

  defp sort_reducer(entry, acc, _fun) do
    [entry|acc]
  end

  defp sort_terminator({ :split, y, x, r, rs, bool }, fun) do
    sort_merge([[y, x | r] | rs], fun, bool)
  end

  defp sort_terminator({ :pivot, y, x, r, rs, s, bool }, fun) do
    sort_merge([[s], [[y, x | r] | rs]], fun, bool)
  end

  defp sort_terminator(acc, _fun) do
    acc
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

  ## uniq

  defp do_uniq([h|t], acc, fun) do
    fun_h = fun.(h)
    case :lists.member(fun_h, acc) do
      true  -> do_uniq(t, acc, fun)
      false -> [h|do_uniq(t, [fun_h|acc], fun)]
    end
  end

  defp do_uniq([], _acc, _fun) do
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

  defp do_zip_next([h|t]), do: { h, t }
  defp do_zip_next([]),    do: { nil, [] }

  ## max

  defp do_max_first([], _) do
    raise Enum.EmptyError
  end

  defp do_max_first([h|t], fun) do
    do_max(t, fun, h, fun.(h))
  end

  defp do_max([], _, acc, _) do
    acc
  end

  defp do_max([h|t], fun, acc, applied_acc) do
    applied = fun.(h)
    if applied > applied_acc do
      do_max(t, fun, h, applied)
    else
      do_max(t, fun, acc, applied_acc)
    end
  end

  ## min

  defp do_min_first([], _) do
    raise Enum.EmptyError
  end

  defp do_min_first([h|t], fun) do
    do_min(t, fun, h, fun.(h))
  end

  defp do_min([], _, acc, _) do
    acc
  end

  defp do_min([h|t], fun, acc, applied_acc) do
    applied = fun.(h)
    if applied < applied_acc do
      do_min(t, fun, h, applied)
    else
      do_min(t, fun, acc, applied_acc)
    end
  end
end

defimpl Enum.Iterator, for: List do
  def reduce([h|t], acc, fun) do
    reduce(t, fun.(h, acc), fun)
  end

  def reduce([], acc, _fun) do
    acc
  end

  def member?([], _),       do: false
  def member?(list, value), do: :lists.member(value, list)

  def count(list), do: length(list)
end

defimpl Enum.Iterator, for: Function do
  def reduce(function, acc, fun) do
    function.(acc, fun)
  end

  def member?(function, value) do
    function.(false, fn(entry, _) ->
      if entry === value, do: throw(:function_member?), else: false
    end)
  catch
    :function_member? -> true
  end

  def count(function) do
    function.(0, fn(_, acc) -> acc + 1 end)
  end
end
