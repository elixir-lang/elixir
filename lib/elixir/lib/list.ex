defmodule List do
  @moduledoc """
  Implements functions that only make sense for lists
  and cannot be part of the Enum protocol. In general,
  favor using the Enum API instead of List.

  A decision was taken to delegate most functions to
  Erlang's standard lib but following Elixir's convention
  of receiving the target (in this case, a list) as the
  first argument.
  """

  @doc """
  Given a list of lists, concatenates the sublists into a single list.

  ## Examples

      List.concat [[1,[2],3], [4], [5,6]]
      #=> [1,[2],3,4,5,6]

  """
  def concat(list) when is_list(list) do
    :lists.append(list)
  end

  @doc """
  Concatenates the list on the right with the list on the left.

  This function produces the same result the `++` operator. The only difference
  is a minor optimization: when the first list contains only one element, we
  simply add it as a head to the second list.

  ## Examples

      List.concat [1,2,3], [4,5,6]
      #=> [1,2,3,4,5,6]

  """
  def concat(list, elements) when is_list(list) and is_list(elements) do
    list ++ elements
  end

  @doc """
  Deletes the given item from the list. Returns a list without the item.
  If the item occurs more than once in the list, just the first occurrence
  is removed.

  ## Examples

      List.delete([1,2,3], 1)
      #=> [2,3]

  """
  def delete(list, item) do
    :lists.delete(item, list)
  end

  @doc """
  Duplicates the given element n times in a list.

  ## Examples

      List.duplicate "hello", 3
      #=> ["hello","hello","hello"]

      List.duplicate [1,2], 2
      #=> [[1,2],[1,2]]
  """
  def duplicate(elem, n) do
    :lists.duplicate(n, elem)
  end

  @doc """
  Flattens the given `list` of nested lists. An optional
  tail can be given that will be added at the end of
  the flattened list.

  ## Examples

      List.flatten [1,[[2],3]]
      #=> [1,2,3]

      List.flatten [1,[[2],3]], [4,5]
      #=> [1,2,3,4,5]

  """
  def flatten(list) do
    :lists.flatten(list)
  end

  def flatten(list, tail) do
    :lists.flatten(list, tail)
  end

  @doc """
  Folds (reduces) the given list to the left with
  a function. Requires an accumulator.

  ## Examples

      List.foldl [5,5], 10, fn x, acc -> x + acc end
      #=> 20

      List.foldl [1,2,3,4], 0, fn x, acc -> x - acc end
      #=> 2

  """
  def foldl(list, acc, function) when is_list(list) and is_function(function) do
    :lists.foldl(function, acc, list)
  end

  @doc """
  Folds (reduces) the given list to the right with
  a function. Requires an accumulator.

  ## Examples

      List.foldr [1,2,3,4], 0, fn x, acc -> x - acc end
      #=> -2

  """
  def foldr(list, acc, function) when is_list(list) and is_function(function) do
    :lists.foldr(function, acc, list)
  end

  @doc false
  def reverse(list) do
    IO.write "[WARNING] List.reverse is deprecated, please use Enum.reverse instead\n#{Exception.formatted_stacktrace}"
    :lists.reverse(list)
  end

  @doc """
  Returns the last element in `list` or nil if the `list` is empty.

  ## Examples

      List.last []
      #=> nil
      List.last [1]
      #=> 1
      List.last [1, 2, 3]
      #=> 3

  """
  def last([]), do: nil

  def last(list) do
    :lists.last(list)
  end

  @doc """
  Checks if the given `term` is included in the list.
  This function simply delegates to `lists:member`
  which is implemented in C for performance.

  ## Examples

      List.member? [1,2,3], 1
      #=> true

      List.member? [1,2,3], 0
      #=> false

  """
  def member?(list, term) do
    :lists.member(term, list)
  end

  @doc """
  Receives a list of tuples and returns the first tuple
  where the item at position `posistion` matches with the
  given `item`.

  ## Examples

      List.keyfind([a: 1, b: 2], :a, 0)
      #=> { :a, 1 }

      List.keyfind([a: 1, b: 2], 2, 1)
      #=> { :b, 2 }

      List.keyfind([a: 1, b: 2], :c, 0)
      #=> nil

  """
  def keyfind(list, key, position, default // nil) do
    :lists.keyfind(key, position + 1, list) || default
  end

  @doc """
  Receives a list of tuples and returns true if there is
  a tuple where the item at position `posistion` matches
  with the given `item`.

  ## Examples

      List.keymember?([a: 1, b: 2], :a, 0)
      #=> true

      List.keymember?([a: 1, b: 2], 2, 1)
      #=> true

      List.keymember?([a: 1, b: 2], :c, 0)
      #=> false

  """
  def keymember?(list, key, position) do
    :lists.keymember(key, position + 1, list)
  end

  @doc """
  Receives a list of tuples and replaces the item
  identified by `key` at position `pos` if it exists.

  ## Examples

      List.keyreplace([a: 1, b: 2], :a, 0, { :a, 3 })
      #=> [a: 3, b: 2]

  """
  def keyreplace(list, key, position, new_tuple) do
    :lists.keyreplace(key, position + 1, list, new_tuple)
  end

  @doc """
  Receives a list of tuples and replaces the item
  identified by `key` at position `pos`. If the item
  does not exist, it is added to the end of the list.

  ## Examples

      List.keystore([a: 1, b: 2], :a, 0, { :a, 3 })
      #=> [a: 3, b: 2]

  """
  def keystore(list, key, position, new_tuple) do
    :lists.keystore(key, position + 1, list, new_tuple)
  end

  @doc """
  Receives a list of tuples and deletes the first tuple
  where the item at position `posistion` matches with the
  given `item`. Returns the new tuple.

  ## Examples

      List.keydelete([a: 1, b: 2], :a, 0)
      #=> [{ :b, 2 }]

      List.keydelete([a: 1, b: 2], 2, 1)
      #=> [{ :a, 1 }]

      List.keydelete([a: 1, b: 2], :c, 0)
      #=> [{ :a, 1 }, { :b, 2 }]

  """
  def keydelete(list, key, position) do
    :lists.keydelete(key, position + 1, list)
  end

  @doc """
  Returns a list of integers in the given range (both ends included when
  possible). An optional step can be provided as well (defaults to 1).

  If first > last and no step is provided, the numbers will be in descending
  order.

  ## Examples

      List.range 1, 3     #=> [1,2,3]
      List.range 1, 8, 2  #=> [1,3,5,7]
      List.range 1, 0     #=> []
      List.range 3, 1     #=> [3,2,1]
      List.range 5, 1, -2 #=> [5, 3, 1]

  """
  def range(first, last, step // nil)

  def range(first, last, step) when is_integer(first) and is_integer(last) and first <= last do
    step = case step do
      nil ->
        :lists.seq(first, last, 1)
      x when x < 0 ->
        []
      _ ->
        :lists.seq(first, last, step)
    end
  end

  def range(first, last, step) when is_integer(first) and is_integer(last) and first > last do
    step = case step do
      nil ->
        :lists.seq(first, last, -1)
      x when x > 0 ->
        []
      _ ->
        :lists.seq(first, last, step)
    end
  end

  @doc """
  Sorts the list by comparing each term. For an alternative
  sorting algorithm, check `Enum.qsort`.

  ## Examples

      List.sort [3, 4, 2, 1, 7]
      #=> [1, 2, 3, 4, 7]

  """
  def sort(list) do
    :lists.sort list
  end

  @doc """
  Sorts the list according to an ordering function. fun(a, b) should
  return true if `a` compares less than or equal to `b`, `false` otherwise.

  ## Examples

      List.sort [3, 4, 2, 1, 7], fn a, b -> b <= a end
      #=> [7, 4, 3, 2, 1]

  """
  def sort(list, fun) do
    :lists.sort fun, list
  end

  @doc """
  Returns a list without duplicated items.

  ## Examples

      List.uniq [1,2,3,2,1]
      #=> [1,2,3]

  """
  def uniq(list) when is_list(list) do
    do_uniq(list, [])
  end

  @doc """
  Wraps the argument in a list.
  If the argument is already a list, returns the list.
  If the argument is nil, returns an empty list.

  ## Examples

      List.wrap [1,2,3] #=> [1,2,3]

  """
  def wrap(list) when is_list(list) do
    list
  end

  def wrap(nil) do
    []
  end

  def wrap(other) do
    [other]
  end

  @doc """
  Zips corresponding elements from two lists (or tuples) into one list of tuples. The
  number of elements in the resulting list is equal to the length of the
  shortest list among the given ones.

  ## Examples

      List.zip [1, 2, 3], [4, 5, 6]
      #=> [{1, 4}, {2, 5}, {3, 6}]

      List.zip [1, 2], [4, 5, 6]
      #=> [{1, 4}, {2, 5}]

  """
  def zip(item1, item2) do
    do_zip(to_list(item1), to_list(item2), [])
  end

  @doc """
  Zips corresponding elements from each list in `list_of_lists`.

  ## Examples

      List.zip [[1, 2], [3, 4], [5, 6]]
      #=> [{1, 3, 5}, {2, 4, 6}]

      List.zip [[1, 2], [3], [5, 6]]
      #=> [{1, 3, 5}]

  """
  def zip([]), do: []
  def zip(list_of_lists) when is_list(list_of_lists) do
    do_zip(list_of_lists, [])
  end

  @doc """
  Unzips the given list of lists or tuples into separate lists and returns a
  list of lists.

  ## Examples

      List.unzip [{1, 2}, {3, 4}]
      #=> [[1, 3], [2, 4]]

      List.unzip [{1, :a, "apple"}, {2, :b, "banana"}, {3, :c}]
      #=> [[1, 2, 3], [:a, :b, :c]]

  """
  def unzip(list) when is_list(list) do
    :lists.map tuple_to_list(&1), zip(list)
  end

  ## Private

  # uniq

  defp do_uniq([h|t], acc) do
    case :lists.member(h, acc) do
      true ->
        do_uniq(t, acc)
      false ->
        [h|do_uniq(t, [h|acc])]
    end
  end

  defp do_uniq([], _acc) do
    []
  end

  # zip

  defp do_zip([h1|t1], [h2|t2], acc) do
    do_zip t1, t2, [{h1, h2}|acc]
  end

  defp do_zip(_, _, acc) do
    :lists.reverse acc
  end

  defp do_zip(list, acc) do
    converter = fn x, acc -> do_zip_each(to_list(x), acc) end
    {mlist, heads} = :lists.mapfoldl converter, [], list

    case heads do
      nil ->
        :lists.reverse acc
      _ ->
      do_zip mlist, [list_to_tuple(:lists.reverse(heads))|acc]
    end
  end

  defp do_zip_each(_, nil) do
    { nil, nil }
  end

  defp do_zip_each([h|t], acc) do
    { t, [h|acc] }
  end

  defp do_zip_each([], _) do
    { nil, nil }
  end

  defp to_list(tuple) when is_tuple(tuple), do: tuple_to_list(tuple)
  defp to_list(list)  when is_list(list),   do: list
end
