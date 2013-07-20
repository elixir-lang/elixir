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

  @compile :inline_list_funcs

  @doc """
  Given a list of lists, concatenates the sublists into a single list.

  ## Examples

      iex> List.concat([[1, [2], 3], [4], [5, 6]])
      [1,[2],3,4,5,6]

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

      iex> List.concat([1, 2, 3], [4, 5, 6])
      [1,2,3,4,5,6]

  """
  def concat(list, elements) when is_list(list) and is_list(elements) do
    list ++ elements
  end

  @doc """
  Deletes the given item from the list. Returns a list without the item.
  If the item occurs more than once in the list, just the first occurrence
  is removed.

  ## Examples

      iex> List.delete([1, 2, 3], 1)
      [2,3]

  """
  def delete(list, item) do
    :lists.delete(item, list)
  end

  @doc """
  Duplicates the given element n times in a list.

  ## Examples

      iex> List.duplicate("hello", 3)
      ["hello","hello","hello"]

      iex> List.duplicate([1, 2], 2)
      [[1,2],[1,2]]
  """
  def duplicate(elem, n) do
    :lists.duplicate(n, elem)
  end

  @doc """
  Flattens the given `list` of nested lists. An optional
  tail can be given that will be added at the end of
  the flattened list.

  ## Examples

      iex> List.flatten([1, [[2], 3]])
      [1,2,3]

      iex> List.flatten([1, [[2], 3]], [4, 5])
      [1,2,3,4,5]

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

      iex> List.foldl([5, 5], 10, fn (x, acc) -> x + acc end)
      20

      iex> List.foldl([1, 2, 3, 4], 0, fn (x, acc) -> x - acc end)
      2

  """
  def foldl(list, acc, function) when is_list(list) and is_function(function) do
    :lists.foldl(function, acc, list)
  end

  @doc """
  Folds (reduces) the given list to the right with
  a function. Requires an accumulator.

  ## Examples

      iex> List.foldr([1, 2, 3, 4], 0, fn (x, acc) -> x - acc end)
      -2

  """
  def foldr(list, acc, function) when is_list(list) and is_function(function) do
    :lists.foldr(function, acc, list)
  end

  @doc """
  Returns the last element in `list` or nil if the `list` is empty.

  ## Examples

      iex> List.last([])
      nil
      iex> List.last([1])
      1
      iex> List.last([1, 2, 3])
      3

  """
  def last([]), do: nil

  def last(list) do
    :lists.last(list)
  end

  @doc """
  Receives a list of tuples and returns the first tuple
  where the item at position `position` matches with the
  given `item`.

  ## Examples

      iex> List.keyfind([a: 1, b: 2], :a, 0)
      { :a, 1 }

      iex> List.keyfind([a: 1, b: 2], 2, 1)
      { :b, 2 }

      iex> List.keyfind([a: 1, b: 2], :c, 0)
      nil

  """
  def keyfind(list, key, position, default // nil) do
    :lists.keyfind(key, position + 1, list) || default
  end

  @doc """
  Receives a list of tuples and returns true if there is
  a tuple where the item at position `position` matches
  with the given `item`.

  ## Examples

      iex> List.keymember?([a: 1, b: 2], :a, 0)
      true

      iex> List.keymember?([a: 1, b: 2], 2, 1)
      true

      iex> List.keymember?([a: 1, b: 2], :c, 0)
      false

  """
  def keymember?(list, key, position) do
    :lists.keymember(key, position + 1, list)
  end

  @doc """
  Receives a list of tuples and replaces the item
  identified by `key` at position `pos` if it exists.

  ## Examples

      iex> List.keyreplace([a: 1, b: 2], :a, 0, { :a, 3 })
      [a: 3, b: 2]

  """
  def keyreplace(list, key, position, new_tuple) do
    :lists.keyreplace(key, position + 1, list, new_tuple)
  end

  @doc """
  Receives a list of tuples and sorts the items
  at position `pos` of the tuples. The sort is stable.

  ## Examples

      iex> List.keysort([a: 5, b: 1, c: 3], 1)
      [b: 1, c: 3, a: 5]

      iex> List.keysort([a: 5, c: 1, b: 3], 0)
      [a: 5, b: 3, c: 1]

  """
  def keysort(list, position) do
    :lists.keysort(position + 1, list)
  end

  @doc """
  Receives a list of tuples and replaces the item
  identified by `key` at position `pos`. If the item
  does not exist, it is added to the end of the list.

  ## Examples

      iex> List.keystore([a: 1, b: 2], :a, 0, { :a, 3 })
      [a: 3, b: 2]

  """
  def keystore(list, key, position, new_tuple) do
    :lists.keystore(key, position + 1, list, new_tuple)
  end

  @doc """
  Receives a list of tuples and deletes the first tuple
  where the item at position `position` matches with the
  given `item`. Returns the new tuple.

  ## Examples

      iex> List.keydelete([a: 1, b: 2], :a, 0)
      [{ :b, 2 }]

      iex> List.keydelete([a: 1, b: 2], 2, 1)
      [{ :a, 1 }]

      iex> List.keydelete([a: 1, b: 2], :c, 0)
      [{ :a, 1 }, { :b, 2 }]

  """
  def keydelete(list, key, position) do
    :lists.keydelete(key, position + 1, list)
  end

  @doc """
  Wraps the argument in a list.
  If the argument is already a list, returns the list.
  If the argument is nil, returns an empty list.

  ## Examples

      iex> List.wrap([1, 2, 3])
      [1,2,3]

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
  Zips corresponding elements from each list in `list_of_lists`.

  ## Examples

      iex> List.zip([[1, 2], [3, 4], [5, 6]])
      [{1, 3, 5}, {2, 4, 6}]

      iex> List.zip([[1, 2], [3], [5, 6]])
      [{1, 3, 5}]

  """
  def zip([]), do: []
  def zip(list_of_lists) when is_list(list_of_lists) do
    do_zip(list_of_lists, [])
  end

  @doc """
  Unzips the given list of lists or tuples into separate lists and returns a
  list of lists.

  ## Examples

      iex> List.unzip([{1, 2}, {3, 4}])
      [[1, 3], [2, 4]]

      iex> List.unzip([{1, :a, "apple"}, {2, :b, "banana"}, {3, :c}])
      [[1, 2, 3], [:a, :b, :c]]

  """
  def unzip(list) when is_list(list) do
    :lists.map tuple_to_list(&1), zip(list)
  end

  @doc """
  Returns a list with an inserted value at specified index. Note that the index
  is capped at the list length and that negative indicies wraps around at the
  end of the list.

  ## Examples

      iex> List.insert_at([1, 2, 3, 4], 2, 0)
      [1, 2, 0, 3, 4]

      iex> List.insert_at([1, 2, 3], 10, 0)
      [1, 2, 3, 0]

      iex> List.insert_at([1, 2, 3], -1, 0)
      [1, 2, 0, 3]

  """
  def insert_at(list, index, value) do
    if index < 0 do
      do_insert_at(list, length(list) + index, value)
    else
      do_insert_at(list, index, value)
    end
  end

  ## Helpers

  # insert_at

  defp do_insert_at([], _index, value) do
    [ value ]
  end

  defp do_insert_at(list, index, value) when index <= 0 do
    [ value | list ]
  end

  defp do_insert_at([h|t], index, value) do
    [ h | do_insert_at(t, index - 1, value) ]
  end

  # zip

  defp do_zip(list, acc) do
    converter = fn x, acc -> do_zip_each(to_list(x), acc) end
    {mlist, heads} = :lists.mapfoldl converter, [], list

    case heads do
      nil -> :lists.reverse acc
      _   -> do_zip mlist, [list_to_tuple(:lists.reverse(heads))|acc]
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
