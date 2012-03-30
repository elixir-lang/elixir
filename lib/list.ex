defmodule List do
  @moduledoc """
  Implements functions that only make sense for lists
  and cannot be part of the Enum protocol. In general,
  favor using the Enum API instead of List.

  A decision was taken to delegate most functions to
  Erlang's standard lib. Small performance gains
  might be acquired by re-implementing them in Elixir.
  """

  # Bifs: member/2, reverse/2
  # Bifs: keymember/3, keysearch/3, keyfind/3

  @doc """
  Access the list via a predicate.

  If an integer, it does an index lookup with the index
  starting with 1. Negative indexes performs a reverse
  lookup, for example, -1 can be used to retrieve the
  last element in the list. Returns nil if an out of
  bounds access occurs.

  If a regular expression, it returns a list with the
  matched contents.

  If an atom, assumes the list is wrapping an orddict
  and access the key in the orddict equals to the given
  atom.

  This implements the same API as the `Access` protocol.

  ## Examples

      list = [ :a, :b, :c ]
      List.access list, -1 #=> :c

  """
  def access(list, access) when is_list(list) do
    Access.List.access(list, access)
  end

  @doc """
  Given a list of lists, concatenates the sublists into a single list.

  ## Examples

      List.concat [[1,[2],3], [4], [5,6]]
      #=> [1,[2],3,4,5,6]

  """
  def concat(list) when is_list(list) do
    Erlang.lists.append(list)
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
  def concat([h], elements) when is_list(elements) do
    [h|elements]
  end

  def concat(list, elements) when is_list(list) and is_list(elements) do
    list ++ elements
  end

  @doc """
  Flattens the given `list` of nested lists. An optional
  tail can be given that will be added at the end of
  the flattened list.

  ## Examples

      List.flatten [1,[[2],3]]
      # => [1,2,3]

      List.flatten [1,[[2],3]], [4,5]
      # => [1,2,3,4,5]

  """
  def flatten(list) when is_list(list) do
    Erlang.lists.flatten(list)
  end

  def flatten(list, tail) when is_list(list) and is_list(tail) do
    Erlang.lists.flatten(list, tail)
  end

  @doc """
  Folds (reduces) the given list to the left with
  a function. Requires an accumulator.

  ## Examples

      List.foldl [5,5], 10, fn(x, acc) -> x + acc end
      #=> 20

      List.foldl [1,2,3,4], 0, fn(x, acc) -> x - acc end
      #=> 2

  """
  def foldl(list, acc, function) when is_list(list) and is_function(function) do
    Erlang.lists.foldl(function, acc, list)
  end

  @doc """
  Folds (reduces) the given list to the right with
  a function. Requires an accumulator.

  ## Examples

      List.foldr [1,2,3,4], 0, fn(x, acc) -> x - acc end
      #=> -2

  """
  def foldr(list, acc, function) when is_list(list) and is_function(function) do
    Erlang.lists.foldr(function, acc, list)
  end

  @doc """
  Reverses the given list. This function simply delegates
  to `lists:reverse` which is implemented in C for performance.

  ## Examples

      List.reverse [1,2,3]
      #=> [3,2,1]

  """
  defdelegate [reverse: 1], to: Erlang.lists

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

  defdelegate [last: 1], to: Erlang.lists

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
  def member?(list, term) when is_list(list) do
    Erlang.lists.member(term, list)
  end

  @doc """
  Prepends the items from the list of the left to the list on the right.
  Note that items are prepended in reverse order. This function does not modify
  the tail and therefore does not duplicate the entries in memory.

  ## Examples

      List.prepend [1], [2, 3]
      #=> [1,2,3]

      List.prepend [1,0], [2, 3]
      #=> [0,1,2,3]

  """
  def prepend([h|t], other) do
    prepend(t, [h|other])
  end

  def prepend([], other) do
    other
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
  def range(first, last, step // nil) when is_integer(first) and is_integer(last) and first <= last do
    step = case step do
    match: nil
      Erlang.lists.seq(first, last, 1)
    match: x when x < 0
      []
    else:
      Erlang.lists.seq(first, last, step)
    end
  end

  def range(first, last, step) when is_integer(first) and is_integer(last) and first > last do
    step = case step do
    match: nil
      Erlang.lists.seq(first, last, -1)
    match: x when x > 0
      []
    else:
      Erlang.lists.seq(first, last, step)
    end
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
  Duplicates the given element n times in a list.

  ## Examples

      List.duplicate "hello", 3
      #=> ["hello","hello","hello"]

      List.duplicate [1,2], 2
      #=> [[1,2],[1,2]]
  """
  def duplicate(elem, n) when is_integer(n) do
    Erlang.lists.duplicate(n, elem)
  end

  @doc """
  Looks for a term in a list and returns its position.
  If term is found in the first position, return 1.
  For not terms not found in list, the return value is nil.

  ### Examples

      List.find_index ['a'], 'b'
      #=> nil
      List.find_index ['a'], 'a'
      #=> 1
  """
  def find_index(list, term) do
    index = Erlang.string.str(list, [term])
    case index == 0 do
    match: true
      nil
    match: false
      index
    end
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

  def wrap(else) do
    [else]
  end

  ## Private

  # uniq

  defp do_uniq([h|t], acc) do
    case Erlang.lists.member(h, acc) do
    match: true
      do_uniq(t, acc)
    match: false
      [h|do_uniq(t, [h|acc])]
    end
  end

  defp do_uniq([], _acc) do
    []
  end
end
