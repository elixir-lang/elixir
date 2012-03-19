# Implements functions that only makes sense to lists
# and cannot be part of the Enum protocol. In general,
# favor using the Enum API instead of List.
#
# A decision was taken to delegate most functions to
# Erlang's standard lib. Small performance gains
# might be acquired by re-implementing them in Elixir.
defmodule List do
  # Bifs: member/2, reverse/2
  # Bifs: keymember/3, keysearch/3, keyfind/3

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

  @doc """
  Appends the list of lists all the given lists together.

  ## Examples

      List.append [[1,[2],3], [4], [5,6]]
      #=> [1,[2],3,4,5,6]

  """
  def append(list) when is_list(list) do
    Erlang.lists.append(list)
  end

  @doc """
  Appends the list on the right to the list on the left.
  If the list on the left contains only one element, the
  we simply add it as a head as an optimization. This
  function does the same as the `++` operator.

  ## Examples

      List.append [1,2,3], [4,5,6]
      #=> [1,2,3,4,5,6]

  """
  def append(list, elements) when is_list(list) and is_list(elements) do
    list ++ elements
  end

  @doc """
  Flattens the given `list` of lists. An optional
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
  Prepend the items given as first argument to list
  as right argument. Note that items are prepended in
  reverse order. This function does not modify the tail
  and therefore does not duplicate the entries in memory.

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
  Returns a list as a sequence from first to last.
  Returns an empty list if last is lower than first.

  ## Examples

      List.seq(1, 3) #=> [1,2,3]
      List.seq(1, 1) #=> [1]

  """
  def seq(first, last) when is_integer(first) and is_integer(last) and first <= last + 1 do
    Erlang.lists.seq(first, last)
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
