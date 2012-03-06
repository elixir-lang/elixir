# Implements functions that only makes sense to lists
# and cannot be part of the Enum protocol. In general,
# favor using the Enum API instead of List.
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
  def append(list) do
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
  def append(left, right) do
    left ++ right
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
  def flatten(list, tail // []) when is_list(list) and is_list(tail) do
    do_flatten(list, tail)
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
  def foldl(list, acc, function) do
    Erlang.lists.foldl(function, acc, list)
  end

  @doc """
  Folds (reduces) the given list to the right with
  a function. Requires an accumulator.

  ## Examples

      List.foldl [1,2,3,4], 0, fn(x, acc) -> x - acc end
      #=> -2

  """
  def foldr(list, acc, function) do
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
    do_seq(last - first + 1, last, [])
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

  ## Private

  # flatten

  defp do_flatten([h|t], tail) when is_list(h) do
    do_flatten(h, do_flatten(t, tail))
  end

  defp do_flatten([h|t], tail) do
    [h|do_flatten(t, tail)]
  end

  defp do_flatten([], tail) do
    tail
  end

  # seq

  defp do_seq(n, x, l) when n >= 4 do
    do_seq(n-4, x-4, [x-3,x-2,x-1,x|l])
  end

  defp do_seq(n, x, l) when n >= 2 do
    do_seq(n-2, x-2, [x-1,x|l])
  end

  defp do_seq(1, x, l) do
    [x|l]
  end

  defp do_seq(0, _, l) do
    l
  end

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