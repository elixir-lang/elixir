# Implements functions that only makes sense to lists
# and cannot be part of the Enum protocol. In general,
# favor using the Enum API instead of List.
defmodule List do
  # Bifs: member/2, reverse/2
  # Bifs: keymember/3, keysearch/3, keyfind/3

  # Appends the list of lists all the given lists together.
  #
  # ## Examples
  #
  #     List.append [[1,[2],3], [4], [5,6]]
  #     #=> [1,[2],3,4,5,6]
  #
  def append(list) do
    Erlang.lists.append(list)
  end

  # Appends the list on the right to the list on the left.
  # If the list on the left contains only one element, the
  # we simply add it as a head as an optimization. This
  # function does the same as the `++` operator.
  #
  # ## Examples
  #
  #     List.append [1,2,3], [4,5,6]
  #     #=> [1,2,3,4,5,6]
  #
  def append([h], right) when is_list(right) do
    [h|right]
  end

  def append(left, right) do
    left ++ right
  end

  # Flattens the given `list` of lists. An optional
  # tail can be given that will be added at the end of
  # the flattened list.
  #
  # ## Examples
  #
  #     List.flatten [1,[[2],3]]
  #     # => [1,2,3]
  #
  #     List.flatten [1,[[2],3]], [4,5]
  #     # => [1,2,3,4,5]
  #
  def flatten(list, tail // []) when is_list(list) & is_list(tail) do
    do_flatten(list, tail)
  end

  # Reverses the given list. This function simply delegates
  # to `lists:reverse` which is implemented in C for performance.
  #
  # ## Examples
  #
  #     List.reverse [1,2,3]
  #     #=> [3,2,1]
  #
  defdelegate [reverse: 1], to: Erlang.lists

  # Checks if the given `term` is included in the list.
  # This function simply delegates to `lists:member`
  # which is implemented in C for performance.
  #
  # ## Examples
  #
  #     List.member? [1,2,3], 1
  #     #=> true
  #
  #     List.member? [1,2,3], 0
  #     #=> false
  #
  def member?(list, term) when is_list(list) do
    Erlang.lists.member(term, list)
  end

  # Returns a list as a sequence from first to last.
  # Raises an error if first is higher than last.
  #
  # ## Examples
  #
  #     List.seq(1, 3) #=> [1,2,3]
  #     List.seq(1, 1) #=> [1]
  #
  def seq(first, last) when is_integer(first) & is_integer(last) & first <= last do
    do_seq(last - first + 1, last, [])
  end

  # Returns a list without duplicated items.
  #
  # ## Examples
  #
  #     List.uniq [1,2,3,2,1]
  #     #=> [1,2,3]
  #
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