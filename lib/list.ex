# Implements functions that only makes sense to lists
# and cannot be part of the Enum protocol. In general,
# favor using the Enum API instead of List.
defmodule List do
  def append(list) do
    Erlang.lists.append(list)
  end

  def append([h], right) do
    [h|right]
  end

  def append(left, right) do
    left ++ right
  end

  def reverse(list) do
    Erlang.lists.reverse(list)
  end

  def uniq(list) do
    _uniq(list, [])
  end

  def wrap(list) when is_list(list), do: list
  def wrap(nil),   do: []
  def wrap(other), do: [other]

  ## Private

  defp _uniq([h|t], acc) do
    case Erlang.lists.member(h, acc) do
    match: true
      _uniq(t, acc)
    match: false
      _uniq(t, [h|acc])
    end
  end

  defp _uniq([], acc) do
    Erlang.lists.reverse(acc)
  end
end