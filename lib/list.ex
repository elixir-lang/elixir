# Implements functions that only makes sense to lists
# and cannot be part of the Enum protocol. In general,
# favor using the Enum API instead of List.
defmodule List do
  def append(list) do
    Erlang.lists.append(list)
  end

  def append([h], right) when is_list(right) do
    [h|right]
  end

  def append(left, right) do
    left ++ right
  end

  def flatten(list, tail // []) when is_list(list) andalso is_list(tail) do
    _flatten(list, tail)
  end

  def reverse(list) when is_list(list) do
    Erlang.lists.reverse(list)
  end

  def uniq(list) when is_list(list) do
    _uniq(list, [])
  end

  def wrap(list) when is_list(list), do: list
  def wrap(nil),   do: []
  def wrap(other), do: [other]

  ## Private

  defp _flatten([h|t], tail) when is_list(h) do
    _flatten(h, _flatten(t, tail))
  end

  defp _flatten([h|t], tail) do
    [h|_flatten(t, tail)]
  end

  defp _flatten([], tail) do
    tail
  end

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