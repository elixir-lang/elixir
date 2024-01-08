defmodule Module.Types.Descr do
  @moduledoc false

  # The descr contains a set-theoretic implementation of types.
  # Types are represented as maps of non-overlapping unions.
  # A bitmap is used to represent non-divisible types. All other
  # types require specific data structures.
  import Bitwise

  @binary 1 <<< 1
  @empty_list 1 <<< 2
  @integer 1 <<< 3
  @float 1 <<< 4
  @pid 1 <<< 5
  @port 1 <<< 6
  @reference 1 <<< 7

  @atom 1 <<< 8
  @non_empty_list 1 <<< 9
  @map 1 <<< 10
  @tuple 1 <<< 11
  @fun 1 <<< 12
  @top (1 <<< 13) - 1

  # Guard helpers

  defguard is_none(map) when map == %{}

  # Type definitions

  def dynamic(), do: :dynamic
  def term(), do: %{bitmap: @top}
  def none(), do: %{}

  def atom(_atom), do: %{bitmap: @atom}
  def binary(), do: %{bitmap: @binary}
  def empty_list(), do: %{bitmap: @empty_list}
  def integer(), do: %{bitmap: @integer}
  def float(), do: %{bitmap: @float}
  def fun(), do: %{bitmap: @fun}
  def map(), do: %{bitmap: @map}
  def non_empty_list(), do: %{bitmap: @non_empty_list}
  def pid(), do: %{bitmap: @pid}
  def port(), do: %{bitmap: @port}
  def reference(), do: %{bitmap: @reference}
  def tuple(), do: %{bitmap: @tuple}

  ## Set operations

  @doc """
  Computes the union of two descrs.
  """
  def union(%{} = left, %{} = right) do
    # Erlang maps:merge_with/3 has to preserve the order in combiner.
    # We don't care about the order, so we have a faster implementation.
    if map_size(left) > map_size(right) do
      iterator_union(:maps.next(:maps.iterator(right)), left)
    else
      iterator_union(:maps.next(:maps.iterator(left)), right)
    end
  end

  @compile {:inline, union: 3}
  defp union(:bitmap, v1, v2), do: bitmap_union(v1, v2)

  @doc """
  Computes the intersection of two descrs.
  """
  def intersection(%{} = left, %{} = right) do
    # Erlang maps:intersect_with/3 has to preserve the order in combiner.
    # We don't care about the order, so we have a faster implementation.
    if map_size(left) > map_size(right) do
      iterator_intersection(:maps.next(:maps.iterator(right)), left, [])
    else
      iterator_intersection(:maps.next(:maps.iterator(left)), right, [])
    end
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  @compile {:inline, intersection: 3}
  defp intersection(:bitmap, v1, v2), do: bitmap_intersection(v1, v2)

  @doc """
  Computes the difference between two types.
  """
  def difference(left = %{}, right = %{}) do
    iterator_difference(:maps.next(:maps.iterator(right)), left)
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  @compile {:inline, difference: 3}
  defp difference(:bitmap, v1, v2), do: bitmap_difference(v1, v2)

  @doc """
  Converts a descr to its quoted representation.
  """
  def to_quoted(%{} = descr) do
    case Enum.flat_map(descr, fn {key, value} -> to_quoted(key, value) end) do
      [] -> {:none, [], []}
      unions -> unions |> Enum.sort() |> Enum.reduce(&{:or, [], [&2, &1]})
    end
  end

  @compile {:inline, to_quoted: 2}
  defp to_quoted(:bitmap, val), do: bitmap_to_quoted(val)

  @doc """
  Converts a descr to its quoted string representation.
  """
  def to_quoted_string(descr) do
    descr
    |> to_quoted()
    |> Code.Formatter.to_algebra()
    |> Inspect.Algebra.format(98)
    |> IO.iodata_to_binary()
  end

  ## Iterator helpers

  defp iterator_union({key, v1, iterator}, map) do
    acc =
      case map do
        %{^key => v2} -> %{map | key => union(key, v1, v2)}
        %{} -> Map.put(map, key, v1)
      end

    iterator_union(:maps.next(iterator), acc)
  end

  defp iterator_union(:none, map), do: map

  defp iterator_intersection({key, v1, iterator}, map, acc) do
    acc =
      case map do
        %{^key => v2} ->
          case intersection(key, v1, v2) do
            0 -> acc
            value -> [{key, value} | acc]
          end

        %{} ->
          acc
      end

    iterator_intersection(:maps.next(iterator), map, acc)
  end

  defp iterator_intersection(:none, _map, acc), do: :maps.from_list(acc)

  defp iterator_difference({key, v2, iterator}, map) do
    acc =
      case map do
        %{^key => v1} ->
          case difference(key, v1, v2) do
            0 -> Map.delete(map, key)
            value -> %{map | key => value}
          end

        %{} ->
          map
      end

    iterator_difference(:maps.next(iterator), acc)
  end

  defp iterator_difference(:none, map), do: map

  ## Bitmaps

  defp bitmap_union(v1, v2), do: v1 ||| v2
  defp bitmap_intersection(v1, v2), do: v1 &&& v2
  defp bitmap_difference(v1, v2), do: v1 - (v1 &&& v2)

  defp bitmap_to_quoted(val) do
    pairs =
      [
        binary: @binary,
        empty_list: @empty_list,
        integer: @integer,
        float: @float,
        pid: @pid,
        port: @port,
        reference: @reference,
        atom: @atom,
        non_empty_list: @non_empty_list,
        map: @map,
        tuple: @tuple,
        fun: @fun
      ]

    for {type, mask} <- pairs,
        (mask &&& val) !== 0,
        do: {type, [], []}
  end
end
