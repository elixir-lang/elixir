defmodule Module.Types.Descr do
  @moduledoc false

  # The descr contains a set-theoretic implementation of types.
  # Types are represented as maps of non-overlapping unions.
  # A bitmap is used to represent non-divisible types. All other
  # types require specific data structures.

  # TODO: When we convert from AST to descr, we need to normalize
  # the dynamic type.
  import Bitwise

  @bit_binary 1 <<< 1
  @bit_empty_list 1 <<< 2
  @bit_integer 1 <<< 3
  @bit_float 1 <<< 4
  @bit_pid 1 <<< 5
  @bit_port 1 <<< 6
  @bit_reference 1 <<< 7

  @bit_atom 1 <<< 8
  @bit_non_empty_list 1 <<< 9
  @bit_map 1 <<< 10
  @bit_tuple 1 <<< 11
  @bit_fun 1 <<< 12
  @bit_top (1 <<< 13) - 1

  # Guard helpers

  @term %{bitmap: @bit_top}
  @none %{}

  # Guard helpers

  @term %{bitmap: @bit_top}
  @none %{}

  # Type definitions

  # TODO: Have an atom for term()
  def dynamic(), do: :dynamic
  def term(), do: @term
  def none(), do: @none

  def atom(_atom), do: %{bitmap: @bit_atom}
  def binary(), do: %{bitmap: @bit_binary}
  def empty_list(), do: %{bitmap: @bit_empty_list}
  def integer(), do: %{bitmap: @bit_integer}
  def float(), do: %{bitmap: @bit_float}
  def fun(), do: %{bitmap: @bit_fun}
  def map(), do: %{bitmap: @bit_map}
  def non_empty_list(), do: %{bitmap: @bit_non_empty_list}
  def pid(), do: %{bitmap: @bit_pid}
  def port(), do: %{bitmap: @bit_port}
  def reference(), do: %{bitmap: @bit_reference}
  def tuple(), do: %{bitmap: @bit_tuple}

  ## Set operations

  @doc """
  Check type is empty.
  """
  def empty?(descr), do: descr == @none

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
        binary: @bit_binary,
        empty_list: @bit_empty_list,
        integer: @bit_integer,
        float: @bit_float,
        pid: @bit_pid,
        port: @bit_port,
        reference: @bit_reference,
        atom: @bit_atom,
        non_empty_list: @bit_non_empty_list,
        map: @bit_map,
        tuple: @bit_tuple,
        fun: @bit_fun
      ]

    for {type, mask} <- pairs,
        (mask &&& val) !== 0,
        do: {type, [], []}
  end
end
