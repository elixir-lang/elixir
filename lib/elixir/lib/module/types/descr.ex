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

  @atom_top {MapSet.new([]), false}
  @bit_non_empty_list 1 <<< 9
  @bit_map 1 <<< 10
  @bit_tuple 1 <<< 11
  @bit_fun 1 <<< 12
  @bit_top (1 <<< 13) - 1

  # Guard helpers

  @term %{bitmap: @bit_top, atom: @atom_top}
  @none %{}

  # Type definitions

  def dynamic(), do: :dynamic
  def term(), do: @term
  def none(), do: @none

  def atom(a), do: %{atom: atom_mk(a)}
  def atom(), do: %{atom: @atom_top}
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

  def boolean(), do: atom([true, false])

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
  defp union(:atom, v1, v2), do: atom_union(v1, v2)

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
  defp intersection(:atom, v1, v2), do: atom_intersection(v1, v2)

  @doc """
  Computes the difference between two types.
  """
  def difference(left = %{}, right = %{}) do
    iterator_difference(:maps.next(:maps.iterator(right)), left)
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  @compile {:inline, difference: 3}
  defp difference(:bitmap, v1, v2), do: bitmap_difference(v1, v2)
  defp difference(:atom, v1, v2), do: atom_difference(v1, v2)

  @doc """
  Compute the negation of a type.
  """
  def negation(%{} = descr), do: difference(@term, descr)

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
  defp to_quoted(:atom, val), do: atom_to_quoted(val)

  @doc """
  Converts a descr to its quoted string representation.
  """
  def to_quoted_string(descr) do
    descr
    |> to_quoted_with_print_heuristic()
    |> Code.Formatter.to_algebra()
    |> Inspect.Algebra.format(98)
    |> IO.iodata_to_binary()
  end

  # Heuristic for when to print a type as `not t`: if the number of top types in `t`
  # is greater than 6, we print it as `not (negation(t))`
  #
  # Hence, we print `not integer()` instead of `atom() or float() or tuple() or ...`
  defp to_quoted_with_print_heuristic(descr) do
    if count_top_types(descr) > 6,
      do: {:not, [], [to_quoted(negation(descr))]},
      else: to_quoted(descr)
  end

  defp count_top_types(%{} = descr) do
    Enum.reduce(descr, 0, fn {key, val}, acc -> acc + count_top_types(key, val) end)
  end

  @compile {:inline, count_top_types: 2}
  defp count_top_types(:bitmap, val), do: bitmap_count_top_types(val)
  defp count_top_types(:atom, val), do: atom_count_top_types(val)

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
        non_empty_list: @bit_non_empty_list,
        map: @bit_map,
        tuple: @bit_tuple,
        fun: @bit_fun
      ]

    for {type, mask} <- pairs,
        (mask &&& val) !== 0,
        do: {type, [], []}
  end

  defp bitmap_count_top_types(val), do: Integer.digits(val, 2) |> Enum.sum()

  ## Atoms

  # The atom component of a type consists of pairs `{MapSet.t(atom()), boolean()}`.
  # If the boolean is true, the type represents the union of the atoms in the set.
  # If the boolean is false, the type represents all the possible atoms, except the ones
  # in the set.
  #
  # Example:
  #   - `{MapSet.new([:a, :b]), true}` represents type `:a or :b`
  #   - `{MapSet.new([:c, :d]), false}` represents type `atom() \ (:c or :d)
  #
  # `{MapSet.new([]), false}` is the `atom()` top type, as it is the difference
  # of `atom()` with an empty list.

  defp atom_mk(a) when is_atom(a), do: {MapSet.new([a]), true}
  defp atom_mk(as) when is_list(as), do: {MapSet.new(as), true}

  defp atom_intersection({s1, neg1}, {s2, neg2}) do
    result_set =
      case {neg1, neg2} do
        {true, true} -> MapSet.intersection(s1, s2)
        {false, false} -> MapSet.union(s1, s2)
        {true, false} -> MapSet.difference(s1, s2)
        {false, true} -> MapSet.difference(s2, s1)
      end

    if MapSet.size(result_set) == 0 do
      0
    else
      {result_set, neg1 or neg2}
    end
  end

  defp atom_union({s1, true}, {s2, true}), do: {MapSet.union(s1, s2), true}
  defp atom_union({s1, false}, {s2, false}), do: {MapSet.intersection(s1, s2), false}
  defp atom_union({s1, true}, {s2, false}), do: {MapSet.difference(s2, s1), false}
  defp atom_union({s1, false}, {s2, true}), do: {MapSet.difference(s1, s2), false}

  defp atom_difference({s1, neg1}, {s2, neg2}) do
    result_set =
      case {neg1, neg2} do
        {true, true} -> MapSet.difference(s1, s2)
        {false, false} -> MapSet.difference(s2, s1)
        {true, false} -> MapSet.intersection(s1, s2)
        {false, true} -> MapSet.union(s1, s2)
      end

    if MapSet.size(result_set) == 0 do
      0
    else
      {result_set, neg1 or not neg2}
    end
  end

  defp literal(lit), do: {:__block__, [], [lit]}
  @boolset MapSet.new([true, false])

  defp atom_to_quoted({a = %MapSet{}, true}) do
    if MapSet.subset?(@boolset, a) do
      MapSet.difference(a, @boolset)
      |> Enum.sort()
      |> Enum.map(&literal/1)
      |> Enum.reduce({:boolean, [], []}, &{:or, [], [&2, &1]})
    else
      Enum.sort(a)
      |> Enum.map(&literal/1)
      |> Enum.reduce(&{:or, [], [&2, &1]})
    end
    |> List.wrap()
  end

  defp atom_to_quoted({a = %MapSet{}, false}) do
    if MapSet.size(a) == 0 do
      {:atom, [], []}
    else
      atom_to_quoted({a, true})
      |> Kernel.then(&{:and, [], [{:atom, [], []}, {:not, [], &1}]})
    end
    |> List.wrap()
  end

  defp atom_count_top_types({a, neg}) do
    if not neg and MapSet.size(a) == 0, do: 1, else: 0
  end
end
