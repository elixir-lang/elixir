defmodule MapSet do
  @moduledoc """
  Functions that work on sets.

  `MapSet` is the "go to" set data structure in Elixir. A set can be constructed
  using `MapSet.new/0`:

      iex> MapSet.new
      #MapSet<[]>

  A set can contain any kind of elements and elements in a set don't have to be
  of the same type. By definition, sets can't contain duplicate elements: when
  inserting an element in a set where it's already present, the insertion is
  simply a no-op.

      iex> set = MapSet.new
      iex> MapSet.put(set, "foo")
      #MapSet<["foo"]>
      iex> set |> MapSet.put("foo") |> MapSet.put("foo")
      #MapSet<["foo"]>

  A `MapSet` is represented internally using the `%MapSet{}` struct. This struct
  can be used whenever there's a need to pattern match on something being a `MapSet`:

      iex> match?(%MapSet{}, MapSet.new())
      true

  Note that, however, the struct fields are private and must not be accessed
  directly; use the functions in this module to perform operations on sets.

  Sets can also be constructed starting from other collection-type data
  structures: for example, see `MapSet.new/1` or `Enum.into/2`.
  """

  @type value :: term

  @opaque t(value) :: %__MODULE__{map: %{optional(value) => true}}
  @type t :: t(term)

  defstruct map: %{}

  @doc """
  Returns a new set.

  ## Examples

      iex> MapSet.new
      #MapSet<[]>

  """
  @spec new :: t
  def new(), do: %MapSet{}

  @doc """
  Creates a set from an enumerable.

  ## Examples

      iex> MapSet.new([:b, :a, 3])
      #MapSet<[3, :a, :b]>
      iex> MapSet.new([3, 3, 3, 2, 2, 1])
      #MapSet<[1, 2, 3]>

  """
  @spec new(Enum.t) :: t
  def new(%__MODULE__{} = mapset), do: mapset
  def new(enumerable) do
    map =
      enumerable
      |> Enum.to_list
      |> do_new([])

    %MapSet{map: map}
  end

  @doc """
  Creates a mapset from an enumerable via the transformation function.

  ## Examples

      iex> MapSet.new([1, 2, 1], fn x -> 2 * x end)
      #MapSet<[2, 4]>

  """
  @spec new(Enum.t, (term -> val)) :: t(val) when val: value
  def new(enumerable, transform) when is_function(transform, 1) do
    map =
      enumerable
      |> Enum.to_list
      |> do_new_transform(transform, [])

    %MapSet{map: map}
  end

  defp do_new([], acc) do
    acc
    |> :lists.reverse
    |> :maps.from_list
  end
  defp do_new([item | rest], acc) do
    do_new(rest, [{item, true} | acc])
  end

  defp do_new_transform([], _fun, acc) do
    acc
    |> :lists.reverse
    |> :maps.from_list
  end
  defp do_new_transform([item | rest], fun, acc) do
    do_new_transform(rest, fun, [{fun.(item), true} | acc])
  end

  @doc """
  Deletes `value` from `set`.

  Returns a new set which is a copy of `set` but without `value`.

  ## Examples

      iex> set = MapSet.new([1, 2, 3])
      iex> MapSet.delete(set, 4)
      #MapSet<[1, 2, 3]>
      iex> MapSet.delete(set, 2)
      #MapSet<[1, 3]>

  """
  @spec delete(t(val1), val2) :: t(val1) when val1: value, val2: value
  def delete(%MapSet{map: map} = set, value) do
    %{set | map: Map.delete(map, value)}
  end

  @doc """
  Returns a set that is `set1` without the members of `set2`.

  ## Examples

      iex> MapSet.difference(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[1]>

  """
  @spec difference(t(val1), t(val2)) :: t(val1) when val1: value, val2: value
  def difference(mapset1, mapset2)

  # If the first set is less than twice the size of the second map,
  # it is fastest to re-accumulate items in the first set that are not
  # present in the second set.
  def difference(%MapSet{map: map1}, %MapSet{map: map2})
      when map_size(map1) < map_size(map2) * 2 do
    map = map1
    |> Map.keys
    |> filter_not_in(map2)

    %MapSet{map: map}
  end

  # If the second set is less than half the size of the first set, it's fastest
  # to simply iterate through each item in the second set, deleting them from
  # the first set.
  def difference(%MapSet{map: map1}, %MapSet{map: map2}) do
    %MapSet{map: Map.drop(map1, Map.keys(map2))}
  end

  defp filter_not_in(keys, map2, acc \\ [])
  defp filter_not_in([], _map2, acc), do: :maps.from_list(acc)
  defp filter_not_in([key | rest], map2, acc) do
    acc = if Map.has_key?(map2, key) do
      acc
    else
      [{key, true} | acc]
    end
    filter_not_in(rest, map2, acc)
  end

  @doc """
  Checks if `set1` and `set2` have no members in common.

  ## Examples

      iex> MapSet.disjoint?(MapSet.new([1, 2]), MapSet.new([3, 4]))
      true
      iex> MapSet.disjoint?(MapSet.new([1, 2]), MapSet.new([2, 3]))
      false

  """
  @spec disjoint?(t, t) :: boolean
  def disjoint?(%MapSet{map: map1}, %MapSet{map: map2}) do
    {map1, map2} = order_by_size(map1, map2)

    map1
    |> Map.keys
    |> none_in?(map2)
  end

  defp none_in?([], _) do
    true
  end
  defp none_in?([key | rest], map2) do
    case Map.has_key?(map2, key) do
      true -> false
      false -> none_in?(rest, map2)
    end
  end

  @doc """
  Checks if two sets are equal.

  The comparison between elements must be done using `===`.

  ## Examples

      iex> MapSet.equal?(MapSet.new([1, 2]), MapSet.new([2, 1, 1]))
      true
      iex> MapSet.equal?(MapSet.new([1, 2]), MapSet.new([3, 4]))
      false

  """
  @spec equal?(t, t) :: boolean
  def equal?(%MapSet{map: map1}, %MapSet{map: map2}) do
    Map.equal?(map1, map2)
  end

  @doc """
  Returns a set containing only members that `set1` and `set2` have in common.

  ## Examples

      iex> MapSet.intersection(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[2]>

      iex> MapSet.intersection(MapSet.new([1, 2]), MapSet.new([3, 4]))
      #MapSet<[]>

  """
  @spec intersection(t(val), t(val)) :: t(val) when val: value
  def intersection(%MapSet{map: map1}, %MapSet{map: map2}) do
    {map1, map2} = order_by_size(map1, map2)

    %MapSet{map: Map.take(map2, Map.keys(map1))}
  end

  @doc """
  Checks if `set` contains `value`.

  ## Examples

      iex> MapSet.member?(MapSet.new([1, 2, 3]), 2)
      true
      iex> MapSet.member?(MapSet.new([1, 2, 3]), 4)
      false

  """
  @spec member?(t, value) :: boolean
  def member?(%MapSet{map: map}, value) do
    Map.has_key?(map, value)
  end

  @doc """
  Inserts `value` into `set` if `set` doesn't already contain it.

  ## Examples

      iex> MapSet.put(MapSet.new([1, 2, 3]), 3)
      #MapSet<[1, 2, 3]>
      iex> MapSet.put(MapSet.new([1, 2, 3]), 4)
      #MapSet<[1, 2, 3, 4]>

  """
  @spec put(t(val), new_val) :: t(val | new_val) when val: value, new_val: value
  def put(%MapSet{map: map} = set, value) do
    %{set | map: Map.put(map, value, true)}
  end

  @doc """
  Returns the number of elements in `set`.

  ## Examples

      iex> MapSet.size(MapSet.new([1, 2, 3]))
      3

  """
  @spec size(t) :: non_neg_integer
  def size(%MapSet{map: map}) do
    map_size(map)
  end

  @doc """
  Checks if `set1`'s members are all contained in `set2`.

  This function checks if `set1` is a subset of `set2`.

  ## Examples

      iex> MapSet.subset?(MapSet.new([1, 2]), MapSet.new([1, 2, 3]))
      true
      iex> MapSet.subset?(MapSet.new([1, 2, 3]), MapSet.new([1, 2]))
      false

  """
  @spec subset?(t, t) :: boolean
  def subset?(%MapSet{map: map1}, %MapSet{map: map2}) do
    if map_size(map1) <= map_size(map2) do
      map1
      |> Map.keys
      |> do_subset?(map2)
    else
      false
    end
  end

  defp do_subset?([], _), do: true
  defp do_subset?([key | rest], map2) do
    if Map.has_key?(map2, key) do
      do_subset?(rest, map2)
    else
      false
    end
  end

  @doc """
  Converts `set` to a list.

  ## Examples

      iex> MapSet.to_list(MapSet.new([1, 2, 3]))
      [1, 2, 3]

  """
  @spec to_list(t(val)) :: [val] when val: value
  def to_list(%MapSet{map: map}) do
    Map.keys(map)
  end

  @doc """
  Returns a set containing all members of `set1` and `set2`.

  ## Examples

      iex> MapSet.union(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[1, 2, 3, 4]>

  """
  @spec union(t(val1), t(val2)) :: t(val1 | val2) when val1: value, val2: value
  def union(%MapSet{map: map1}, %MapSet{map: map2}) do
    %MapSet{map: Map.merge(map1, map2)}
  end

  defp order_by_size(map1, map2) when map_size(map1) > map_size(map2), do: {map2, map1}
  defp order_by_size(map1, map2), do: {map1, map2}

  defimpl Enumerable do
    def reduce(set, acc, fun), do: Enumerable.List.reduce(MapSet.to_list(set), acc, fun)
    def member?(set, val),     do: {:ok, MapSet.member?(set, val)}
    def count(set),            do: {:ok, MapSet.size(set)}
  end

  defimpl Collectable do
    def into(original) do
      {original, fn
        set, {:cont, x} -> MapSet.put(set, x)
        set, :done -> set
        _, :halt -> :ok
      end}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(set, opts) do
      concat ["#MapSet<", Inspect.List.inspect(MapSet.to_list(set), opts), ">"]
    end
  end
end
