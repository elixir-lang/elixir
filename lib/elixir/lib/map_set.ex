defmodule MapSet do
  @moduledoc """
  Functions that work on sets.

  `MapSet` is the "go to" set data structure in Elixir. A set can be constructed
  using `MapSet.new/0`:

      iex> MapSet.new()
      #MapSet<[]>

  A set can contain any kind of elements, and elements in a set don't have to be
  of the same type. By definition, sets can't contain duplicate elements: when
  inserting an element in a set where it's already present, the insertion is
  simply a no-op.

      iex> map_set = MapSet.new()
      iex> MapSet.put(map_set, "foo")
      #MapSet<["foo"]>
      iex> map_set |> MapSet.put("foo") |> MapSet.put("foo")
      #MapSet<["foo"]>

  A `MapSet` is represented internally using the `%MapSet{}` struct. This struct
  can be used whenever there's a need to pattern match on something being a `MapSet`:

      iex> match?(%MapSet{}, MapSet.new())
      true

  Note that, however, the struct fields are private and must not be accessed
  directly; use the functions in this module to perform operations on sets.

  `MapSet`s can also be constructed starting from other collection-type data
  structures: for example, see `MapSet.new/1` or `Enum.into/2`.

  `MapSet` is built on top of `Map`, this means that they share many properties,
  including logarithmic time complexity. See the documentation for `Map` for more
  information on its execution time complexity.
  """

  # MapSets have an underlying Map. MapSet elements are keys of said map,
  # and this empty list is their associated dummy value.
  @dummy_value []

  @type value :: term

  @opaque t(value) :: %__MODULE__{map: %{optional(value) => []}}
  @type t :: t(term)

  # TODO: Remove version key on v2.0
  defstruct map: %{}, version: 2

  @doc """
  Returns a new set.

  ## Examples

      iex> MapSet.new()
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
  @spec new(Enum.t()) :: t
  def new(enumerable)

  def new(%__MODULE__{} = map_set), do: map_set

  def new(enumerable) do
    map =
      enumerable
      |> Enum.to_list()
      |> new_from_list([])

    %MapSet{map: map}
  end

  @doc """
  Creates a set from an enumerable via the transformation function.

  ## Examples

      iex> MapSet.new([1, 2, 1], fn x -> 2 * x end)
      #MapSet<[2, 4]>

  """
  @spec new(Enum.t(), (term -> val)) :: t(val) when val: value
  def new(enumerable, transform) when is_function(transform, 1) do
    map =
      enumerable
      |> Enum.to_list()
      |> new_from_list_transform(transform, [])

    %MapSet{map: map}
  end

  defp new_from_list([], acc) do
    Map.new(acc)
  end

  defp new_from_list([element | rest], acc) do
    new_from_list(rest, [{element, @dummy_value} | acc])
  end

  defp new_from_list_transform([], _fun, acc) do
    Map.new(acc)
  end

  defp new_from_list_transform([element | rest], fun, acc) do
    new_from_list_transform(rest, fun, [{fun.(element), @dummy_value} | acc])
  end

  @doc """
  Deletes `value` from `map_set`.

  Returns a new set which is a copy of `map_set` but without `value`.

  ## Examples

      iex> map_set = MapSet.new([1, 2, 3])
      iex> MapSet.delete(map_set, 4)
      #MapSet<[1, 2, 3]>
      iex> MapSet.delete(map_set, 2)
      #MapSet<[1, 3]>

  """
  @spec delete(t(val1), val2) :: t(val1) when val1: value, val2: value
  def delete(%MapSet{map: map} = map_set, value) do
    %{map_set | map: Map.delete(map, value)}
  end

  @doc """
  Returns a set that is `map_set1` without the members of `map_set2`.

  ## Examples

      iex> MapSet.difference(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[1]>

  """
  @spec difference(t(val1), t(val2)) :: t(val1) when val1: value, val2: value
  def difference(map_set1, map_set2)

  # If the first set is less than twice the size of the second map,
  # it is fastest to re-accumulate elements in the first set that are not
  # present in the second set.
  def difference(%MapSet{map: map1}, %MapSet{map: map2})
      when map_size(map1) < map_size(map2) * 2 do
    map =
      map1
      |> Map.keys()
      |> filter_not_in(map2, [])

    %MapSet{map: map}
  end

  # If the second set is less than half the size of the first set, it's fastest
  # to simply iterate through each element in the second set, deleting them from
  # the first set.
  def difference(%MapSet{map: map1} = map_set, %MapSet{map: map2}) do
    %{map_set | map: Map.drop(map1, Map.keys(map2))}
  end

  defp filter_not_in([], _map2, acc), do: Map.new(acc)

  defp filter_not_in([key | rest], map2, acc) do
    case map2 do
      %{^key => _} -> filter_not_in(rest, map2, acc)
      _ -> filter_not_in(rest, map2, [{key, @dummy_value} | acc])
    end
  end

  @doc """
  Checks if `map_set1` and `map_set2` have no members in common.

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
    |> Map.keys()
    |> none_in?(map2)
  end

  defp none_in?([], _) do
    true
  end

  defp none_in?([key | rest], map2) do
    case map2 do
      %{^key => _} -> false
      _ -> none_in?(rest, map2)
    end
  end

  @doc """
  Checks if two sets are equal.

  The comparison between elements must be done using `===/2`.

  ## Examples

      iex> MapSet.equal?(MapSet.new([1, 2]), MapSet.new([2, 1, 1]))
      true
      iex> MapSet.equal?(MapSet.new([1, 2]), MapSet.new([3, 4]))
      false

  """
  @spec equal?(t, t) :: boolean
  def equal?(%MapSet{map: map1, version: version}, %MapSet{map: map2, version: version}) do
    Map.equal?(map1, map2)
  end

  # Elixir v1.5 change the map representation, so on
  # version mismatch we need to compare the keys directly.
  def equal?(%MapSet{map: map1}, %MapSet{map: map2}) do
    map_size(map1) == map_size(map2) and map_subset?(Map.keys(map1), map2)
  end

  @doc """
  Returns a set containing only members that `map_set1` and `map_set2` have in common.

  ## Examples

      iex> MapSet.intersection(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[2]>

      iex> MapSet.intersection(MapSet.new([1, 2]), MapSet.new([3, 4]))
      #MapSet<[]>

  """
  @spec intersection(t(val), t(val)) :: t(val) when val: value
  def intersection(%MapSet{map: map1} = map_set, %MapSet{map: map2}) do
    {map1, map2} = order_by_size(map1, map2)
    %{map_set | map: Map.take(map2, Map.keys(map1))}
  end

  @doc """
  Checks if `map_set` contains `value`.

  ## Examples

      iex> MapSet.member?(MapSet.new([1, 2, 3]), 2)
      true
      iex> MapSet.member?(MapSet.new([1, 2, 3]), 4)
      false

  """
  @spec member?(t, value) :: boolean
  def member?(%MapSet{map: map}, value) do
    match?(%{^value => _}, map)
  end

  @doc """
  Inserts `value` into `map_set` if `map_set` doesn't already contain it.

  ## Examples

      iex> MapSet.put(MapSet.new([1, 2, 3]), 3)
      #MapSet<[1, 2, 3]>
      iex> MapSet.put(MapSet.new([1, 2, 3]), 4)
      #MapSet<[1, 2, 3, 4]>

  """
  @spec put(t(val), new_val) :: t(val | new_val) when val: value, new_val: value
  def put(%MapSet{map: map} = map_set, value) do
    %{map_set | map: Map.put(map, value, @dummy_value)}
  end

  @doc """
  Returns the number of elements in `map_set`.

  ## Examples

      iex> MapSet.size(MapSet.new([1, 2, 3]))
      3

  """
  @spec size(t) :: non_neg_integer
  def size(%MapSet{map: map}) do
    map_size(map)
  end

  @doc """
  Checks if `map_set1`'s members are all contained in `map_set2`.

  This function checks if `map_set1` is a subset of `map_set2`.

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
      |> Map.keys()
      |> map_subset?(map2)
    else
      false
    end
  end

  defp map_subset?([], _), do: true

  defp map_subset?([key | rest], map2) do
    match?(%{^key => _}, map2) and map_subset?(rest, map2)
  end

  @doc """
  Converts `map_set` to a list.

  ## Examples

      iex> MapSet.to_list(MapSet.new([1, 2, 3]))
      [1, 2, 3]

  """
  @spec to_list(t(val)) :: [val] when val: value
  def to_list(%MapSet{map: map}) do
    Map.keys(map)
  end

  @doc """
  Returns a set containing all members of `map_set1` and `map_set2`.

  ## Examples

      iex> MapSet.union(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[1, 2, 3, 4]>

  """
  @spec union(t(val1), t(val2)) :: t(val1 | val2) when val1: value, val2: value
  def union(map_set1, map_set2)

  def union(%MapSet{map: map1, version: version} = map_set, %MapSet{map: map2, version: version}) do
    %{map_set | map: Map.merge(map1, map2)}
  end

  def union(%MapSet{map: map1}, %MapSet{map: map2}) do
    map = new_from_list(Map.keys(map1) ++ Map.keys(map2), [])
    %MapSet{map: map}
  end

  @compile {:inline, [order_by_size: 2]}
  defp order_by_size(map1, map2) when map_size(map1) > map_size(map2), do: {map2, map1}
  defp order_by_size(map1, map2), do: {map1, map2}

  defimpl Enumerable do
    def count(map_set) do
      {:ok, MapSet.size(map_set)}
    end

    def member?(map_set, val) do
      {:ok, MapSet.member?(map_set, val)}
    end

    def slice(map_set) do
      size = MapSet.size(map_set)
      {:ok, size, &Enumerable.List.slice(MapSet.to_list(map_set), &1, &2, size)}
    end

    def reduce(map_set, acc, fun) do
      Enumerable.List.reduce(MapSet.to_list(map_set), acc, fun)
    end
  end

  defimpl Collectable do
    def into(map_set) do
      fun = fn
        list, {:cont, x} -> [{x, []} | list]
        list, :done -> %{map_set | map: Map.merge(map_set.map, Map.new(list))}
        _, :halt -> :ok
      end

      {[], fun}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(map_set, opts) do
      opts = %Inspect.Opts{opts | charlists: :as_lists}
      concat(["#MapSet<", Inspect.List.inspect(MapSet.to_list(map_set), opts), ">"])
    end
  end
end
