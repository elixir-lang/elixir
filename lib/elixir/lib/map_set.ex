defmodule MapSet do
  @moduledoc """
  Functions that work on sets.

  A set is a data structure that can contain unique elements of any kind,
  without any particular order. `MapSet` is the "go to" set data structure in Elixir.

  A set can be constructed using `MapSet.new/0`:

      iex> MapSet.new()
      MapSet.new([])

  Elements in a set don't have to be of the same type and they can be
  populated from an [enumerable](`t:Enumerable.t/0`) using `MapSet.new/1`:

      iex> MapSet.new([1, :two, {"three"}])
      MapSet.new([1, :two, {"three"}])

  Elements can be inserted using `MapSet.put/2`:

      iex> MapSet.new([2]) |> MapSet.put(4) |> MapSet.put(0)
      MapSet.new([0, 2, 4])

  By definition, sets can't contain duplicate elements: when
  inserting an element in a set where it's already present, the insertion is
  simply a no-op.

      iex> map_set = MapSet.new()
      iex> MapSet.put(map_set, "foo")
      MapSet.new(["foo"])
      iex> map_set |> MapSet.put("foo") |> MapSet.put("foo")
      MapSet.new(["foo"])

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

  @opaque internal(value) :: %{optional(value) => []}
  @type t(value) :: %__MODULE__{map: internal(value)}
  @type t :: t(term)

  # TODO: Remove version key when we require Erlang/OTP 24
  # TODO: Implement the functions in this module using Erlang/OTP 24 new sets
  defstruct map: %{}, version: 2

  @doc """
  Returns a new set.

  ## Examples

      iex> MapSet.new()
      MapSet.new([])

  """
  @spec new :: t
  def new(), do: %MapSet{}

  @doc """
  Creates a set from an enumerable.

  ## Examples

      iex> MapSet.new([:b, :a, 3])
      MapSet.new([3, :a, :b])
      iex> MapSet.new([3, 3, 3, 2, 2, 1])
      MapSet.new([1, 2, 3])

  """
  @spec new(Enumerable.t()) :: t
  def new(enumerable)

  def new(%__MODULE__{} = map_set), do: map_set

  def new(enumerable) do
    keys = Enum.to_list(enumerable)
    %MapSet{map: Map.from_keys(keys, @dummy_value)}
  end

  @doc """
  Creates a set from an enumerable via the transformation function.

  ## Examples

      iex> MapSet.new([1, 2, 1], fn x -> 2 * x end)
      MapSet.new([2, 4])

  """
  @spec new(Enumerable.t(), (term -> val)) :: t(val) when val: value
  def new(enumerable, transform) when is_function(transform, 1) do
    keys = Enum.map(enumerable, transform)
    %MapSet{map: Map.from_keys(keys, @dummy_value)}
  end

  @doc """
  Deletes `value` from `map_set`.

  Returns a new set which is a copy of `map_set` but without `value`.

  ## Examples

      iex> map_set = MapSet.new([1, 2, 3])
      iex> MapSet.delete(map_set, 4)
      MapSet.new([1, 2, 3])
      iex> MapSet.delete(map_set, 2)
      MapSet.new([1, 3])

  """
  @spec delete(t(val1), val2) :: t(val1) when val1: value, val2: value
  def delete(%MapSet{map: map} = map_set, value) do
    %{map_set | map: Map.delete(map, value)}
  end

  @doc """
  Returns a set that is `map_set1` without the members of `map_set2`.

  ## Examples

      iex> MapSet.difference(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      MapSet.new([1])

  """
  @spec difference(t(val1), t(val2)) :: t(val1) when val1: value, val2: value
  def difference(map_set1, map_set2)

  # If the first set is less than twice the size of the second map, it is fastest
  # to re-accumulate elements in the first set that are not present in the second set.
  def difference(%MapSet{map: map1}, %MapSet{map: map2})
      when map_size(map1) < map_size(map2) * 2 do
    map =
      map1
      |> :maps.iterator()
      |> :maps.next()
      |> filter_not_in(map2, [])

    %MapSet{map: map}
  end

  # If the second set is less than half the size of the first set, it's fastest
  # to simply iterate through each element in the second set, deleting them from
  # the first set.
  def difference(%MapSet{map: map1} = map_set, %MapSet{map: map2}) do
    %{map_set | map: Map.drop(map1, Map.keys(map2))}
  end

  defp filter_not_in(:none, _map2, acc), do: Map.from_keys(acc, @dummy_value)

  defp filter_not_in({key, _val, iter}, map2, acc) do
    if is_map_key(map2, key) do
      filter_not_in(:maps.next(iter), map2, acc)
    else
      filter_not_in(:maps.next(iter), map2, [key | acc])
    end
  end

  @doc """
  Returns a set with elements that are present in only one but not both sets.

  ## Examples

      iex> MapSet.symmetric_difference(MapSet.new([1, 2, 3]), MapSet.new([2, 3, 4]))
      MapSet.new([1, 4])
  """
  @doc since: "1.14.0"
  @spec symmetric_difference(t(val1), t(val2)) :: t(val1 | val2) when val1: value, val2: value
  def symmetric_difference(%MapSet{map: map1}, %MapSet{map: map2}) do
    {small, large} = order_by_size(map1, map2)

    map =
      large
      |> :maps.iterator()
      |> :maps.next()
      |> disjointer(small, [])

    %MapSet{map: map}
  end

  defp disjointer(:none, small, list) do
    list |> Map.from_keys(@dummy_value) |> Map.merge(small)
  end

  defp disjointer({key, _val, iter}, small, list) do
    if is_map_key(small, key) do
      iter
      |> :maps.next()
      |> disjointer(Map.delete(small, key), list)
    else
      iter
      |> :maps.next()
      |> disjointer(small, [key | list])
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
    |> :maps.iterator()
    |> :maps.next()
    |> none_in?(map2)
  end

  defp none_in?(:none, _), do: true

  defp none_in?({key, _val, iter}, map2) do
    not is_map_key(map2, key) and none_in?(:maps.next(iter), map2)
  end

  @doc """
  Checks if two sets are equal.

  The comparison between elements is done using `===/2`,
  which a set with `1` is not equivalent to a set with
  `1.0`.

  ## Examples

      iex> MapSet.equal?(MapSet.new([1, 2]), MapSet.new([2, 1, 1]))
      true
      iex> MapSet.equal?(MapSet.new([1, 2]), MapSet.new([3, 4]))
      false
      iex> MapSet.equal?(MapSet.new([1]), MapSet.new([1.0]))
      false

  """
  @spec equal?(t, t) :: boolean
  def equal?(%MapSet{map: map1, version: version}, %MapSet{map: map2, version: version}) do
    map1 === map2
  end

  # Elixir v1.5 changed the map representation, so on
  # version mismatch we need to compare the keys directly.
  def equal?(%MapSet{map: map1}, %MapSet{map: map2}) do
    map_size(map1) == map_size(map2) and all_in?(map1, map2)
  end

  @doc """
  Returns a set containing only members that `map_set1` and `map_set2` have in common.

  ## Examples

      iex> MapSet.intersection(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      MapSet.new([2])

      iex> MapSet.intersection(MapSet.new([1, 2]), MapSet.new([3, 4]))
      MapSet.new([])

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
    is_map_key(map, value)
  end

  @doc """
  Inserts `value` into `map_set` if `map_set` doesn't already contain it.

  ## Examples

      iex> MapSet.put(MapSet.new([1, 2, 3]), 3)
      MapSet.new([1, 2, 3])
      iex> MapSet.put(MapSet.new([1, 2, 3]), 4)
      MapSet.new([1, 2, 3, 4])

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
    map_size(map1) <= map_size(map2) and all_in?(map1, map2)
  end

  defp all_in?(:none, _), do: true

  defp all_in?({key, _val, iter}, map2) do
    :erlang.is_map_key(key, map2) and all_in?(:maps.next(iter), map2)
  end

  defp all_in?(map1, map2) when is_map(map1) and is_map(map2) do
    map1
    |> :maps.iterator()
    |> :maps.next()
    |> all_in?(map2)
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
      MapSet.new([1, 2, 3, 4])

  """
  @spec union(t(val1), t(val2)) :: t(val1 | val2) when val1: value, val2: value
  def union(map_set1, map_set2)

  def union(%MapSet{map: map1, version: version} = map_set, %MapSet{map: map2, version: version}) do
    %{map_set | map: Map.merge(map1, map2)}
  end

  def union(%MapSet{map: map1}, %MapSet{map: map2}) do
    keys = Map.keys(map1) ++ Map.keys(map2)
    %MapSet{map: Map.from_keys(keys, @dummy_value)}
  end

  @compile {:inline, [order_by_size: 2]}
  defp order_by_size(map1, map2) when map_size(map1) > map_size(map2), do: {map2, map1}
  defp order_by_size(map1, map2), do: {map1, map2}

  @doc """
  Filters the set by returning only the elements from `set` for which invoking
  `fun` returns a truthy value.

  Also see `reject/2` which discards all elements where the function returns
  a truthy value.

  > Note: if you find yourself doing multiple calls to `MapSet.filter/2`
  > and `MapSet.reject/2` in a pipeline, it is likely more efficient
  > to use `Enum.map/2` and `Enum.filter/2` instead and convert to
  > a map at the end using `Map.new/1`.

  ## Examples

      iex> MapSet.filter(MapSet.new(1..5), fn x -> x > 3 end)
      MapSet.new([4, 5])

      iex> MapSet.filter(MapSet.new(["a", :b, "c"]), &is_atom/1)
      MapSet.new([:b])

  """
  @doc since: "1.14.0"
  @spec filter(t(a), (a -> as_boolean(term))) :: t(a) when a: value
  def filter(%MapSet{map: map}, fun) when is_map(map) and is_function(fun) do
    iter = :maps.iterator(map)
    next = :maps.next(iter)
    keys = filter_keys(next, fun)
    %MapSet{map: Map.from_keys(keys, @dummy_value)}
  end

  defp filter_keys(:none, _fun), do: []

  defp filter_keys({key, _value, iter}, fun) do
    if fun.(key) do
      [key | filter_keys(:maps.next(iter), fun)]
    else
      filter_keys(:maps.next(iter), fun)
    end
  end

  @doc """
  Returns a set by excluding the elements from `set` for which invoking `fun`
  returns a truthy value.

  See also `filter/2`.

  ## Examples

      iex> MapSet.reject(MapSet.new(1..5), fn x -> rem(x, 2) != 0 end)
      MapSet.new([2, 4])

      iex> MapSet.reject(MapSet.new(["a", :b, "c"]), &is_atom/1)
      MapSet.new(["a", "c"])

  """
  @doc since: "1.14.0"
  @spec reject(t(a), (a -> as_boolean(term))) :: t(a) when a: value
  def reject(%MapSet{map: map}, fun) when is_map(map) and is_function(fun) do
    iter = :maps.iterator(map)
    next = :maps.next(iter)
    keys = reject_keys(next, fun)
    %MapSet{map: Map.from_keys(keys, @dummy_value)}
  end

  defp reject_keys(:none, _fun), do: []

  defp reject_keys({key, _value, iter}, fun) do
    if fun.(key) do
      reject_keys(:maps.next(iter), fun)
    else
      [key | reject_keys(:maps.next(iter), fun)]
    end
  end

  defimpl Enumerable do
    def count(map_set) do
      {:ok, MapSet.size(map_set)}
    end

    def member?(map_set, val) do
      {:ok, MapSet.member?(map_set, val)}
    end

    def slice(map_set) do
      size = MapSet.size(map_set)
      {:ok, size, &MapSet.to_list/1}
    end

    def reduce(map_set, acc, fun) do
      Enumerable.List.reduce(MapSet.to_list(map_set), acc, fun)
    end
  end

  defimpl Collectable do
    def into(map_set) do
      fun = fn
        list, {:cont, x} -> [x | list]
        list, :done -> %{map_set | map: Map.merge(map_set.map, Map.from_keys(list, []))}
        _, :halt -> :ok
      end

      {[], fun}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(map_set, opts) do
      opts = %Inspect.Opts{opts | charlists: :as_lists}
      concat(["MapSet.new(", Inspect.List.inspect(MapSet.to_list(map_set), opts), ")"])
    end
  end
end
