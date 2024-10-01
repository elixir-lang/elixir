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

  `MapSet` is built on top of Erlang's
  [`:sets`](https://www.erlang.org/doc/man/sets.html) (version 2). This means
  that they share many properties, including logarithmic time complexity. Erlang
  `:sets` (version 2) are implemented on top of maps, so see the documentation
  for `Map` for more information on its execution time complexity.
  """

  @type value :: term

  @opaque internal(value) :: :sets.set(value)
  @type t(value) :: %__MODULE__{map: internal(value)}
  @type t :: t(term)

  # The key name is :map because the MapSet implementation used to be based on top of maps before
  # Elixir 1.15 (and Erlang/OTP 24, which introduced :sets version 2). :sets v2's internal
  # representation is, anyways, exactly the same as MapSet's previous implementation. We cannot
  # change the :map key name here because we'd break backwards compatibility with code compiled
  # with Elixir 1.14 and earlier and executed on Elixir 1.15+.
  defstruct map: :sets.new(version: 2)

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
    set =
      enumerable
      |> Enum.to_list()
      |> :sets.from_list(version: 2)

    %MapSet{map: set}
  end

  @doc """
  Creates a set from an enumerable via the transformation function.

  ## Examples

      iex> MapSet.new([1, 2, 1], fn x -> 2 * x end)
      MapSet.new([2, 4])

  """
  @spec new(Enumerable.t(), (term -> val)) :: t(val) when val: value
  def new(enumerable, transform) when is_function(transform, 1) do
    set =
      enumerable
      |> Enum.map(transform)
      |> :sets.from_list(version: 2)

    %MapSet{map: set}
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
  def delete(%MapSet{map: set} = map_set, value) do
    %{map_set | map: :sets.del_element(value, set)}
  end

  @doc """
  Returns a set that is `map_set1` without the members of `map_set2`.

  ## Examples

      iex> MapSet.difference(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      MapSet.new([1])

  """
  @spec difference(t(val1), t(val2)) :: t(val1) when val1: value, val2: value
  def difference(%MapSet{map: set1} = map_set1, %MapSet{map: set2} = _map_set2) do
    %{map_set1 | map: :sets.subtract(set1, set2)}
  end

  @doc """
  Returns a set with elements that are present in only one but not both sets.

  ## Examples

      iex> MapSet.symmetric_difference(MapSet.new([1, 2, 3]), MapSet.new([2, 3, 4]))
      MapSet.new([1, 4])

  """
  @doc since: "1.14.0"
  @spec symmetric_difference(t(val1), t(val2)) :: t(val1 | val2) when val1: value, val2: value
  def symmetric_difference(%MapSet{map: set1} = map_set1, %MapSet{map: set2} = _map_set2) do
    {small, large} = if :sets.size(set1) <= :sets.size(set2), do: {set1, set2}, else: {set2, set1}

    disjointer_fun = fn elem, {small, acc} ->
      if :sets.is_element(elem, small) do
        {:sets.del_element(elem, small), acc}
      else
        {small, [elem | acc]}
      end
    end

    {new_small, list} = :sets.fold(disjointer_fun, {small, []}, large)
    %{map_set1 | map: :sets.union(new_small, :sets.from_list(list, version: 2))}
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
  def disjoint?(%MapSet{map: set1}, %MapSet{map: set2}) do
    :sets.is_disjoint(set1, set2)
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
  def equal?(%MapSet{map: set1}, %MapSet{map: set2}) do
    set1 === set2
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
  def intersection(%MapSet{map: set1} = map_set1, %MapSet{map: set2} = _map_set2) do
    %{map_set1 | map: :sets.intersection(set1, set2)}
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
  def member?(%MapSet{map: set}, value) do
    :sets.is_element(value, set)
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
  def put(%MapSet{map: set} = map_set, value) do
    %{map_set | map: :sets.add_element(value, set)}
  end

  @doc """
  Returns the number of elements in `map_set`.

  ## Examples

      iex> MapSet.size(MapSet.new([1, 2, 3]))
      3

  """
  @spec size(t) :: non_neg_integer
  def size(%MapSet{map: set}) do
    :sets.size(set)
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
  def subset?(%MapSet{map: set1}, %MapSet{map: set2}) do
    :sets.is_subset(set1, set2)
  end

  @doc """
  Converts `map_set` to a list.

  ## Examples

      iex> MapSet.to_list(MapSet.new([1, 2, 3]))
      [1, 2, 3]

  """
  @spec to_list(t(val)) :: [val] when val: value
  def to_list(%MapSet{map: set}) do
    :sets.to_list(set)
  end

  @doc """
  Returns a set containing all members of `map_set1` and `map_set2`.

  ## Examples

      iex> MapSet.union(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      MapSet.new([1, 2, 3, 4])

  """
  @spec union(t(val1), t(val2)) :: t(val1 | val2) when val1: value, val2: value
  def union(%MapSet{map: set1} = map_set1, %MapSet{map: set2} = _map_set2) do
    %{map_set1 | map: :sets.union(set1, set2)}
  end

  @doc """
  Filters the set by returning only the elements from `map_set` for which invoking
  `fun` returns a truthy value.

  Also see `reject/2` which discards all elements where the function returns
  a truthy value.

  > #### Performance considerations {: .tip}
  >
  > If you find yourself doing multiple calls to `MapSet.filter/2`
  > and `MapSet.reject/2` in a pipeline, it is likely more efficient
  > to use `Enum.map/2` and `Enum.filter/2` instead and convert to
  > a map at the end using `MapSet.new/1`.

  ## Examples

      iex> MapSet.filter(MapSet.new(1..5), fn x -> x > 3 end)
      MapSet.new([4, 5])

      iex> MapSet.filter(MapSet.new(["a", :b, "c"]), &is_atom/1)
      MapSet.new([:b])

  """
  @doc since: "1.14.0"
  @spec filter(t(a), (a -> as_boolean(term))) :: t(a) when a: value
  def filter(%MapSet{map: set} = map_set, fun) when is_function(fun) do
    pred = fn element -> !!fun.(element) end
    %{map_set | map: :sets.filter(pred, set)}
  end

  @doc """
  Returns a set by excluding the elements from `map_set` for which invoking `fun`
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
  def reject(%MapSet{map: set} = map_set, fun) when is_function(fun) do
    pred = fn element -> !fun.(element) end
    %{map_set | map: :sets.filter(pred, set)}
  end

  @doc """
  Splits the `map_set` into two `MapSet`s according to the given function `fun`.

  `fun` receives each element in the `map_set` as its only argument. Returns
  a tuple with the first `MapSet` containing all the elements in `map_set` for which
  applying `fun` returned a truthy value, and a second `MapSet` with all the elements
  for which applying `fun` returned a falsy value (`false` or `nil`).

  ## Examples

      iex> {while_true, while_false} = MapSet.split_with(MapSet.new([1, 2, 3, 4]), fn v -> rem(v, 2) == 0 end)
      iex> while_true
      MapSet.new([2, 4])
      iex> while_false
      MapSet.new([1, 3])

      iex> {while_true, while_false} = MapSet.split_with(MapSet.new(), fn {_k, v} -> v > 50 end)
      iex> while_true
      MapSet.new([])
      iex> while_false
      MapSet.new([])

  """
  @doc since: "1.15.0"
  @spec split_with(MapSet.t(), (term() -> as_boolean(term))) :: {MapSet.t(), MapSet.t()}
  def split_with(%MapSet{map: map}, fun) when is_function(fun, 1) do
    {while_true, while_false} = Map.split_with(map, fn {key, _} -> fun.(key) end)
    {%MapSet{map: while_true}, %MapSet{map: while_false}}
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
    def into(%@for{map: set} = map_set) do
      fun = fn
        list, {:cont, x} -> [x | list]
        list, :done -> %{map_set | map: :sets.union(set, :sets.from_list(list, version: 2))}
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
