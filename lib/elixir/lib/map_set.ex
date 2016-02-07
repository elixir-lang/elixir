defmodule MapSet do
  @moduledoc """
  A set of functions for working with sets.

  The `MapSet` is represented internally as a struct,
  therefore `%MapSet{}` can be used whenever there is a
  need to match on any `MapSet`. Note though the struct
  fields are private and must not be accessed directly.
  Instead, use the functions in this module.
  """

  @opaque t :: %__MODULE__{map: map}
  @type value :: term
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
  def new(enumerable) do
    Enum.reduce(enumerable, %MapSet{}, &put(&2, &1))
  end

  @doc """
  Creates a mapset from an enumerable via the transformation function.

  ## Examples

      iex> MapSet.new([1, 2, 1], fn x -> 2 * x end)
      #MapSet<[2, 4]>

  """
  @spec new(Enum.t, (term -> term)) :: t
  def new(enumerable, transform) do
    Enum.reduce(enumerable, %MapSet{}, &put(&2, transform.(&1)))
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
  @spec delete(t, value) :: t
  def delete(%MapSet{map: map} = set, term) do
    %{set | map: Map.delete(map, term)}
  end

  @doc """
  Returns a set that is `set1` without the members of `set2`.

  ## Examples

      iex> MapSet.difference(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[1]>

  """
  @spec difference(t, t) :: t
  def difference(%MapSet{map: map1}, %MapSet{map: map2}) do
    map = :maps.fold(fn value, _, acc ->
      Map.delete(acc, value)
    end, map1, map2)
    %MapSet{map: map}
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
    :maps.fold(fn value, _, _ ->
      if Map.has_key?(map2, value) do
        throw({:halt, false})
      else
        true
      end
    end, true, map1)
  catch
    {:halt, false} -> false
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
  @spec intersection(t, t) :: t
  def intersection(%MapSet{map: map1}, %MapSet{map: map2}) do
    {map1, map2} = order_by_size(map1, map2)
    map = :maps.fold(fn value, _, acc ->
      if Map.has_key?(map2, value) do
        Map.put(acc, value, true)
      else
        acc
      end
    end, %{}, map1)
    %MapSet{map: map}
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
  @spec put(t, value) :: t
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
      :maps.fold(fn value, _, _ ->
        if Map.has_key?(map2, value) do
          true
        else
          throw({:halt, false})
        end
      end, true, map1)
    else
      false
    end
  catch
    {:halt, false} -> false
  end

  @doc """
  Converts `set` to a list.

  ## Examples

      iex> MapSet.to_list(MapSet.new([1, 2, 3]))
      [1, 2, 3]

  """
  @spec to_list(t) :: list
  def to_list(%MapSet{map: map}) do
    Map.keys(map)
  end

  @doc """
  Returns a set containing all members of `set1` and `set2`.

  ## Examples

      iex> MapSet.union(MapSet.new([1, 2]), MapSet.new([2, 3, 4]))
      #MapSet<[1, 2, 3, 4]>

  """
  @spec union(t, t) :: t
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
