defmodule MapSet do
  @moduledoc """
  A set store.

  The `MapSet` is represented internally as a struct, therefore
  `%MapSet{}` can be used whenever there is a need to match
  on any `MapSet`. Note though the struct fields are private and
  must not be accessed directly. Instead, use the functions on this
  or in the `Set` module.

  The `MapSet` is implemented using `Map` data type.
  For more information about the functions
  and their APIs, please consult the `Set` module.
  """

  @behaviour Set

  defstruct map: %{}

  def new(), do: %MapSet{}

  def delete(%MapSet{map: map} = set, term) do
    %{set | map: Map.delete(map, term)}
  end

  def difference(%MapSet{} = set1, %MapSet{} = set2) do
    reduce(set2, {:cont, set1}, fn value, acc ->
      {:cont, delete(acc, value)}
    end) |> elem(1)
  end

  def disjoint?(%MapSet{} = set1, %MapSet{} = set2) do
    if size(set1) > size(set2), do: {set1, set2} = {set2, set1}
    reduce(set1, {:cont, true}, fn value, _ ->
      if member?(set2, value) do
        {:halt, false}
      else
        {:cont, true}
      end
    end) |> elem(1)
  end

  def equal?(%MapSet{map: map1}, %MapSet{map: map2}) do
    Map.equal?(map1, map2)
  end

  def intersection(%MapSet{} = set1, %MapSet{} = set2) do
    if size(set1) > size(set2), do: {set1, set2} = {set2, set1}
    reduce(set1, {:cont, new}, fn value, acc ->
      if member?(set2, value) do
        {:cont, put(acc, value)}
      else
        {:cont, acc}
      end
    end) |> elem(1)
  end

  def member?(%MapSet{map: map}, value) do
    Map.has_key?(map, value)
  end

  def put(%MapSet{map: map} = set, value) do
    %{set | map: Map.put(map, value, nil)}
  end

  def size(%MapSet{map: map}) do
    map_size(map)
  end

  def subset?(%MapSet{} = set1, %MapSet{} = set2) do
    if size(set1) <= size(set2) do
      reduce(set1, {:cont, true}, fn value, _ ->
        if member?(set2, value), do: {:cont, true}, else: {:halt, false}
      end) |> elem(1)
    else
      false
    end
  end

  @doc false
  def reduce(%MapSet{} = set, acc, fun) do
    Enumerable.List.reduce(to_list(set), acc, fun)
  end

  def to_list(%MapSet{map: map}) do
    Map.keys(map)
  end

  def union(%MapSet{map: map1}, %MapSet{map: map2}) do
    %MapSet{map: Map.merge(map1, map2)}
  end

  defimpl Enumerable do
    def reduce(set, acc, fun), do: MapSet.reduce(set, acc, fun)
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
