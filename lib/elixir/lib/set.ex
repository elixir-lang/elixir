defmodule Set do
  @moduledoc ~S"""
  Generic API for sets.

  This module is deprecated, use the `MapSet` module instead.
  """

  @moduledoc deprecated: "Use MapSet instead"

  @type value :: any
  @type values :: [value]
  @type t :: map

  message = "Use the MapSet module for working with sets"

  defmacrop target(set) do
    quote do
      case unquote(set) do
        %module{} -> module
        set -> unsupported_set(set)
      end
    end
  end

  @deprecated message
  def delete(set, value) do
    target(set).delete(set, value)
  end

  @deprecated message
  def difference(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    if target1 == target2 do
      target1.difference(set1, set2)
    else
      Enumerable.reduce(set2, {:cont, set1}, fn v, acc ->
        {:cont, target1.delete(acc, v)}
      end)
      |> elem(1)
    end
  end

  @deprecated message
  def disjoint?(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    if target1 == target2 do
      target1.disjoint?(set1, set2)
    else
      Enumerable.reduce(set2, {:cont, true}, fn member, acc ->
        case target1.member?(set1, member) do
          false -> {:cont, acc}
          _ -> {:halt, false}
        end
      end)
      |> elem(1)
    end
  end

  @deprecated message
  def empty(set) do
    target(set).empty(set)
  end

  @deprecated message
  def equal?(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    cond do
      target1 == target2 ->
        target1.equal?(set1, set2)

      target1.size(set1) == target2.size(set2) ->
        do_subset?(target1, target2, set1, set2)

      true ->
        false
    end
  end

  @deprecated message
  def intersection(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    if target1 == target2 do
      target1.intersection(set1, set2)
    else
      Enumerable.reduce(set1, {:cont, target1.new}, fn v, acc ->
        {:cont, if(target2.member?(set2, v), do: target1.put(acc, v), else: acc)}
      end)
      |> elem(1)
    end
  end

  @deprecated message
  def member?(set, value) do
    target(set).member?(set, value)
  end

  @deprecated message
  def put(set, value) do
    target(set).put(set, value)
  end

  @deprecated message
  def size(set) do
    target(set).size(set)
  end

  @deprecated message
  def subset?(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    if target1 == target2 do
      target1.subset?(set1, set2)
    else
      do_subset?(target1, target2, set1, set2)
    end
  end

  @deprecated message
  def to_list(set) do
    target(set).to_list(set)
  end

  @deprecated message
  def union(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    if target1 == target2 do
      target1.union(set1, set2)
    else
      Enumerable.reduce(set2, {:cont, set1}, fn v, acc ->
        {:cont, target1.put(acc, v)}
      end)
      |> elem(1)
    end
  end

  defp do_subset?(_target1, target2, set1, set2) do
    Enumerable.reduce(set1, {:cont, true}, fn member, acc ->
      case target2.member?(set2, member) do
        true -> {:cont, acc}
        _ -> {:halt, false}
      end
    end)
    |> elem(1)
  end

  defp unsupported_set(set) do
    raise ArgumentError, "unsupported set: #{inspect(set)}"
  end
end
