defmodule Map do
  @moduledoc """
  A Dict implementation that works on maps.

  Maps are key-value stores where keys are compared using
  the match operator (`===`). Maps can be created with
  the `%{}` special form defined in the `Kernel.SpecialForms`
  module.

  For more information about the functions in this module and
  their APIs, please consult the `Dict` module.
  """

  use Dict.Behaviour

  defdelegate [keys(map), values(map), size(map), merge(map1, map2), to_list(map)], to: :maps

  @doc """
  Returns a new empty map.
  """
  def new, do: %{}

  @doc false
  def new(pairs) do
    IO.write :stderr, "Map.new/1 is deprecated, please use Enum.into/2 instead\n#{Exception.format_stacktrace}"
    :maps.from_list pairs
  end

  @doc false
  def new(list, transform) when is_function(transform) do
    IO.write :stderr, "Map.new/2 is deprecated, please use Enum.into/2 instead\n#{Exception.format_stacktrace}"
    Enum.map(list, transform) |> :maps.from_list
  end

  def has_key?(map, key), do: :maps.is_key(key, map)

  def fetch(map, key), do: :maps.find(key, map)

  def pop(map, key, default \\ nil) do
    {get(map, key, default), delete(map, key)}
  end

  def put(map, key, val) do
    :maps.put(key, val, map)
  end

  def put_new(map, key, val) do
    case has_key?(map, key) do
      true  -> map
      false -> :maps.put(key, val, map)
    end
  end

  def delete(map, key), do: :maps.remove(key, map)

  def merge(map1, map2, callback) do
    :maps.fold fn k, v2, acc ->
      update(acc, k, v2, fn(v1) -> callback.(k, v1, v2) end)
    end, map1, map2
  end

  def split(map, keys) do
    acc = {%{}, %{}}

    :maps.fold fn k, v, {take, drop} ->
      if k in keys do
        {put(take, k, v), drop}
      else
        {take, put(drop, k, v)}
      end
    end, acc, map
  end

  def update!(map, key, fun) do
    case :maps.find(key, map) do
      :error ->
        raise(KeyError, key: key, term: map)
      {:ok, val} ->
        :maps.put(key, fun.(val), map)
    end
  end

  def update(map, key, initial, fun) do
    case :maps.find(key, map) do
      :error ->
        :maps.put(key, initial, map)
      {:ok, val} ->
        :maps.put(key, fun.(val), map)
    end
  end

  def equal?(map, map), do: true
  def equal?(_, _), do: false
end
