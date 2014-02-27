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

  defdelegate [keys(map), values(map), size(map)], to: :maps

  @doc """
  Returns a new empty map.
  """
  def new, do: %{}

  @doc """
  Creates a new map from the given pairs.

  ## Examples

      Map.new [{:b, 1}, {:a, 2}]
      #=> %{a: 2, b: 1}

  """
  def new(pairs) do
    :maps.from_list pairs
  end

  @doc """
  Creates a new map from the given pairs
  via the given transformation function.

  ## Examples

      Map.new ["a", "b"], fn x -> {x, x} end
      %{"a" => "a", "b" => "b"}

  """
  def new(list, transform) when is_function(transform) do
    Enum.map(list, transform) |> :maps.from_list
  end

  def has_key?(map, key), do: :maps.is_key(key, map)

  def fetch(map, key), do: :maps.find(key, map)

  def pop(map, key, default \\ nil) do
    { get(map, key, default), delete(map, key) }
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

  def merge(map1, map2) when is_map(map1) and is_map(map2) do
    :maps.merge(map1, map2)
  end

  def merge(map, enum, callback \\ fn(_k, _v1, v2) -> v2 end) do
    Enum.reduce enum, map, fn { k, v2 }, acc ->
      update(acc, k, v2, fn(v1) -> callback.(k, v1, v2) end)
    end
  end

  def split(map, keys) do
    acc = { %{}, %{} }

    Enum.reduce map, acc, fn({ k, v }, { take, drop }) ->
      if k in keys do
        { put(take, k, v), drop }
      else
        { take, put(drop, k, v) }
      end
    end
  end

  def update!(map, key, fun) do
    case :maps.find(key, map) do
      :error ->
        raise(KeyError, key: key)
      { :ok, val } ->
        :maps.put(key, fun.(val), map)
    end
  end

  def update(map, key, initial, fun) do
    case :maps.find(key, map) do
      :error ->
        :maps.put(key, initial, map)
      { :ok, val } ->
        :maps.put(key, fun.(val), map)
    end
  end

  def empty(_), do: %{}

  def equal?(map, map), do: true
  def equal?(_, _), do: false

  def reduce(map, acc, fun) do
    Enumerable.Map.reduce(map, acc, fun)
  end

  def to_list(map), do: :maps.to_list map
end
