defmodule Map do
  @moduledoc """
  A `Dict` implementation that works on maps.

  Maps are key-value stores where keys are compared using
  the match operator (`===`). Maps can be created with
  the `%{}` special form defined in the `Kernel.SpecialForms`
  module.

  For more information about the functions in this module and
  their APIs, please consult the `Dict` module.
  """

  use Dict

  defdelegate [keys(map), values(map), merge(map1, map2), to_list(map)], to: :maps

  @compile {:inline, fetch: 2, put: 3, delete: 2, has_key?: 2}

  # TODO: Deprecate by 1.3
  # TODO: Remove by 1.4
  @doc false
  def size(map) do
    map_size(map)
  end

  @doc """
  Returns a new empty map.
  """
  def new, do: %{}

  def has_key?(map, key), do: :maps.is_key(key, map)

  def fetch(map, key), do: :maps.find(key, map)

  def put(map, key, val) do
    :maps.put(key, val, map)
  end

  def delete(map, key), do: :maps.remove(key, map)

  def merge(map1, map2, callback) do
    :maps.fold fn k, v2, acc ->
      update(acc, k, v2, fn(v1) -> callback.(k, v1, v2) end)
    end, map1, map2
  end

  @doc """
  Updates the value in the map with the given function.
  """
  def update!(%{} = map, key, fun) do
    case fetch(map, key) do
      {:ok, value} ->
        put(map, key, fun.(value))
      :error ->
        :erlang.error({:badkey, key})
    end
  end

  def update!(map, _key, _fun), do: :erlang.error({:badmap, map})

  @doc """
  Gets a value and updates a map in one operation.
  """
  def get_and_update(%{} = map, key, fun) do
    current_value = case :maps.find(key, map) do
      {:ok, value} -> value
      :error -> nil
    end

    {get, update} = fun.(current_value)
    {get, :maps.put(key, update, map)}
  end

  def get_and_update(map, _key, _fun), do: :erlang.error({:badmap, map})

  @doc """
  Gets a value and updates a map only if the key exists in one operation.
  """
  def get_and_update!(%{} = map, key, fun) do
    case :maps.find(key, map) do
      {:ok, value} ->
        {get, update} = fun.(value)
        {get, :maps.put(key, update, map)}
      :error ->
        :erlang.error({:badkey, key})
    end
  end

  def get_and_update!(map, _key, _fun), do: :erlang.error({:badmap, map})

  @doc """
  Converts a struct to map.

  It accepts the struct module or a struct itself and
  simply removes the `__struct__` field from the struct.

  ## Example

      defmodule User do
        defstruct [:name]
      end

      Map.from_struct(User)
      #=> %{name: nil}

      Map.from_struct(%User{name: "john"})
      #=> %{name: "john"}

  """
  def from_struct(struct) when is_atom(struct) do
    :maps.remove(:__struct__, struct.__struct__)
  end

  def from_struct(%{__struct__: _} = struct) do
    :maps.remove(:__struct__, struct)
  end

  def equal?(map1, map2)
  def equal?(%{} = map1, %{} = map2), do: map1 === map2
end
