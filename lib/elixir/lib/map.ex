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

  use Dict

  defdelegate [keys(map), values(map), size(map), merge(map1, map2), to_list(map)], to: :maps

  @compile {:inline, fetch: 2, put: 3, delete: 2, has_key?: 2}

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

  def get_and_update(map, key, fun) do
    current_value = case :maps.find(key, map) do
      {:ok, value} -> value
      :error -> nil
    end

    {get, update} = fun.(current_value)
    {get, :maps.put(key, update, map)}
  end

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

  @doc """
  Converts all map keys to atoms.

  ## Example

      map = %{"foo" => "bar", "fu" => "baz"}

      Map.atomize_keys(map)
      #=> %{foo: "bar", fu: "baz"}

      map_mixed = %{"foo" => "bar", fu: "baz"}

      Map.atomize_keys(map_mixed)
      # => %{foo: "bar", fu: "baz"}
  """

  def atomize_keys(map) do
    :maps.fold fn k, v, acc ->
      if is_binary(k) do
        put(acc, String.to_atom(k), v)
      else
        put(acc, k, v)
      end
    end, %{}, map
  end
end
