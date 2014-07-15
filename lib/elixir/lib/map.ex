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

  @doc """
  Returns a map from a struct

  ## Example

      defmodule User do
        defstruct [:name]
      end

      Map.from_struct(%User{name: "valim"})
      #=> %{name: "valim"}

  """
  def from_struct(struct) do
    Enum.into(:maps.without([:__struct__], struct), %{})
  end

  def equal?(%{} = map1, %{} = map2), do: map1 === map2
end
