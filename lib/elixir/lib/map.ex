defmodule Map do
  @moduledoc """
  A set of functions for working with maps.

  Maps are key-value stores where keys can be any value and
  are compared using the match operator (`===`). Maps can be
  created with the `%{}` special form defined in the
  `Kernel.SpecialForms` module.
  """

  @type key :: any
  @type value :: any
  @compile {:inline, fetch: 2, put: 3, delete: 2, has_key?: 2}

  @doc """
  Returns all keys from the map.

  ## Examples

      iex> Map.keys(%{a: 1, b: 2})
      [:a, :b]

  """
  @spec keys(map) :: [key]
  defdelegate keys(map), to: :maps

  @doc """
  Returns all values from the map.

  ## Examples

      iex> Map.values(%{a: 1, b: 2})
      [1, 2]

  """
  @spec values(map) :: [value]
  defdelegate values(map), to: :maps

  @doc """
  Converts the map to a list.

  ## Examples

      iex> Map.to_list(%{a: 1})
      [a: 1]
      iex> Map.to_list(%{1 => 2})
      [{1, 2}]

  """
  @spec to_list(map) :: [{term, term}]
  defdelegate to_list(map), to: :maps

  @doc """
  Returns a new empty map.

  ## Examples

      iex> Map.new
      %{}

  """
  @spec new :: map
  def new, do: %{}

  @doc """
  Creates a map from an enumerable.

  Duplicated keys are removed; the latest one prevails.

  ## Examples

      iex> Map.new([{:b, 1}, {:a, 2}])
      %{a: 2, b: 1}
      iex> Map.new([a: 1, a: 2, a: 3])
      %{a: 3}

  """
  @spec new(Enum.t) :: map
  def new(enumerable) do
    Enum.reduce(enumerable, %{}, fn {k, v}, acc -> put(acc, k, v) end)
  end

  @doc """
  Creates a map from an enumerable via the transformation function.

  Duplicated entries are removed; the latest one prevails.

  ## Examples

      iex> Map.new([:a, :b], fn x -> {x, x} end)
      %{a: :a, b: :b}

  """
  @spec new(Enum.t, (term -> {key, value})) :: map
  def new(enumerable, transform) do
    fun = fn el, acc ->
      {k, v} = transform.(el)
      put(acc, k, v)
    end
    Enum.reduce(enumerable, %{}, fun)
  end

  @doc """
  Returns whether a given `key` exists in the given `map`.

  ## Examples

      iex> Map.has_key?(%{a: 1}, :a)
      true
      iex> Map.has_key?(%{a: 1}, :b)
      false

  """
  @spec has_key?(map, key) :: boolean
  def has_key?(map, key), do: :maps.is_key(key, map)

  @doc """
  Fetches the value for a specific `key` and returns it in a tuple.

  If the `key` does not exist, returns `:error`.

  ## Examples

      iex> Map.fetch(%{a: 1}, :a)
      {:ok, 1}
      iex> Map.fetch(%{a: 1}, :b)
      :error

  """
  @spec fetch(map, key) :: {:ok, value} | :error
  def fetch(map, key), do: :maps.find(key, map)

  @doc """
  Fetches the value for specific `key`.

  If `key` does not exist, a `KeyError` is raised.

  ## Examples

      iex> Map.fetch!(%{a: 1}, :a)
      1
      iex> Map.fetch!(%{a: 1}, :b)
      ** (KeyError) key :b not found in: %{a: 1}

  """
  @spec fetch!(map, key) :: value | no_return
  def fetch!(map, key) do
    case fetch(map, key) do
      {:ok, value} -> value
      :error -> raise KeyError, key: key, term: map
    end
  end

  @doc """
  Puts the given `value` under `key` unless the entry `key`
  already exists.

  ## Examples

      iex> Map.put_new(%{a: 1}, :b, 2)
      %{b: 2, a: 1}
      iex> Map.put_new(%{a: 1, b: 2}, :a, 3)
      %{a: 1, b: 2}

  """
  @spec put_new(map, key, value) :: map
  def put_new(map, key, value) do
    case has_key?(map, key) do
      true  -> map
      false -> put(map, key, value)
    end
  end

  @doc """
  Evaluates `fun` and puts the result under `key`
  in map unless `key` is already present.

  This is useful if the value is very expensive to calculate or
  generally difficult to setup and teardown again.

  ## Examples

      iex> map = %{a: 1}
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   3
      ...> end
      iex> Map.put_new_lazy(map, :a, fun)
      %{a: 1}
      iex> Map.put_new_lazy(map, :b, fun)
      %{a: 1, b: 3}

  """
  @spec put_new_lazy(map, key, (() -> value)) :: map
  def put_new_lazy(map, key, fun) when is_function(fun, 0) do
    case has_key?(map, key) do
      true  -> map
      false -> put(map, key, fun.())
    end
  end

  @doc """
  Takes all entries corresponding to the given keys and
  returns them in a new map.

  ## Examples

      iex> Map.take(%{a: 1, b: 2, c: 3}, [:a, :c, :e])
      %{a: 1, c: 3}

  """
  @spec take(map, [key]) :: map
  def take(map, keys) do
    Enum.reduce(keys, [], fn key, acc ->
      case fetch(map, key) do
        {:ok, value} -> [{key, value} | acc]
        :error -> acc
      end
    end)
    |> :maps.from_list
  end

  @doc """
  Gets the value for a specific `key`.

  If `key` does not exist, return the default value
  (`nil` if no default value).

  ## Examples

      iex> Map.get(%{}, :a)
      nil
      iex> Map.get(%{a: 1}, :a)
      1
      iex> Map.get(%{a: 1}, :b)
      nil
      iex> Map.get(%{a: 1}, :b, 3)
      3

  """
  @spec get(map, key) :: value
  @spec get(map, key, value) :: value
  def get(map, key, default \\ nil) do
    case fetch(map, key) do
      {:ok, value} -> value
      :error -> default
    end
  end

  @doc """
  Gets the value for a specific `key`.

  If `key` does not exist, lazily evaluates `fun` and returns its result.

  This is useful if the default value is very expensive to calculate or
  generally difficult to setup and teardown again.

  ## Examples

      iex> map = %{a: 1}
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   13
      ...> end
      iex> Map.get_lazy(map, :a, fun)
      1
      iex> Map.get_lazy(map, :b, fun)
      13

  """
  @spec get_lazy(map, key, (() -> value)) :: value
  def get_lazy(map, key, fun) when is_function(fun, 0) do
    case fetch(map, key) do
      {:ok, value} -> value
      :error -> fun.()
    end
  end

  @doc """
  Puts the given `value` under `key`.

  ## Examples

      iex> Map.put(%{a: 1}, :b, 2)
      %{a: 1, b: 2}
      iex> Map.put(%{a: 1, b: 2}, :a, 3)
      %{a: 3, b: 2}

  """
  @spec put(map, key, value) :: map
  def put(map, key, val) do
    :maps.put(key, val, map)
  end

  @doc """
  Deletes the entries in the map for a specific `key`.

  If the `key` does not exist, returns the map unchanged.

  ## Examples

      iex> Map.delete(%{a: 1, b: 2}, :a)
      %{b: 2}
      iex> Map.delete(%{b: 2}, :a)
      %{b: 2}

  """
  @spec delete(map, key) :: map
  def delete(map, key), do: :maps.remove(key, map)

  @doc """
  Merges two maps into one.

  All keys in `map2` will be added to `map1`, overriding any existing one.

  ## Examples

      iex> Map.merge(%{a: 1, b: 2}, %{a: 3, d: 4})
      %{a: 3, b: 2, d: 4}

  """
  @spec merge(map, map) :: map
  defdelegate merge(map1, map2), to: :maps

  @doc """
  Merges two maps into one.

  All keys in `map2` will be added to `map1`. The given function will
  be invoked with the key, value1 and value2 to solve conflicts.

  ## Examples

      iex> Map.merge(%{a: 1, b: 2}, %{a: 3, d: 4}, fn _k, v1, v2 ->
      ...>   v1 + v2
      ...> end)
      %{a: 4, b: 2, d: 4}

  """
  @spec merge(map, map, (key, value, value -> value)) :: map
  def merge(map1, map2, callback) do
    :maps.fold fn k, v2, acc ->
      update(acc, k, v2, fn(v1) -> callback.(k, v1, v2) end)
    end, map1, map2
  end

  @doc """
  Updates the `key` in `map` with the given function.

  If the `key` does not exist, inserts the given `initial` value.

  ## Examples

      iex> Map.update(%{a: 1}, :a, 13, &(&1 * 2))
      %{a: 2}
      iex> Map.update(%{a: 1}, :b, 11, &(&1 * 2))
      %{a: 1, b: 11}

  """
  @spec update(map, key, value, (value -> value)) :: map
  def update(map, key, initial, fun) do
    case fetch(map, key) do
      {:ok, value} ->
        put(map, key, fun.(value))
      :error ->
        put(map, key, initial)
    end
  end

  @doc """
  Returns and removes all values associated with `key` in the `map`.

  ## Examples

      iex> Map.pop(%{a: 1}, :a)
      {1, %{}}
      iex> Map.pop(%{a: 1}, :b)
      {nil, %{a: 1}}
      iex> Map.pop(%{a: 1}, :b, 3)
      {3, %{a: 1}}

  """
  @spec pop(map, key, value) :: {value, map}
  def pop(map, key, default \\ nil) do
    case map do
      %{^key => value} -> {value, delete(map, key)}
      %{} -> {default, map}
    end
  end

  @doc """
  Lazily returns and removes all values associated with `key` in the `map`.

  This is useful if the default value is very expensive to calculate or
  generally difficult to setup and teardown again.

  ## Examples

      iex> map = %{a: 1}
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   13
      ...> end
      iex> Map.pop_lazy(map, :a, fun)
      {1, %{}}
      iex> Map.pop_lazy(map, :b, fun)
      {13, %{a: 1}}

  """
  @spec pop_lazy(map, key, (() -> value)) :: {value, map}
  def pop_lazy(map, key, fun) when is_function(fun, 0) do
    case fetch(map, key) do
      {:ok, value} -> {value, delete(map, key)}
      :error -> {fun.(), map}
    end
  end

  @doc """
  Drops the given keys from the map.

  ## Examples

      iex> Map.drop(%{a: 1, b: 2, c: 3}, [:b, :d])
      %{a: 1, c: 3}

  """
  @spec drop(map, [key]) :: map
  def drop(map, keys) do
    Enum.reduce(keys, map, &delete(&2, &1))
  end

  @doc """
  Takes all entries corresponding to the given keys and extracts them into a
  separate map.

  Returns a tuple with the new map and the old map with removed keys.

  Keys for which there are no entries in the map are ignored.

  ## Examples

      iex> Map.split(%{a: 1, b: 2, c: 3}, [:a, :c, :e])
      {%{a: 1, c: 3}, %{b: 2}}

  """
  @spec split(map, [key]) :: {map, map}
  def split(map, keys) do
    Enum.reduce(keys, {new, map}, fn key, {inc, exc} = acc ->
      case fetch(exc, key) do
        {:ok, value} ->
          {put(inc, key, value), delete(exc, key)}
        :error ->
          acc
      end
    end)
  end

  @doc """
  Updates the `key` with the given function.

  If the `key` does not exist, raises `KeyError`.

  ## Examples

      iex> Map.update!(%{a: 1}, :a, &(&1 * 2))
      %{a: 2}

      iex> Map.update!(%{a: 1}, :b, &(&1 * 2))
      ** (KeyError) key :b not found

  """
  @spec update!(map, key, (value -> value)) :: map | no_return
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
  Gets the value from `key` and updates it, all in one pass.

  This `fun` argument receives the value of `key` (or `nil` if `key`
  is not present) and must return a two-element tuple: the "get" value (the
  retrieved value, which can be operated on before being returned) and the new
  value to be stored under `key`.

  The returned value may be a tuple with the "get" value returned by
  `fun` and a new map with the updated value under `key`. The function
  may also return `:pop`, implying the current value shall be removed
  from the map and returned.

  ## Examples

      iex> Map.get_and_update(%{a: 1}, :a, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      {1, %{a: "new value!"}}

      iex> Map.get_and_update(%{a: 1}, :b, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      {nil, %{b: "new value!", a: 1}}

      iex> Map.get_and_update(%{a: 1}, :a, fn _ -> :pop end)
      {1, %{}}

      iex> Map.get_and_update(%{a: 1}, :b, fn _ -> :pop end)
      {nil, %{a: 1}}

  """
  @spec get_and_update(map, key, (value -> {get, value} | :pop)) :: {get, map} when get: term
  def get_and_update(%{} = map, key, fun) do
    current =
      case :maps.find(key, map) do
        {:ok, value} -> value
        :error -> nil
      end

    case fun.(current) do
      {get, update} -> {get, :maps.put(key, update, map)}
      :pop          -> {current, :maps.remove(key, map)}
    end
  end

  def get_and_update(map, _key, _fun), do: :erlang.error({:badmap, map})

  @doc """
  Gets the value from `key` and updates it. Raises if there is no `key`.

  This `fun` argument receives the value of `key` and must return a
  two-element tuple: the "get" value (the retrieved value, which can be
  operated on before being returned) and the new value to be stored under
  `key`.

  The returned value is a tuple with the "get" value returned by `fun` and a
  new map with the updated value under `key`.

  ## Examples

      iex> Map.get_and_update!(%{a: 1}, :a, fn(current_value) ->
      ...>   {current_value, "new value!"}
      ...> end)
      {1, %{a: "new value!"}}

      iex> Map.get_and_update!(%{a: 1}, :b, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      ** (KeyError) key :b not found

  """
  @spec get_and_update!(map, key, (value -> {get, value})) :: {get, map} | no_return when get: term
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
  @spec from_struct(atom | struct) :: map
  def from_struct(struct) when is_atom(struct) do
    :maps.remove(:__struct__, struct.__struct__)
  end

  def from_struct(%{__struct__: _} = struct) do
    :maps.remove(:__struct__, struct)
  end

  @doc """
  Checks if two maps are equal.

  Two maps are considered to be equal if they contain
  the same keys and those keys contain the same values.

  ## Examples

      iex> Map.equal?(%{a: 1, b: 2}, %{b: 2, a: 1})
      true
      iex> Map.equal?(%{a: 1, b: 2}, %{b: 1, a: 2})
      false

  """
  @spec equal?(map, map) :: boolean
  def equal?(%{} = map1, %{} = map2), do: map1 === map2

  @doc false
  def size(map) do
    IO.write :stderr, "warning: Map.size/1 is deprecated, please use Kernel.map_size/1\n" <>
                      Exception.format_stacktrace
    map_size(map)
  end
end
