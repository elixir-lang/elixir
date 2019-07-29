defmodule Map do
  @moduledoc """
  Maps are the "go to" key-value data structure in Elixir.

  Maps can be created with the `%{}` syntax, and key-value pairs can be
  expressed as `key => value`:

      iex> %{}
      %{}
      iex> %{"one" => :two, 3 => "four"}
      %{3 => "four", "one" => :two}

  Key-value pairs in a map do not follow any order (that's why the printed map
  in the example above has a different order than the map that was created).

  Maps do not impose any restriction on the key type: anything can be a key in a
  map. As a key-value structure, maps do not allow duplicated keys. Keys are
  compared using the exact-equality operator (`===/2`). If colliding keys are defined
  in a map literal, the last one prevails.

  When the key in a key-value pair is an atom, the `key: value` shorthand syntax
  can be used (as in many other special forms), provided key-value pairs are put at
  the end:

      iex> %{"hello" => "world", a: 1, b: 2}
      %{:a => 1, :b => 2, "hello" => "world"}

  Keys in maps can be accessed through some of the functions in this module
  (such as `Map.get/3` or `Map.fetch/2`) or through the `map[]` syntax provided
  by the `Access` module:

      iex> map = %{a: 1, b: 2}
      iex> Map.fetch(map, :a)
      {:ok, 1}
      iex> map[:b]
      2
      iex> map["non_existing_key"]
      nil

  For accessing atom keys, one may also `map.key`. Note that while `map[key]` will
  return `nil` if `map` doesn't contain `key`, `map.key` will raise if `map` doesn't
  contain the key `:key`.

      iex> map = %{foo: "bar", baz: "bong"}
      iex> map.foo
      "bar"
      iex> map.non_existing_key
      ** (KeyError) key :non_existing_key not found in: %{baz: "bong", foo: "bar"}

  The two syntaxes for accessing keys reveal the dual nature of maps. The `map[key]`
  syntax is used for dynamically created maps that may have any key, of any type.
  `map.key` is used with maps that hold a predetermined set of atoms keys, which are
  expected to always be present. Structs, defined via `defstruct/1`, are one example
  of such "static maps", where the keys can also be checked during compile time.

  Maps can be pattern matched on. When a map is on the left-hand side of a
  pattern match, it will match if the map on the right-hand side contains the
  keys on the left-hand side and their values match the ones on the left-hand
  side. This means that an empty map matches every map.

      iex> %{} = %{foo: "bar"}
      %{foo: "bar"}
      iex> %{a: a} = %{:a => 1, "b" => 2, [:c, :e, :e] => 3}
      iex> a
      1
      iex> %{:c => 3} = %{:a => 1, 2 => :b}
      ** (MatchError) no match of right hand side value: %{2 => :b, :a => 1}

  Variables can be used as map keys both when writing map literals as well as
  when matching:

      iex> n = 1
      1
      iex> %{n => :one}
      %{1 => :one}
      iex> %{^n => :one} = %{1 => :one, 2 => :two, 3 => :three}
      %{1 => :one, 2 => :two, 3 => :three}

  Maps also support a specific update syntax to update the value stored under
  *existing* atom keys:

      iex> map = %{one: 1, two: 2}
      iex> %{map | one: "one"}
      %{one: "one", two: 2}
      iex> %{map | three: 3}
      ** (KeyError) key :three not found

  The functions in this module that need to find a specific key work in logarithmic time.
  This means that the time it takes to find keys grows as the map grows, but it's not
  directly proportional to the map size. In comparison to finding an element in a list,
  it performs better because lists have a linear time complexity. Some functions,
  such as `keys/1` and `values/1`, run in linear time because they need to get to every
  element in the map.

  Maps also implement the `Enumerable` protocol, so many functions to work with maps
  are found in the `Enum` module. Additionally, the following functions for maps are
  found in `Kernel`:

    * `map_size/1`

  """

  @type key :: any
  @type value :: any
  @compile {:inline, fetch: 2, fetch!: 2, get: 2, put: 3, delete: 2, has_key?: 2, replace!: 3}

  @doc """
  Returns all keys from `map`.

  Inlined by the compiler.

  ## Examples

      iex> Map.keys(%{a: 1, b: 2})
      [:a, :b]

  """
  @spec keys(map) :: [key]
  defdelegate keys(map), to: :maps

  @doc """
  Returns all values from `map`.

  Inlined by the compiler.

  ## Examples

      iex> Map.values(%{a: 1, b: 2})
      [1, 2]

  """
  @spec values(map) :: [value]
  defdelegate values(map), to: :maps

  @doc """
  Converts `map` to a list.

  Each key-value pair in the map is converted to a two-element tuple `{key,
  value}` in the resulting list.

  Inlined by the compiler.

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

      iex> Map.new()
      %{}

  """
  @spec new :: map
  def new, do: %{}

  @doc """
  Creates a map from an `enumerable`.

  Duplicated keys are removed; the latest one prevails.

  ## Examples

      iex> Map.new([{:b, 1}, {:a, 2}])
      %{a: 2, b: 1}
      iex> Map.new(a: 1, a: 2, a: 3)
      %{a: 3}

  """
  @spec new(Enumerable.t()) :: map
  def new(enumerable)
  def new(list) when is_list(list), do: :maps.from_list(list)
  def new(%_{} = struct), do: new_from_enum(struct)
  def new(%{} = map), do: map
  def new(enum), do: new_from_enum(enum)

  defp new_from_enum(enumerable) do
    enumerable
    |> Enum.to_list()
    |> :maps.from_list()
  end

  @doc """
  Creates a map from an `enumerable` via the given transformation function.

  Duplicated keys are removed; the latest one prevails.

  ## Examples

      iex> Map.new([:a, :b], fn x -> {x, x} end)
      %{a: :a, b: :b}

  """
  @spec new(Enumerable.t(), (term -> {key, value})) :: map
  def new(enumerable, transform) when is_function(transform, 1) do
    enumerable
    |> Enum.to_list()
    |> new_transform(transform, [])
  end

  defp new_transform([], _fun, acc) do
    acc
    |> :lists.reverse()
    |> :maps.from_list()
  end

  defp new_transform([element | rest], fun, acc) do
    new_transform(rest, fun, [fun.(element) | acc])
  end

  @doc """
  Returns whether the given `key` exists in the given `map`.

  Inlined by the compiler.

  ## Examples

      iex> Map.has_key?(%{a: 1}, :a)
      true
      iex> Map.has_key?(%{a: 1}, :b)
      false

  """
  @spec has_key?(map, key) :: boolean
  def has_key?(map, key), do: :maps.is_key(key, map)

  @doc """
  Fetches the value for a specific `key` in the given `map`.

  If `map` contains the given `key` with value `value`, then `{:ok, value}` is
  returned. If `map` doesn't contain `key`, `:error` is returned.

  Inlined by the compiler.

  ## Examples

      iex> Map.fetch(%{a: 1}, :a)
      {:ok, 1}
      iex> Map.fetch(%{a: 1}, :b)
      :error

  """
  @spec fetch(map, key) :: {:ok, value} | :error
  def fetch(map, key), do: :maps.find(key, map)

  @doc """
  Fetches the value for a specific `key` in the given `map`, erroring out if
  `map` doesn't contain `key`.

  If `map` contains the given `key`, the corresponding value is returned. If
  `map` doesn't contain `key`, a `KeyError` exception is raised.

  Inlined by the compiler.

  ## Examples

      iex> Map.fetch!(%{a: 1}, :a)
      1
      iex> Map.fetch!(%{a: 1}, :b)
      ** (KeyError) key :b not found in: %{a: 1}

  """
  @spec fetch!(map, key) :: value
  def fetch!(map, key) do
    :maps.get(key, map)
  end

  @doc """
  Puts the given `value` under `key` unless the entry `key`
  already exists in `map`.

  ## Examples

      iex> Map.put_new(%{a: 1}, :b, 2)
      %{a: 1, b: 2}
      iex> Map.put_new(%{a: 1, b: 2}, :a, 3)
      %{a: 1, b: 2}

  """
  @spec put_new(map, key, value) :: map
  def put_new(map, key, value) do
    case map do
      %{^key => _value} ->
        map

      %{} ->
        put(map, key, value)

      other ->
        :erlang.error({:badmap, other})
    end
  end

  @doc false
  @deprecated "Use Map.fetch/2 + Map.put/3 instead"
  def replace(map, key, value) do
    case map do
      %{^key => _value} ->
        put(map, key, value)

      %{} ->
        map

      other ->
        :erlang.error({:badmap, other})
    end
  end

  @doc """
  Alters the value stored under `key` to `value`, but only
  if the entry `key` already exists in `map`.

  If `key` is not present in `map`, a `KeyError` exception is raised.

  Inlined by the compiler.

  ## Examples

      iex> Map.replace!(%{a: 1, b: 2}, :a, 3)
      %{a: 3, b: 2}

      iex> Map.replace!(%{a: 1}, :b, 2)
      ** (KeyError) key :b not found in: %{a: 1}

  """
  @doc since: "1.5.0"
  @spec replace!(map, key, value) :: map
  def replace!(map, key, value) do
    :maps.update(key, value, map)
  end

  @doc """
  Evaluates `fun` and puts the result under `key`
  in `map` unless `key` is already present.

  This function is useful in case you want to compute the value to put under
  `key` only if `key` is not already present (e.g., the value is expensive to
  calculate or generally difficult to setup and teardown again).

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
    case map do
      %{^key => _value} ->
        map

      %{} ->
        put(map, key, fun.())

      other ->
        :erlang.error({:badmap, other})
    end
  end

  @doc """
  Returns a new map with all the key-value pairs in `map` where the key
  is in `keys`.

  If `keys` contains keys that are not in `map`, they're simply ignored.

  ## Examples

      iex> Map.take(%{a: 1, b: 2, c: 3}, [:a, :c, :e])
      %{a: 1, c: 3}

  """
  @spec take(map, [key]) :: map
  def take(map, keys)

  def take(map, keys) when is_map(map) and is_list(keys) do
    take(keys, map, _acc = [])
  end

  def take(map, keys) when is_map(map) do
    IO.warn(
      "Map.take/2 with an Enumerable of keys that is not a list is deprecated. " <>
        " Use a list of keys instead."
    )

    take(map, Enum.to_list(keys))
  end

  def take(non_map, _keys) do
    :erlang.error({:badmap, non_map})
  end

  defp take([], _map, acc) do
    :maps.from_list(acc)
  end

  defp take([key | rest], map, acc) do
    acc =
      case map do
        %{^key => value} -> [{key, value} | acc]
        %{} -> acc
      end

    take(rest, map, acc)
  end

  @doc """
  Gets the value for a specific `key` in `map`.

  If `key` is present in `map` with value `value`, then `value` is
  returned. Otherwise, `default` is returned.

  If `default` is not provided, `nil` is used.

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
  @spec get(map, key, value) :: value
  def get(map, key, default \\ nil) do
    case map do
      %{^key => value} ->
        value

      %{} ->
        default

      other ->
        :erlang.error({:badmap, other}, [map, key, default])
    end
  end

  @doc """
  Gets the value for a specific `key` in `map`.

  If `key` is present in `map` with value `value`, then `value` is
  returned. Otherwise, `fun` is evaluated and its result is returned.

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
    case map do
      %{^key => value} ->
        value

      %{} ->
        fun.()

      other ->
        :erlang.error({:badmap, other}, [map, key, fun])
    end
  end

  @doc """
  Puts the given `value` under `key` in `map`.

  Inlined by the compiler.

  ## Examples

      iex> Map.put(%{a: 1}, :b, 2)
      %{a: 1, b: 2}
      iex> Map.put(%{a: 1, b: 2}, :a, 3)
      %{a: 3, b: 2}

  """
  @spec put(map, key, value) :: map
  def put(map, key, value) do
    :maps.put(key, value, map)
  end

  @doc """
  Deletes the entry in `map` for a specific `key`.

  If the `key` does not exist, returns `map` unchanged.

  Inlined by the compiler.

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

  All keys in `map2` will be added to `map1`, overriding any existing one
  (i.e., the keys in `map2` "have precedence" over the ones in `map1`).

  If you have a struct and you would like to merge a set of keys into the
  struct, do not use this function, as it would merge all keys on the right
  side into the struct, even if the key is not part of the struct. Instead,
  use `Kernel.struct/2`.

  Inlined by the compiler.

  ## Examples

      iex> Map.merge(%{a: 1, b: 2}, %{a: 3, d: 4})
      %{a: 3, b: 2, d: 4}

  """
  @spec merge(map, map) :: map
  defdelegate merge(map1, map2), to: :maps

  @doc """
  Merges two maps into one, resolving conflicts through the given `fun`.

  All keys in `map2` will be added to `map1`. The given function will be invoked
  when there are duplicate keys; its arguments are `key` (the duplicate key),
  `value1` (the value of `key` in `map1`), and `value2` (the value of `key` in
  `map2`). The value returned by `fun` is used as the value under `key` in
  the resulting map.

  ## Examples

      iex> Map.merge(%{a: 1, b: 2}, %{a: 3, d: 4}, fn _k, v1, v2 ->
      ...>   v1 + v2
      ...> end)
      %{a: 4, b: 2, d: 4}

  """
  @spec merge(map, map, (key, value, value -> value)) :: map
  def merge(map1, map2, fun) when is_function(fun, 3) do
    if map_size(map1) > map_size(map2) do
      folder = fn key, val2, acc ->
        update(acc, key, val2, fn val1 -> fun.(key, val1, val2) end)
      end

      :maps.fold(folder, map1, map2)
    else
      folder = fn key, val2, acc ->
        update(acc, key, val2, fn val1 -> fun.(key, val2, val1) end)
      end

      :maps.fold(folder, map2, map1)
    end
  end

  @doc """
  Updates the `key` in `map` with the given function.

  If `key` is present in `map` with value `value`, `fun` is invoked with
  argument `value` and its result is used as the new value of `key`. If `key` is
  not present in `map`, `initial` is inserted as the value of `key`. The initial
  value will not be passed through the update function.

  ## Examples

      iex> Map.update(%{a: 1}, :a, 13, &(&1 * 2))
      %{a: 2}
      iex> Map.update(%{a: 1}, :b, 11, &(&1 * 2))
      %{a: 1, b: 11}

  """
  @spec update(map, key, value, (value -> value)) :: map
  def update(map, key, initial, fun) when is_function(fun, 1) do
    case map do
      %{^key => value} ->
        put(map, key, fun.(value))

      %{} ->
        put(map, key, initial)

      other ->
        :erlang.error({:badmap, other}, [map, key, initial, fun])
    end
  end

  @doc """
  Returns and removes the value associated with `key` in `map`.

  If `key` is present in `map` with value `value`, `{value, new_map}` is
  returned where `new_map` is the result of removing `key` from `map`. If `key`
  is not present in `map`, `{default, map}` is returned.

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
    case :maps.take(key, map) do
      {_, _} = tuple -> tuple
      :error -> {default, map}
    end
  end

  @doc """
  Returns and removes the value associated with `key` in `map` or raises
  if `key` is not present.

  Behaves the same as `pop/3` but raises if `key` is not present in `map`.

  ## Examples

      iex> Map.pop!(%{a: 1}, :a)
      {1, %{}}
      iex> Map.pop!(%{a: 1, b: 2}, :a)
      {1, %{b: 2}}
      iex> Map.pop!(%{a: 1}, :b)
      ** (KeyError) key :b not found in: %{a: 1}

  """
  @doc since: "1.10.0"
  @spec pop!(map, key) :: {value, map}
  def pop!(map, key) do
    case :maps.take(key, map) do
      {_, _} = tuple -> tuple
      :error -> raise KeyError, key: key, term: map
    end
  end

  @doc """
  Lazily returns and removes the value associated with `key` in `map`.

  If `key` is present in `map` with value `value`, `{value, new_map}` is
  returned where `new_map` is the result of removing `key` from `map`. If `key`
  is not present in `map`, `{fun_result, map}` is returned, where `fun_result`
  is the result of applying `fun`.

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
    case map do
      %{^key => value} ->
        {value, delete(map, key)}

      %{} ->
        {fun.(), map}

      other ->
        :erlang.error({:badmap, other}, [map, key, fun])
    end
  end

  @doc """
  Drops the given `keys` from `map`.

  If `keys` contains keys that are not in `map`, they're simply ignored.

  ## Examples

      iex> Map.drop(%{a: 1, b: 2, c: 3}, [:b, :d])
      %{a: 1, c: 3}

  """
  @spec drop(map, [key]) :: map
  def drop(map, keys)

  def drop(map, keys) when is_map(map) and is_list(keys) do
    drop_keys(keys, map)
  end

  def drop(map, keys) when is_map(map) do
    IO.warn(
      "Map.drop/2 with an Enumerable of keys that is not a list is deprecated. " <>
        " Use a list of keys instead."
    )

    drop(map, Enum.to_list(keys))
  end

  def drop(non_map, keys) do
    :erlang.error({:badmap, non_map}, [non_map, keys])
  end

  defp drop_keys([], acc), do: acc

  defp drop_keys([key | rest], acc) do
    drop_keys(rest, delete(acc, key))
  end

  @doc """
  Takes all entries corresponding to the given `keys` in `map` and extracts
  them into a separate map.

  Returns a tuple with the new map and the old map with removed keys.

  Keys for which there are no entries in `map` are ignored.

  ## Examples

      iex> Map.split(%{a: 1, b: 2, c: 3}, [:a, :c, :e])
      {%{a: 1, c: 3}, %{b: 2}}

  """
  @spec split(map, [key]) :: {map, map}
  def split(map, keys)

  def split(map, keys) when is_map(map) and is_list(keys) do
    split(keys, [], map)
  end

  def split(map, keys) when is_map(map) do
    IO.warn(
      "Map.split/2 with an Enumerable of keys that is not a list is deprecated. " <>
        " Use a list of keys instead."
    )

    split(map, Enum.to_list(keys))
  end

  def split(non_map, keys) do
    :erlang.error({:badmap, non_map}, [non_map, keys])
  end

  defp split([], included, excluded) do
    {:maps.from_list(included), excluded}
  end

  defp split([key | rest], included, excluded) do
    case excluded do
      %{^key => value} ->
        split(rest, [{key, value} | included], delete(excluded, key))

      _other ->
        split(rest, included, excluded)
    end
  end

  @doc """
  Updates `key` with the given function.

  If `key` is present in `map` with value `value`, `fun` is invoked with
  argument `value` and its result is used as the new value of `key`. If `key` is
  not present in `map`, a `KeyError` exception is raised.

  ## Examples

      iex> Map.update!(%{a: 1}, :a, &(&1 * 2))
      %{a: 2}

      iex> Map.update!(%{a: 1}, :b, &(&1 * 2))
      ** (KeyError) key :b not found in: %{a: 1}

  """
  @spec update!(map, key, (value -> value)) :: map
  def update!(map, key, fun) when is_function(fun, 1) do
    value = fetch!(map, key)
    put(map, key, fun.(value))
  end

  @doc """
  Gets the value from `key` and updates it, all in one pass.

  `fun` is called with the current value under `key` in `map` (or `nil` if `key`
  is not present in `map`) and must return a two-element tuple: the "get" value
  (the retrieved value, which can be operated on before being returned) and the
  new value to be stored under `key` in the resulting new map. `fun` may also
  return `:pop`, which means the current value shall be removed from `map` and
  returned (making this function behave like `Map.pop(map, key)`).

  The returned value is a tuple with the "get" value returned by
  `fun` and a new map with the updated value under `key`.

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
  def get_and_update(map, key, fun) when is_function(fun, 1) do
    current = get(map, key)

    case fun.(current) do
      {get, update} ->
        {get, put(map, key, update)}

      :pop ->
        {current, delete(map, key)}

      other ->
        raise "the given function must return a two-element tuple or :pop, got: #{inspect(other)}"
    end
  end

  @doc """
  Gets the value from `key` and updates it. Raises if there is no `key`.

  Behaves exactly like `get_and_update/3`, but raises a `KeyError` exception if
  `key` is not present in `map`.

  ## Examples

      iex> Map.get_and_update!(%{a: 1}, :a, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      {1, %{a: "new value!"}}

      iex> Map.get_and_update!(%{a: 1}, :b, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      ** (KeyError) key :b not found in: %{a: 1}

      iex> Map.get_and_update!(%{a: 1}, :a, fn _ ->
      ...>   :pop
      ...> end)
      {1, %{}}

  """
  @spec get_and_update!(map, key, (value -> {get, value} | :pop)) :: {get, map}
        when get: term
  def get_and_update!(map, key, fun) when is_function(fun, 1) do
    value = fetch!(map, key)

    case fun.(value) do
      {get, update} ->
        {get, put(map, key, update)}

      :pop ->
        {value, delete(map, key)}

      other ->
        raise "the given function must return a two-element tuple or :pop, got: #{inspect(other)}"
    end
  end

  @doc """
  Converts a `struct` to map.

  It accepts the struct module or a struct itself and
  simply removes the `__struct__` field from the given struct
  or from a new struct generated from the given module.

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
    delete(struct.__struct__(), :__struct__)
  end

  def from_struct(%_{} = struct) do
    delete(struct, :__struct__)
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
  def equal?(map1, map2)

  def equal?(%{} = map1, %{} = map2), do: map1 === map2
  def equal?(%{} = map1, map2), do: :erlang.error({:badmap, map2}, [map1, map2])
  def equal?(term, other), do: :erlang.error({:badmap, term}, [term, other])

  @doc false
  @deprecated "Use Kernel.map_size/1 instead"
  def size(map) do
    map_size(map)
  end
end
