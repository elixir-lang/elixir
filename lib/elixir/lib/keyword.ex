defmodule Keyword do
  @moduledoc """
  A keyword is a list of tuples where the first element
  of the tuple is an atom and the second element can be
  any value.

  A keyword may have duplicated keys so it is not strictly
  a dictionary. However most of the functions in this module
  behaves exactly as a dictionary and mimic the API defined
  by the `Dict` behaviour.

  For example, `Keyword.get` will get the first entry matching
  the given key, regardless if duplicated entries exist.
  Similarly, `Keyword.put` and `Keyword.delete` ensure all
  duplicated entries for a given key are removed when invoked.
  A handful of functions exist to handle duplicated keys, in
  particular, `from_enum` allows creating a new keywords without
  removing duplicated keys, `get_values` returns all values for
  a given key and `delete_first` deletes just one of the existing
  entries.

  Since a keyword list is simply a list, all the operations defined
  in `Enum` and `List` can also be applied.
  """

  @type key :: atom
  @type value :: any

  @type t :: [{key, value}]
  @type t(value) :: [{ key, value }]

  @doc """
  Creates a Keyword from an enum. Unlike `Keyword.new`
  which behaves as a dict, `Keyword.from_enum` does not remove
  duplicated entries.
  """
  @spec from_enum(Enum.t) :: t
  def from_enum(enum) do
    IO.write :stderr, "Keyword.from_enum/1 is deprecated, please use Enum.into/2 instead\n#{Exception.format_stacktrace}"
    Enum.to_list(enum)
  end

  @doc """
  Checks if the given argument is a keywords list or not.
  """
  @spec keyword?(term) :: boolean
  def keyword?([{ key, _value } | rest]) when is_atom(key) do
    keyword?(rest)
  end

  def keyword?([]),     do: true
  def keyword?(_other), do: false

  @doc """
  Returns an empty keyword list, i.e. an empty list.
  """
  @spec new :: t
  def new do
    []
  end

  @doc """
  Creates a keyword from an enumerable.

  Duplicated entries are removed, the latest one prevails.
  I.e. differently from `Enum.into(enumerable, [])`,
  `Keyword.new(enumerable)` guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([{:b, 1}, {:a, 2}])
      [a: 2, b: 1]

  """
  @spec new(Enum.t) :: t
  def new(pairs) do
    Enum.reduce pairs, [], fn { k, v }, keywords ->
      put(keywords, k, v)
    end
  end

  @doc """
  Creates a keyword from an enumerable via the transformation function.

  Duplicated entries are removed, the latest one prevails.
  I.e. differently from `Enum.into(enumerable, [], fun)`,
  `Keyword.new(enumerable, fun)` guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([:a, :b], fn (x) -> {x, x} end) |> Enum.sort
      [a: :a, b: :b]

  """
  @spec new(Enum.t, ({key, value} -> {key, value})) :: t
  def new(pairs, transform) do
    Enum.reduce pairs, [], fn i, keywords ->
      { k, v } = transform.(i)
      put(keywords, k, v)
    end
  end

  @doc """
  Gets the value for a specific `key`.

  If `key` does not exist, return default value (`nil` if no default value).

  If duplicated entries exist, the first one is returned.
  Use `get_values/2` to retrieve all entries.

  ## Examples

      iex> Keyword.get([a: 1], :a)
      1

      iex> Keyword.get([a: 1], :b)
      nil

      iex> Keyword.get([a: 1], :b, 3)
      3

  """
  @spec get(t, key) :: value
  @spec get(t, key, value) :: value
  def get(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      { ^key, value } -> value
      false -> default
    end
  end

  @doc """
  Fetches the value for a specific `key` and returns it in a tuple.
  If the `key` does not exist, returns `:error`.

  ## Examples

      iex> Keyword.fetch([a: 1], :a)
      { :ok, 1 }

      iex> Keyword.fetch([a: 1], :b)
      :error

  """
  @spec fetch(t, key) :: value
  def fetch(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      { ^key, value } -> { :ok, value }
      false -> :error
    end
  end

  @doc """
  Fetches the value for specific `key`. If `key` does not exist,
  a `KeyError` is raised.

  ## Examples

      iex> Keyword.fetch!([a: 1], :a)
      1

      iex> Keyword.fetch!([a: 1], :b)
      ** (KeyError) key :b not found in: [a: 1]

  """
  @spec fetch!(t, key) :: value | no_return
  def fetch!(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      { ^key, value } -> value
      false -> raise(KeyError, key: key, term: keywords)
    end
  end

  @doc """
  Gets all values for a specific `key`.

  ## Examples

      iex> Keyword.get_values([a: 1, a: 2], :a)
      [1,2]

  """
  @spec get_values(t, key) :: [value]
  def get_values(keywords, key) when is_list(keywords) and is_atom(key) do
    for { k, v } <- keywords, key == k, do: v
  end

  @doc """
  Returns all keys from the keyword list. Duplicated
  keys appear duplicated in the final list of keys.

  ## Examples

      iex> Keyword.keys([a: 1, b: 2])
      [:a,:b]

      iex> Keyword.keys([a: 1, b: 2, a: 3])
      [:a,:b,:a]

  """
  @spec keys(t) :: [key]
  def keys(keywords) when is_list(keywords) do
    for { key, _ } <- keywords, do: key
  end

  @doc """
  Returns all values from the keyword list.

  ## Examples

      iex> Keyword.values([a: 1, b: 2])
      [1,2]

  """
  @spec values(t) :: [value]
  def values(keywords) when is_list(keywords) do
    for { _, value } <- keywords, do: value
  end

  @doc """
  Deletes the entry in the keyword list for a `key` with `value`.
  If no `key` with `value` exists, returns the keyword list unchanged.

  ## Examples

      iex> Keyword.delete([a: 1, b: 2], :a, 1)
      [b: 2]

      iex> Keyword.delete([a: 1, b: 2, a: 3], :a, 3)
      [a: 1, b: 2]

      iex> Keyword.delete([b: 2], :a, 5)
      [b: 2]

  """
  @spec delete(t, key, value) :: t
  def delete(keywords, key, value) when is_list(keywords) and is_atom(key) do
    for { k, v } = tuple <- keywords, key != k or value != v, do: tuple
  end

  @doc """
  Deletes all entries in the keyword list for a specific `key`.
  If the `key` does not exist, returns the keyword list unchanged.
  Use `delete_first` to delete just the first entry in case of
  duplicated keys.

  ## Examples

      iex> Keyword.delete([a: 1, b: 2], :a)
      [b: 2]

      iex> Keyword.delete([a: 1, b: 2, a: 3], :a)
      [b: 2]

      iex> Keyword.delete([b: 2], :a)
      [b: 2]

  """
  @spec delete(t, key) :: t
  def delete(keywords, key) when is_list(keywords) and is_atom(key) do
    for { k, _ } = tuple <- keywords, key != k, do: tuple
  end

  @doc """
  Deletes the first entry in the keyword list for a specific `key`.
  If the `key` does not exist, returns the keyword list unchanged.

  ## Examples

      iex> Keyword.delete_first([a: 1, b: 2, a: 3], :a)
      [b: 2, a: 3]

      iex> Keyword.delete_first([b: 2], :a)
      [b: 2]

  """
  @spec delete_first(t, key) :: t
  def delete_first(keywords, key) when is_list(keywords) and is_atom(key) do
    :lists.keydelete(key, 1, keywords)
  end

  @doc """
  Puts the given `value` under `key`.

  If a previous value is already stored, all entries are
  removed and the value is overridden.

  ## Examples

      iex> Keyword.put([a: 1, b: 2], :a, 3)
      [a: 3, b: 2]

      iex> Keyword.put([a: 1, b: 2, a: 4], :a, 3)
      [a: 3, b: 2]

  """
  @spec put(t, key, value) :: t
  def put(keywords, key, value) when is_list(keywords) and is_atom(key) do
    [{key, value}|delete(keywords, key)]
  end

  @doc """
  Puts the given `value` under `key` unless the entry `key`
  already exists.

  ## Examples

      iex> Keyword.put_new([a: 1], :b, 2)
      [b: 2, a: 1]

      iex> Keyword.put_new([a: 1, b: 2], :a, 3)
      [a: 1, b: 2]

  """
  @spec put_new(t, key, value) :: t
  def put_new(keywords, key, value) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      { ^key, _ } -> keywords
      false -> [{key, value}|keywords]
    end
  end

  @doc """
  Checks if two keywords are equal. I.e. they contain
  the same keys and those keys contain the same values.

  ## Examples

      iex> Keyword.equal?([a: 1, b: 2], [b: 2, a: 1])
      true

  """
  @spec equal?(t, t) :: boolean
  def equal?(left, right) when is_list(left) and is_list(right) do
    :lists.sort(left) == :lists.sort(right)
  end

  @doc """
  Merges two keyword lists into one. If they have duplicated
  entries, the one given as second argument wins.

  ## Examples

      iex> Keyword.merge([a: 1, b: 2], [a: 3, d: 4]) |> Enum.sort
      [a: 3, b: 2, d: 4]

  """
  @spec merge(t, t) :: t
  def merge(d1, d2) when is_list(d1) and is_list(d2) do
    d2 ++ for({ k, _ } = tuple <- d1, not has_key?(d2, k), do: tuple)
  end

  @doc """
  Merges two keyword lists into one. If they have duplicated
  entries, the given function is invoked to solve conflicts.

  ## Examples

      iex> Keyword.merge([a: 1, b: 2], [a: 3, d: 4], fn (_k, v1, v2) ->
      ...>  v1 + v2
      ...> end)
      [a: 4, b: 2, d: 4]

  """
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(d1, d2, fun) when is_list(d1) and is_list(d2) do
    do_merge(d2, d1, fun)
  end

  defp do_merge([{ k, v2 }|t], acc, fun) do
    do_merge t, update(acc, k, v2, fn(v1) -> fun.(k, v1, v2) end), fun
  end

  defp do_merge([], acc, _fun) do
    acc
  end

  @doc """
  Returns whether a given `key` exists in the given `keywords`.

  ## Examples

      iex> Keyword.has_key?([a: 1], :a)
      true

      iex> Keyword.has_key?([a: 1], :b)
      false

  """
  @spec has_key?(t, key) :: boolean
  def has_key?(keywords, key) when is_list(keywords) and is_atom(key) do
    :lists.keymember(key, 1, keywords)
  end

  @doc """
  Updates the `key` with the given function. If the `key` does
  not exist, raises `KeyError`.

  ## Examples

      iex> Keyword.update!([a: 1], :a, &(&1 * 2))
      [a: 2]

      iex> Keyword.update!([a: 1], :b, &(&1 * 2))
      ** (KeyError) key :b not found in: [a: 1]

  """
  @spec update!(t, key, (value -> value)) :: t | no_return
  def update!(keywords, key, fun) do
    update!(keywords, key, fun, keywords)
  end

  defp update!([{key, value}|keywords], key, fun, _dict) do
    [{key, fun.(value)}|delete(keywords, key)]
  end

  defp update!([{_, _} = e|keywords], key, fun, dict) do
    [e|update!(keywords, key, fun, dict)]
  end

  defp update!([], key, _fun, dict) when is_atom(key) do
    raise(KeyError, key: key, term: dict)
  end

  @doc """
  Updates the `key` with the given function. If the `key` does
  not exist, inserts the given `initial` value.

  ## Examples

      iex> Keyword.update([a: 1], :a, 13, &(&1 * 2))
      [a: 2]

      iex> Keyword.update([a: 1], :b, 11, &(&1 * 2))
      [a: 1, b: 11]

  """
  @spec update(t, key, value, (value -> value)) :: t
  def update([{key, value}|keywords], key, _initial, fun) do
    [{key, fun.(value)}|delete(keywords, key)]
  end

  def update([{_, _} = e|keywords], key, initial, fun) do
    [e|update(keywords, key, initial, fun)]
  end

  def update([], key, initial, _fun) when is_atom(key) do
    [{key, initial}]
  end

  @doc """
  Splits the given keywords in two given the given keys.
  Duplicated keys are preserved in the split keyword list.

  ## Examples

      iex> d = [a: 1, b: 2, c: 3, d: 4]
      iex> Keyword.split(d, [:a, :c, :e])
      { [a: 1, c: 3], [b: 2, d: 4] }

      iex> d = [a: 1, b: 2, c: 3, d: 4, a: 5]
      iex> Keyword.split(d, [:a, :c, :e])
      { [a: 1, c: 3, a: 5], [b: 2, d: 4] }

  """
  def split(keywords, keys) when is_list(keywords) do
    acc = { [], [] }

    { take, drop } = Enum.reduce keywords, acc, fn({ k, v }, { take, drop }) ->
      case k in keys do
        true  -> { [{k, v}|take], drop }
        false -> { take, [{k, v}|drop] }
      end
    end

    { Enum.reverse(take), Enum.reverse(drop) }
  end

  @doc """
  Takes the given keys from the dict.
  Duplicated keys are preserved in the new keyword list.

  ## Examples

      iex> d = [a: 1, b: 2, c: 3, d: 4]
      iex> Keyword.take(d, [:a, :c, :e])
      [a: 1, c: 3]

      iex> d = [a: 1, b: 2, c: 3, d: 4, a: 5]
      iex> Keyword.take(d, [:a, :c, :e])
      [a: 1, c: 3, a: 5]

  """
  def take(keywords, keys) when is_list(keywords) do
    for { k, _ } = tuple <- keywords, k in keys, do: tuple
  end

  @doc """
  Drops the given keys from the dict.
  Duplicated keys are preserved in the new keyword list.

  ## Examples

      iex> d = [a: 1, b: 2, c: 3, d: 4]
      iex> Keyword.drop(d, [:b, :d])
      [a: 1, c: 3]

      iex> d = [a: 1, b: 2, c: 3, d: 4, a: 5]
      iex> Keyword.drop(d, [:b, :d])
      [a: 1, c: 3, a: 5]

  """
  def drop(keywords, keys) when is_list(keywords) do
    for { k, _ } = tuple <- keywords, not k in keys, do: tuple
  end

  @doc """
  Returns the first value associated with `key` in the keyword
  list as well as the keyword list without `key`.

  All duplicated entries are removed. See `pop_first/3` for
  removing only the first entry.

  ## Examples

      iex> Keyword.pop [a: 1], :a
      {1,[]}

      iex> Keyword.pop [a: 1], :b
      {nil,[a: 1]}

      iex> Keyword.pop [a: 1], :b, 3
      {3,[a: 1]}

      iex> Keyword.pop [a: 1], :b, 3
      {3,[a: 1]}

      iex> Keyword.pop [a: 1, a: 2], :a
      {1,[]}

  """
  def pop(keywords, key, default \\ nil) when is_list(keywords) do
    { get(keywords, key, default), delete(keywords, key) }
  end

  @doc """
  Returns the first value associated with `key` in the keyword
  list as well as the keyword list without that particular ocurrence
  of `key`.

  Duplicated entries are not removed.

  ## Examples

      iex> Keyword.pop_first [a: 1], :a
      {1,[]}

      iex> Keyword.pop_first [a: 1], :b
      {nil,[a: 1]}

      iex> Keyword.pop_first [a: 1], :b, 3
      {3,[a: 1]}

      iex> Keyword.pop_first [a: 1], :b, 3
      {3,[a: 1]}

      iex> Keyword.pop_first [a: 1, a: 2], :a
      {1,[a: 2]}

  """
  def pop_first(keywords, key, default \\ nil) when is_list(keywords) do
    { get(keywords, key, default), delete_first(keywords, key) }
  end
end
