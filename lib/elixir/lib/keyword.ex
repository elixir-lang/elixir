defmodule Keyword do
  @moduledoc """
  A keyword is a list of tuples where the first element
  of the tuple is an atom and the second element can be
  any value.

  A keyword may have duplicated keys, so it is not strictly
  a dictionary. However most of the functions in this module
  allows it to behave exactly as a dictionary. For example,
  `Keyword.get` will get the first entry matching the given
  key, regardless if duplicated entries exist. Similarly,
  `Keyword.put` and `Keyword.delete` ensure all duplicated
  entries for a given key are removed when invoked.
  """

  @type key :: atom
  @type value :: any
  @type t :: [{key, value}]

  @doc """
  Creates a Keyword from an enum. Unlike `Keyword.new`
  which behaves as a dict, `Keyword.from_enum` does not remove
  duplicated entries.
  """
  @spec from_enum(Enum.t) :: t
  def from_enum(enum) when is_list(enum) do
    enum
  end

  def from_enum(enum) do
    Enum.map(enum, fn(x) -> x end)
  end

  @doc """
  Checks if the given argument is a keywords list or not
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
  Creates a Keyword from an enumerable. Similar to dicts,
  duplicated entries are removed, the latest one prevails.

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
  Creates a Keyword from an enumerable with the
  help of the transformation function. Duplicated
  entries are removed, the latest one prevails.

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
  def get(keywords, key, default // nil) when is_atom(key) do
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
  def fetch(keywords, key) when is_atom(key) do
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
      ** (KeyError) key not found: :b

  """
  @spec fetch!(t, key) :: value | no_return
  def fetch!(keywords, key) when is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      { ^key, value } -> value
      false -> raise(KeyError, key: key)
    end
  end

  @doc """
  Gets all values for a specific `key`.

  ## Examples

      iex> Keyword.get_values([a: 1, a: 2], :a)
      [1,2]

  """
  @spec get_values(t, key) :: [value]
  def get_values(keywords, key) when is_atom(key) do
    lc { k, v } inlist keywords, key == k, do: v
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
  def keys(keywords) do
    lc { key, _ } inlist keywords, do: key
  end

  @doc """
  Returns all values from the keyword list.

  ## Examples

      iex> Keyword.values([a: 1, b: 2])
      [1,2]

  """
  @spec values(t) :: [value]
  def values(keywords) do
    lc { _, value } inlist keywords, do: value
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
  def delete(keywords, key) when is_atom(key) do
    lc { k, _ } = tuple inlist keywords, key != k, do: tuple
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
  def delete_first(keywords, key) when is_atom(key) do
    :lists.keydelete(key, 1, keywords)
  end

  @doc """
  Puts the given `value` under `key`.

  If a previous value is already stored, all entries are
  removed and the value is overriden.

  ## Examples

      iex> Keyword.put([a: 1, b: 2], :a, 3)
      [a: 3, b: 2]

      iex> Keyword.put([a: 1, b: 2, a: 4], :a, 3)
      [a: 3, b: 2]

  """
  @spec put(t, key, value) :: t
  def put(keywords, key, value) when is_atom(key) do
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
  def put_new(keywords, key, value) when is_atom(key) do
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
  def equal?(left, right) do
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
  def merge(d1, d2) do
    d2 ++ lc({ k, _ } = tuple inlist d1, not has_key?(d2, k), do: tuple)
  end

  @doc """
  Merges two keyword lists into one. If they have duplicated
  entries, the given function is invoked to solve conflicts.

  ## Examples

      iex> Keyword.merge([a: 1, b: 2], [a: 3, d: 4], fn (_k, v1, v2) ->
      ...>  v1 + v2
      iex> end)
      [a: 4, b: 2, d: 4]

  """
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(d1, d2, fun) do
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
  def has_key?(keywords, key) when is_atom(key) do
    :lists.keymember(key, 1, keywords)
  end

  @doc false
  def update(dict, key, fun) when is_function(fun, 1) do
    IO.write "Keyword.update/3 is deprecated, please use Keyword.update!/3 instead\n#{Exception.format_stacktrace}"
    update!(dict, key, fun)
  end

  @doc """
  Updates the `key` with the given function. If the `key` does
  not exist, raises `KeyError`.

  ## Examples

      iex> Keyword.update!([a: 1], :a, &1 * 2)
      [a: 2]

      iex> Keyword.update!([a: 1], :b, &1 * 2)
      ** (KeyError) key not found: :b

  """
  @spec update!(t, key, (value -> value)) :: t | no_return
  def update!([{key, value}|keywords], key, fun) do
    [{key, fun.(value)}|delete(keywords, key)]
  end

  def update!([{_, _} = e|keywords], key, fun) do
    [e|update!(keywords, key, fun)]
  end

  def update!([], key, _fun) when is_atom(key) do
    raise(KeyError, key: key)
  end

  @doc """
  Updates the `key` with the given function. If the `key` does
  not exist, inserts the given `initial` value.

  ## Examples

      iex> Keyword.update([a: 1], :a, 13, &1 * 2)
      [a: 2]

      iex> Keyword.update([a: 1], :b, 11, &1 * 2)
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
end
