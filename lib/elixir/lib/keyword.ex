defmodule Keyword do
  @moduledoc """
  A keyword is a list of tuples where the first element
  of the tuple is an atom and the second element can be
  any value. The list is sorted by the first element of
  each tuple.

  A keyword may have duplicated keys, so it is not strictly
  a dictionary. However most of the functions in this module
  allows it to behave exactly as a dictionary. For example,
  `Keyword.get` will get the first entry matching the given
  key, regardless if duplicated entries exist. Similarly,
  `Keyword.put` and `Keyword.delete` ensure all duplicated
  entries for a given key are removed when invoked.
  """

  @doc """
  Creates a Keyword from enum. Differently from `Keyword.new`
  which behaves as a dict, `Keyword.from_enum` do not remove
  duplicated entries.
  """
  def from_enum(enum) do
    Enum.qsort(enum)
  end

  @doc """
  Returns an empty keyword list, i.e. an empty list.
  """
  def new do
    []
  end

  @doc """
  Creates a Keyword from an enumerable. Similarly to dicts,
  duplicated entries are removed, the latest one prevails.

  ## Examples

      Keyword.new [{:b,1},{:a,2}]
      #=> [a: 2, b: 1]

  """
  def new(pairs) do
    Enum.reduce pairs, [], fn {k, v}, keywords ->
      put(keywords, k, v)
    end
  end

  @doc """
  Creates a Keyword from an enumerable with the
  help of the transformation function. Duplicated
  entries are removed, the latest one prevails.

  ## Examples

      Keyword.new [:a, :b], fn x -> {x,x} end
      #=> [a: :a, b: :b]

  """
  def new(pairs, transform) do
    Enum.reduce pairs, [], fn i, keywords ->
      { k, v } = transform.(i)
      put(keywords, k, v)
    end
  end

  @doc """
  Gets the value for specific key.

  If key not exist return default value (nil if no default value)
  exists.

  If duplicated entries exist, the first one is returned.
  Use get_values/2 to retrieve all entries.

  ## Examples

      Keyword.get [a: 1], :a      #=> 1
      Keyword.get [a: 1], :b      #=> nil
      Keyword.get [a: 1], :b, 3   #=> 3

  """
  def get(keywords, key, default // nil)
  def get([{k, _}|_], key, default) when key < k, do: default
  def get([{k, _}|d], key, default) when key > k, do: get(d, key, default)
  def get([{_, value}|_], _key, _default),        do: value
  def get([], _, default),                        do: default

  @doc """
  Gets the value for specific key. If key does not exist,
  an error is raised.

  ## Examples

      Keyword.get! [a: 1], :a      #=> 1
      Keyword.get! [a: 1], :b      #=> raises KeyError[key: :b]

  """
  def get!([{k, _}|_], key) when key < k, do: raise(Keyword.KeyError, key: key)
  def get!([{k, _}|d], key) when key > k, do: get!(d, key)
  def get!([{_, value}|_], _key),         do: value
  def get!([], key),                      do: raise(Keyword.KeyError, key: key)

  @doc """
  Gets all values for a specific key.

  ## Examples

      Keyword.get_values [a: 1, a: 2], :a
      #=> [1,2]

  """
  def get_values([{k, _}|_], key) when key < k, do: []
  def get_values([{k, _}|d], key) when key > k, do: get_values(d, key)
  def get_values([{_, value}|d], key),          do: [value|get_values(d, key)]
  def get_values([], _),                        do: []

  @doc """
  Returns all keys from the keyword list. Duplicated
  keys appear duplicated in the final list of keys.

  ## Examples

      Keyword.keys [a: 1, b: 2] #=> [:a,:b]

  """
  def keys(keywords) do
    lc { key, _ } inlist keywords, do: key
  end

  @doc """
  Returns all values.

  ## Examples

      Keyword.values [a: 1, b: 2] #=> [1,2]

  """
  def values(keywords) do
    lc { _, value } inlist keywords, do: value
  end

  @doc """
  Deletes all entries in the keyword list for a specific key.
  If the key does not exist, returns the keyword list unchanged.
  Use `delete_first` to delete just the first entry in case of
  duplicated keys.

  ## Examples

      Keyword.delete [a: 1, b: 2], :a   #=> [b: 2]
      Keyword.delete [b: 2], :a         #=> [b: 2]

  """
  def delete([{k, _}|_] = keywords, key) when key < k, do: keywords
  def delete([{k, _} = e|tail], key) when key > k, do: [e|delete(tail, key)]
  def delete([{_, _}|tail], key),                  do: delete(tail, key)
  def delete([], _), do: []

  @doc """
  Sets the given `value` under `key`.

  If a previous value is already stored, all entries are
  removed and the value is overriden.

  Use `put_other/3` to add a new value for an existing key
  without removing previous ones.

  ## Examples

      Keyword.put [a: 1, b: 2], :a, 3
      #=> [a: 3, b: 2]

  """
  def put([{k, _} = e|keywords], key, value) when key < k and is_atom(key) do
    [{key, value},e|keywords]
  end

  def put([{k, _} = e|keywords], key, value) when key > k do
    [e|put(keywords, key, value)]
  end

  def put([{key, _}|keywords], key, value) when is_atom(key) do
    [{key, value}|delete(keywords, key)]
  end

  def put([], key, value) when is_atom(key) do
    [{key, value}]
  end

  @doc """
  Merges two keyword lists into one. If they have duplicated
  entries, the one given as second argument wins.

  ## Examples

      Keyword.merge [a: 1, b: 2], [a: 3, d: 4]
      #=> [a:3, b:2, d: 4]

  """
  def merge(d1, d2) do
    merge(d1, d2, fn _k, _v1, v2 -> v2 end)
  end

  @doc """
  Merges two keyword lists into one. If they have duplicated
  entries, the given function is invoked to solve conflicts.

  ## Examples

      Keyword.merge [a: 1, b: 2], [a: 3, d: 4], fn _k, v1, v2 ->
        v1 + v2
      end
      #=> [a:4, b:2, d: 4]

  """
  def merge([{k1, _} = e1|d1], [{k2, _} = e2|d2], fun) when k1 < k2 and is_atom(k1) do
    [e1|merge(d1, [e2|d2], fun)]
  end

  def merge([{k1, _} = e1|d1], [{k2, _} = e2|d2], fun) when k1 > k2 and is_atom(k2) do
    [e2|merge([e1|d1], d2, fun)]
  end

  def merge([{k1, v1}|d1], [{k1, v2}|d2], fun) do
    [{k1, fun.(k1, v1, v2)}|merge(d1, d2, fun)]
  end

  def merge([], d2, _fun), do: d2
  def merge(d1, [], _fun), do: d1

  @doc false
  def key?(list, key) do
    IO.write "[WARNING] Keyword.key? is deprecated, please use Keyword.has_key? instead\n#{Exception.formatted_stacktrace}"
    has_key?(list, key)
  end

  @doc """
  Returns whether a given key exists in the given keywords.

  ## Examples

      Keyword.has_key?([a: 1], :a)
      #=> true
      Keyword.has_key?([a: 1], :b)
      #=> false

  """
  def has_key?([{k, _}|_], key) when key < k, do: false
  def has_key?([{k, _}|d], key) when key > k, do: has_key?(d, key)
  def has_key?([{_, _}|_], _key),             do: true
  def has_key?([], _),                        do: false

  @doc """
  Updates the key with the given function. If the key does
  not exist, raises `Keyword.KeyError`.

  ## Examples

      Keyword.update([a: 1], :a, &1 * 2)
      #=> [a: 2]
      Keyword.update([a: 1], :b, &1 * 2)
      #=> Keyword.KeyError

  """
  def update([{k, _}|_], key, _fun) when key < k and is_atom(key) do
    raise(Keyword.KeyError, key: key)
  end

  def update([{k, _} = e|keywords], key, fun) when key > k do
    [e|update(keywords, key, fun)]
  end

  def update([{key, value}|keywords], key, fun) when is_atom(key) do
    [{key, fun.(value)}|delete(keywords, key)]
  end

  def update([], key, _fun) when is_atom(key) do
    raise(Keyword.KeyError, key: key)
  end

  @doc """
  Updates the key with the given function. If the key does
  not exist, inserts the given `initial` value.

  ## Examples

      Keyword.update([a: 1], :a, 13, &1 * 2)
      #=> [a: 2]
      Keyword.update([a: 1], :b, 11, &1 * 2)
      #=> [a: 1, b: 11]

  """
  def update([{k, _} = e|keywords], key, initial, _fun) when key < k and is_atom(key) do
    [{key, initial},e|keywords]
  end

  def update([{k, _} = e|keywords], key, initial, fun) when key > k do
    [e|update(keywords, key, initial, fun)]
  end

  def update([{key, value}|keywords], key, _initial, fun) when is_atom(key) do
    [{key, fun.(value)}|delete(keywords, key)]
  end

  def update([], key, initial, _fun) when is_atom(key) do
    [{key, initial}]
  end
end
