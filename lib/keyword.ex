defmodule Keyword do
  @moduledoc """
  A keyword is a list of tuples where the first element
  of the tuple is an atom and the second element can be
  any value. The list is sorted by the first element of
  each tuple.

  A keyword may have duplicated keys, so it is not strictly
  a keywordsionary. However most of the functions in this module
  allows it to behave exactly as a keywordsionary. For example,
  `Keyword.get` will get the first entry matching the given
  key, regardless if duplicated entries exist.  Similarly,
  `Keyword.put` and `Keyword.delete` ensure all duplicated
  entries for a given key are removed when invoked.

  This module uses `==` as operator to check if two keys
  are equal or not.
  """

  @doc """
  Creates a Keyword from an enumerable. Duplicated
  entries are removed, the latest one prevails.

  ## Examples

      Keyword.from_enum [{b,1},{a,2}]
      #=> [a: 2, b: 1]

  """
  def from_enum(pairs) do
    Enum.reduce pairs, [], fn({k, v}, keywords) ->
      put(keywords, k, v)
    end
  end

  @doc """
  Creates a Keyword from an enumerable with the
  help of the transformation function. Duplicated
  entries are removed, the latest one prevails.

  ## Examples

      Keyword.from_enum [:a, :b], fn(x) -> {x,x} end
      #=> [a: :a, b: :b]
  """
  def from_enum(pairs, transform) do
    Enum.reduce pairs, [], fn(i, keywords) ->
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
  Returns all keys from the keywords list. Duplicated
  keys appear duplicated in the final list of keys.

  ## Examples

      Keyword.keys [a: 1, b: 2] #=> [:a,:b]

  """
  def keys(keywords) do
    lc { key, _ } in keywords, do: key
  end

  @doc """
  Returns all values.

  ## Examples

      Keyword.values [a: 1, b: 2] #=> [1,2]
  """
  def values(keywords) do
    lc { _, value } in keywords, do: value
  end

  @doc """
  Deletes all entries in the keywords list for a specific key.
  If the key does not exist, returns the keywords list unchanged.
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

  Use `prepend/3` to add a new value for an existing key
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
  Merges two keywords lists into one. If they have duplicated
  entries, the one given as second argument wins.

  ## Examples

      Keyword.merge [a: 1, b: 2], [a: 3, d: 4]
      #=> [a:3, b:2, d: 4]
  """
  def merge(d1, d2) do
    merge(d1, d2, fn(_k, _v1, v2) -> v2 end)
  end

  @doc """
  Merges two keywords lists into one. If they have duplicated
  entries, the given function is invoked to solve conflicts.

  ## Examples

      Keyword.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
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

  @doc """
  Returns whether a given key exists in the given keywords.

  ### Examples
      Keyword.key?([a:, 1], :a)
      #=> true
      Keyword.key?([a:, 1], :b)
      #=> false
  """
  def key?([{k, _}|_], key) when key < k, do: false
  def key?([{k, _}|d], key) when key > k, do: key?(d, key)
  def key?([{_, _}|_], _key),             do: true
  def key?([], _),                        do: false
end
