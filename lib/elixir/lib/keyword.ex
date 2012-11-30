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
  Creates a Keyword from enum. Differently from `Keyword.new`
  which behaves as a dict, `Keyword.from_enum` do not remove
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
    case atom_to_list(key) do
      'Elixir-' ++ _ -> false
      _ -> keyword?(rest)
    end
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
  Creates a Keyword from an enumerable. Similarly to dicts,
  duplicated entries are removed, the latest one prevails.

  ## Examples

      Keyword.new [{:b,1},{:a,2}]
      #=> [a: 2, b: 1]

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

      Keyword.new [:a, :b], fn x -> {x,x} end
      #=> [a: :a, b: :b]

  """
  @spec new(Enum.t, ({key, value} -> {key, value})) :: t
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
  @spec get(t, key) :: value
  @spec get(t, key, value) :: value
  def get(keywords, key, default // nil) when is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      { ^key, value } -> value
      false -> default
    end
  end

  @doc """
  Gets the value for specific key. If key does not exist,
  an error is raised.

  ## Examples

      Keyword.get! [a: 1], :a      #=> 1
      Keyword.get! [a: 1], :b      #=> raises KeyError[key: :b]

  """
  @spec get!(t, key) :: value | no_return
  def get!(keywords, key) when is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      { ^key, value } -> value
      false -> raise(KeyError, key: key)
    end
  end  

  @doc """
  Gets all values for a specific key.

  ## Examples

      Keyword.get_values [a: 1, a: 2], :a
      #=> [1,2]

  """
  @spec get_values(t, key) :: [value]
  def get_values(keywords, key) when is_atom(key) do
    lc { k, v } inlist keywords, key == k, do: v
  end

  @doc """
  Returns all keys from the keyword list. Duplicated
  keys appear duplicated in the final list of keys.

  ## Examples

      Keyword.keys [a: 1, b: 2] #=> [:a,:b]

  """
  @spec keys(t) :: [key]
  def keys(keywords) do
    lc { key, _ } inlist keywords, do: key
  end

  @doc """
  Returns all values.

  ## Examples

      Keyword.values [a: 1, b: 2] #=> [1,2]

  """
  @spec values(t) :: [value]
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
  @spec delete(t, key) :: t
  def delete(keywords, key) when is_atom(key) do
    lc { k, _ } = tuple inlist keywords, key != k, do: tuple
  end

  @doc """
  Sets the given `value` under `key`.

  If a previous value is already stored, all entries are
  removed and the value is overriden.

  ## Examples

      Keyword.put [a: 1, b: 2], :a, 3
      #=> [a: 3, b: 2]

  """
  @spec put(t, key, value) :: t
  def put(keywords, key, value) when is_atom(key) do
    [{key, value}|delete(keywords, key)]
  end

  @doc """
  Checks if two keywords are equal. I.e. they contain
  the same keys and those keys contain the same values.

  ## Examples

      Keyword.equal? [a: 1, b: 2], [b: 2, a: 1]
      #=> true

  """
  @spec equal?(t, t) :: boolean
  def equal?(left, right) do
    :lists.sort(left) == :lists.sort(right)
  end

  @doc """
  Merges two keyword lists into one. If they have duplicated
  entries, the one given as second argument wins.

  ## Examples

      Keyword.merge [a: 1, b: 2], [a: 3, d: 4]
      #=> [a:3, b:2, d: 4]

  """
  @spec merge(t, t) :: t
  def merge(d1, d2) do
    d2 ++ lc({ k, _ } = tuple inlist d1, not has_key?(d2, k), do: tuple)
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
  Returns whether a given key exists in the given keywords.

  ## Examples

      Keyword.has_key?([a: 1], :a)
      #=> true
      Keyword.has_key?([a: 1], :b)
      #=> false

  """
  @spec has_key?(t, key) :: boolean
  def has_key?(keywords, key) when is_atom(key) do
    :lists.keymember(key, 1, keywords)
  end

  @doc """
  Updates the key with the given function. If the key does
  not exist, raises `KeyError`.

  ## Examples

      Keyword.update([a: 1], :a, &1 * 2)
      #=> [a: 2]
      Keyword.update([a: 1], :b, &1 * 2)
      #=> KeyError

  """
  @spec update(t, key, (value -> value)) :: t | no_return
  def update([{key, value}|keywords], key, fun) do
    [{key, fun.(value)}|delete(keywords, key)]
  end

  def update([{_, _} = e|keywords], key, fun) do
    [e|update(keywords, key, fun)]
  end

  def update([], key, _fun) when is_atom(key) do
    raise(KeyError, key: key)
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
