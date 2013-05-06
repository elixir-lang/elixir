defmodule List.Dict do
  @moduledoc """
  A Dict implementation that works on lists of two-items tuples.

  For more information about the functions and their APIs, please
  consult the `Dict` module.
  """

  @doc """
  Returns a new `List.Dict`, i.e. an empty list.
  """
  def new, do: []

  @doc """
  Creates a new `List.Dict` from the given pairs.
  """
  def new(pairs) do
    Enum.map pairs, fn({ k, v }) -> { k, v } end
  end

  @doc """
  Creates a new `List.Dict` from the given pairs
  via the given transformation function.
  """
  def new(list, transform) when is_function(transform) do
    Enum.map list, transform
  end

  @doc """
  Returns all keys in the dict.
  """
  def keys(dict) do
    lc { key, _ } inlist dict, do: key
  end

  @doc """
  Returns all values in the dict.
  """
  def values(dict) do
    lc { _, value } inlist dict, do: value
  end

  @doc """
  Returns the dict size.
  """
  def size(dict) do
    length(dict)
  end

  @doc """
  Returns true if the dict has the given key.
  """
  def has_key?(dict, key) do
    :lists.keymember(key, 1, dict)
  end

  @doc """
  Returns the value under key from the given
  dict or default if no key is set.
  """
  def get(dict, key, default // nil) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, value } -> value
      false -> default
    end
  end

  @doc """
  Returns the value under the given key
  raises `KeyError` if the key does not exist.
  """
  def get!(dict, key) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, value } -> value
      false -> raise(KeyError, key: key)
    end
  end

  @doc """
  Returns the values taken out from dict as well
  as a dict without keys which were taken out.
  Keys to take out are specified as another dict
  which contains key default_value pairs.
  """
  def pop(dict, defaults) do
    Enum.reduce defaults, {new, dict},
       fn({ key, default }, {acc, dict}) ->
         {put(acc, key, get(dict, key, default)), delete(dict, key)}
       end
  end

  @doc """
  Returns the value under the given key
  from the dict as well as the dict without that key.
  """
  def pop_value(dict, key, default // nil) do
    {get(dict, key, default), delete(dict, key)}
  end

  @doc """
  Puts the given key-value pair in the dict.
  """
  def put(dict, key, val) do
    [{key, val}|delete(dict, key)]
  end

  @doc """
  Puts the given key-value pair in the dict
  if no entry exists yet.
  """
  def put_new(dict, key, val) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, _ } -> dict
      false -> [{key,val}|dict]
    end
  end

  @doc """
  Deletes the entry under the given key from the dict.
  """
  def delete(dict, key) do
    lc { k, _ } = tuple inlist dict, key != k, do: tuple
  end

  @doc """
  Merges the given Enumerable into the dict.
  """
  def merge(dict, enum, callback // fn(_k, _v1, v2) -> v2 end)

  def merge(dict1, dict2, fun) do
    Enum.reduce dict2, dict1, fn { k, v2 }, acc ->
      update(acc, k, v2, fn(v1) -> fun.(k, v1, v2) end)
    end
  end

  @doc """
  Updates the key in the dict according to the given function.
  """
  def update([{key, value}|dict], key, fun) do
    [{key, fun.(value)}|delete(dict, key)]
  end

  def update([{_, _} = e|dict], key, fun) do
    [e|update(dict, key, fun)]
  end

  def update([], key, _fun) do
    raise(KeyError, key: key)
  end

  @doc """
  Updates the key in the dict according to the given function
  or uses the given initial value if no entry exists.
  """
  def update([{key, value}|dict], key, _initial, fun) do
    [{key, fun.(value)}|delete(dict, key)]
  end

  def update([{_, _} = e|dict], key, initial, fun) do
    [e|update(dict, key, initial, fun)]
  end

  def update([], key, initial, _fun) do
    [{key, initial}]
  end

  @doc """
  Returns an empty `List.Dict`.
  """
  def empty(_dict), do: []

  @doc """
  Check if the List.Dict is equal to another List.Dict.
  """
  def equal?(dict, other) do
    :lists.keysort(1, dict) == :lists.keysort(1, other)
  end

  @doc """
  Converts the dict to a list.
  """
  def to_list(dict), do: dict
end
