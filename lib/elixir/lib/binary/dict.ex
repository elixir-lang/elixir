defmodule Binary.Dict do
  @moduledoc """
  This module implements a dictionary that forces the keys to be
  converted to binaries on insertion. Currently it is implemented
  using an `OrdDict`, but this may change in the future.

  Check the `Dict` module for examples and documentation.
  """

  use Dict.Common

  defmacrop dict(data) do
    quote do
      { Binary.Dict, unquote(data) }
    end
  end

  def keys(dict(data)) do
    lc { k, _ } inlist data, do: k
  end

  def values(dict(data)) do
    lc { _, v } inlist data, do: v
  end

  def size(dict(data)) do
    length(data)
  end

  def has_key?(dict(data), key) do
    :orddict.is_key to_binary(key), data
  end

  def get(dict(data), key, default) do
    case :orddict.find(to_binary(key), data) do
      {:ok, value} -> value
      :error       -> default
    end
  end

  def get!(dict(data), key) do
    case :orddict.find(to_binary(key), data) do
      {:ok, value} -> value
      :error       -> raise(KeyError, key: key)
    end
  end

  def put(dict(data), key, value) do
    dict(:orddict.store to_binary(key), value, data)
  end

  def delete(dict(data), key) do
    dict(:orddict.erase to_binary(key), data)
  end

  def merge(dict(d1), dict(d2), fun) do
    dict(:orddict.merge fun, d1, d2)
  end

  def merge(dict(_) = d1, d2, fun) do
    merge(d1, new(d2), fun)
  end

  def update(dict(data), key, fun) do
    dict(:orddict.update to_binary(key), fun, data)
  end

  def update(dict(data), key, initial, fun) do
    dict(:orddict.update to_binary(key), fun, initial, data)
  end

  def empty(_) do
    dict([])
  end

  def to_list(dict(data)) do
    data
  end
end

defimpl Enum.Iterator, for: Binary.Dict do
  def iterator({ Binary.Dict, data }), do: data
  def count({ Binary.Dict, data }),    do: length(data)
end

defimpl Access, for: Binary.Dict do
  def access(dict, key), do: Binary.Dict.get(dict, key, nil)
end