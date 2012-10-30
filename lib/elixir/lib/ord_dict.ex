defmodule OrdDict do
  @moduledoc """
  This module implements a dictionary type that stores items
  as a list of tuples. It is a simple wrapper around
  [Erlang's orddict module](http://www.erlang.org/doc/man/orddict.html)
  and exposed via the `Dict` module.

  Check the `Dict` module for examples and documentation.
  """

  use Dict.Common

  defmacrop dict(data) do
    quote do
      { OrdDict, unquote(data) }
    end
  end

  @doc false
  def keys(dict(data)) do
    lc { k, _ } inlist data, do: k
  end

  @doc false
  def values(dict(data)) do
    lc { _, v } inlist data, do: v
  end

  @doc false
  def size(dict(data)) do
    length(data)
  end

  @doc false
  def has_key?(dict(data), key) do
    :orddict.is_key key, data
  end

  @doc false
  def get(dict(data), key, default) do
    case :orddict.find(key, data) do
      {:ok, value} -> value
      :error       -> default
    end
  end

  @doc false
  def get!(dict(data), key) do
    case :orddict.find(key, data) do
      {:ok, value} -> value
      :error       -> raise(KeyError, key: key)
    end
  end

  @doc false
  def put(dict(data), key, value) do
    dict(:orddict.store key, value, data)
  end

  @doc false
  def delete(dict(data), key) do
    dict(:orddict.erase key, data)
  end

  @doc false
  def merge(dict(d1), dict(d2), fun) do
    dict(:orddict.merge fun, d1, d2)
  end

  @doc false
  def merge(dict(_) = d1, d2, fun) do
    merge(d1, new(d2), fun)
  end

  @doc false
  def update(dict(data), key, fun) do
    dict(:orddict.update key, fun, data)
  end

  @doc false
  def update(dict(data), key, initial, fun) do
    dict(:orddict.update key, fun, initial, data)
  end

  @doc false
  def empty(_) do
    dict([])
  end

  @doc false
  def to_list(dict(data)) do
    data
  end
end

defimpl Enum.Iterator, for: OrdDict do
  def iterator({ OrdDict, data }), do: data
  def count({ OrdDict, data }),    do: length(data)
end

defimpl Access, for: OrdDict do
  def access(dict, key), do: OrdDict.get(dict, key, nil)
end
