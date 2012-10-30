defmodule HashDict do
  @moduledoc """
  This module implements a dictionary type based on
  hashing of the keys. It is a simple wrapper around
  [Erlang's dict module](http://www.erlang.org/doc/man/dict.html)
  and exposed via the `Dict` module.

  Check the `Dict` module for examples and documentation.
  """

  use Dict.Common

  defmacrop dict(data) do
    quote do
      { HashDict, unquote(data) }
    end
  end

  @doc false
  def keys(dict(data)) do
    :dict.fetch_keys data
  end

  @doc false
  def values(dict(data)) do
    :dict.fold fn _key, value, acc ->
      [value|acc]
    end, [], data
  end

  @doc false
  def size(dict(data)) do
    :dict.size data
  end

  @doc false
  def has_key?(dict(data), key) do
    :dict.is_key key, data
  end

  @doc false
  def get(dict(data), key, default) do
    case :dict.find(key, data) do
      {:ok, value} -> value
      :error       -> default
    end
  end

  @doc false
  def get!(dict(data), key) do
    case :dict.find(key, data) do
      {:ok, value} -> value
      :error       -> raise(KeyError, key: key)
    end
  end

  @doc false
  def put(dict(data), key, value) do
    dict(:dict.store key, value, data)
  end

  @doc false
  def delete(dict(data), key) do
    dict(:dict.erase key, data)
  end

  @doc false
  def merge(dict(d1), dict(d2), fun) do
    dict(:dict.merge fun, d1, d2)
  end

  @doc false
  def merge(dict(_) = d1, d2, fun) do
    merge(d1, new(d2), fun)
  end

  @doc false
  def update(dict(data), key, fun) do
    dict(:dict.update key, fun, data)
  end

  @doc false
  def update(dict(data), key, initial, fun) do
    dict(:dict.update key, fun, initial, data)
  end

  @doc false
  def empty(_) do
    dict(:dict.new)
  end

  @doc false
  def to_list(dict(data)) do
    :dict.to_list data
  end
end

defimpl Enum.Iterator, for: HashDict do
  def iterator(dict), do: HashDict.to_list(dict)
  def count(dict),    do: HashDict.size(dict)
end

defimpl Access, for: HashDict do
  def access(dict, key), do: HashDict.get(dict, key, nil)
end