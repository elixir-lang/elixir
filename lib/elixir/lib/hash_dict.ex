defimpl Dict, for: HashDict do
  defmacrop dict(data) do
    quote do
      { HashDict, unquote(data) }
    end
  end

  def keys(dict(data)) do
    :dict.fetch_keys data
  end

  def values(dict(data)) do
    :dict.fold fn _key, value, acc ->
      [value|acc]
    end, [], data
  end

  def size(dict(data)) do
    :dict.size data
  end

  def has_key?(dict(data), key) do
    :dict.is_key key, data
  end

  def get(dict(data), key, default // nil) do
    case :dict.find(key, data) do
      {:ok, value} ->
        value
      :error ->
        default
    end
  end

  def put(dict(data), key, value) do
    dict(:dict.store key, value, data)
  end

  def delete(dict(data), key) do
    dict(:dict.erase key, data)
  end

  def merge(dict(d1), dict(d2)) do
    dict(:dict.merge fn _k, _v1, v2 -> v2 end, d1, d2)
  end

  def merge(dict(d1), dict(d2), fun) do
    dict(:dict.merge fun, d1, d2)
  end

  def update(dict(data), key, fun) do
    dict(:dict.update key, fun, data)
  end

  def update(dict(data), key, initial, fun) do
    dict(:dict.update key, fun, initial, data)
  end

  def empty(_) do
    dict(:dict.new)
  end

  def to_list(dict(data)) do
    :dict.to_list data
  end
end

defimpl Enum.Iterator, for: HashDict do
  def iterator({ HashDict, data }), do: :dict.to_list(data)
  def count({ HashDict, data }), do: :dict.size(data)
end

defmodule HashDict do
  @moduledoc """
  This module implements a dictionary based on hashing of the keys.
  It is a simple wrapper around [Erlang's dict module](http://www.erlang.org/doc/man/dict.html)
  and exposed via the `Dict` protocol.
  """

  use Dict.Common, Dict.HashDict
end
