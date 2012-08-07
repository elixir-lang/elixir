defmodule Orddict do
  @moduledoc """
  This module implements a dictionary based that stores items
  as a list of tuples. It is a simple wrapper around
  [Erlang's orddict module](http://www.erlang.org/doc/man/orddict.html)
  and exposed via the `Dict` module.
  """

  use Dict.Common

  defmacrop dict(data) do
    quote do
      { Orddict, unquote(data) }
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
    :orddict.is_key key, data
  end

  def get(dict(data), key, default // nil) do
    case :orddict.find(key, data) do
      {:ok, value} ->
        value
      :error ->
        default
    end
  end

  def put(dict(data), key, value) do
    dict(:orddict.store key, value, data)
  end

  def delete(dict(data), key) do
    dict(:orddict.erase key, data)
  end

  def merge(dict(d1), dict(d2)) do
    dict(:orddict.merge fn _k, _v1, v2 -> v2 end, d1, d2)
  end

  def merge(dict(d1), dict(d2), fun) do
    dict(:orddict.merge fun, d1, d2)
  end

  def update(dict(data), key, fun) do
    dict(:orddict.update key, fun, data)
  end

  def update(dict(data), key, initial, fun) do
    dict(:orddict.update key, fun, initial, data)
  end

  def empty(_) do
    dict([])
  end

  def to_list(dict(data)) do
    data
  end
end

defimpl Enum.Iterator, for: Orddict do
  def iterator({ Orddict, data }), do: data
  def count({ Orddict, data }),    do: length(data)
end

defimpl Enum.OrdIterator, for: Orddict do
  def iterator({ Orddict, data }), do: data
  def to_list({ h, next }, _),     do: [h|next]
end

defimpl Access, for: Orddict do
  def access(dict, key), do: Orddict.get(dict, key, nil)
end