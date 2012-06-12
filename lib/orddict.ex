defrecord Orddict.Record, data: nil

alias Orddict.Record, as: O

defimpl Dict, for: Orddict.Record do
  def keys(O[data: data]) do
    lc { k, _ } inlist data, do: k
  end

  def values(O[data: data]) do
    lc { _, v } inlist data, do: v
  end

  def size(O[data: data]) do
    length(data)
  end

  def has_key?(O[data: data], key) do
    :orddict.is_key key, data
  end

  def get(O[data: data], key, default // nil) do
    case :orddict.find(key, data) do
      {:ok, value} ->
        value
      :error ->
        default
    end
  end

  def put(O[data: data], key, value) do
    O[data: :orddict.store key, value, data]
  end

  def delete(O[data: data], key) do
    O[data: :orddict.erase key, data]
  end

  def merge(O[data: d1], O[data: d2]) do
    O[data: :orddict.merge fn _k, _v1, v2 -> v2 end, d1, d2]
  end

  def merge(O[data: d1], O[data: d2], fun) do
    O[data: :orddict.merge fun, d1, d2]
  end

  def update(O[data: data], key, fun) do
    O[data: :orddict.update key, fun, data]
  end

  def update(O[data: data], key, initial, fun) do
    O[data: :orddict.update key, fun, initial, data]
  end

  def empty(_) do
    O[data: []]
  end

  def to_list(O[data: data]) do
    data
  end
end

defimpl Enum.Iterator, for: Orddict.Record do
  def iterator(O[data: data]), do: data
  def count(O[data: data]),    do: length(data)
end

defimpl Enum.OrdIterator, for: Orddict.Record do
  def iterator(O[data: data]), do: data
  def to_list(h, next), do: [h|next]
end

defmodule Orddict do
  @moduledoc """
  This module implements a dictionary based that stores items
  as a list of tuples. It is a simple wrapper around
  [Erlang's orddict module](http://www.erlang.org/doc/man/orddict.html)
  and exposed via the `Dict` protocol.
  """
  use Dict.Common, Dict.Orddict.Record
end