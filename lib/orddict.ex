defrecord Orddict.Record, data: nil

defimpl Dict, for: Orddict.Record do
  refer Orddict.Record, as: O

  def keys(O[data: data]) do
    lc { k, _ } in data, do: k
  end

  def values(O[data: data]) do
    lc { _, v } in data, do: v
  end

  def size(O[data: data]) do
    length(data)
  end

  def has_key?(O[data: data], key) do
    :orddict.is_key key, data
  end

  def get(O[data: data], key, default // nil) do
    case :orddict.find(key, data) do
    match: {:ok, value}
      value
    match: :error
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
    O[data: :orddict.merge fn(_k, _v1, v2) -> v2 end, d1, d2]
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

defmodule Orddict do
  @moduledoc """
  This module implements a dictionary based that stores items
  as a list of tuples. It is a simple wrapper around
  [Erlang's orddict module](http://www.erlang.org/doc/man/orddict.html)
  and exposed via the `Dict` protocol.
  """
  use Dict.Common, Dict.Orddict.Record
end