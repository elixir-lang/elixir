defrecord HashDict.Record, data: nil

defimpl Dict, for: HashDict.Record do
  refer HashDict.Record, as: HD

  def keys(HD[data: data]) do
    :dict.fetch_keys data
  end

  def values(HD[data: data]) do
    :dict.fold fn(_key, value, acc) ->
      [value|acc]
    end, [], data
  end

  def size(HD[data: data]) do
    :dict.size data
  end

  def has_key?(HD[data: data], key) do
    :dict.is_key key, data
  end

  def get(HD[data: data], key, default // nil) do
    case :dict.find(key, data) do
    match: {:ok, value}
      value
    match: :error
      default
    end
  end

  def put(HD[data: data], key, value) do
    HD[data: :dict.store key, value, data]
  end

  def delete(HD[data: data], key) do
    HD[data: :dict.erase key, data]
  end

  def merge(HD[data: d1], HD[data: d2]) do
    HD[data: :dict.merge fn(_k, _v1, v2) -> v2 end, d1, d2]
  end

  def merge(HD[data: d1], HD[data: d2], fun) do
    HD[data: :dict.merge fun, d1, d2]
  end

  def update(HD[data: data], key, fun) do
    HD[data: :dict.update key, fun, data]
  end

  def update(HD[data: data], key, initial, fun) do
    HD[data: :dict.update key, fun, initial, data]
  end

  def empty(_) do
    HD[data: :dict.new]
  end

  def to_list(HD[data: data]) do
    :dict.to_list data
  end
end

defmodule HashDict do
  @moduledoc """
  This module implements a dictionary based on hashing of the keys.
  It is a simple wrapper around [Erlang's dict module](http://www.erlang.org/doc/man/dict.html)
  and exposed via the `Dict` protocol.
  """

  use Dict.Common, Dict.HashDict.Record
end
