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

  def put(dict, key, value) do
    dict.update_data(:orddict.store key, value, &1)
  end

  def put(dict, {key, value}) do
    dict.update_data(:orddict.store key, value, &1)
  end

  def delete(dict, key) do
    dict.update_data(:orddict.erase key, &1)
  end

  def merge(d1, d2) do
    d1.update_data(:orddict.merge fn(_k, _v1, v2) -> v2 end, &1, d2.data)
  end

  def merge(d1, d2, fun) do
    d1.update_data(:orddict.merge fun, &1, d2.data)
  end

  def update(dict, key, fun) do
    dict.update_data(:orddict.update key, fun, &1)
  end

  def update(dict, key, initial, fun) do
    dict.update_data(:orddict.update key, fun, initial, &1)
  end

  def empty(_) do
    O[data: []]
  end

  def to_list(O[data: data]) do
    data
  end
end

defmodule Orddict do
  use Dict.Common, Dict.Orddict.Record
end