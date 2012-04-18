defrecord Orddict.Record, data: nil

defimpl Dict, for: Orddict.Record do
  def keys(dict) do
    :orddict.fetch_keys dict.data
  end

  def values(dict) do
    :orddict.fold fn(_key, value, acc) ->
      [value|acc]
    end, [], dict.data
  end

  def size(dict) do
    :orddict.size dict.data
  end

  def has_key?(dict, key) do
    :orddict.is_key key, dict.data
  end

  def get(dict, key, default // nil) do
    case :orddict.find(key, dict.data) do
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
    Orddict.Record.new(data: [])
  end

  def to_list(dict) do
    dict.data
  end
end

defmodule Orddict do
  use Dict.Common, :"Orddict.Record"
end
