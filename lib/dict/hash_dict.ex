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

  def put(dict, key, value) do
    dict.update_data(:dict.store key, value, &1)
  end

  def put(dict, {key, value}) do
    dict.update_data(:dict.store key, value, &1)
  end

  def delete(dict, key) do
    dict.update_data(:dict.erase key, &1)
  end

  def merge(d1, d2) do
    d1.update_data(:dict.merge fn(_k, _v1, v2) -> v2 end, &1, d2.data)
  end

  def merge(d1, d2, fun) do
    d1.update_data(:dict.merge fun, &1, d2.data)
  end

  def update(dict, key, fun) do
    dict.update_data(:dict.update key, fun, &1)
  end

  def update(dict, key, initial, fun) do
    dict.update_data(:dict.update key, fun, initial, &1)
  end

  def empty(_) do
    HD[data: :dict.new]
  end

  def to_list(HD[data: data]) do
    :dict.to_list data
  end
end

defmodule HashDict do
  use Dict.Common, Dict.HashDict.Record
end
