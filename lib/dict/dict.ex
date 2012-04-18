defrecord HashDict.Record, data: nil

defimpl Dict, for: HashDict.Record do
  def keys(dict) do
    :dict.fetch_keys dict.data
  end

  def values(dict) do
    :dict.fold fn(_key, value, acc) ->
      [value|acc]
    end, [], dict.data
  end

  def size(dict) do
    :dict.size dict.data
  end

  def has_key?(dict, key) do
    :dict.is_key key, dict.data
  end

  def get(dict, key, default // nil) do
    case :dict.find(key, dict.data) do
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
    HashDict.Record.new(data: :dict.new)
  end

  def to_list(dict) do
    :dict.to_list dict.data
  end
end

defmodule HashDict do
  use Dict.Common, :"HashDict.Record"
end

defimpl Enum.Iterator, for: HashDict.Record do
  import Enum.Iterator.List, only: [iterate: 1]

  def iterator(dict) do
    { iterate(&1), iterate({ to_list(dict), fn(pairs, do: extend(Dict.HashDict.Record.empty(dict), pairs)) }) }
  end

  def ordered_iterator(_) do
    raise ArgumentError, message: "Dict does not support ordering"
  end

  def to_list(dict), do: Dict.HashDict.Record.to_list(dict)

  defp extend(dict, pairs) do
    List.foldl pairs, dict, fn(pair, dict) ->
      Dict.HashDict.Record.put dict, pair
    end
  end
end
