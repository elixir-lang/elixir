defmodule Orddict do

  def from_list(pairs) do
    List.foldl pairs, [], fn({k, v}, dict){ store(dict, k, v) }
  end

  # Fetches value from the dictionary for specific key.
  # If key not exist return default value
  def fetch([{k, _}|_], key, default) when key < k, do: default
  def fetch([{k, _}|d], key, default) when key > k, do: fetch(d, key, default)
  def fetch([{_k, value}|_], _key, _default),    do: value
  def fetch([], _, default),                    do: default

  # Returns all keys of dictionary
  def keys([{k, _}|t]), do: [k|keys(t)]
  def keys([]),        do: []

  # Returns all values of dictionary
  def values([{_, v}|t]), do: [v|values(t)]
  def values([]),        do: []

  # Deletes key, value entry from dictionary for specific key
  def delete([{k, _} = e|dict], key) when key < k, do: [e|dict]
  def delete([{k, _} = e|dict], key) when key > k, do: [e|delete(dict, key)]
  def delete([{_k, _v}|dict], _key), do: dict
  def delete([], _), do: []

  # Stores key, value entry in dictionary
  def store([{k, _} = e|dict], key, value) when key < k, do: [{key, value},e|dict]
  def store([{k, _} = e|dict], key, value) when key > k, do: [e|store(dict, key, value)]
  def store([{_, _}|dict], key, value), do: [{key, value}|dict]
  def store([], key, value), do: [{key, value}]

  # Merges two dictionaries in one
  def merge([{k1, _} = e1|d1], [{k2, _} = e2|d2]) when k1 < k2, do: [e1|merge(d1, [e2|d2])];
  def merge([{k1, _} = e1|d1], [{k2, _} = e2|d2]) when k1 > k2, do: [e2|merge([e1|d1], d2)];
  def merge([{k1, _v1}|d1], [{_k2, v2}|d2]), do: [{k1, v2}|merge(d1, d2)];
  def merge([], d2), do: d2
  def merge(d1, []), do: d1
end