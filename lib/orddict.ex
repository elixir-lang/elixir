defmodule Orddict do
  def from_list(pairs) do
    Erlang.lists.foldl fn({k, v}, dict){ store(dict, k, v) }, [], pairs
  end

  def from_enum(pairs) do
    Enum.foldl pairs, [], fn({k, v}, dict){ store(dict, k, v) }
  end

  # Gets value from the dictionary for specific key.
  # If key not exist return default value (nil if no default value)
  # exists.
  #
  # ## Examples
  #
  #     Orddict.get [a: 1], :a      #=> 1
  #     Orddict.get [a: 1], :b      #=> nil
  #     Orddict.get [a: 1], :b, 3   #=> 3
  #
  def get([{k, _}|_], key, default) when key < k, do: default
  def get([{k, _}|d], key, default) when key > k, do: get(d, key, default)
  def get([{_k, value}|_], _key, _default),       do: value
  def get([], _, default // nil),                 do: default

  # Returns all keys of dictionary.
  #
  # ## Examples
  #
  #     Orddict.keys [a: 1, b: 2] #=> [:a,:b]
  def keys(dict) do
    lc { key, _ } in dict, do: key
  end

  # Returns all values of dictionary.
  #
  # ## Examples
  #
  #     Orddict.values [a: 1, b: 2] #=> [1,2]
  def values(dict) do
    lc { _, value } in dict, do: value
  end

  # Deletes key, value entry from dictionary for specific key.
  # If the key does not exist, returns the dictionary unchanged.
  #
  # ## Examples
  #
  #     Orddict.delete [a: 1, b: 2], :a   #=> [b: 2]
  #     Orddict.delete [b: 2], :a         #=> [b: 2]
  #
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