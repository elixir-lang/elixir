defmodule Orddict do
  @doc """
  Creates an Orddict from an enumerable.
  
  ## Examples
  
      Orddict.from_enum [{b,1},{a,2}]
      #=> [a: 2, b: 1]
  """
  def from_enum(pairs) do
    Enum.reduce pairs, [], fn({k, v}, dict) ->
      put(dict, k, v)
    end
  end

  @doc """
  Creates an Orddict from an enumerable with the
  help of the transformation function.
  
  ## Examples
  
      Orddict.from_enum [:a, :b], fn(x) -> {x,x} end
      #=> [a: :a, b: :b]
  """
  def from_enum(pairs, transform) do
    Enum.reduce pairs, [], fn(i, dict) ->
      { k, v } = transform.(i)
      put(dict, k, v)
    end
  end

  @doc """
  Gets value from the dictionary for specific key.
  If key not exist return default value (nil if no default value)
  exists.
  
  ## Examples
  
      Orddict.get [a: 1], :a      #=> 1
      Orddict.get [a: 1], :b      #=> nil
      Orddict.get [a: 1], :b, 3   #=> 3
  """
  def get([{k, _}|_], key, default) when key < k, do: default
  def get([{k, _}|d], key, default) when key > k, do: get(d, key, default)
  def get([{_k, value}|_], _key, _default),       do: value
  def get([], _, default // nil),                 do: default

  @doc """
  Returns all keys of dictionary.
  
  ## Examples
  
      Orddict.keys [a: 1, b: 2] #=> [:a,:b]
  """
  def keys(dict) do
    lc { key, _ } in dict, do: key
  end

  @doc """
  Returns all values of dictionary.
  
  ## Examples
  
      Orddict.values [a: 1, b: 2] #=> [1,2]
  """
  def values(dict) do
    lc { _, value } in dict, do: value
  end

  @doc """
  Deletes key, value entry from dictionary for specific key.
  If the key does not exist, returns the dictionary unchanged.
  
  ## Examples
  
      Orddict.delete [a: 1, b: 2], :a   #=> [b: 2]
      Orddict.delete [b: 2], :a         #=> [b: 2]
  """
  def delete([{k, _} = e|dict], key) when key < k, do: [e|dict]
  def delete([{k, _} = e|dict], key) when key > k, do: [e|delete(dict, key)]
  def delete([{_k, _v}|dict], _key), do: dict
  def delete([], _), do: []

  @doc """
  Sets the given `value` under `key` for the given dictionary.
  If a previous value is already stored, it is overriden.
  
  ## Examples
  
      Orddict.put [a: 1, b: 2], :a, 3
      #=> [a: 3, b: 2]
  """
  def put([{k, _} = e|dict], key, value) when key < k, do: [{key, value},e|dict]
  def put([{k, _} = e|dict], key, value) when key > k, do: [e|put(dict, key, value)]
  def put([{_, _}|dict], key, value), do: [{key, value}|dict]
  def put([], key, value), do: [{key, value}]

  @doc """
  Merges two dictionaries into one. If the dictionaries have
  duplicated entries, the one given as second argument wins.
  
  ## Examples
  
      Orddict.merge [a: 1, b: 2], [a: 3, d: 4]
      #=> [a:3, b:2, d: 4]
  """
  def merge(d1, d2) do
    merge(d1, d2, fn(_k, _v1, v2) -> v2 end)
  end

  @doc """
  Merges two dictionaries into one. If the dictionaries have
  duplicated entries, the given function is invoked to solve
  conflicts.
  
  ## Examples
  
      Orddict.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
        v1 + v2
      end
      #=> [a:4, b:2, d: 4]
  """
  def merge([{k1, _} = e1|d1], [{k2, _} = e2|d2], fun) when k1 < k2, do: [e1|merge(d1, [e2|d2], fun)];
  def merge([{k1, _} = e1|d1], [{k2, _} = e2|d2], fun) when k1 > k2, do: [e2|merge([e1|d1], d2, fun)];
  def merge([{k1, v1}|d1], [{k1, v2}|d2], fun), do: [{k1, fun.(k1, v1, v2)}|merge(d1, d2, fun)];
  def merge([], d2, _fun), do: d2
  def merge(d1, [], _fun), do: d1
end
