# We're assuming that the List contains sorted pairs of key-value entries
defimpl GenDict, for: List do
  def keys(dict) do
    :orddict.fetch_keys dict
  end

  def values(dict) do
    Enum.map dict, fn({_k, v}) -> v end
  end

  def size(dict) do
    :orddict.size dict
  end

  def has_key?(dict, key) do
    :orddict.is_key key, dict
  end

  def get(dict, key, default // nil) do
    case :orddict.find(key, dict) do
    match: {:ok, value}
      value
    match: :error
      default
    end
  end

  def put(dict, key, value) do
    :orddict.store key, value, dict
  end

  def put(dict, {key, value}) do
    :orddict.store key, value, dict
  end

  def delete(dict, key) do
    :orddict.erase key, dict
  end

  def merge(d1, d2) do
    :orddict.merge fn(_k, _v1, v2) -> v2 end, d1, d2
  end

  def merge(d1, d2, fun) do
    :orddict.merge fun, d1, d2
  end

  def extend(dict, pairs) when is_list(pairs) do
    Enum.reduce pairs, dict, fn(pair, dict) ->
      put(dict, pair)
    end
  end

  def extend(dict, pairs, transform) when is_list(pairs) and is_function(transform) do
    Enum.reduce pairs, dict, fn(i, dict) ->
      pair = transform.(i)
      put(dict, pair)
    end
  end

  def extend(dict, keys, values) when is_list(keys) and is_list(values) do
    if :erlang.length(keys) !== :erlang.length(values) do
      raise ArgumentError, "Both arguments must have equal size"
    else:
      extend(dict, List.zip(keys, values))
    end
  end

  def update(dict, key, fun) do
    :orddict.update key, fun, dict
  end

  def update(dict, key, initial, fun) do
    :orddict.update key, fun, initial, dict
  end
end

defmodule Orddict do
  @doc """
  Creates a new empty Orddict.
  """
  def new do
    []
  end

  @doc """
  Creates a new Orddict with one entry.
  """
  def new({key, value}) do
    GenDict.put new(), {key, value}
  end

  @doc """
  Creates a new Orddict from a list of pairs.

  ## Examples

      Dict.new [{:b,1},{:a,2}]
      #=> [a: 1, b: 2]

  """
  def new(pairs) when is_list(pairs) do
    GenDict.extend new(), pairs
  end

  @doc """
  Creates a new Orddict from a list of elements with the
  help of the transformation function.

  ## Examples

      Dict.new ["a", "b"], fn(x) -> {x, x} end
      #=> ["a": "a", "b": "b"]
  """
  def new(list, transform) when is_list(list) and is_function(transform) do
    GenDict.extend new(), list, transform
  end

  @doc """
  Creates a new Orddict with one entry for each element in `keys` and a
  corresponding element in `values`. Raises an error if `keys` and `values`
  have different size.
  """
  def new(keys, values) when is_list(keys) and is_list(values) do
    GenDict.extend new(), keys, values
  end
end
