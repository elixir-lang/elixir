# We're assuming that the List contains sorted pairs of key-value entries
defimpl PDict, for: List do
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

  def update(dict, key, fun) do
    :orddict.update key, fun, dict
  end

  def update(dict, key, initial, fun) do
    :orddict.update key, fun, initial, dict
  end

  def empty(_) do
    []
  end
end

defmodule Orddict do
  use Dict.Common, :List
end
