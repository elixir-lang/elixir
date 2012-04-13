defimpl PDict, for: Tuple do
  def keys(dict) do
    :dict.fetch_keys dict
  end

  def values(dict) do
    :dict.fold fn(_key, value, acc) ->
      [value|acc]
    end, [], dict
  end

  def size(dict) do
    :dict.size dict
  end

  def has_key?(dict, key) do
    :dict.is_key key, dict
  end

  def get(dict, key, default // nil) do
    case :dict.find(key, dict) do
    match: {:ok, value}
      value
    match: :error
      default
    end
  end

  def put(dict, key, value) do
    :dict.store key, value, dict
  end

  def put(dict, {key, value}) do
    :dict.store key, value, dict
  end

  def delete(dict, key) do
    :dict.erase key, dict
  end

  def merge(d1, d2) do
    :dict.merge fn(_k, _v1, v2) -> v2 end, d1, d2
  end

  def merge(d1, d2, fun) do
    :dict.merge fun, d1, d2
  end

  def update(dict, key, fun) do
    :dict.update key, fun, dict
  end

  def update(dict, key, initial, fun) do
    :dict.update key, fun, initial, dict
  end

  def empty(_) do
    :dict.new
  end
end

defmodule Dict do
  use Dict.Common

  @doc """
  Creates a new empty Dict.
  """
  def new do
    :dict.new
  end
end
