defrecord Dict.Record, d: nil

defimpl GenDict, for: Dict.Record do
  def keys(dict) do
    :dict.fetch_keys dict.d
  end

  def values(dict) do
    :dict.fold fn(_key, value, acc) ->
      [value|acc]
    end, [], dict.d
  end

  def size(dict) do
    :dict.size dict.d
  end

  def has_key?(dict, key) do
    :dict.is_key key, dict.d
  end

  def get(dict, key, default // nil) do
    case :dict.find(key, dict.d) do
    match: {:ok, value}
      value
    match: :error
      default
    end
  end

  def put(dict, key, value) do
    dict.update_d(:dict.store key, value, &1)
  end

  def put(dict, {key, value}) do
    dict.update_d(:dict.store key, value, &1)
  end

  def delete(dict, key) do
    dict.update_d(:dict.erase key, &1)
  end

  def merge(d1, d2) do
    d1.update_d(:dict.merge fn(_k, _v1, v2) -> v2 end, &1, d2.d)
  end

  def merge(d1, d2, fun) do
    d1.update_d(:dict.merge fun, &1, d2.d)
  end
end

defmodule Dict do
  @doc """
  Creates a new empty dict.
  """
  def new do
    Dict.Record.new [d: :dict.new]
  end

  @doc """
  Creates a new dict with one entry.
  """
  def new({key, value}) do
    GenDict.put new(), {key, value}
  end

  @doc """
  Creates a new dict from a list of pairs.

  ## Examples

      Dict.new [{:b,1},{:a,2}]
      #=> [a: 1, b: 2]

  """
  def new(pairs) when is_list(pairs) do
    Enum.reduce pairs, new(), fn(pair, dict) ->
      GenDict.put(dict, pair)
    end
  end

  @doc """
  Creates a new dict from a list of pairs with the
  help of the transformation function.

  ## Examples

      Dict.new ["a", "b"], fn(x) -> {x, x} end
      #=> ["a": "a", "b": "b"]
  """
  def new(pairs, transform) when is_list(pairs) and is_function(transform) do
    Enum.reduce pairs, new(), fn(i, dict) ->
      pair = transform.(i)
      GenDict.put(dict, pair)
    end
  end

  @doc """
  Creates a new dict with one entry for each element in `keys` and a
  corresponding element in `values`. Raises an error if `keys` and `values`
  have different size.
  """
  def new(keys, values) when is_list(keys) and is_list(values) do
    if :erlang.length(keys) !== :erlang.length(values) do
      raise ArgumentError, "Both arguments must have equal size"
    else:
      new List.zip(keys, values)
    end
  end

end

