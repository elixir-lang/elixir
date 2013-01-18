defmodule List.Dict do
  @doc false
  def new, do: []

  @doc false
  def new(pairs) do
    Enum.reduce pairs, new, fn { k, v }, acc ->
      [ {k, v} | acc ]
    end
  end

  @doc false
  def new(list, transform) when is_function(transform) do
    Enum.reduce list, [], fn i, acc ->
      { k, v } = transform.(i)
      [ {k, v} | acc ]
    end
  end

  @doc false
  def keys(dict) do
    lc { key, _ } inlist dict, do: key
  end

  @doc false
  def values(dict) do
    lc { _, value } inlist dict, do: value
  end

  @doc false
  def size(dict) do
    length(dict)
  end

  @doc false
  def has_key?(dict, key) do
    :lists.keymember(key, 1, dict)
  end

  @doc false
  def get(dict, key, default) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, value } -> value
      false -> default
    end
  end

  @doc false
  def get!(dict, key) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, value } -> value
      false -> raise(KeyError, key: key)
    end
  end

  @doc false
  def put(dict, key, val) do
    [{key, val}|delete(dict, key)]
  end

  @doc false
  def put_new(dict, key, val) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, _ } -> dict
      false -> [{key,val}|dict]
    end
  end

  @doc false
  def delete(dict, key) do
    lc { k, _ } = tuple inlist dict, key != k, do: tuple
  end

  @doc false
  def merge(dict1, dict2, fun) do
    Enum.reduce dict2, dict1, fn { k, v2 }, acc ->
      update(acc, k, v2, fn(v1) -> fun.(k, v1, v2) end)
    end
  end

  @doc false
  def update([{key, value}|dict], key, fun) do
    [{key, fun.(value)}|delete(dict, key)]
  end

  def update([{_, _} = e|dict], key, fun) do
    [e|update(dict, key, fun)]
  end

  def update([], key, _fun) do
    raise(KeyError, key: key)
  end

  @doc false
  def update([{key, value}|dict], key, _initial, fun) do
    [{key, fun.(value)}|delete(dict, key)]
  end

  def update([{_, _} = e|dict], key, initial, fun) do
    [e|update(dict, key, initial, fun)]
  end

  def update([], key, initial, _fun) do
    [{key, initial}]
  end

  @doc false
  def empty(_dict), do: []

  @doc false
  def to_list(dict), do: dict
end
