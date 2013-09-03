defmodule ListDict do
  @moduledoc """
  A Dict implementation that works on lists of two-items tuples.

  For more information about the functions and their APIs, please
  consult the `Dict` module.
  """

  @doc """
  Returns a new `ListDict`, i.e. an empty list.
  """
  def new, do: []

  @doc """
  Creates a new `ListDict` from the given pairs.
  """
  def new(pairs) do
    Enum.to_list pairs
  end

  @doc """
  Creates a new `ListDict` from the given pairs
  via the given transformation function.
  """
  def new(list, transform) when is_function(transform) do
    Enum.map list, transform
  end

  def keys(dict) do
    lc { key, _ } inlist dict, do: key
  end

  def values(dict) do
    lc { _, value } inlist dict, do: value
  end

  def size(dict) do
    length(dict)
  end

  def has_key?(dict, key) do
    :lists.keymember(key, 1, dict)
  end

  def get(dict, key, default // nil) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, value } -> value
      false -> default
    end
  end

  def fetch(dict, key) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, value } -> { :ok, value }
      false -> :error
    end
  end

  def fetch!(dict, key) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, value } -> value
      false -> raise(KeyError, key: key)
    end
  end

  def pop(dict, key, default // nil) do
    { get(dict, key, default), delete(dict, key) }
  end

  def put(dict, key, val) do
    [{key, val}|delete(dict, key)]
  end

  def put_new(dict, key, val) do
    case :lists.keyfind(key, 1, dict) do
      { ^key, _ } -> dict
      false -> [{key, val}|dict]
    end
  end

  def delete(dict, key) do
    lc { k, _ } = tuple inlist dict, key != k, do: tuple
  end

  def merge(dict, enum, callback // fn(_k, _v1, v2) -> v2 end)

  def merge(dict1, dict2, fun) do
    Enum.reduce dict2, dict1, fn { k, v2 }, acc ->
      update(acc, k, v2, fn(v1) -> fun.(k, v1, v2) end)
    end
  end

  def split(dict, keys) do
    acc = { new(), new() }
    {take, drop} = Enum.reduce dict, acc, fn({ k, v }, { take, drop }) ->
      if :lists.member(k, keys) do
        { [{k, v}|take], drop }
      else
        { take, [{k, v}|drop] }
      end
    end
    
    {Enum.reverse(take), Enum.reverse(drop)}
  end

  def take(dict, keys) do
    lc { k, _ } = tuple inlist dict, :lists.member(k, keys), do: tuple
  end

  def drop(dict, keys) do
    lc { k, _ } = tuple inlist dict, not :lists.member(k, keys), do: tuple
  end

  def update!([{key, value}|dict], key, fun) do
    [{key, fun.(value)}|delete(dict, key)]
  end

  def update!([{_, _} = e|dict], key, fun) do
    [e|update!(dict, key, fun)]
  end

  def update!([], key, _fun) do
    raise(KeyError, key: key)
  end

  def update([{key, value}|dict], key, _initial, fun) do
    [{key, fun.(value)}|delete(dict, key)]
  end

  def update([{_, _} = e|dict], key, initial, fun) do
    [e|update(dict, key, initial, fun)]
  end

  def update([], key, initial, _fun) do
    [{key, initial}]
  end

  def empty(_dict), do: []

  def equal?(dict, other) do
    :lists.keysort(1, dict) == :lists.keysort(1, other)
  end

  def to_list(dict), do: dict
end
