defmodule ListDict do
  @moduledoc """
  A Dict implementation that works on lists of two-items tuples.

  This dictionary is only recommended for keeping a small amount
  of values. Other dict alternatives are more viable for keeping
  any other amount than a handful.

  For more information about the functions and their APIs, please
  consult the `Dict` module.
  """

  @doc """
  Returns a new `ListDict`, i.e. an empty list.
  """
  def new, do: []

  @doc false
  def new(pairs) do
    IO.write :stderr, "ListDict.new/1 is deprecated, please use Enum.into/2 instead\n#{Exception.format_stacktrace}"
    Enum.to_list pairs
  end

  @doc false
  def new(list, transform) when is_function(transform) do
    IO.write :stderr, "ListDict.new/2 is deprecated, please use Enum.into/3 instead\n#{Exception.format_stacktrace}"
    Enum.map list, transform
  end

  def keys(dict) do
    for { key, _ } <- dict, do: key
  end

  def values(dict) do
    for { _, value } <- dict, do: value
  end

  def size(dict) do
    length(dict)
  end

  def has_key?(dict, key)
  def has_key?([{ key, _ }|_], key), do: true
  def has_key?([{ _, _ }|t], key), do: has_key?(t, key)
  def has_key?([], _key), do: false

  def get(dict, key, default \\ nil)
  def get([{ key, value }|_], key, _default), do: value
  def get([{ _, _ }|t], key, default), do: get(t, key, default)
  def get([], _key, default), do: default

  def fetch(dict, key)
  def fetch([{ key, value }|_], key), do: { :ok, value }
  def fetch([{ _, _ }|t], key), do: fetch(t, key)
  def fetch([], _key), do: :error

  def fetch!(dict, key) do
    case fetch(dict, key) do
      { :ok, value } -> value
      :error -> raise(KeyError, key: key, term: dict)
    end
  end

  def pop(dict, key, default \\ nil) do
    { get(dict, key, default), delete(dict, key) }
  end

  def put(dict, key, val) do
    [{key, val}|delete(dict, key)]
  end

  def put_new(dict, key, val) do
    case has_key?(dict, key) do
      true  -> dict
      false -> [{key, val}|dict]
    end
  end

  def delete(dict, key)
  def delete([{ key, _ }|t], key), do: t
  def delete([{ _, _ } = h|t], key), do: [h|delete(t, key)]
  def delete([], _key), do: []

  def merge(dict, enum, callback \\ fn(_k, _v1, v2) -> v2 end) do
    Enum.reduce enum, dict, fn { k, v2 }, acc ->
      update(acc, k, v2, fn(v1) -> callback.(k, v1, v2) end)
    end
  end

  def split(dict, keys) do
    acc = { [], [] }

    {take, drop} = Enum.reduce dict, acc, fn({ k, v }, { take, drop }) ->
      if k in keys do
        { [{k, v}|take], drop }
      else
        { take, [{k, v}|drop] }
      end
    end

    {Enum.reverse(take), Enum.reverse(drop)}
  end

  def take(dict, keys) do
    for { k, _ } = tuple <- dict, k in keys, do: tuple
  end

  def drop(dict, keys) do
    for { k, _ } = tuple <- dict, not k in keys, do: tuple
  end

  def update!(list, key, fun) do
    update!(list, key, fun, list)
  end

  defp update!([{key, value}|list], key, fun, _dict) do
    [{key, fun.(value)}|delete(list, key)]
  end

  defp update!([{_, _} = e|list], key, fun, dict) do
    [e|update!(list, key, fun, dict)]
  end

  defp update!([], key, _fun, dict) do
    raise(KeyError, key: key, term: dict)
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

  def empty(_dict) do
    IO.write :stderr, "ListDict.empty/1 is deprecated, please use Collectable.empty/1 instead\n#{Exception.format_stacktrace}"
    []
  end

  def equal?(dict, other) do
    :lists.keysort(1, dict) === :lists.keysort(1, other)
  end

  @doc false
  def reduce(_,           { :halt, acc }, _fun),   do: { :halted, acc }
  def reduce(list,        { :suspend, acc }, fun), do: { :suspended, acc, &reduce(list, &1, fun) }
  def reduce([],          { :cont, acc }, _fun),   do: { :done, acc }
  def reduce([{_,_}=h|t], { :cont, acc }, fun),    do: reduce(t, fun.(h, acc), fun)

  def to_list(dict), do: dict
end
