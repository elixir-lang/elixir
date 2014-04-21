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

  defmacrop deprecated(key) do
    quote do
      unless is_atom(unquote(key)) do
        IO.write :stderr, "ListDict is deprecated, please use Map instead\n#{Exception.format_stacktrace}"
      end
    end
  end

  @doc false
  def new(pairs) do
    IO.write :stderr, "ListDict is deprecated, please use Map instead\n#{Exception.format_stacktrace}"
    Enum.to_list pairs
  end

  @doc false
  def new(list, transform) when is_function(transform) do
    IO.write :stderr, "ListDict is deprecated, please use Map instead\n#{Exception.format_stacktrace}"
    Enum.map list, transform
  end

  def keys(dict) do
    for {key, _} <- dict, do: key
  end

  def values(dict) do
    for {_, value} <- dict, do: value
  end

  def size(dict) do
    length(dict)
  end

  def has_key?(dict, key) do
    deprecated(key)
    do_has_key?(dict, key)
  end

  defp do_has_key?([{key, _}|_], key), do: true
  defp do_has_key?([{_, _}|t], key), do: do_has_key?(t, key)
  defp do_has_key?([], _key), do: false

  def get(dict, key, default \\ nil) do
    deprecated(key)
    do_get(dict, key, default)
  end
  defp do_get([{key, value}|_], key, _default), do: value
  defp do_get([{_, _}|t], key, default), do: do_get(t, key, default)
  defp do_get([], _key, default), do: default

  def fetch(dict, key) do
    deprecated(key)
    do_fetch(dict, key)
  end
  defp do_fetch([{key, value}|_], key), do: {:ok, value}
  defp do_fetch([{_, _}|t], key), do: do_fetch(t, key)
  defp do_fetch([], _key), do: :error

  def fetch!(dict, key) do
    deprecated(key)
    case fetch(dict, key) do
      {:ok, value} -> value
      :error -> raise(KeyError, key: key, term: dict)
    end
  end

  def pop(dict, key, default \\ nil) do
    deprecated(key)
    {do_get(dict, key, default), do_delete(dict, key)}
  end

  def put(dict, key, val) do
    deprecated(key)
    [{key, val}|do_delete(dict, key)]
  end

  def put_new(dict, key, val) do
    deprecated(key)
    case do_has_key?(dict, key) do
      true  -> dict
      false -> [{key, val}|dict]
    end
  end

  def delete(dict, key) do
    deprecated(key)
    do_delete(dict, key)
  end

  defp do_delete([{key, _}|t], key), do: t
  defp do_delete([{_, _} = h|t], key), do: [h|do_delete(t, key)]
  defp do_delete([], _key), do: []

  def merge(dict, enum, callback \\ fn(_k, _v1, v2) -> v2 end) do
    Enum.reduce enum, dict, fn {k, v2}, acc ->
      update(acc, k, v2, fn(v1) -> callback.(k, v1, v2) end)
    end
  end

  def split(dict, keys) do
    acc = {[], []}

    {take, drop} = Enum.reduce dict, acc, fn({k, v}, {take, drop}) ->
      if k in keys do
        {[{k, v}|take], drop}
      else
        {take, [{k, v}|drop]}
      end
    end

    {Enum.reverse(take), Enum.reverse(drop)}
  end

  def take(dict, keys) do
    for {k, _} = tuple <- dict, k in keys, do: tuple
  end

  def drop(dict, keys) do
    for {k, _} = tuple <- dict, not k in keys, do: tuple
  end

  def update!(list, key, fun) do
    deprecated(key)
    update!(list, key, fun, list)
  end

  defp update!([{key, value}|list], key, fun, _dict) do
    [{key, fun.(value)}|do_delete(list, key)]
  end

  defp update!([{_, _} = e|list], key, fun, dict) do
    [e|update!(list, key, fun, dict)]
  end

  defp update!([], key, _fun, dict) do
    raise(KeyError, key: key, term: dict)
  end

  def update(dict, key, initial, fun) do
    deprecated(key)
    do_update(dict, key, initial, fun)
  end

  defp do_update([{key, value}|dict], key, _initial, fun) do
    [{key, fun.(value)}|do_delete(dict, key)]
  end

  defp do_update([{_, _} = e|dict], key, initial, fun) do
    [e|do_update(dict, key, initial, fun)]
  end

  defp do_update([], key, initial, _fun) do
    [{key, initial}]
  end

  def equal?(dict, other) do
    :lists.keysort(1, dict) === :lists.keysort(1, other)
  end

  def to_list(dict) do
    dict
  end
end
