defmodule HashDict do
  @moduledoc """
  A key-value store.

  The `HashDict` is implemented using tries, which grows in
  space as the number of keys grows, working well with both
  small and large set of keys. For more information about the
  functions and their APIs, please consult the `Dict` module.
  """

  use Dict.Behaviour

  @node_bitmap 0b111
  @node_shift 3
  @node_size 8
  @node_template :erlang.make_tuple(@node_size, [])

  defrecordp :trie, HashDict,
    size: 0,
    root: @node_template

  # Inline common instructions
  @compile :inline_list_funcs
  @compile { :inline, key_hash: 1, key_mask: 1, key_shift: 1 }

  @doc """
  Creates a new empty dict.
  """
  @spec new :: Dict.t
  def new do
    trie()
  end

  @doc false
  @spec new(Enum.t) :: Dict.t
  def new(enum) do
    IO.write :stderr, "HashDict.new/1 is deprecated, please use Enum.into/2 instead\n#{Exception.format_stacktrace}"
    Enum.reduce enum, trie(), fn { k, v }, dict ->
      put(dict, k, v)
    end
  end

  @doc false
  @spec new(Enum.t, (term -> {key :: term, value ::term})) :: Dict.t
  def new(enum, transform) when is_function(transform) do
    IO.write :stderr, "HashDict.new/2 is deprecated, please use Enum.into/3 instead\n#{Exception.format_stacktrace}"
    Enum.reduce enum, trie(), fn i, dict ->
      { k, v } = transform.(i)
      put(dict, k, v)
    end
  end

  def empty(trie()) do
    IO.write :stderr, "HashDict.empty/1 is deprecated, please use Collectable.empty/1 instead\n#{Exception.format_stacktrace}"
    trie()
  end

  def put(trie(root: root, size: size), key, value) do
    { root, counter } = do_put(root, key, value, key_hash(key))
    trie(root: root, size: size + counter)
  end

  def update!(trie(root: root, size: size) = dict, key, fun) when is_function(fun, 1) do
    { root, counter } = do_update(root, key, fn -> raise KeyError, key: key, term: dict end,
                                  fun, key_hash(key))
    trie(root: root, size: size + counter)
  end

  def update(trie(root: root, size: size), key, initial, fun) when is_function(fun, 1) do
    { root, counter } = do_update(root, key, fn -> initial end, fun, key_hash(key))
    trie(root: root, size: size + counter)
  end

  def fetch(trie(root: root), key) do
    do_fetch(root, key, key_hash(key))
  end

  def delete(dict, key) do
    case dict_delete(dict, key) do
      { dict, _value } -> dict
      :error           -> dict
    end
  end

  def pop(dict, key, default \\ nil) do
    case dict_delete(dict, key) do
      { dict, value } -> { value, dict }
      :error          -> { default, dict }
    end
  end

  def size(trie(size: size)) do
    size
  end

  @doc false
  def reduce(trie(root: root), acc, fun) do
    do_reduce(root, acc, fun, @node_size, fn
      {:suspend, acc} -> {:suspended, acc, &{ :done, elem(&1, 1) }}
      {:halt, acc}    -> {:halted, acc}
      {:cont, acc}    -> {:done, acc}
    end)
  end

  def split(dict, keys) do
    Enum.reduce keys, { new, dict }, fn key, { inc, exc } = acc ->
      case dict_delete(exc, key) do
        { exc, value } -> { put(inc, key, value), exc }
        :error -> acc
      end
    end
  end

  def merge(trie(size: size1) = dict1, trie(size: size2) = dict2, callback) when size1 < size2 do
    reduce(dict1, { :cont, dict2 }, fn { k, v1 }, acc ->
      { :cont, update(acc, k, v1, &callback.(k, v1, &1)) }
    end) |> elem(1)
  end

  def merge(trie() = dict1, trie() = dict2, callback) do
    reduce(dict2, { :cont, dict1 }, fn { k, v2 }, acc ->
      { :cont, update(acc, k, v2, &callback.(k, &1, v2)) }
    end) |> elem(1)
  end

  ## General helpers

  defp dict_delete(trie(root: root, size: size), key) do
    case do_delete(root, key, key_hash(key)) do
      { root, value } -> { trie(root: root, size: size - 1), value }
      :error          -> :error
    end
  end

  ## Dict manipulation

  defp do_fetch(node, key, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [^key|v]     -> {:ok, v}
      {^key, v, _} -> {:ok, v}
      {_, _, n}    -> do_fetch(n, key, key_shift(hash))
      _            -> :error
    end
  end

  defp do_put(node, key, value, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        {set_elem(node, index, [key|value]), 1}
      [^key|_] ->
        {set_elem(node, index, [key|value]), 0}
      [k|v] ->
        n = set_elem(@node_template, key_mask(key_shift(hash)), [key|value])
        {set_elem(node, index, {k, v, n}), 1}
      {^key, _, n} ->
        {set_elem(node, index, {key, value, n}), 0}
      {k, v, n} ->
        {n, counter} = do_put(n, key, value, key_shift(hash))
        {set_elem(node, index, {k, v, n}), counter}
    end
  end

  defp do_update(node, key, initial, fun, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        {set_elem(node, index, [key|initial.()]), 1}
      [^key|value] ->
        {set_elem(node, index, [key|fun.(value)]), 0}
      [k|v] ->
        n = set_elem(@node_template, key_mask(key_shift(hash)), [key|initial.()])
        {set_elem(node, index, {k, v, n}), 1}
      {^key, value, n} ->
        {set_elem(node, index, {key, fun.(value), n}), 0}
      {k, v, n} ->
        {n, counter} = do_update(n, key, initial, fun, key_shift(hash))
        {set_elem(node, index, {k, v, n}), counter}
    end
  end

  defp do_delete(node, key, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        :error
      [^key|value] ->
        {set_elem(node, index, []), value}
      [_|_] ->
        :error
      {^key, value, n} ->
        {set_elem(node, index, do_compact_node(n)), value}
      {k, v, n} ->
        case do_delete(n, key, key_shift(hash)) do
          {@node_template, value} ->
            {set_elem(node, index, [k|v]), value}
          {n, value} ->
            {set_elem(node, index, {k, v, n}), value}
          :error ->
            :error
        end
    end
  end

  Enum.each 0..(@node_size - 1), fn index ->
    defp do_compact_node(node) when elem(node, unquote(index)) != [] do
      case elem(node, unquote(index)) do
        [k|v] ->
          case set_elem(node, unquote(index), []) do
            @node_template -> [k|v]
            n -> {k, v, n}
          end
        {k, v, n} ->
          {k, v, set_elem(node, unquote(index), do_compact_node(n))}
      end
    end
  end

  ## Dict reduce

  defp do_reduce_each(_node, {:halt, acc}, _fun, _next) do
    {:halted, acc}
  end

  defp do_reduce_each(node, {:suspend, acc}, fun, next) do
    {:suspended, acc, &do_reduce_each(node, &1, fun, next)}
  end

  defp do_reduce_each([], acc, _fun, next) do
    next.(acc)
  end

  defp do_reduce_each([k|v], {:cont, acc}, fun, next) do
    next.(fun.({k,v}, acc))
  end

  defp do_reduce_each({k, v, n}, {:cont, acc}, fun, next) do
    do_reduce(n, fun.({k, v}, acc), fun, @node_size, next)
  end

  defp do_reduce(node, acc, fun, count, next) when count > 0 do
    do_reduce_each(:erlang.element(count, node), acc, fun, &do_reduce(node, &1, fun, count - 1, next))
  end

  defp do_reduce(_node, acc, _fun, 0, next) do
    next.(acc)
  end

  ## Key operations

  import Bitwise

  defp key_hash(key) do
    :erlang.phash2(key)
  end

  defp key_mask(hash) do
    hash &&& @node_bitmap
  end

  defp key_shift(hash) do
    hash >>> @node_shift
  end
end

defimpl Enumerable, for: HashDict do
  def reduce(dict, acc, fun),  do: HashDict.reduce(dict, acc, fun)
  def member?(dict, { k, v }), do: { :ok, match?({ :ok, ^v }, HashDict.fetch(dict, k)) }
  def member?(_dict, _),       do: { :ok, false }
  def count(dict),             do: { :ok, HashDict.size(dict) }
end

defimpl Access, for: HashDict do
  def access(dict, key), do: HashDict.get(dict, key, nil)
end

defimpl Collectable, for: HashDict do
  def empty(_dict) do
    HashDict.new
  end

  def into(original) do
    { original, fn
      dict, { :cont, { k, v } } -> Dict.put(dict, k, v)
      dict, :done -> dict
      _, :halt -> :ok
    end }
  end
end
