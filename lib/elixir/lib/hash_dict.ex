defmodule HashDict do
  @moduledoc """
  WARNING: this module is deprecated.

  Use the `Map` module instead.
  """

  # TODO: Deprecate every function by 1.4

  use Dict

  @node_bitmap 0b111
  @node_shift 3
  @node_size 8
  @node_template :erlang.make_tuple(@node_size, [])

  @opaque t :: %__MODULE__{size: non_neg_integer, root: term}
  @doc false
  defstruct size: 0, root: @node_template

  # Inline common instructions
  @compile :inline_list_funcs
  @compile {:inline, key_hash: 1, key_mask: 1, key_shift: 1}

  @doc """
  Creates a new empty dict.
  """
  @spec new :: Dict.t
  def new do
    %HashDict{}
  end

  def put(%HashDict{root: root, size: size}, key, value) do
    {root, counter} = do_put(root, key, value, key_hash(key))
    %HashDict{root: root, size: size + counter}
  end

  def update!(%HashDict{root: root, size: size} = dict, key, fun) when is_function(fun, 1) do
    {root, counter} = do_update(root, key, fn -> raise KeyError, key: key, term: dict end,
                                fun, key_hash(key))
    %HashDict{root: root, size: size + counter}
  end

  def update(%HashDict{root: root, size: size}, key, initial, fun) when is_function(fun, 1) do
    {root, counter} = do_update(root, key, fn -> initial end, fun, key_hash(key))
    %HashDict{root: root, size: size + counter}
  end

  def fetch(%HashDict{root: root}, key) do
    do_fetch(root, key, key_hash(key))
  end

  def delete(dict, key) do
    case dict_delete(dict, key) do
      {dict, _value} -> dict
      :error         -> dict
    end
  end

  def pop(dict, key, default \\ nil) do
    case dict_delete(dict, key) do
      {dict, value} -> {value, dict}
      :error        -> {default, dict}
    end
  end

  def size(%HashDict{size: size}) do
    size
  end

  @doc false
  def reduce(%HashDict{root: root}, acc, fun) do
    do_reduce(root, acc, fun, @node_size, fn
      {:suspend, acc} -> {:suspended, acc, &{:done, elem(&1, 1)}}
      {:halt, acc}    -> {:halted, acc}
      {:cont, acc}    -> {:done, acc}
    end)
  end

  ## General helpers

  @doc false
  def dict_delete(%HashDict{root: root, size: size}, key) do
    case do_delete(root, key, key_hash(key)) do
      {root, value} -> {%HashDict{root: root, size: size - 1}, value}
      :error        -> :error
    end
  end

  ## Dict manipulation

  defp do_fetch(node, key, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [^key | v]   -> {:ok, v}
      {^key, v, _} -> {:ok, v}
      {_, _, n}    -> do_fetch(n, key, key_shift(hash))
      _            -> :error
    end
  end

  defp do_put(node, key, value, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        {put_elem(node, index, [key | value]), 1}
      [^key | _] ->
        {put_elem(node, index, [key | value]), 0}
      [k | v] ->
        n = put_elem(@node_template, key_mask(key_shift(hash)), [key | value])
        {put_elem(node, index, {k, v, n}), 1}
      {^key, _, n} ->
        {put_elem(node, index, {key, value, n}), 0}
      {k, v, n} ->
        {n, counter} = do_put(n, key, value, key_shift(hash))
        {put_elem(node, index, {k, v, n}), counter}
    end
  end

  defp do_update(node, key, initial, fun, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        {put_elem(node, index, [key | initial.()]), 1}
      [^key | value] ->
        {put_elem(node, index, [key | fun.(value)]), 0}
      [k | v] ->
        n = put_elem(@node_template, key_mask(key_shift(hash)), [key | initial.()])
        {put_elem(node, index, {k, v, n}), 1}
      {^key, value, n} ->
        {put_elem(node, index, {key, fun.(value), n}), 0}
      {k, v, n} ->
        {n, counter} = do_update(n, key, initial, fun, key_shift(hash))
        {put_elem(node, index, {k, v, n}), counter}
    end
  end

  defp do_delete(node, key, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        :error
      [^key | value] ->
        {put_elem(node, index, []), value}
      [_ | _] ->
        :error
      {^key, value, n} ->
        {put_elem(node, index, do_compact_node(n)), value}
      {k, v, n} ->
        case do_delete(n, key, key_shift(hash)) do
          {@node_template, value} ->
            {put_elem(node, index, [k | v]), value}
          {n, value} ->
            {put_elem(node, index, {k, v, n}), value}
          :error ->
            :error
        end
    end
  end

  Enum.each 0..(@node_size - 1), fn index ->
    defp do_compact_node(node) when elem(node, unquote(index)) != [] do
      case elem(node, unquote(index)) do
        [k | v] ->
          case put_elem(node, unquote(index), []) do
            @node_template -> [k | v]
            n -> {k, v, n}
          end
        {k, v, n} ->
          {k, v, put_elem(node, unquote(index), do_compact_node(n))}
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

  defp do_reduce_each([k | v], {:cont, acc}, fun, next) do
    next.(fun.({k, v}, acc))
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
  def member?(dict, {k, v}), do: {:ok, match?({:ok, ^v}, HashDict.fetch(dict, k))}
  def member?(_dict, _),       do: {:ok, false}
  def count(dict),             do: {:ok, HashDict.size(dict)}
end

defimpl Collectable, for: HashDict do
  def into(original) do
    {original, fn
      dict, {:cont, {k, v}} -> HashDict.put(dict, k, v)
      dict, :done -> dict
      _, :halt -> :ok
    end}
  end
end

defimpl Inspect, for: HashDict do
  import Inspect.Algebra

  def inspect(dict, opts) do
    concat ["#HashDict<", Inspect.List.inspect(HashDict.to_list(dict), opts), ">"]
  end
end
