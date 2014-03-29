defmodule HashSet do
  @moduledoc """
  A set store.

  The `HashSet` is implemented using tries, which grows in
  space as the number of keys grows, working well with both
  small and large set of keys. For more information about the
  functions and their APIs, please consult the `Set` module.
  """

  @behaviour Set

  @node_bitmap 0b111
  @node_shift 3
  @node_size 8
  @node_template :erlang.make_tuple(@node_size, [])

  defrecordp :trie, HashSet,
    size: 0,
    root: @node_template

  # Inline common instructions
  @compile :inline_list_funcs
  @compile { :inline, key_hash: 1, key_mask: 1, key_shift: 1 }

  @doc """
  Creates a new empty set.
  """
  @spec new :: Set.t
  def new do
    trie()
  end

  @doc false
  @spec new(Enum.t) :: Set.t
  def new(enum) do
    IO.write :stderr, "HashSet.new/1 is deprecated, please use Enum.into/2 instead\n#{Exception.format_stacktrace}"
    Enum.reduce enum, trie(), fn i, set ->
      put(set, i)
    end
  end

  @doc false
  @spec new(Enum.t, (term -> term)) :: Set.t
  def new(enum, transform) when is_function(transform) do
    IO.write :stderr, "HashSet.new/2 is deprecated, please use Enum.into/3 instead\n#{Exception.format_stacktrace}"
    Enum.reduce enum, trie(), fn i, set ->
      put(set, transform.(i))
    end
  end

  def union(trie(size: size1) = set1, trie(size: size2) = set2) when size1 <= size2 do
    set_fold set1, set2, fn v, acc -> put(acc, v) end
  end

  def union(trie() = set1, trie() = set2) do
    set_fold set2, set1, fn v, acc -> put(acc, v) end
  end

  def intersection(trie() = set1, trie() = set2) do
    set_fold set1, trie(), fn v, acc ->
      if member?(set2, v), do: put(acc, v), else: acc
    end
  end

  def difference(trie() = set1, trie() = set2) do
    set_fold set2, set1, fn v, acc -> delete(acc, v) end
  end

  def to_list(set) do
    set_fold(set, [], &[&1|&2]) |> :lists.reverse
  end

  def equal?(trie(size: size1) = set1, trie(size: size2) = set2) do
    case size1 do
      ^size2 -> subset?(set1, set2)
      _      -> false
    end
  end

  def subset?(trie() = set1, trie() = set2) do
    reduce(set1, { :cont, true }, fn member, acc ->
      case member?(set2, member) do
        true -> { :cont, acc }
        _    -> { :halt, false }
      end
    end) |> elem(1)
  end

  def disjoint?(trie() = set1, trie() = set2) do
    reduce(set2, { :cont, true }, fn member, acc ->
      case member?(set1, member) do
        false -> { :cont, acc }
        _     -> { :halt, false }
      end
    end) |> elem(1)
  end

  def empty(trie()) do
    IO.write :stderr, "HashSet.empty/1 is deprecated, please use Collectable.empty/1 instead\n#{Exception.format_stacktrace}"
    trie()
  end

  def member?(trie(root: root), term) do
    do_member?(root, term, key_hash(term))
  end

  def put(trie(root: root, size: size), term) do
    {root, counter} = do_put(root, term, key_hash(term))
    trie(root: root, size: size + counter)
  end

  def delete(trie(root: root, size: size) = set, term) do
    case do_delete(root, term, key_hash(term)) do
      {:ok, root} -> trie(root: root, size: size - 1)
      :error      -> set
    end
  end

  @doc false
  def reduce(trie(root: root), acc, fun) do
    do_reduce(root, acc, fun, @node_size, fn
      {:suspend, acc} -> {:suspended, acc, &{ :done, elem(&1, 1) }}
      {:halt, acc}    -> {:halted, acc}
      {:cont, acc}    -> {:done, acc}
    end)
  end

  def size(trie(size: size)) do
    size
  end

  ## Set helpers

  defp set_fold(trie(root: root), acc, fun) do
    do_fold(root, acc, fun, @node_size)
  end

  ## Set manipulation

  defp do_member?(node, term, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      []        -> false
      [^term|_] -> true
      [_]       -> false
      [_|n]     -> do_member?(n, term, key_shift(hash))
    end
  end

  defp do_put(node, term, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        {set_elem(node, index, [term]), 1}
      [^term|_] ->
        {node, 0}
      [t] ->
        n = set_elem(@node_template, key_mask(key_shift(hash)), [term])
        {set_elem(node, index, [t|n]), 1}
      [t|n] ->
        {n, counter} = do_put(n, term, key_shift(hash))
        {set_elem(node, index, [t|n]), counter}
    end
  end

  defp do_delete(node, term, hash) do
    index = key_mask(hash)
    case elem(node, index) do
      [] ->
        :error
      [^term] ->
        {:ok, set_elem(node, index, [])}
      [_] ->
        :error
      [^term|n] ->
        {:ok, set_elem(node, index, do_compact_node(n))}
      [t|n] ->
        case do_delete(n, term, key_shift(hash)) do
          {:ok, @node_template} ->
            {:ok, set_elem(node, index, [t])}
          {:ok, n} ->
            {:ok, set_elem(node, index, [t|n])}
          :error ->
            :error
        end
    end
  end

  Enum.each 0..(@node_size - 1), fn index ->
    defp do_compact_node(node) when elem(node, unquote(index)) != [] do
      case elem(node, unquote(index)) do
        [t] ->
          case set_elem(node, unquote(index), []) do
            @node_template -> [t]
            n -> [t|n]
          end
        [t|n] ->
          [t|set_elem(node, unquote(index), do_compact_node(n))]
      end
    end
  end

  ## Set fold

  defp do_fold_each([], acc, _fun),   do: acc
  defp do_fold_each([t], acc, fun),   do: fun.(t, acc)
  defp do_fold_each([t|n], acc, fun), do: do_fold(n, fun.(t, acc), fun, @node_size)

  defp do_fold(node, acc, fun, count) when count > 0 do
    acc = do_fold_each(:erlang.element(count, node), acc, fun)
    do_fold(node, acc, fun, count - 1)
  end

  defp do_fold(_node, acc, _fun, 0) do
    acc
  end

  ## Set reduce

  defp do_reduce_each(_node, {:halt, acc}, _fun, _next) do
    {:halted, acc}
  end

  defp do_reduce_each(node, {:suspend, acc}, fun, next) do
    {:suspended, acc, &do_reduce_each(node, &1, fun, next)}
  end

  defp do_reduce_each([], acc, _fun, next) do
    next.(acc)
  end

  defp do_reduce_each([t], {:cont, acc}, fun, next) do
    next.(fun.(t, acc))
  end

  defp do_reduce_each([t|n], {:cont, acc}, fun, next) do
    do_reduce(n, fun.(t, acc), fun, @node_size, next)
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

defimpl Enumerable, for: HashSet do
  def reduce(set, acc, fun), do: HashSet.reduce(set, acc, fun)
  def member?(set, v),       do: { :ok, HashSet.member?(set, v) }
  def count(set),            do: { :ok, HashSet.size(set) }
end

defimpl Collectable, for: HashSet do
  def empty(_dict) do
    HashSet.new
  end

  def into(original) do
    { original, fn
      set, { :cont, x } -> HashSet.put(set, x)
      set, :done -> set
      _, :halt -> :ok
    end }
  end
end
