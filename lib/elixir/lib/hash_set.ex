defmodule HashSet do
  @moduledoc """
  Tuple-based HashSet implementation.

  This module is deprecated. Use the `MapSet` module instead.
  """

  @moduledoc deprecated: "Use MapSet instead"

  @node_bitmap 0b111
  @node_shift 3
  @node_size 8
  @node_template :erlang.make_tuple(@node_size, [])

  message = "Use the MapSet module instead"

  @opaque t :: %__MODULE__{size: non_neg_integer, root: term}
  @doc false
  defstruct size: 0, root: @node_template

  # Inline common instructions
  @compile :inline_list_funcs
  @compile {:inline, key_hash: 1, key_mask: 1, key_shift: 1}

  @deprecated message
  @spec new :: Set.t()
  def new do
    %HashSet{}
  end

  @deprecated message
  def union(%HashSet{size: size1} = set1, %HashSet{size: size2} = set2) when size1 <= size2 do
    set_fold(set1, set2, fn v, acc -> put(acc, v) end)
  end

  @deprecated message
  def union(%HashSet{} = set1, %HashSet{} = set2) do
    set_fold(set2, set1, fn v, acc -> put(acc, v) end)
  end

  @deprecated message
  def intersection(%HashSet{} = set1, %HashSet{} = set2) do
    set_fold(set1, %HashSet{}, fn v, acc ->
      if member?(set2, v), do: put(acc, v), else: acc
    end)
  end

  @deprecated message
  def difference(%HashSet{} = set1, %HashSet{} = set2) do
    set_fold(set2, set1, fn v, acc -> delete(acc, v) end)
  end

  @deprecated message
  def to_list(set) do
    set_fold(set, [], &[&1 | &2]) |> Enum.reverse()
  end

  @deprecated message
  def equal?(%HashSet{size: size1} = set1, %HashSet{size: size2} = set2) do
    case size1 do
      ^size2 -> subset?(set1, set2)
      _ -> false
    end
  end

  @deprecated message
  def subset?(%HashSet{} = set1, %HashSet{} = set2) do
    reduce(set1, {:cont, true}, fn member, acc ->
      case member?(set2, member) do
        true -> {:cont, acc}
        _ -> {:halt, false}
      end
    end)
    |> elem(1)
  end

  @deprecated message
  def disjoint?(%HashSet{} = set1, %HashSet{} = set2) do
    reduce(set2, {:cont, true}, fn member, acc ->
      case member?(set1, member) do
        false -> {:cont, acc}
        _ -> {:halt, false}
      end
    end)
    |> elem(1)
  end

  @deprecated message
  def member?(%HashSet{root: root}, term) do
    do_member?(root, term, key_hash(term))
  end

  @deprecated message
  def put(%HashSet{root: root, size: size}, term) do
    {root, counter} = do_put(root, term, key_hash(term))
    %HashSet{root: root, size: size + counter}
  end

  @deprecated message
  def delete(%HashSet{root: root, size: size} = set, term) do
    case do_delete(root, term, key_hash(term)) do
      {:ok, root} -> %HashSet{root: root, size: size - 1}
      :error -> set
    end
  end

  @doc false
  def reduce(%HashSet{root: root}, acc, fun) do
    do_reduce(root, acc, fun, @node_size, fn
      {:suspend, acc} -> {:suspended, acc, &{:done, elem(&1, 1)}}
      {:halt, acc} -> {:halted, acc}
      {:cont, acc} -> {:done, acc}
    end)
  end

  @deprecated message
  def size(%HashSet{size: size}) do
    size
  end

  ## Set helpers

  defp set_fold(%HashSet{root: root}, acc, fun) do
    do_fold(root, acc, fun, @node_size)
  end

  ## Set manipulation

  defp do_member?(node, term, hash) do
    index = key_mask(hash)

    case elem(node, index) do
      [] -> false
      [^term | _] -> true
      [_] -> false
      [_ | n] -> do_member?(n, term, key_shift(hash))
    end
  end

  defp do_put(node, term, hash) do
    index = key_mask(hash)

    case elem(node, index) do
      [] ->
        {put_elem(node, index, [term]), 1}

      [^term | _] ->
        {node, 0}

      [t] ->
        n = put_elem(@node_template, key_mask(key_shift(hash)), [term])
        {put_elem(node, index, [t | n]), 1}

      [t | n] ->
        {n, counter} = do_put(n, term, key_shift(hash))
        {put_elem(node, index, [t | n]), counter}
    end
  end

  defp do_delete(node, term, hash) do
    index = key_mask(hash)

    case elem(node, index) do
      [] ->
        :error

      [^term] ->
        {:ok, put_elem(node, index, [])}

      [_] ->
        :error

      [^term | n] ->
        {:ok, put_elem(node, index, do_compact_node(n))}

      [t | n] ->
        case do_delete(n, term, key_shift(hash)) do
          {:ok, @node_template} ->
            {:ok, put_elem(node, index, [t])}

          {:ok, n} ->
            {:ok, put_elem(node, index, [t | n])}

          :error ->
            :error
        end
    end
  end

  Enum.each(0..(@node_size - 1), fn index ->
    defp do_compact_node(node) when elem(node, unquote(index)) != [] do
      case elem(node, unquote(index)) do
        [t] ->
          case put_elem(node, unquote(index), []) do
            @node_template -> [t]
            n -> [t | n]
          end

        [t | n] ->
          [t | put_elem(node, unquote(index), do_compact_node(n))]
      end
    end
  end)

  ## Set fold

  defp do_fold_each([], acc, _fun), do: acc
  defp do_fold_each([t], acc, fun), do: fun.(t, acc)
  defp do_fold_each([t | n], acc, fun), do: do_fold(n, fun.(t, acc), fun, @node_size)

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

  defp do_reduce_each([t | n], {:cont, acc}, fun, next) do
    do_reduce(n, fun.(t, acc), fun, @node_size, next)
  end

  defp do_reduce(node, acc, fun, count, next) when count > 0 do
    do_reduce_each(
      :erlang.element(count, node),
      acc,
      fun,
      &do_reduce(node, &1, fun, count - 1, next)
    )
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
  def reduce(set, acc, fun) do
    # Avoid warnings about HashSet being deprecated.
    module = HashSet
    module.reduce(set, acc, fun)
  end

  def member?(set, term) do
    # Avoid warnings about HashSet being deprecated.
    module = HashSet
    {:ok, module.member?(set, term)}
  end

  def count(set) do
    # Avoid warnings about HashSet being deprecated.
    module = HashSet
    {:ok, module.size(set)}
  end

  def slice(_set) do
    {:error, __MODULE__}
  end
end

defimpl Collectable, for: HashSet do
  def into(original) do
    # Avoid warnings about HashSet being deprecated.
    module = HashSet

    collector_fun = fn
      set, {:cont, term} -> module.put(set, term)
      set, :done -> set
      _, :halt -> :ok
    end

    {original, collector_fun}
  end
end

defimpl Inspect, for: HashSet do
  import Inspect.Algebra

  def inspect(set, opts) do
    # Avoid warnings about HashSet being deprecated.
    module = HashSet
    concat(["#HashSet<", Inspect.List.inspect(module.to_list(set), opts), ">"])
  end
end
