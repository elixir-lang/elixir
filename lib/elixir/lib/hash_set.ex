defmodule HashSet do
  @moduledoc """
  A set store.

  The `HashSet` is meant to work well with both small and
  large sets. It is an implementation of the `Set` behaviour.
  For more information about the functions and their APIs,
  please consult the `Set` module.
  """

  @behaviour Set

  # The ordered record contains a single bucket.
  @ordered_threshold 8

  defrecordp :ordered, HashSet,
    size: 0,
    bucket: []

  # The bucketed record contains a series of buckets.
  @expand_load 5
  @contract_load 2
  @node_bitmap 0b1111
  @node_shift 4
  @node_size 16
  @node_template :erlang.make_tuple(@node_size, [])

  @expand_default (@node_size * @expand_load)
  @contract_default @contract_load

  defrecordp :trie, HashSet,
    size: 0,
    depth: 0,
    expand_on: @expand_default,
    contract_on: @contract_default,
    root: @node_template

  import Bitwise

  @compile :inline_list_funcs
  @compile { :inline, bucket_hash: 1, bucket_index: 1, bucket_nth_index: 2, bucket_next: 1 }

  @doc """
  Creates a new empty set.
  """
  def new() do
    ordered()
  end

  @doc """
  Creates a new set from the given enumerable.

  ## Examples

      iex> HashSet.new [1, 1, 2, 3, 3] |> HashSet.to_list
      [1,2,3]

  """
  def new(members) do
    Enum.reduce members, ordered(), fn member, set ->
      put(set, member)
    end
  end

  def union(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) and elem(set1, 1) <= elem(set2, 1) do
    set_fold set1, set2, fn v1, acc ->
      put(acc, v1)
    end
  end

  def union(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) do
    set_fold set2, set1, fn v, acc ->
      put(acc, v)
    end
  end

  def intersection(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) and elem(set1, 1) <= elem(set2, 1) do
    set_filter set1, fn e -> set_member?(set2, e) end
  end

  def intersection(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) do
    set_filter set2, fn e -> set_member?(set1, e) end
  end

  def difference(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) do
    set_filter set1, fn m -> not set_member?(set2, m) end
  end

  def member?(set, member) when is_record(set, HashSet) do
    set_member?(set, member)
  end

  def empty(_) do
    ordered()
  end

  def size(set) do
    elem(set, 1)
  end

  def to_list(ordered(bucket: bucket)) do
    bucket
  end

  def to_list(set) do
    set_fold(set, [], &[&1|&2]) |> :lists.reverse
  end

  def put(set, member) do
    { set, _ } = set_put(set, member)
    set
  end

  def delete(set, member) do
    { set, _ } = set_delete(set, member)
    set
  end

  def equal?(set1, set2) do
    size = elem(set1, 1)
    case elem(set2, 1) do
      ^size ->
        set_equal?(set1, set2)
      _ ->
        false
    end
  end

  def subset?(set1, set2) do
    set_equal?(set1, set2)
  end

  def disjoint?(set1, set2) do
    set_disjoint?(set1, set2)
  end

  @doc false
  def reduce(ordered(bucket: bucket), acc, fun) do
    :lists.foldl(fun, acc, bucket)
  end

  def reduce(trie() = set, acc, fun) do
    set_fold(set, acc, fun)
  end

  ## HashSet-wide functions

  defp set_filter(ordered(bucket: bucket, size: size), fun) do
    { new, removed_count } = bucket_filter(bucket, fun, [], 0)
    ordered(bucket: new, size: size - removed_count)
  end

  defp set_filter(trie(root: root, depth: depth, size: size) = set, fun) do
    { new, removed_count } = node_filter(root, depth, fun, @node_size)

    if depth > 0 and trie(set, :contract_on) >= (size - removed_count) do
      contract_trie(trie(root: new,
                         size: size - removed_count,
                         depth: depth,
                         contract_on: trie(set, :contract_on),
                         expand_on:   trie(set, :expand_on)))
    else
      trie(size: size - removed_count, root: new, depth: depth)
    end
  end

  defp set_put(ordered(size: @ordered_threshold, bucket: bucket), member) do
    root = node_relocate(bucket, 0)
    set_put(trie(size: @ordered_threshold, root: root), member)
  end

  defp set_put(ordered(size: size, bucket: bucket) = set, member) do
    { new, count } = bucket_put(bucket, member)
    { ordered(set, size: size + count, bucket: new), count }
  end

  defp set_put(trie(root: root, depth: depth, size: size, expand_on: size, contract_on: contract_on) = set, member) do
    root = node_expand(root, depth, depth + 1)
    set = trie(set, root: root, depth: depth + 1,
      expand_on: size * @node_size, contract_on: contract_on * @node_size)
    set_put(set, member)
  end

  defp set_put(trie(root: root, size: size, depth: depth) = set, member) do
    pos = bucket_hash(member)
    { root, count } = node_put(root, depth, pos, member)
    { trie(set, size: size + count, root: root), count }
  end

  defp set_member?(ordered(bucket: bucket), member) do
    :lists.member(member, bucket)
  end

  defp set_member?(trie(root: root, depth: depth), member) do
    :lists.member(member, node_bucket(root, depth, bucket_hash(member)))
  end

  defp set_delete(ordered(bucket: bucket, size: size) = set, member) do
    case bucket_delete(bucket, member) do
      { _, 0 } ->
        { set, 0 }
      { new_bucket, -1 } ->
        { ordered(set, size: size - 1, bucket: new_bucket), -1 }
    end
  end

  defp set_delete(trie(root: root, size: size, depth: depth) = set, member) do
    pos = bucket_hash(member)
    case node_delete(root, depth, pos, member) do
      { _, 0 } ->
        { set, 0 }
      { root, -1 } ->
        { if depth > 0 and trie(set, :contract_on) == size do
          root = node_contract(root, depth)
          trie(set,
            root: root,
            size: size - 1,
            depth: depth - 1,
            contract_on: div(size, @node_size),
            expand_on: div(trie(set, :expand_on), @node_size))
        else
          trie(set, size: size - 1, root: root)
        end, -1 }
    end
  end

  defp set_equal?(set1, set2) do
    try do
      reduce(set1, true, fn member, acc ->
        case member?(set2, member) do
          true -> acc
          _    -> throw(:error)
        end
      end)
    catch
      :error -> false
    end
  end

  defp set_disjoint?(set1, set2) do
    try do
      reduce(set1, true, fn member, acc ->
        case member?(set2, member) do
          false -> acc
          _     -> throw(:error)
        end
      end)
    catch
      :error -> false
    end
  end

  defp set_fold(ordered(bucket: bucket), acc, fun) do
    bucket_fold(bucket, acc, fun)
  end

  defp set_fold(trie(root: root, depth: depth), acc, fun) do
    node_fold(root, depth, acc, fun, @node_size)
  end

  ## Bucket helpers

  defp bucket_filter([e|bucket], fun, acc, count) do
    case fun.(e) do
      true  -> bucket_filter(bucket, fun, [e|acc], count)
      false -> bucket_filter(bucket, fun, acc, count + 1)
    end
  end

  defp bucket_filter([], _fun, acc, count) do
    { :lists.reverse(acc), count }
  end

  defp bucket_put([m|_]=bucket, member) when m > member do
    { [member|bucket], 1 }
  end

  defp bucket_put([member|bucket], member) do
    { [member|bucket], 0 }
  end

  defp bucket_put([e|bucket], member) do
    { rest, count } = bucket_put(bucket, member)
    { [e|rest], count }
  end

  defp bucket_put([], member) do
    { [member], 1 }
  end

  defp bucket_put!([m|_]=bucket, member)     when m > member, do: [member|bucket]
  defp bucket_put!([member|bucket], member), do: [member|bucket]
  defp bucket_put!([e|bucket], member),      do: [e|bucket_put!(bucket, member)]
  defp bucket_put!([], member),              do: [member]

  # Deletes a key from the bucket
  defp bucket_delete([m,_|_]=bucket, member) when m > member do
    { bucket, 0 }
  end

  defp bucket_delete([member|bucket], member) do
    { bucket, -1 }
  end

  defp bucket_delete([e|bucket], member) do
    { rest, count } = bucket_delete(bucket, member)
    { [e|rest], count }
  end

  defp bucket_delete([], _member) do
    { [], 0 }
  end

  defp bucket_fold(bucket, acc, fun) do
    :lists.foldl(fun, acc, bucket)
  end

  defp bucket_hash(key) do
    :erlang.phash2(key)
  end

  defp bucket_nth_index(hash, n) do
    (hash >>> (@node_shift * n)) &&& @node_bitmap
  end

  defp bucket_index(hash) do
    hash &&& @node_bitmap
  end

  defp bucket_next(hash) do
    hash >>> @node_shift
  end

  # Trie resizing

  defp contract_trie(trie(depth: 0) = set) do
    set
  end

  defp contract_trie(trie(root: root, depth: depth, size: size, contract_on: contract_on, expand_on: expand_on) = set) when size <= contract_on do
    new_contract_on = div(contract_on, @node_size)
    new_expand_on   = div(expand_on, @node_size)

    if new_contract_on == 0, do: new_contract_on =  @contract_default
    if new_expand_on == 0,   do: new_expand_on = @expand_default

    contract_trie(trie(set, root: node_contract(root, depth),
                             size: size,
                             depth: depth - 1,
                             contract_on: new_contract_on,
                             expand_on: new_expand_on))
  end

  defp contract_trie(set) do
    set
  end

  # Node helpers

  # Gets a bucket from the node
  defp node_bucket(node, 0, hash) do
    elem(node, bucket_index(hash))
  end

  defp node_bucket(node, depth, hash) do
    child = elem(node, bucket_index(hash))
    node_bucket(child, depth - 1, bucket_next(hash))
  end

  defp node_put(node, 0, hash, member) do
    pos = bucket_index(hash)
    { new, count } = bucket_put(elem(node, pos), member)
    { set_elem(node, pos, new), count }
  end

  defp node_put(node, depth, hash, member) do
    pos = bucket_index(hash)
    { new, count } = node_put(elem(node, pos), depth - 1, bucket_next(hash), member)
    { set_elem(node, pos, new), count }
  end

  # Deletes a key from the bucket
  defp node_delete(node, 0, hash, member) do
    pos = bucket_index(hash)
    case bucket_delete(elem(node, pos), member) do
      { _, 0 } -> { node, 0 }
      { new, -1 } -> { set_elem(node, pos, new), -1 }
    end
  end

  defp node_delete(node, depth, hash, member) do
    pos = bucket_index(hash)
    case node_delete(elem(node, pos), depth - 1, bucket_next(hash), member) do
      { _, 0 } -> { node, 0 }
      { new, -1 } -> { set_elem(node, pos, new), -1 }
    end
  end

  defp node_fold(bucket, -1, acc, fun, _) do
    bucket_fold(bucket, acc, fun)
  end

  defp node_fold(node, depth, acc, fun, count) when count >= 1 do
    acc = node_fold(:erlang.element(count, node), depth - 1, acc, fun, @node_size)
    node_fold(node, depth, acc, fun, count - 1)
  end

  defp node_fold(_node, _, acc, _fun, 0) do
    acc
  end

  defp node_filter(bucket, -1, fun, _) do
    bucket_filter(bucket, fun, [], 0)
  end

  defp node_filter(node, depth, fun, count) when count >= 1 do
    case node_filter(:erlang.element(count, node), depth - 1, fun, @node_size) do
      { _, 0 } ->
        node_filter(node, depth, fun, count - 1)
      { new_element, count1 } ->
        { new_node, count2 } = node_filter(:erlang.setelement(count, node, new_element), depth, fun, count - 1)
        { new_node, count1 + count2 }
    end
  end

  defp node_filter(node, _,  _fun, 0) do
   { node, 0 }
  end

  defp node_relocate(node // @node_template, bucket, n) do
    :lists.foldl fn member, acc ->
      pos = member |> bucket_hash() |> bucket_nth_index(n)
      set_elem(acc, pos, bucket_put!(elem(acc, pos), member))
    end, node, bucket
  end

  # Node resizing
  defp node_expand({ b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16 }, 0, n) do
    { node_relocate(b1, n), node_relocate(b2, n), node_relocate(b3, n),
      node_relocate(b4, n), node_relocate(b5, n), node_relocate(b6, n),
      node_relocate(b7, n), node_relocate(b8, n), node_relocate(b9, n),
      node_relocate(b10, n), node_relocate(b11, n), node_relocate(b12, n),
      node_relocate(b13, n), node_relocate(b14, n), node_relocate(b15, n),
      node_relocate(b16, n) }
  end

  defp node_expand({ b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16 }, depth, n) do
    depth = depth - 1
    { node_expand(b1, depth, n), node_expand(b2, depth, n), node_expand(b3, depth, n),
      node_expand(b4, depth, n), node_expand(b5, depth, n), node_expand(b6, depth, n),
      node_expand(b7, depth, n), node_expand(b8, depth, n), node_expand(b9, depth, n),
      node_expand(b10, depth, n), node_expand(b11, depth, n), node_expand(b12, depth, n),
      node_expand(b13, depth, n), node_expand(b14, depth, n), node_expand(b15, depth, n),
      node_expand(b16, depth, n) }
  end

  defp node_contract({ b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16 }, depth) when depth > 0 do
    depth = depth - 1
    { node_contract(b1, depth), node_contract(b2, depth), node_contract(b3, depth),
      node_contract(b4, depth), node_contract(b5, depth), node_contract(b6, depth),
      node_contract(b7, depth), node_contract(b8, depth), node_contract(b9, depth),
      node_contract(b10, depth), node_contract(b11, depth), node_contract(b12, depth),
      node_contract(b13, depth), node_contract(b14, depth), node_contract(b15, depth),
      node_contract(b16, depth) }
  end

  defp node_contract({ b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16 }, 0) do
    b1 |> each_contract(b2) |> each_contract(b3) |> each_contract(b4)
       |> each_contract(b5) |> each_contract(b6) |> each_contract(b7)
       |> each_contract(b8) |> each_contract(b9) |> each_contract(b10)
       |> each_contract(b11) |> each_contract(b12) |> each_contract(b13)
       |> each_contract(b14) |> each_contract(b15) |> each_contract(b16)
  end

  defp each_contract([m1|acc], [m2|_]=bucket) when m1 < m2, do: [m1|each_contract(acc, bucket)]
  defp each_contract(acc, [m|bucket]), do: [m|each_contract(acc, bucket)]
  defp each_contract([], bucket), do: bucket
  defp each_contract(acc, []), do: acc
end

defimpl Enumerable, for: HashSet do
  def reduce(set, acc, fun), do: HashSet.reduce(set, acc, fun)
  def member?(set, v),       do: HashSet.member?(set, v)
  def count(set),            do: HashSet.size(set)
end
