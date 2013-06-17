defmodule HashSet do
  # The ordered record contains a single bucket.
  @ordered_threshold 8

  defrecordp :ordered,
    size: 0,
    bucket: []

  # The bucketed record contains a series of buckets.
  @expand_load 5
  @contract_load 2
  @node_bitmap 0b111
  @node_shift 3
  @node_size 8
  @node_template :erlang.make_tuple(@node_size, [])

  defrecordp :trie,
    size: 0,
    depth: 0,
    expand_on: @node_size * @expand_load,
    contract_on: @contract_load,
    root: @node_template

  import Bitwise

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

  @doc """
  Returns a set combining the two input sets.

  ## Examples

      iex> HashSet.union(HashSet.new([1,2]), HashSet.new([2,3,4])) |> HashSet.to_list
      [1,2,3,4]

  """
  def union(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) and elem(set1, 1) < elem(set2, 1) do
    set_fold set1, set2, fn v1, acc ->
      put(acc, v1)
    end
  end

  def union(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) do
    set_fold set2, set1, fn v, acc ->
      put(acc, v)
    end
  end

  @doc """
  Returns a set containing only members in common between the two input sets.

  ## Examples

      iex> HashSet.intersection(HashSet.new([1,2]), HashSet.new([2,3,4])) |> HashSet.to_list
      [2]

  """
  def intersection(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) do
    set_intersect(set1, set2)
  end

  @doc """
  Returns a set that is the first set without members of the second set.

  ## Examples

      iex> HashSet.difference(HashSet.new([1,2]), HashSet.new([2,3,4])) |> HashSet.to_list
      [1]

  """
  def difference(set1, set2) when is_record(set1, HashSet) and is_record(set2, HashSet) do
    set_difference(set1, set2)
  end

  @doc """
  Checks if the set has the given value.
  """
  def member?(set, member) when is_record(set, HashSet) do
    case set_get(set, member) do
        ^member -> true
        _       -> false
    end
  end

  @doc """
  Returns an empty set.
  """
  def empty(_) do
    ordered()
  end

  @doc """
  Returns the set size.
  """
  def size(set) do
    elem(set, 1)
  end

  @doc """
  Converts a set to a list.
  """
  def to_list(ordered(bucket: bucket)) do
    bucket
  end

  def to_list(set) do
    set_fold(set, [], [&1|&2]) |> :lists.reverse
  end

  @doc """
  Puts the given value into the set if it does not already contain it.
  """
  def put(set, member) do
    { set, _ } = set_put(set, { :put, member })
    set
  end

  @doc """
  Deletes a value from the set.
  """
  def delete(set, member) do
    { set, _, _ } = set_delete(set, member)
    set
  end

  @doc """
  Checks if two sets are equal
  """
  def equal?(set1, set2) do
    size = elem(set1, 1)
    case elem(set2, 1) do
      ^size ->
        set_equal?(set1, set2)
      _ ->
        false
    end
  end

  @doc """
  Checks if set1's members are all contained in set2.

  ## Examples

      iex> HashSet.subset?(HashSet.new([1, 2]), HashSet.new([1, 2, 3]))
      true
      iex> HashSet.subset?(HashSet.new([1, 2, 3]), HashSet.new([1, 2]))
      false
  """
  def subset?(set1, set2) do
    set_equal?(set1, set2)
  end

  @doc """
  Checks if set1 and set2 have no members in common.

  ## Examples

      iex> HashSet.disjoint?(HashSet.new([1, 2]), HashSet.new([3, 4]))
      true
      iex> HashSet.disjoint?(HashSet.new([1, 2]), HashSet.new([2, 3]))
      false

  """
  def disjoint?(set1, set2) do
    set_disjoint?(set1, set2)
  end

  @doc """
  Returns a set with members that tested as true with the passed function.

  ## Examples

      iex> HashSet.filter(HashSet.new([1, 2]), fn m -> m == 1 end) |> HashSet.to_list
      [1]

  """
  def filter(set, fun) do
    set_filter(set, fun)
  end

  def reduce(ordered(bucket: bucket), acc, fun) do
    :lists.foldl(fun, acc, bucket)
  end

  def reduce(trie() = set, acc, fun) do
    set_fold(set, acc, fun)
  end

  ## HashSet-wide functions

  defp set_intersect(ordered(size: size1) = set1, ordered(size: size2) = set2) when size1 <= size2 do
    set_filter set1, fn e ->
      member?(set2, e)
    end
  end

  defp set_intersect(ordered() = set1, ordered() = set2) do
    set_intersect(set2, set1)
  end

  defp set_intersect(trie(size: size1) = set1, trie(size: size2) = set2) when size1 > size2 do
    set_intersect(set2, set1)
  end

  defp set_intersect(trie() = set1, set2) do
    set_fold set2, ordered(), fn m, acc ->
      if member?(set1, m) do
        put acc, m
      else
        acc
      end
    end
  end

  defp set_difference(ordered() = set1, ordered() = set2) do
    set_filter set1, fn m ->
      !member?(set2, m)
    end
  end

  defp set_difference(trie() = set1, set2) do
    set_fold set2, set1, fn m, acc ->
      delete(acc, m)
    end
  end

  defp set_filter(ordered(bucket: bucket, size: size) = set, fun) do
    {new, removed_count} = filter_bucket(bucket, fun)
    ordered(bucket: new, size: size - removed_count)
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

  defp set_put(trie(root: root, size: size, depth: depth) = set,  { :put, member }) do
    pos = bucket_hash(member)
    { root, count } = node_put(root, depth, pos, {:put, member})
    { trie(set, size: size + count, root: root), count }
  end

  defp set_get(ordered(bucket: bucket), member) do
    bucket_get(bucket, member)
  end

  defp set_get(trie(root: root, depth: depth), member) do
    bucket_get(node_bucket(root, depth, bucket_hash(member)), member)
  end

  defp set_delete(ordered(bucket: bucket, size: size) = set, member) do
    case bucket_delete(bucket, member) do
      { _, value, 0 } ->
        { set, value, 0 }
      { new_bucket, value, -1 } ->
        { ordered(set, size: size - 1, bucket: new_bucket), value, -1 }
    end
  end

  defp set_delete(trie(root: root, size: size, depth: depth) = set, member) do
    pos = bucket_hash(member)
    case node_delete(root, depth, pos, member) do
      { _, value, 0 } ->
        { set, value, 0 }
      { root, _, -1 } ->
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
        end, member, -1 }
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

  defp filter_bucket([e|bucket], fun) do
    case fun.(e) do
      true  ->
        { new, count } = filter_bucket(bucket, fun)
        { [e | new], count }
      false ->
        { new, count } = filter_bucket(bucket, fun)
        { new, count + 1 }
    end
  end

  defp filter_bucket([], fun) do
    {[], 0}
  end

  defp bucket_put([m|_]=bucket, { :put, member }) when m > member do
    { [member|bucket], 1 }
  end

  defp bucket_put([member|bucket], { :put, member }) do
    { [member|bucket], 0 }
  end

  defp bucket_put([e|bucket], member) do
    { rest, count } = bucket_put(bucket, member)
    { [e|rest], count }
  end

  defp bucket_put([], { :put, member }) do
    { [member], 1 }
  end

  defp bucket_put!([m|_]=bucket, member)     when m > member, do: [member|bucket]
  defp bucket_put!([member|bucket], member), do: [member|bucket]
  defp bucket_put!([e|bucket], member),      do: [e|bucket_put!(bucket, member)]
  defp bucket_put!([], member),              do: [member]

  # Deletes a key from the bucket
  defp bucket_delete([m,_|_]=bucket, member) when m > member do
    { bucket, nil, 0 }
  end

  defp bucket_delete([member|bucket], member) do
    { bucket, member, -1 }
  end

  defp bucket_delete([e|bucket], member) do
    { rest, value, count } = bucket_delete(bucket, member)
    { [e|rest], value, count }
  end

  defp bucket_delete([], _member) do
    { [], nil, 0 }
  end

  defp bucket_get([member|_], member) do
    member
  end

  defp bucket_get([member|bucket], candidate) when candidate > member do
    bucket_get(bucket, candidate)
  end

  defp bucket_get(_, _member) do
    nil
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
      { _, value, 0 } -> { node, value, 0 }
      { new, value, -1 } -> { set_elem(node, pos, new), value, -1 }
    end
  end

  defp node_delete(node, depth, hash, member) do
    pos = bucket_index(hash)
    case node_delete(elem(node, pos), depth - 1, bucket_next(hash), member) do
      { _, value, 0 } -> { node, value, 0 }
      { new, value, -1 } -> { set_elem(node, pos, new), value, -1 }
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

  defp node_relocate(node // @node_template, bucket, n) do
    :lists.foldl fn member, acc ->
      pos = member |> bucket_hash() |> bucket_nth_index(n)
      set_elem(acc, pos, bucket_put!(elem(acc, pos), member))
    end, node, bucket
  end

  # Node resizing
  defp node_expand({ b1, b2, b3, b4, b5, b6, b7, b8 }, 0, n) do
    { node_relocate(b1, n), node_relocate(b2, n), node_relocate(b3, n),
      node_relocate(b4, n), node_relocate(b5, n), node_relocate(b6, n),
      node_relocate(b7, n), node_relocate(b8, n) }
  end

  defp node_expand({ b1, b2, b3, b4, b5, b6, b7, b8 }, depth, n) do
    depth = depth - 1
    { node_expand(b1, depth, n), node_expand(b2, depth, n), node_expand(b3, depth, n),
      node_expand(b4, depth, n), node_expand(b5, depth, n), node_expand(b6, depth, n),
      node_expand(b7, depth, n), node_expand(b8, depth, n) }
  end

  defp node_contract({ b1, b2, b3, b4, b5, b6, b7, b8 }, depth) when depth > 0 do
    depth = depth - 1
    { node_contract(b1, depth), node_contract(b2, depth), node_contract(b3, depth),
      node_contract(b4, depth), node_contract(b5, depth), node_contract(b6, depth),
      node_contract(b7, depth), node_contract(b8, depth) }
  end

  defp node_contract({ b1, b2, b3, b4, b5, b6, b7, b8 }, 0) do
    b1 |> each_contract(b2) |> each_contract(b3) |> each_contract(b4)
       |> each_contract(b5) |> each_contract(b6) |> each_contract(b7)
       |> each_contract(b8)
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

defimpl Binary.Inspect, for: HashSet do
  import Kernel, except: [inspect: 2]

  def inspect(set, opts) do
    "#HashSet<" <> Kernel.inspect(HashSet.to_list(set), opts) <> ">"
  end
end