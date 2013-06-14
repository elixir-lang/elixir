defmodule Set do
  # The ordered record contains a single bucket
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

  def new() do
    ordered()
  end

  def new(members) do
    Enum.reduce members, ordered(), fn member, set ->
      put(set, member)
    end
  end

  def union(set1, set2) when is_record(set1, Set) and is_record(set2, Set) and elem(set1, 1) < elem(set2, 1) do
    set_fold set1, set2, fn v1, acc ->
      put(acc, v1)
    end
  end

  def union(set1, set2) when is_record(set1, Set) and is_record(set2, Set) do
    set_fold set2, set1, fn v, acc ->
      put(acc, v)
    end
  end

  def intersection(set1, set2) when is_record(set1, Set) and is_record(set2, Set) do
    set_intersect(set1, set2)
  end

  def difference(set1, set2) when is_record(set1, Set) and is_record(set2, Set) do
    set_difference(set1, set2)
  end

  def member?(set, member) when is_record(set, Set) do
    case set_get(set, member) do
        ^member -> true
        _       -> false
    end
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

  def reduce(ordered(bucket: bucket), acc, fun) do
    :lists.foldl(fun, acc, bucket)
  end

  def reduce(trie() = set, acc, fun) do
    set_fold(set, acc, fun)
  end

  def put(set, member) do
    { set, _ } = set_put(set, { :put, member })
    set
  end

  def delete(set, member) do
    { set, _, _ } = set_delete(set, member)
    set
  end

  def to_list(ordered(bucket: bucket)) do
    bucket
  end

  def to_list(set) do
    set_fold(set, [], [&1|&2]) |> :lists.reverse
  end

  def reduce(ordered(bucket: bucket), acc, fun) do
    :lists.foldl(fun, acc, bucket)
  end

  ## Set-wide functions

  defp set_intersect(ordered(bucket: bucket1), ordered(bucket: bucket2)) do
    new(bucket_intersect(bucket1, bucket2))
  end

  defp set_difference(ordered(bucket: bucket1), ordered(bucket: bucket2)) do
    new(bucket_difference(bucket1, bucket2))
  end

  defp set_put(ordered(size: @ordered_threshold, bucket: bucket), member) do
    root = node_relocate(bucket, 0)
    set_put(trie(size: @ordered_threshold, root: root), member)
  end

  defp set_put(ordered(size: size, bucket: bucket) = set, member) do
    { new, count } = bucket_put(bucket, member)
    { ordered(set, size: size + count, bucket: new), count }
  end

  defp set_put(trie(root: root, size: size, depth: depth) = set, member) do
    pos = bucket_hash(member)
    { root, count } = node_put(root, depth, pos, member)
    { trie(set, size: size + count, root: root), count }
  end

  defp set_get(ordered(bucket: bucket), member) do
    bucket_get(bucket, member)
  end

  defp set_delete(ordered(bucket: bucket, size: size) = set, member) do
    case bucket_delete(bucket, member) do
      { _, value, 0 } ->
        { set, value, 0 }
      { new_bucket, value, -1 } ->
        { ordered(set, size: size - 1, bucket: new_bucket), value, -1 }
    end
  end

  defp set_fold(ordered(bucket: bucket), acc, fun) do
    bucket_fold(bucket, acc, fun)
  end

  defp set_fold(trie(root: root, depth: depth), acc, fun) do
    node_fold(root, depth, acc, fun, @node_size)
  end

  ## Bucket helpers

  defp bucket_intersect([m1 | _] = bucket1, [m2 | bucket2]) when m1 > m2 do
    bucket_intersect(bucket1, bucket2)
  end

  defp bucket_intersect([m1 | bucket1], [m2 | _] = bucket2) when m2 > m1 do
    bucket_intersect(bucket1, bucket2)
  end

  defp bucket_intersect([member | bucket1], [member | bucket2]) do
    [member | bucket_intersect(bucket1, bucket2)]
  end

  defp bucket_intersect([], _) do
    []
  end

  defp bucket_intersect(_, []) do
    []
  end

  defp bucket_difference([m | bucket1], [m | bucket2]) do
    bucket_difference(bucket1, bucket2)
  end

  defp bucket_difference([m1 | _] = bucket1, [m2 | bucket2]) when m1 > m2 do
    [m2 | bucket_difference(bucket1, bucket2)]
  end

  defp bucket_difference([m1 | bucket1], [m2 | _] = bucket2) when m2 > m1 do
    [m1 | bucket_difference(bucket1, bucket2)]
  end

  defp bucket_difference([], bucket) do
    bucket
  end

  defp bucket_difference(bucket, []) do
    bucket
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

  defp bucket_get([member|bucket], member) do
    member
  end

  defp bucket_get([member|bucket], candidate) when candidate > member do
    bucket_get(bucket, candidate)
  end

  defp bucket_get(_, member) do
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

end

defimpl Binary.Inspect, for: Set do
  import Kernel, except: [inspect: 2]

  def inspect(set, opts) do
    "#Set<" <> Kernel.inspect(Set.to_list(set), opts) <> ">"
  end
end

defimpl Enumerable, for: Set do
  def reduce(set, acc, fun),  do: Set.reduce(set, acc, fun)
end
