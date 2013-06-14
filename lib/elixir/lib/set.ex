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

  def put(set, member) do
    { set, _ } = set_put(set, { :put, member })
    set
  end

  def delete(set, member) do
    { set, _, _ } = set_delete(set, member)
    set
  end

  ## Set-wide functions

  defp set_intersect(ordered(bucket: bucket1), ordered(bucket: bucket2)) do
    new_bucket = bucket_intersect(bucket1, bucket2)
    new(new_bucket)
  end

  defp set_put(ordered(size: size, bucket: bucket) = set, member) do
    { new, count } = bucket_put(bucket, member)
    { ordered(set, size: size + count, bucket: new), count }
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

  defp bucket_fold(bucket, acc, fun) do
    :lists.foldl(fun, acc, bucket)
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
