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

  def new(vals) do
    Enum.reduce vals, ordered(), fn v, set ->
      put(set, v)
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

  def put(set, value) do
    { set, _ } = set_put(set, { :put, value })
    set
  end

  def delete(set, value) do
    { set, _, _ } = set_delete(set, value)
    set
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

  ## Bucket helpers

  defp bucket_put([v|_]=bucket, { :put, value }) when v > value do
    { [value|bucket], 1 }
  end

  defp bucket_put([value|bucket], { :put, value }) do
    { [value|bucket], 0 }
  end

  defp bucket_put([e|bucket], value) do
    { rest, count } = bucket_put(bucket, value)
    { [e|rest], count }
  end

  defp bucket_put([], { :put, value }) do
    { [value], 1 }
  end

  # Deletes a key from the bucket
  defp bucket_delete([v,_|_]=bucket, value) when v > value do
    { bucket, nil, 0 }
  end

  defp bucket_delete([value|bucket], value) do
    { bucket, value, -1 }
  end

  defp bucket_delete([e|bucket], value) do
    { rest, v, count } = bucket_delete(bucket, value)
    { [e|rest], v, count }
  end

  defp bucket_delete([], _value) do
    { [], nil, 0 }
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
