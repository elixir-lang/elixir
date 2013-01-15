defmodule HashDict do
  # A dictionary (key-value) implementation based on dynamic hashing.
  #
  # This implementation is based on Erlang's dict implementation which
  # is based on "The Design and Implementation of Dynamic Hashing for
  # Sets and Tables in Icon" by Griswold and Townsend.
  #
  # However, compared to dict, it provides many enhancements:
  #
  # 1. Buckets are ordered sets, this gives us faster access and
  #    modification times
  # 2. It uses erlang:phash2 to calculate the hash
  # 3. The dictionary first starts with a single bucket, instead of
  #    a segment with 16 buckets. This allow us to skip hashing
  #    altogher for small dictionaries, providing faster operations
  #    and reducing memory consumption
  # 4. Once we reach 12 elements, the bucket is promoted to a segment
  #    (i.e. a set of buckets). At this point, it works as a simple
  #    hash table with fixed size of 32
  # 5. Once the segment starts having high density (i.e. 32 * 5 items),
  #    we promote it to a full Dict, similar to one that we would find
  #    in Erlang. However, since this promotion happens just late, we
  #    can afford larger segments which results in faster times
  #
  # Notice that promotions are not expensive because a dict is actually
  # made of segments, and each segment is made of buckets. Starting with
  # a bucket simply allows us to remove considerably overhead if we had
  # to manage all those structures from scratch.

  # Constants values used to rehash the dictionary
  @expand_load 5
  @contract_load 3
  @segment_size 32
  @segment_template :erlang.make_tuple(@segment_size, [])
  @ordered_threshold 12
  @bucketed_threshold @segment_size * @expand_load

  # The ordered record contains a single bucket
  defrecordp :ordered,
    size: 0,
    bucket: []

  # The bucketed record contains a series of buckets
  # represented exactly by one segment
  defrecordp :bucketed,
    size: 0,
    buckets: @segment_template

  # The segmented record contains one or more segments
  # (i.e. a series of buckets)
  defrecordp :segmented,
    size: 0,
    n: @segment_size,
    maxn: @segment_size,
    bso: div(@segment_size, 2),
    exp_size: @expand_load * @segment_size,
    con_size: @contract_load * @segment_size,
    segments: nil

  @doc """
  Creates an instance of HashDict.
  """
  def new do
    ordered()
  end

  @doc """
  Puts the given key and value in the dict.
  """
  def put(dict, key, value)

  def put(ordered(size: size, bucket: bucket) = dict, key, value) when is_list(bucket) do
    case bucket_put(bucket, key, value) do
      { new_bucket, 0 } -> ordered(dict, bucket: new_bucket)
      { new_bucket, 1 } ->
        if size == @ordered_threshold do
          buckets = rehash_bucket(new_bucket, @segment_template)
          bucketed(size: size + 1, buckets: buckets)
        else
          ordered(dict, size: size + 1, bucket: new_bucket)
        end
    end
  end

  def put(bucketed(buckets: buckets, size: size) = dict, key, value) do
    pos = bucket_hash(key)
    case bucket_put(elem(buckets, pos), key, value) do
      { new_bucket, 0 } ->
        bucketed(dict, buckets: setelem(buckets, pos, new_bucket))
      { new_bucket, 1 } ->
        if size == @bucketed_threshold do
          maybe_expand segmented(size: size), { setelem(buckets, pos, new_bucket) }, 1
        else
          bucketed(dict, size: size + 1, buckets: setelem(buckets, pos, new_bucket))
        end
    end
  end

  def put(segmented(segments: segments) = dict, key, value) do
    slot = bucket_slot(dict, key)
    { segments, count } = segments_update segments, slot, bucket_put(&1, key, value)
    maybe_expand dict, segments, count
  end

  @doc """
  Gets a value from the dict.
  """
  def get(dict, key, default // nil)

  def get(ordered(bucket: bucket), key, default) when is_list(bucket) do
    bucket_get(bucket, key, default)
  end

  def get(bucketed(buckets: buckets), key, default) do
    pos = bucket_hash(key)
    bucket_get(elem(buckets, pos), key, default)
  end

  def get(segmented(segments: segments) = dict, key, default) do
    slot   = bucket_slot(dict, key)
    bucket = segments_get(segments, slot)
    bucket_get(bucket, key, default)
  end

  @doc """
  Deletes a value from the dict.
  """
  def delete(ordered(bucket: bucket, size: size) = dict, key) when is_list(bucket) do
    case bucket_delete(bucket, key) do
      { _, 0 } ->
        ordered
      { new_bucket, -1 } ->
        ordered(dict, size: size - 1, bucket: new_bucket)
    end
  end

  def delete(bucketed(buckets: buckets, size: size) = dict, key) do
    pos = bucket_hash(key)
    case bucket_delete(elem(buckets, pos), key) do
      { _, 0 } ->
        bucketed
      { new_bucket, -1 } ->
        bucketed(dict, size: size - 1, buckets: setelem(buckets, pos, new_bucket))
    end
  end

  def delete(segmented(segments: segments) = dict, key) do
    slot = bucket_slot(dict, key)
    { segments, count } = segments_update segments, slot, bucket_delete(&1, key)
    maybe_contract dict, segments, count
  end

  @doc """
  Returns the dict size.
  """
  def size(dict) do
    elem(dict, 1)
  end

  ## Bucket helpers

  # Gets a value from the bucket
  defp bucket_get(bucket, key, default) do
    case :lists.keyfind(key, 1, bucket) do
      { ^key, value } -> value
      false -> default
    end
  end

  # Puts a value in the bucket
  defp bucket_put([{k,_}=e|bucket], key, value) when key < k do
    { [{key,value},e|bucket], 1 }
  end

  defp bucket_put([{k,_}=e|bucket], key, value) when key > k do
    { rest, count } = bucket_put(bucket, key, value)
    { [e|rest], count }
  end

  defp bucket_put([{_,_}|bucket], key, value) do
    { [{key,value}|bucket], 0 }
  end

  defp bucket_put([], key, value) do
    { [{key,value}], 1 }
  end

  # Puts a value in the bucket without returning
  # the operation value
  defp bucket_put!([{k,_}=e|bucket], key, value) when key < k, do: [{key,value},e|bucket]
  defp bucket_put!([{k,_}=e|bucket], key, value) when key > k, do: [e|bucket_put!(bucket, key, value)]
  defp bucket_put!([{_,_}|bucket], key, value), do: [{key,value}|bucket]
  defp bucket_put!([], key, value), do: [{key,value}]

  # Deletes a key from the bucket
  defp bucket_delete([{k,_}|_] = bucket, key) when key < k do
    { bucket, 0 }
  end

  defp bucket_delete([{k,_}=e|bucket], key) when key > k do
    { rest, count } = bucket_delete(bucket, key)
    { [e|rest], count }
  end

  defp bucket_delete([{_,_}|bucket], _key) do
    { bucket, -1 }
  end

  defp bucket_delete([], _key) do
    { [], 0 }
  end

  # Merges two buckets
  defp bucket_merge([{k1, _} = e1|d1], [{k2, _} = e2|d2]) when k1 < k2 do
    [e1|bucket_merge(d1, [e2|d2])]
  end

  defp bucket_merge([{k1, _} = e1|d1], [{k2, _} = e2|d2]) when k1 > k2 do
    [e2|bucket_merge([e1|d1], d2)]
  end

  defp bucket_merge([], d2), do: d2
  defp bucket_merge(d1, []), do: d1

  # Rehashes a bucket into a given segment
  defp rehash_bucket(bucket, segment) do
    :lists.foldl fn { key, value }, acc ->
      pos = bucket_hash(key)
      setelem(acc, pos, bucket_put!(elem(acc, pos), key, value))
    end, segment, bucket
  end

  defp bucket_hash(key) do
    :erlang.phash2(key, @segment_size)
  end

  ## Segments helpers

  # Gets a bucket from a set of segments
  defp segments_get(segments, slot) do
    segment_pos = div(slot, @segment_size)
    bucket_pos  = rem(slot, @segment_size)
    elem(elem(segments, segment_pos), bucket_pos)
  end

  # Puts a bucket in a set of segments
  defp segments_put(segments, slot, bucket) do
    segment_pos = div(slot, @segment_size)
    bucket_pos  = rem(slot, @segment_size)
    segment = setelem(elem(segments, segment_pos), bucket_pos, bucket)
    setelem(segments, segment_pos, segment)
  end

  # Update a bucket in a set of segments
  defp segments_update(segments, slot, fun) do
    segment_pos = div(slot, @segment_size)
    bucket_pos  = rem(slot, @segment_size)

    segment = elem(segments, segment_pos)
    bucket  = elem(segment, bucket_pos)

    { bucket, res } = fun.(bucket)

    segment = setelem(segment, bucket_pos, bucket)
    { setelem(segments, segment_pos, segment), res }
  end

  defp bucket_slot(segmented(maxn: maxn, n: n, bso: bso), key) do
    h = :erlang.phash2(key, maxn)
    if h >= n, do: h - bso, else: h
  end

  defp rehash([{ key, _ } = keybag|t], slot1, slot2, maxn) do
    { l1, l2 } = rehash(t, slot1, slot2, maxn)
    case :erlang.phash2(key, maxn) do
      ^slot1 -> { [keybag|l1], l2 }
      ^slot2 -> { l1, [keybag|l2] }
    end
  end

  defp rehash([], _, _, _), do: { [], [] }

  defp maybe_expand(dict, segments, 0) do
    segmented(dict, segments: segments)
  end

  defp maybe_expand(segmented(size: size, exp_size: exp_size) = dict, segments, 1) when size >= exp_size do
    dict = maybe_expand_segments(dict, segments)
    n    = segmented(dict, :n) + 1

    segments = segmented(dict, :segments)
    slot1    = (n - segmented(dict, :bso)) - 1
    bucket   = segments_get(segments, slot1)
    slot2    = (n - 1)
    {b1,b2}  = rehash(bucket, slot1, slot2, segmented(dict, :maxn))
    segments = segments_put(segments, slot1, b1)
    segments = segments_put(segments, slot2, b2)

    segmented(dict,
      size: size + 1,
      n: n,
      exp_size: n * @expand_load,
      con_size: n * @contract_load,
      segments: segments)
  end

  defp maybe_expand(segmented(size: size) = dict, segments, 1) do
    segmented(dict, size: size + 1, segments: segments)
  end

  defp maybe_expand_segments(segmented(n: n, maxn: maxn, bso: bso) = dict, segments) when n == maxn do
    segmented(dict,
      maxn: 2 * maxn,
      bso: 2 * bso,
      segments: expand_segments(segments, @segment_template))
  end

  defp maybe_expand_segments(dict, segments) do
    segmented(dict, segments: segments)
  end

  defp expand_segments({b1}, empty), do:
    {b1,empty}
  defp expand_segments({b1,b2}, empty), do:
    {b1,b2,empty,empty}
  defp expand_segments({b1,b2,b3,b4}, empty), do:
    {b1,b2,b3,b4,empty,empty,empty,empty}
  defp expand_segments({b1,b2,b3,b4,b5,b6,b7,b8}, empty), do:
    {b1,b2,b3,b4,b5,b6,b7,b8,
     empty,empty,empty,empty,empty,empty,empty,empty}
  defp expand_segments({b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16}, empty), do:
    {b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,
     empty,empty,empty,empty,empty,empty,empty,empty,
     empty,empty,empty,empty,empty,empty,empty,empty}
  defp expand_segments(segs, empty), do:
    list_to_tuple(tuple_to_list(segs) ++ :lists.duplicate(tuple_size(segs), empty))

  defp maybe_contract(dict, segments, 0) do
    segmented(dict, segments: segments)
  end

  defp maybe_contract(segmented(size: size, con_size: con_size, n: n) = dict, segments, -1)
      when size <= con_size and n > @segment_size do
    slot1 = (n - segmented(dict, :bso)) - 1
    b1    = segments_get(segments, slot1)
    slot2 = (n - 1)
    b2    = segments_get(segments, slot2)

    segments = segments_put(segments, slot1, bucket_merge(b1, b2))
    segments = segments_put(segments, slot2, [])

    n    = n - 1
    dict = segmented(dict,
      n: n,
      size: size - 1,
      exp_size: n * @expand_load,
      con_size: n * @contract_load,
      segments: segments)
    maybe_contract_segments(dict)
  end

  defp maybe_contract(segmented(size: size) = dict, segments, -1) do
    segmented(dict, size: size - 1, segments: segments)
  end

  defp maybe_contract_segments(segmented(n: n, maxn: maxn, bso: bso, segments: segments) = dict) when n == bso do
    segmented(dict,
      maxn: div(maxn, 2),
      bso: div(bso, 2),
      segments: contract_segments(segments))
  end

  defp maybe_contract_segments(dict), do: dict

  defp contract_segments({b1,_}), do:
    {b1}
  defp contract_segments({b1,b2,_,_}), do:
    {b1,b2}
  defp contract_segments({b1,b2,b3,b4,_,_,_,_}), do:
    {b1,b2,b3,b4}
  defp contract_segments({b1,b2,b3,b4,b5,b6,b7,b8,_,_,_,_,_,_,_,_}), do:
    {b1,b2,b3,b4,b5,b6,b7,b8}
  defp contract_segments({b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,
      _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}), do:
    {b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16}
  defp contract_segments(segs), do:
    list_to_tuple(:lists.sublist(tuple_to_list(segs), 1, div tuple_size(segs), 2))
end
