defmodule HashDict do
  @moduledoc """
  A key-value store.

  The `HashDict` is meant to work well with both small and
  large set of keys and it is an implementation of the `Dict`
  behaviour. For more information about the functions and
  their APIs, please consult the `Dict` module.
  """

  use Dict.Behaviour

  # A dictionary (key-value) implementation based on dynamic hashing.
  #
  # This implementation is based on hash tries. We first start with
  # a set of 8 buckets and expand when the density is about 5 entries
  # per bucket. We use bit shifting to make rehashing faster on
  # expansion.
  #
  # Compared to dict, it provides many enhancements:
  #
  # 1. HashDict buckets are ordered sets, this gives us faster access
  #    and modification times
  #
  # 2. It uses phash2 to calculate the hash (instead of phash)
  #
  # 3. The dictionary first starts with a single bucket, instead of
  #    a set of 8 buckets. This allow us to skip hashing altogher
  #    for small dictionaries, providing faster operations and
  #    reducing memory consumption
  #
  # 4. Once we reach 8 elements, the dictionary is promoted to a
  #    set of buckets

  # The ordered record contains a single bucket
  @ordered_threshold 8

  defrecordp :ordered, HashDict,
    size: 0,
    bucket: []

  # The bucketed record contains a series of buckets.
  @expand_load 5
  @contract_load 2
  @node_bitmap 0b111
  @node_shift 3
  @node_size 8
  @node_template :erlang.make_tuple(@node_size, [])

  defrecordp :trie, HashDict,
    size: 0,
    depth: 0,
    expand_on: @node_size * @expand_load,
    contract_on: @contract_load,
    root: @node_template

  import Bitwise

  # Let's inline common instructions
  @compile :inline_list_funcs
  @compile { :inline, bucket_hash: 1, bucket_index: 1, bucket_nth_index: 2, bucket_next: 1 }

  @doc """
  Creates a new empty dict.
  """
  @spec new :: Dict.t
  def new do
    ordered()
  end

  @doc """
  Creates a new dict from the given enumerable.

  ## Examples

      HashDict.new [{:b, 1}, {:a, 2}]
      #=> #HashDict<[a: 2, b: 1]>

  """
  @spec new(list({key :: term, value :: term})) :: Dict.t
  def new(pairs) do
    Enum.reduce pairs, ordered(), fn { k, v }, dict ->
      put(dict, k, v)
    end
  end

  @doc """
  Creates a new dict from the enumerable with the
  help of the transformation function.

  ## Examples

      HashDict.new ["a", "b"], fn x -> {x, x} end
      #=> #HashDict<[{"a","a"},{"b","b"}]>

  """
  @spec new(list, (term -> {key :: term, value ::term})) :: Dict.t
  def new(list, transform) when is_function(transform) do
    Enum.reduce list, new(), fn i, dict ->
      { k, v } = transform.(i)
      put(dict, k, v)
    end
  end

  def put(dict, key, value) do
    { dict, _ } = dict_put(dict, key, { :put, value })
    dict
  end

  @doc false
  def update(dict, key, fun) when is_function(fun, 1) do
    IO.write "HashDict.update/3 is deprecated, please use HashDict.update!/3 instead\n#{Exception.format_stacktrace}"
    update!(dict, key, fun)
  end

  def update!(dict, key, fun) when is_function(fun, 1) do
    case dict_put(dict, key, { :update, nil, fun }) do
      { dict, 0 } ->
        dict
      { _dict, 1 } ->
        raise KeyError, key: key
    end
  end

  def update(dict, key, initial, fun) when is_function(fun, 1) do
    { dict, _ } = dict_put(dict, key, { :update, initial, fun })
    dict
  end

  def fetch(ordered(bucket: bucket), key) do
    bucket_get(bucket, key)
  end

  def fetch(trie(root: root, depth: depth), key) do
    bucket_get(node_bucket(root, depth, bucket_hash(key)), key)
  end

  def pop(dict, key, default // nil) do
    case dict_delete(dict, key) do
      { dict, _, 0 } -> { default, dict }
      { dict, value, _ } -> { value, dict }
    end
  end

  def delete(dict, key) do
    { dict, _, _ } = dict_delete(dict, key)
    dict
  end

  def size(dict) do
    elem(dict, 1)
  end

  def empty(_) do
    ordered()
  end

  def merge(dict, enum, callback // fn(_k, _v1, v2) -> v2 end)

  def merge(dict1, dict2, callback) when is_record(dict1, HashDict) and is_record(dict2, HashDict) and elem(dict1, 1) < elem(dict2, 1) do
    dict_fold dict1, dict2, fn [k|v1], acc ->
      update(acc, k, v1, callback.(k, v1, &1))
    end
  end

  def merge(dict1, dict2, callback) when is_record(dict1, HashDict) and is_record(dict2, HashDict) do
    dict_fold dict2, dict1, fn [k|v2], acc ->
      update(acc, k, v2, callback.(k, &1, v2))
    end
  end

  def merge(dict, enumerable, callback) when is_record(dict, HashDict) do
    super(dict, enumerable, callback)
  end

  def split(dict, keys) do
    split(keys, new, dict)
  end

  defp split([], including, excluding) do
    { including, excluding }
  end

  defp split([key|keys], including, excluding) do
    case dict_delete(excluding, key) do
      { excluding, _, 0 } -> split(keys, including, excluding)
      { excluding, value, _ } -> split(keys, put(including, key, value), excluding)
    end
  end

  @doc false
  def reduce(ordered(bucket: bucket), acc, fun) do
    bucket_reduce(bucket, acc, fun)
  end

  def reduce(trie(root: root, depth: depth), acc, fun) do
    node_reduce(root, depth, acc, fun, @node_size)
  end

  ## Dict-wide functions

  defp dict_fold(ordered(bucket: bucket), acc, fun) do
    bucket_fold(bucket, acc, fun)
  end

  defp dict_fold(trie(root: root, depth: depth), acc, fun) do
    node_fold(root, depth, acc, fun, @node_size)
  end

  defp dict_put(ordered(size: @ordered_threshold, bucket: bucket), key, value) do
    root = node_relocate(bucket, 0)
    dict_put(trie(size: @ordered_threshold, root: root), key, value)
  end

  defp dict_put(ordered(size: size, bucket: bucket) = dict, key, value) do
    { new, count } = bucket_put(bucket, key, value)
    { ordered(dict, size: size + count, bucket: new), count }
  end

  defp dict_put(trie(root: root, depth: depth, size: size, expand_on: size, contract_on: contract_on) = dict, key, value) do
    root = node_expand(root, depth, depth + 1)
    dict = trie(dict, root: root, depth: depth + 1,
      expand_on: size * @node_size, contract_on: contract_on * @node_size)
    dict_put(dict, key, value)
  end

  defp dict_put(trie(root: root, size: size, depth: depth) = dict, key, value) do
    pos = bucket_hash(key)
    { root, count } = node_put(root, depth, pos, key, value)
    { trie(dict, size: size + count, root: root), count }
  end

  defp dict_delete(ordered(bucket: bucket, size: size) = dict, key) do
    case bucket_delete(bucket, key) do
      { _, value, 0 } ->
        { dict, value, 0 }
      { new_bucket, value, -1 } ->
        { ordered(dict, size: size - 1, bucket: new_bucket), value, -1 }
    end
  end

  defp dict_delete(trie(root: root, size: size, depth: depth) = dict, key) do
    pos = bucket_hash(key)
    case node_delete(root, depth, pos, key) do
      { _, value, 0 } ->
        { dict, value, 0 }
      { root, value, -1 } ->
        { if depth > 0 and trie(dict, :contract_on) == size do
          root = node_contract(root, depth)
          trie(dict,
            root: root,
            size: size - 1,
            depth: depth - 1,
            contract_on: div(size, @node_size),
            expand_on: div(trie(dict, :expand_on), @node_size))
        else
          trie(dict, size: size - 1, root: root)
        end, value, -1 }
    end
  end

  ## Bucket helpers

  # Get value from the bucket
  defp bucket_get([[k|_]|_bucket], key) when k > key do
    :error
  end

  defp bucket_get([[key|value]|_bucket], key) do
    { :ok, value }
  end

  defp bucket_get([_e|bucket], key) do
    bucket_get(bucket, key)
  end

  defp bucket_get([], _key) do
    :error
  end

  # Puts a value in the bucket
  defp bucket_put([[k|_]|_]=bucket, key, { :put, value }) when k > key do
    { [[key|value]|bucket], 1 }
  end

  defp bucket_put([[k|_]|_]=bucket, key, { :update, initial, _fun }) when k > key do
    { [[key|initial]|bucket], 1 }
  end

  defp bucket_put([[key|_]|bucket], key, { :put, value }) do
    { [[key|value]|bucket], 0 }
  end

  defp bucket_put([[key|value]|bucket], key, { :update, _initial, fun }) do
    { [[key|fun.(value)]|bucket], 0 }
  end

  defp bucket_put([e|bucket], key, value) do
    { rest, count } = bucket_put(bucket, key, value)
    { [e|rest], count }
  end

  defp bucket_put([], key, { :put, value }) do
    { [[key|value]], 1 }
  end

  defp bucket_put([], key, { :update, initial, _fun }) do
    { [[key|initial]], 1 }
  end

  # Puts a value in the bucket without returning
  # the operation value
  defp bucket_put!([[k|_]|_]=bucket, key, value) when k > key, do: [[key|value]|bucket]
  defp bucket_put!([[key|_]|bucket], key, value), do: [[key|value]|bucket]
  defp bucket_put!([e|bucket], key, value), do: [e|bucket_put!(bucket, key, value)]
  defp bucket_put!([], key, value), do: [[key|value]]

  # Deletes a key from the bucket
  defp bucket_delete([[k|_]|_]=bucket, key) when k > key do
    { bucket, nil, 0 }
  end

  defp bucket_delete([[key|value]|bucket], key) do
    { bucket, value, -1 }
  end

  defp bucket_delete([e|bucket], key) do
    { rest, value, count } = bucket_delete(bucket, key)
    { [e|rest], value, count }
  end

  defp bucket_delete([], _key) do
    { [], nil, 0 }
  end

  # Reduces the bucket
  defp bucket_reduce([[k|v]|t], acc, fun) do
    bucket_reduce(t, fun.({ k, v }, acc), fun)
  end

  defp bucket_reduce([], acc, _fun) do
    acc
  end

  defp bucket_fold(bucket, acc, fun) do
    :lists.foldl(fun, acc, bucket)
  end

  defp bucket_hash(key) do
    :erlang.phash2(key)
  end

  defp bucket_index(hash) do
    hash &&& @node_bitmap
  end

  defp bucket_nth_index(hash, n) do
    (hash >>> (@node_shift * n)) &&& @node_bitmap
  end

  defp bucket_next(hash) do
    hash >>> @node_shift
  end

  ## Node helpers

  # Gets a bucket from the node
  defp node_bucket(node, 0, hash) do
    elem(node, bucket_index(hash))
  end

  defp node_bucket(node, depth, hash) do
    child = elem(node, bucket_index(hash))
    node_bucket(child, depth - 1, bucket_next(hash))
  end

  # Puts a key-value into a node
  defp node_put(node, 0, hash, key, value) do
    pos = bucket_index(hash)
    { new, count } = bucket_put(elem(node, pos), key, value)
    { set_elem(node, pos, new), count }
  end

  defp node_put(node, depth, hash, key, value) do
    pos = bucket_index(hash)
    { new, count } = node_put(elem(node, pos), depth - 1, bucket_next(hash), key, value)
    { set_elem(node, pos, new), count }
  end

  # Deletes a key from the bucket
  defp node_delete(node, 0, hash, key) do
    pos = bucket_index(hash)
    case bucket_delete(elem(node, pos), key) do
      { _, value, 0 } -> { node, value, 0 }
      { new, value, -1 } -> { set_elem(node, pos, new), value, -1 }
    end
  end

  defp node_delete(node, depth, hash, key) do
    pos = bucket_index(hash)
    case node_delete(elem(node, pos), depth - 1, bucket_next(hash), key) do
      { _, value, 0 } -> { node, value, 0 }
      { new, value, -1 } -> { set_elem(node, pos, new), value, -1 }
    end
  end

  # Reduces a node recursively
  defp node_reduce(bucket, -1, acc, fun, _) do
    bucket_reduce(bucket, acc, fun)
  end

  defp node_reduce(node, depth, acc, fun, count) when count >= 1 do
    acc = node_reduce(:erlang.element(count, node), depth - 1, acc, fun, @node_size)
    node_reduce(node, depth, acc, fun, count - 1)
  end

  defp node_reduce(_node, _, acc, _fun, 0) do
    acc
  end

  # Folds a node recursively
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

  defp each_contract([[k|_v]=e|acc], [[key|_value]|_]=bucket) when k < key, do: [e|each_contract(acc, bucket)]
  defp each_contract(acc, [e|bucket]), do: [e|each_contract(acc, bucket)]
  defp each_contract([], bucket), do: bucket
  defp each_contract(acc, []), do: acc

  defp node_relocate(node // @node_template, bucket, n) do
    :lists.foldl fn [key|value], acc ->
      pos = key |> bucket_hash() |> bucket_nth_index(n)
      set_elem(acc, pos, bucket_put!(elem(acc, pos), key, value))
    end, node, bucket
  end
end

defimpl Enumerable, for: HashDict do
  def reduce(dict, acc, fun),  do: HashDict.reduce(dict, acc, fun)
  def member?(dict, { k, v }), do: match?({ :ok, ^v }, HashDict.fetch(dict, k))
  def member?(_dict, _),       do: false
  def count(dict),             do: HashDict.size(dict)
end

defimpl Access, for: HashDict do
  def access(dict, key), do: HashDict.get(dict, key, nil)
end
