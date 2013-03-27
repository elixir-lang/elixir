defmodule HashDict do
  @moduledoc """
  A key-value store.

  The `HashDict` is meant to work well with both small and
  large set of keys and it is an implementation of the `Dict`
  behaviour. For more information about the functions and
  their APIs, please consult the `Dict` module.
  """

  @behaviour Dict

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

  # Let's inline common instructions
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

      HashDict.new [{:b,1},{:a,2}]
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

  @doc """
  Puts the given key and value in the dict.
  """
  def put(dict, key, value) do
    { dict, _ } = dict_put(dict, key, { :put, value })
    dict
  end

  @doc """
  Puts the given value under key in the dictionary
  only if one does not exist yet.
  """
  def put_new(dict, key, value) do
    update(dict, key, value, fn(v) -> v end)
  end

  @doc """
  Updates the key in the dictionary according
  to the given function. Raises if the key does
  not exist in the dictionary.
  """
  def update(dict, key, fun) when is_function(fun, 1) do
    case dict_put(dict, key, { :update, nil, fun }) do
      { dict, 0 } ->
        dict
      { _dict, 1 } ->
        raise KeyError, key: key
    end
  end

  @doc """
  Updates the key in the dictionary according
  to the given function. Adds initial value if
  the key does not exist in the dicionary.
  """
  def update(dict, key, initial, fun) when is_function(fun, 1) do
    { dict, _ } = dict_put(dict, key, { :update, initial, fun })
    dict
  end

  @doc """
  Gets the value under key from the dict.
  """
  def get(dict, key, default // nil) do
    case dict_get(dict, key) do
      { ^key, value } -> value
      false -> default
    end
  end

  @doc """
  Gets the value under key from the dict,
  raises KeyError if such key does not exist.
  """
  def get!(dict, key) when is_tuple(dict) do
    case dict_get(dict, key) do
      { ^key, value } -> value
      false -> raise(KeyError, key: key)
    end
  end

  @doc """
  Returns the values taken out from dict as well
  as a dict without keys which were taken out.
  Keys to take out are specified as another dict
  which contains key default_value pairs.
  """
  def pop(dict1, dict2) when is_record(dict1, HashDict) and is_record(dict2, HashDict) and elem(dict1, 1) < elem(dict2, 1) do
    dict_fold dict1, {dict2, new}, fn { key, value }, {acc, dict}  ->
      case dict_get(acc, key) do
        { ^key, _default } -> {put(acc, key, value), delete(dict, key)}
        false -> {acc, dict}
      end
    end
  end

  def pop(dict1, dict2) when is_record(dict1, HashDict) and is_record(dict2, HashDict) do
    dict_fold dict2, {new, dict1}, fn { key, default }, {acc, dict} ->
        {put(acc, key, get(dict, key, default)), delete(dict, key)}
    end
  end

  def pop(dict, defaults) do
    Enum.reduce defaults, {new, dict},
       fn({ key, default }, {acc, dict}) ->
         {put(acc, key, get(dict, key, default)), delete(dict, key)}
       end
  end

  @doc """
  Returns the value under key from the dict as well as the dict without key.
  """
  def pop_value(dict, key, default // nil) do
    {get(dict, key, default), delete(dict, key)}
  end

  @doc """
  Checks if the dict has the given key.
  """
  def has_key?(dict, key) do
    match? { ^key, _ }, dict_get(dict, key)
  end

  @doc """
  Deletes a value from the dict.
  """
  def delete(ordered(bucket: bucket, size: size) = dict, key) do
    case bucket_delete(bucket, key) do
      { _, 0 } ->
        dict
      { new_bucket, -1 } ->
        ordered(dict, size: size - 1, bucket: new_bucket)
    end
  end

  def delete(trie(root: root, size: size, depth: depth) = dict, key) do
    pos = bucket_hash(key)
    case node_delete(root, depth, pos, key) do
      { _, 0 } ->
        dict
      { root, -1 } ->
        if depth > 0 and trie(dict, :contract_on) == size do
          root = node_contract(root, depth, depth - 1)
          trie(dict,
            root: root,
            size: size - 1,
            depth: depth - 1,
            contract_on: div(size, @node_size),
            expand_on: div(trie(dict, :expand_on), @node_size))
        else
          trie(dict, size: size - 1, root: root)
        end
    end
  end

  @doc """
  Returns the dict size.
  """
  def size(dict) do
    elem(dict, 1)
  end

  @doc """
  Returns an empty dict.
  """
  def empty(_) do
    ordered()
  end

  def equal?(ordered(bucket: a, size: size), ordered(bucket: b, size: ^size)) do
    a == b
  end

  def equal?(trie(size: size) = a, trie(size: ^size) = b) do
    a == b
  end

  def equal?(ordered() = a, trie() = b) do
    equal?(b, a)
  end

  def equal?(trie(size: size) = a, ordered(bucket: b, size: ^size)) do
    :lists.keysort(1, to_list(a)) == :lists.keysort(1, b)
  end

  def equal?(_, _) do
    false
  end

  @doc """
  Converts the dict to a list.
  """
  def to_list(ordered(bucket: bucket)) do
    bucket
  end

  def to_list(dict) do
    dict_fold(dict, [], [&1|&2])
  end

  @doc """
  Get all keys in the dict.
  """
  def keys(dict) do
    dict_fold(dict, [], fn { k, _ }, acc -> [k|acc] end)
  end

  @doc """
  Get all values in the dict.
  """
  def values(dict) do
    dict_fold(dict, [], fn { _, v }, acc -> [v|acc] end)
  end

  @doc """
  Merges two dictionaries.
  """
  def merge(dict, enum, callback // fn(_k, _v1, v2) -> v2 end)

  def merge(dict1, dict2, callback) when is_record(dict1, HashDict) and is_record(dict2, HashDict) and elem(dict1, 1) < elem(dict2, 1) do
    dict_fold dict1, dict2, fn { k, v1 }, acc ->
      update(acc, k, v1, callback.(k, v1, &1))
    end
  end

  def merge(dict1, dict2, callback) when is_record(dict1, HashDict) and is_record(dict2, HashDict) do
    dict_fold dict2, dict1, fn { k, v2 }, acc ->
      update(acc, k, v2, callback.(k, &1, v2))
    end
  end

  def merge(dict1, dict2, callback) when is_record(dict1, HashDict) do
    Enum.reduce dict2, dict1, fn { k, v2 }, acc ->
      update(acc, k, v2, callback.(k, &1, v2))
    end
  end

  ## Dict-wide functions

  defp dict_get(ordered(bucket: bucket), key) do
    :lists.keyfind(key, 1, bucket)
  end

  defp dict_get(trie(root: root, depth: depth), key) do
    :lists.keyfind(key, 1, node_bucket(root, depth, bucket_hash(key)))
  end

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

  ## Bucket helpers

  # Puts a value in the bucket
  defp bucket_put([{k,_}=e|bucket], key, { :put, value }) when key < k do
    { [{key,value},e|bucket], 1 }
  end

  defp bucket_put([{k,_}=e|bucket], key, { :update, initial, _fun }) when key < k do
    { [{key,initial},e|bucket], 1 }
  end

  defp bucket_put([{k,_}=e|bucket], key, value) when key > k do
    { rest, count } = bucket_put(bucket, key, value)
    { [e|rest], count }
  end

  defp bucket_put([{_,_}|bucket], key, { :put, value }) do
    { [{key,value}|bucket], 0 }
  end

  defp bucket_put([{_,value}|bucket], key, { :update, _initial, fun }) do
    { [{key,fun.(value)}|bucket], 0 }
  end

  defp bucket_put([], key, { :put, value }) do
    { [{key,value}], 1 }
  end

  defp bucket_put([], key, { :update, initial, _fun }) do
    { [{key,initial}], 1 }
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

  # Folds the bucket
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
      { _, 0 }    -> { node, 0 }
      { new, -1 } -> { set_elem(node, pos, new), -1 }
    end
  end

  defp node_delete(node, depth, hash, key) do
    pos = bucket_index(hash)
    case node_delete(elem(node, pos), depth - 1, bucket_next(hash), key) do
      { _, 0 }    -> { node, 0 }
      { new, -1 } -> { set_elem(node, pos, new), -1 }
    end
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

  defp node_contract({ b1, b2, b3, b4, b5, b6, b7, b8 }, depth, n) when depth > 1 do
    depth = depth - 1
    { node_contract(b1, depth, n), node_contract(b2, depth, n), node_contract(b3, depth, n),
      node_contract(b4, depth, n), node_contract(b5, depth, n), node_contract(b6, depth, n),
      node_contract(b7, depth, n), node_contract(b8, depth, n) }
  end

  defp node_contract({ b1, b2, b3, b4, b5, b6, b7, b8 }, 1, n) do
    @node_template |> each_contract(b1, n) |> each_contract(b2, n) |> each_contract(b3, n)
                   |> each_contract(b4, n) |> each_contract(b5, n) |> each_contract(b6, n)
                   |> each_contract(b7, n) |> each_contract(b8, n)
  end

  defp each_contract(acc, { b1, b2, b3, b4, b5, b6, b7, b8 }, n) do
    acc |> node_relocate(b1, n) |> node_relocate(b2, n) |> node_relocate(b3, n)
        |> node_relocate(b4, n) |> node_relocate(b5, n) |> node_relocate(b6, n)
        |> node_relocate(b7, n) |> node_relocate(b8, n)
  end

  defp node_relocate(node // @node_template, bucket, n) do
    :lists.foldl fn { key, value }, acc ->
      pos = key |> bucket_hash() |> bucket_nth_index(n)
      set_elem(acc, pos, bucket_put!(elem(acc, pos), key, value))
    end, node, bucket
  end
end

defimpl Enum.Iterator, for: HashDict do
  def iterator(dict), do: HashDict.to_list(dict)
  def count(dict),    do: HashDict.size(dict)
end

defimpl Access, for: HashDict do
  def access(dict, key), do: HashDict.get(dict, key, nil)
end

defimpl Binary.Inspect, for: HashDict do
  import Kernel, except: [inspect: 2]

  def inspect(dict, opts) do
    "#HashDict<" <> Kernel.inspect(HashDict.to_list(dict), opts) <> ">"
  end
end
