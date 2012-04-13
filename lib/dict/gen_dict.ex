defprotocol GenDict, [
#  @doc """
#  Returns a list containing all dict's keys.
#
#  For Keyword and Orddict, the keys are guaranteed to be sorted. For other
#  types of dicts, the order is not enforced.
#
#  ## Examples
#
#      GenDict.keys [a: 1, b: 2]  #=> [:a,:b]
#
#  """
  keys(dict),

#  @doc """
#  Returns a list containing all dict's values.
#
#  ## Examples
#
#      GenDict.values [a: 1, b: 2]  #=> [1,2]
#
#  """
  values(dict),

#  @doc """
#  Returns the number of elements in `dict`.
#
#  ## Examples
#
#      GenDict.size [a: 1, b: 2]  #=> 2
#
#  """
  size(dict),

#  @doc """
#  Returns whether the given key exists in the given dict.
#
#  ## Examples
#
#      GenDict.has_key?([a:, 1], :a)  #=> true
#      GenDict.has_key?([a:, 1], :b)  #=> false
#  """
  has_key?(dict, key),

#  @doc """
#  Returns the value associated with `key` in `dict`. If `dict` does not
#  contain `key`, returns `default` (or nil if not provided).
#
#  ## Examples
#
#      GenDict.get [a: 1], :a     #=> 1
#      GenDict.get [a: 1], :b     #=> nil
#      GenDict.get [a: 1], :b, 3  #=> 3
#  """
  get(dict, key, default),

#  @doc """
#  Stores the given `value` under `key` in `dict`.
#  If `dict` already has `key`, the stored value is replaced by the new one.
#
#  ## Examples
#
#      GenDict.put [a: 1, b: 2], :a, 3
#      #=> [a: 3, b: 2]
#      GenDict.put [a: 1, b: 2], {:c, 3}
#      #=> [a: 1, b: 2, c: 3]
#  """
  put(dict, key, val),
  put(dict, pair),

#  @doc """
#  Removes the entry stored under the given key from `dict`.
#  If `dict` does not contain `key`, returns the dictionary unchanged.
#
#  ## Examples
#
#      GenDict.delete [a: 1, b: 2], :a  #=> [b: 2]
#      GenDict.delete [b: 2], :a        #=> [b: 2]
#  """
  delete(dict, key),

#  @doc """
#  Merges two dicts into one. If the dicts have duplicated entries, the one
#  given as second argument wins.
#
#  ## Examples
#
#      GenDict.merge [a: 1, b: 2], [a: 3, d: 4]
#      #=> [a:3, b:2, d: 4]
#  """
  merge(dict1, dict2),

#  @doc """
#  Merges two dicts into one. If the dicts have duplicated entries, the given
#  function is invoked to solve conflicts.
#
#  ## Examples
#
#      GenDict.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
#        v1 + v2
#      end
#      #=> [a: 4, b: 2, d: 4]
#  """
  merge(dict1, dict2, fun),

#  @doc """
#  Extends the dict with entries from the list of pairs `pairs`. Values for
#  existing keys are replaced by the new values from the `pairs` list.
#
#  ## Examples
#
#      GenDict.extend [a: 1, b: 2], [{:a, 3}, {:c, 4}]
#      #=> [a: 3, b: 2, c: 4]
#
#  """
  extend(dict, pairs),

#  @doc """
#  Extends the dict with entries obtained by applying the transformation
#  function `transform` to each element in `list`. The overwrite semantics is
#  similar to extend/2.
#
#  ## Examples
#
#      GenDict.extend [a: 1, b: 2], [:a, :c], fn(x) -> {x, x} end
#      #=> [a: :a, b: 1, c: :c]
#
#  """
  extend(dict, list, transform),

#  @doc """
#  Extends the dict with entries formed by corresponding elements from `keys`
#  and `values`. Raises an error if `keys` and `values` have different size.
#
#  ## Examples
#
#      GenDict.extend [a: 1, b: 2], [:b, :c], [3, 4]
#      #=> [a: 1, b: 3, c: 4]
#
#  """
  extend(dict, keys, values),

#  @doc """
#  Update a value in `dict` by calling `fun` on the value to get a new
#  value. An exception is generated if `key` is not present in the dict.
#
#  ## Examples
#
#      GenDict.update [a: 1, b: 2], :a, fn(val) -> -val end
#      #=> [a: -1, b: 2]
#
#  """
  update(dict, key, fun),

#  @doc """
#  Update a value in `dict` by calling `fun` on the value to get a new value. If
#  `key` is not present in `dict` then `initial` will be stored as the first
#  value.
#
#  ## Examples
#
#      GenDict.update [a: 1, b: 2], 3, :c, fn(val) -> -val end
#      #=> [a: 1, b: 2, c: 3]
#
#  """
  update(dict, key, initial, fun)
], only: [Record]
