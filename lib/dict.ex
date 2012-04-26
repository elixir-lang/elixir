defprotocol Dict do
  @only [Record]

  @doc """
  Returns a list containing all dict's keys.

  For Keyword and Orddict, the keys are guaranteed to be sorted. For other
  types of dicts, the order is not enforced.

  ## Examples

      Dict.keys [a: 1, b: 2]  #=> [:a,:b]

  """
  def keys(dict)

  @doc """
  Returns a list containing all dict's values.

  ## Examples

      Dict.values [a: 1, b: 2]  #=> [1,2]

  """
  def values(dict)

  @doc """
  Returns the number of elements in `dict`.

  ## Examples

      Dict.size [a: 1, b: 2]  #=> 2

  """
  def size(dict)

  @doc """
  Returns whether the given key exists in the given dict.

  ## Examples

      Dict.has_key?([a:, 1], :a)  #=> true
      Dict.has_key?([a:, 1], :b)  #=> false

  """
  def has_key?(dict, key)

  @doc """
  Returns the value associated with `key` in `dict`. If `dict` does not
  contain `key`, returns `default` (or nil if not provided).

  ## Examples

      Dict.get [a: 1], :a     #=> 1
      Dict.get [a: 1], :b     #=> nil
      Dict.get [a: 1], :b, 3  #=> 3

  """
  def get(dict, key)
  def get(dict, key, default)

  @doc """
  Stores the given `value` under `key` in `dict`.
  If `dict` already has `key`, the stored value is replaced by the new one.

  ## Examples

      Dict.put [a: 1, b: 2], :a, 3
      #=> [a: 3, b: 2]
      Dict.put [a: 1, b: 2], {:c, 3}
      #=> [a: 1, b: 2, c: 3]

  """
  def put(dict, pair)
  def put(dict, key, val)

  @doc """
  Removes the entry stored under the given key from `dict`.
  If `dict` does not contain `key`, returns the dictionary unchanged.

  ## Examples

      Dict.delete [a: 1, b: 2], :a  #=> [b: 2]
      Dict.delete [b: 2], :a        #=> [b: 2]

  """
  def delete(dict, key)

  @doc """
  Merges two dicts into one. If the dicts have duplicated entries, the one
  given as second argument wins.

  ## Examples

      Dict.merge [a: 1, b: 2], [a: 3, d: 4]
      #=> [a:3, b:2, d: 4]

  """
  def merge(dict1, dict2)

  @doc """
  Merges two dicts into one. If the dicts have duplicated entries, the given
  function is invoked to solve conflicts.

  ## Examples

      Dict.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
        v1 + v2
      end
      #=> [a: 4, b: 2, d: 4]

  """
  def merge(dict1, dict2, fun)

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new
  value. An exception is generated if `key` is not present in the dict.

  ## Examples

      Dict.update [a: 1, b: 2], :a, fn(val) -> -val end
      #=> [a: -1, b: 2]

  """
  def update(dict, key, fun)

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new value. If
  `key` is not present in `dict` then `initial` will be stored as the first
  value.

  ## Examples

      Dict.update [a: 1, b: 2], :c, 3, fn(val) -> -val end
      #=> [a: 1, b: 2, c: 3]

  """
  def update(dict, key, initial, fun)

  @doc """
  Returns an empty dict of the same type as `dict`.
  """
  def empty(dict)

  @doc """
  Returns a list of key-value pairs stored in `dict`.
  No particular order is enforced.
  """
  def to_list(dict)
end