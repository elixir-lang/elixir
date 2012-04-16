defprotocol PDict, [
#  @doc """
#  Returns a list containing all dict's keys.
#
#  For Keyword and Orddict, the keys are guaranteed to be sorted. For other
#  types of dicts, the order is not enforced.
#
#  ## Examples
#
#      PDict.keys [a: 1, b: 2]  #=> [:a,:b]
#
#  """
  keys(dict),

#  @doc """
#  Returns a list containing all dict's values.
#
#  ## Examples
#
#      PDict.values [a: 1, b: 2]  #=> [1,2]
#
#  """
  values(dict),

#  @doc """
#  Returns the number of elements in `dict`.
#
#  ## Examples
#
#      PDict.size [a: 1, b: 2]  #=> 2
#
#  """
  size(dict),

#  @doc """
#  Returns whether the given key exists in the given dict.
#
#  ## Examples
#
#      PDict.has_key?([a:, 1], :a)  #=> true
#      PDict.has_key?([a:, 1], :b)  #=> false
#  """
  has_key?(dict, key),

#  @doc """
#  Returns the value associated with `key` in `dict`. If `dict` does not
#  contain `key`, returns `default` (or nil if not provided).
#
#  ## Examples
#
#      PDict.get [a: 1], :a     #=> 1
#      PDict.get [a: 1], :b     #=> nil
#      PDict.get [a: 1], :b, 3  #=> 3
#  """
  get(dict, key),
  get(dict, key, default),

#  @doc """
#  Stores the given `value` under `key` in `dict`.
#  If `dict` already has `key`, the stored value is replaced by the new one.
#
#  ## Examples
#
#      PDict.put [a: 1, b: 2], :a, 3
#      #=> [a: 3, b: 2]
#      PDict.put [a: 1, b: 2], {:c, 3}
#      #=> [a: 1, b: 2, c: 3]
#  """
  put(dict, pair),
  put(dict, key, val),

#  @doc """
#  Removes the entry stored under the given key from `dict`.
#  If `dict` does not contain `key`, returns the dictionary unchanged.
#
#  ## Examples
#
#      PDict.delete [a: 1, b: 2], :a  #=> [b: 2]
#      PDict.delete [b: 2], :a        #=> [b: 2]
#  """
  delete(dict, key),

#  @doc """
#  Merges two dicts into one. If the dicts have duplicated entries, the one
#  given as second argument wins.
#
#  ## Examples
#
#      PDict.merge [a: 1, b: 2], [a: 3, d: 4]
#      #=> [a:3, b:2, d: 4]
#  """
  merge(dict1, dict2),

#  @doc """
#  Merges two dicts into one. If the dicts have duplicated entries, the given
#  function is invoked to solve conflicts.
#
#  ## Examples
#
#      PDict.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
#        v1 + v2
#      end
#      #=> [a: 4, b: 2, d: 4]
#  """
  merge(dict1, dict2, fun),

#  @doc """
#  Update a value in `dict` by calling `fun` on the value to get a new
#  value. An exception is generated if `key` is not present in the dict.
#
#  ## Examples
#
#      PDict.update [a: 1, b: 2], :a, fn(val) -> -val end
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
#      PDict.update [a: 1, b: 2], :c, 3, fn(val) -> -val end
#      #=> [a: 1, b: 2, c: 3]
#
#  """
  update(dict, key, initial, fun),

#  @doc """
#  Returns an empty dict of the same type as `dict`.
#  """
  empty(dict)
], only: [Record, List]


defmodule Dict.Common do
  defmacro __using__(module, impl_ref) do
    # This is an optimization trick that allows us to reference the
    # implementation module directly
    ref = Module.concat(PDict, impl_ref)
    quote do
      @doc """
      Creates a new empty dict.
      """
      def new do
        unquote(ref).empty(nil)
      end

      @doc """
      Creates a new dict with one entry.
      """
      def new({key, value}) do
        unquote(ref).put unquote(module).new(), {key, value}
      end

      @doc """
      Creates a new dict from a list of pairs.

      ## Examples

          Dict.new [{:b,1},{:a,2}]
          #=> [a: 1, b: 2]

      """
      def new(pairs) when is_list(pairs) do
        Enum.reduce pairs, unquote(module).new(), fn(pair, dict) ->
          unquote(ref).put(dict, pair)
        end
      end

      @doc """
      Creates a new dict from a list of elements with the
      help of the transformation function.

      ## Examples

          Dict.new ["a", "b"], fn(x) -> {x, x} end
          #=> ["a": "a", "b": "b"]
      """
      def new(list, transform) when is_list(list) and is_function(transform) do
        Enum.reduce list, unquote(module).new(), fn(i, dict) ->
          pair = transform.(i)
          unquote(ref).put(dict, pair)
        end
      end

      @doc """
      Creates a new dict with one entry for each element in `keys` and a
      corresponding element in `values`. Raises an error if `keys` and `values`
      have different size.
      """
      def new(keys, values) when is_list(keys) and is_list(values) do
        if :erlang.length(keys) !== :erlang.length(values) do
          raise ArgumentError, message: "Both arguments must have equal size"
        else:
          unquote(module).new List.zip(keys, values)
        end
      end
    end
  end
end
