defmodule Dict do
  @moduledoc """
  This module specifies the Dict API expected to be
  implemented by different dictionaries. It also provides
  functions that redirect to the underlying Dict based on
  the tuple signature.

  The keyword list used throughout Elixir cannot be
  manipulated via the Dict module, you must use the
  Keyword module instead. This distinction is intentional:
  the Dict module is meant to work on structures that work
  as storage.

  To create a new dict, use the `new` functions defined
  by each dict type:

      OrdDict.new [{:a, 1}, {:b, 2}]
      HashDict.new  #=> creates an empty HashDict

  For simplicity's sake, in the examples below everytime
  `new` is used, it implies one of the module-specific
  calls like the two above. Likewise, when the result of
  a function invocation is shown in the form `[a: 1, b: 2]`,
  it implies that the returned value is actually of the
  same dict type as the input one.

  """

  use Behaviour

  @type key :: any
  @type value :: any
  @type t :: tuple

  defcallback delete(t, key) :: t
  defcallback empty(t) :: t
  defcallback get(t, key, value) :: value
  defcallback has_key?(t, key) :: boolean
  defcallback keys(t) :: list(key)
  defcallback merge(t, t, (key, value, value -> value)) :: t
  defcallback put(t, key, value) :: t
  defcallback size(t) :: non_neg_integer()
  defcallback to_list(t) :: list()
  defcallback update(t, key, (value -> value)) :: t
  defcallback values(t) :: list(value)

  @doc """
  Returns a list containing all dict's keys.
  The keys are not guaranteed to be sorted, unless
  the underlying dict implementation defines so.

  ## Examples

      d = new [a: 1, b: 2]
      Dict.keys d  #=> [:a,:b]

  """
  @spec keys(t) :: [key]
  def keys(dict) do
    elem(dict, 0).keys(dict)
  end

  @doc """
  Returns a list containing all dict's values.

  ## Examples

      d = new [a: 1, b: 2]
      Dict.values d  #=> [1,2]

  """
  @spec values(t) :: [value]
  def values(dict) do
    elem(dict, 0).values(dict)
  end

  @doc """
  Returns the number of elements in `dict`.

  ## Examples

      d = new [a: 1, b: 2]
      Dict.size d  #=> 2

  """
  @spec size(t) :: non_neg_integer
  def size(dict) do
    elem(dict, 0).size(dict)
  end

  @doc """
  Returns whether the given key exists in the given dict.

  ## Examples

      d = new [a: 1]
      Dict.has_key?(d, :a)  #=> true
      Dict.has_key?(d, :b)  #=> false

  """
  @spec has_key?(t, key) :: boolean
  def has_key?(dict, key) do
    elem(dict, 0).has_key?(dict, key)
  end

  @doc """
  Returns the value associated with `key` in `dict`. If `dict` does not
  contain `key`, returns `default` (or nil if not provided).

  ## Examples

      d = new [a: 1]
      Dict.get d, :a     #=> 1
      Dict.get d, :b     #=> nil
      Dict.get d, :b, 3  #=> 3

  """
  @spec get(t, key) :: value | nil
  @spec get(t, key, value) :: value
  def get(dict, key, default // nil) do
    elem(dict, 0).get(dict, key, default)
  end

  @doc """
  Returns the value associated with `key` in `dict`. If `dict` does not
  contain `key`, it raises `KeyError`.

  ## Examples

      d = new [a: 1]
      Dict.get d, :a     #=> 1
      Dict.get d, :b     #=> raises KeyError[key: :b]

  """
  @spec get!(t, key) :: value | no_return
  def get!(dict, key) do
    elem(dict, 0).get!(dict, key)
  end

  @doc """
  Stores the given `value` under `key` in `dict`.
  If `dict` already has `key`, the stored value is replaced by the new one.

  ## Examples

      d = new [a: 1, b: 2]
      Dict.put d, :a, 3
      #=> [a: 3, b: 2]

  """
  @spec put(t, key, value) :: t
  def put(dict, key, val) do
    elem(dict, 0).put(dict, key, val)
  end

  @doc """
  Removes the entry stored under the given key from `dict`.
  If `dict` does not contain `key`, returns the dictionary unchanged.

  ## Examples

      d = new [a: 1, b: 2]
      Dict.delete d, :a      #=> [b: 2]

      d = new [b: 2]
      Dict.delete d, :a      #=> [b: 2]

  """
  @spec delete(t, key) :: t
  def delete(dict, key) do
    elem(dict, 0).delete(dict, key)
  end

  @doc """
  Merges two dicts into one. If the dicts have duplicated entries,
  the one given as second argument wins. In case the second argument
  is not of the same kind as the first one, it is converted to the
  same kind before merging as long as it implements the `Enum` protocol.

  ## Examples

      d1 = new [a: 1, b: 2]
      d2 = new [a: 3, d: 4]
      Dict.merge d1, d2
      #=> [a: 3, b: 2, d: 4]

  """
  @spec merge(t, t) :: t
  def merge(dict1, dict2) do
    merge(dict1, dict2, fn(_k, _v1, v2) -> v2 end)
  end

  @doc """
  Merges two dicts into one. If the dicts have duplicated entries, the given
  function is invoked to solve conflicts.

  ## Examples

      d1 = new [a: 1, b: 2]
      d2 = new [a: 3, d: 4]
      Dict.merge d1, d2, fn _k, v1, v2 ->
        v1 + v2
      end
      #=> [a: 4, b: 2, d: 4]

  """
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(dict1, dict2, fun) do
    elem(dict1, 0).merge(dict1, dict2, fun)
  end

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new
  value. An exception is generated if `key` is not present in the dict.

  ## Examples

      d = new [a: 1, b: 2]
      Dict.update d, :a, fn val -> -val end
      #=> [a: -1, b: 2]

  """
  @spec update(t, key, (value -> value)) :: t
  def update(dict, key, fun) do
    elem(dict, 0).update(dict, key, fun)
  end

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new value. If
  `key` is not present in `dict` then `initial` will be stored as the first
  value.

  ## Examples

      d = new [a: 1, b: 2]
      Dict.update d, :c, 3, fn val -> -val end
      #=> [a: 1, b: 2, c: 3]

  """
  @spec update(t, key, value, (value -> value)) :: t
  def update(dict, key, initial, fun) do
    elem(dict, 0).update(dict, key, initial, fun)
  end

  @doc """
  Returns an empty dict of the same type as `dict`.
  """
  @spec empty(t) :: t
  def empty(dict) do
    elem(dict, 0).empty(dict)
  end

  @doc """
  Returns a list of key-value pairs stored in `dict`.
  No particular order is enforced.
  """
  @spec to_list(t) :: list
  def to_list(dict) do
    elem(dict, 0).to_list(dict)
  end
end
