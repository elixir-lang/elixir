defmodule Dict do
  @moduledoc %B"""
  This module specifies the Dict API expected to be
  implemented by different dictionaries. It also provides
  functions that redirect to the underlying Dict, allowing
  a developer to work with different Dict implementations
  using one API.

  To create a new dict, use the `new` functions defined
  by each dict type:

      HashDict.new  #=> creates an empty HashDict

  For simplicity's sake, in the examples below everytime
  `new` is used, it implies one of the module-specific
  calls like above. Likewise, when the result of a function
  invocation is shown in the form `[a: 1, b: 2]`, it implies
  that the returned value is actually of the same dict type
  as the input one.

  ## Protocols

  Besides implementing the functions in this module, all
  dictionaries are also required to implement the `Access`
  protocol:

      iex> dict = HashDict.new
      ...> dict = Dict.put(dict, :hello, :world)
      ...> dict[:hello]
      :world

  And also the `Enumerable` protocol, allowing one to write:

      Enum.each(dict, fn ({ k, v }) ->
        IO.puts "#{k}: #{v}"
      end)

  """

  use Behaviour

  @type key :: any
  @type value :: any
  @type keys :: [ key ]
  @type t :: tuple | list

  defcallback delete(t, key) :: t
  defcallback drop(t, keys) :: t
  defcallback empty(t) :: t
  defcallback equal?(t, t) :: boolean
  defcallback get(t, key) :: value
  defcallback get(t, key, value) :: value
  defcallback get!(t, key) :: value | no_return
  defcallback has_key?(t, key) :: boolean
  defcallback keys(t) :: list(key)
  defcallback merge(t, t) :: t
  defcallback merge(t, t, (key, value, value -> value)) :: t
  defcallback pop(t, key) :: {value, t}
  defcallback pop(t, key, value) :: {value, t}
  defcallback put(t, key, value) :: t
  defcallback put_new(t, key, value) :: t
  defcallback size(t) :: non_neg_integer()
  defcallback split(t, keys) :: {t, t}
  defcallback take(t, keys) :: t
  defcallback to_list(t) :: list()
  defcallback update(t, key, (value -> value)) :: t | no_return
  defcallback update(t, key, value, (value -> value)) :: t
  defcallback values(t) :: list(value)

  defmacrop target(dict) do
    quote do
      cond do
        is_tuple(unquote(dict)) ->
          elem(unquote(dict), 0)
        is_list(unquote(dict)) ->
          ListDict
      end
    end
  end

  @doc """
  Returns a list containing all dict's keys.
  The keys are not guaranteed to be sorted, unless
  the underlying dict implementation defines so.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> Enum.sort(Dict.keys(d))
      [:a,:b]

  """
  @spec keys(t) :: [key]
  def keys(dict) do
    target(dict).keys(dict)
  end

  @doc """
  Returns a list containing all dict's values.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> Enum.sort(Dict.values(d))
      [1,2]

  """
  @spec values(t) :: [value]
  def values(dict) do
    target(dict).values(dict)
  end

  @doc """
  Returns the number of elements in `dict`.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> Dict.size(d)
      2

  """
  @spec size(t) :: non_neg_integer
  def size(dict) do
    target(dict).size(dict)
  end

  @doc """
  Returns whether the given key exists in the given dict.

  ## Examples

      iex> d = HashDict.new([a: 1])
      ...> Dict.has_key?(d, :a)
      true

      iex> d = HashDict.new([a: 1])
      ...> Dict.has_key?(d, :b)
      false

  """
  @spec has_key?(t, key) :: boolean
  def has_key?(dict, key) do
    target(dict).has_key?(dict, key)
  end

  @doc """
  Returns the value associated with `key` in `dict`. If `dict` does not
  contain `key`, returns `default` (or nil if not provided).

  ## Examples

      iex> d = HashDict.new([a: 1])
      ...> Dict.get(d, :a)
      1

      iex> d = HashDict.new([a: 1])
      ...> Dict.get(d, :b)
      nil

      iex> d = HashDict.new([a: 1])
      ...> Dict.get(d, :b, 3)
      3
  """
  @spec get(t, key, value) :: value
  def get(dict, key, default // nil) do
    target(dict).get(dict, key, default)
  end

  @doc false
  def get!(dict, key) do
    target(dict).get!(dict, key)
  end

  @doc """
  Returns the `{ :ok, value }` associated with `key` in `dict`.
  If `dict` does not contain `key`, returns `:error`.

  ## Examples

      iex> d = HashDict.new([a: 1])
      ...> Dict.fetch(d, :a)
      { :ok, 1 }

      iex> d = HashDict.new([a: 1])
      ...> Dict.fetch(d, :b)
      :error

  """
  @spec fetch(t, key) :: value
  def fetch(dict, key) do
    target(dict).fetch(dict, key)
  end

  @doc """
  Returns the value associated with `key` in `dict`. If `dict` does not
  contain `key`, it raises `KeyError`.

  ## Examples

      iex> d = HashDict.new([a: 1])
      ...> Dict.fetch!(d, :a)
      1
      iex> d = HashDict.new([a: 1])
      ...> Dict.fetch!(d, :b)
      ** (KeyError) key not found: :b

  """
  @spec fetch!(t, key) :: value | no_return
  def fetch!(dict, key) do
    target(dict).fetch!(dict, key)
  end

  @doc """
  Stores the given `value` under `key` in `dict`.
  If `dict` already has `key`, the stored value is replaced by the new one.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.put(d, :a, 3)
      ...> Dict.get(d, :a)
      3

  """
  @spec put(t, key, value) :: t
  def put(dict, key, val) do
    target(dict).put(dict, key, val)
  end

  @doc """
  Puts the given `value` under `key` in `dict` unless `key` already exists.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.put_new(d, :a, 3)
      ...> Dict.get(d, :a)
      1

  """
  @spec put_new(t, key, value) :: t
  def put_new(dict, key, val) do
    target(dict).put_new(dict, key, val)
  end

  @doc """
  Removes the entry stored under the given key from `dict`.
  If `dict` does not contain `key`, returns the dictionary unchanged.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.delete(d, :a)
      ...> Dict.get(d, :a)
      nil

      iex> d = HashDict.new([b: 2])
      ...> Dict.delete(d, :a) == d
      true

  """
  @spec delete(t, key) :: t
  def delete(dict, key) do
    target(dict).delete(dict, key)
  end

  @doc """
  Merges the given enum into the dict. In case one of the enum entries
  alread exist in the dict, it is given higher preference.

  ## Examples

      iex> d1 = HashDict.new([a: 1, b: 2])
      ...> d2 = HashDict.new([a: 3, d: 4])
      ...> d = Dict.merge(d1, d2)
      ...> [a: Dict.get(d, :a), b: Dict.get(d, :b), d: Dict.get(d, :d)]
      [a: 3, b: 2, d: 4]

  """
  @spec merge(t, t) :: t
  def merge(dict, enum) do
    merge(dict, enum, fn(_k, _v1, v2) -> v2 end)
  end

  @doc """
  Merges the given enum into the dict. In case one of the enum entries
  alread exist in the dict, the given function is invoked to solve
  conflicts.

  ## Examples

      iex> d1 = HashDict.new([a: 1, b: 2])
      ...> d2 = HashDict.new([a: 3, d: 4])
      ...> d = Dict.merge(d1, d2, fn(_k, v1, v2) ->
      ...>   v1 + v2
      ...> end)
      ...> [a: Dict.get(d, :a), b: Dict.get(d, :b), d: Dict.get(d, :d)]
      [a: 4, b: 2, d: 4]

  """
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(dict, enum, fun) do
    target(dict).merge(dict, enum, fun)
  end

  @doc """
  Returns the value associated with `key` in `dict` as
  well as the `dict` without `key`.

  ## Examples

      iex> dict = HashDict.new [a: 1]
      ...> {v, d} = Dict.pop dict, :a
      ...> {v, Enum.sort(d)}
      {1,[]}

      iex> dict = HashDict.new [a: 1]
      ...> {v, d} = Dict.pop dict, :b
      ...> {v, Enum.sort(d)}
      {nil,[a: 1]}

      iex> dict = HashDict.new [a: 1]
      ...> {v, d} = Dict.pop dict, :b, 3
      ...> {v, Enum.sort(d)}
      {3,[a: 1]}

  """
  @spec pop(t, key, value) :: {value, t}
  def pop(dict, key, default // nil) do
    target(dict).pop(dict, key, default)
  end

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new
  value. An exception is generated if `key` is not present in the dict.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.update(d, :a, fn(val) -> -val end)
      ...> Dict.get(d, :a)
      -1

  """
  @spec update(t, key, (value -> value)) :: t
  def update(dict, key, fun) do
    target(dict).update(dict, key, fun)
  end

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new value. If
  `key` is not present in `dict` then `initial` will be stored as the first
  value.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.update(d, :c, 3, fn(val) -> -val end)
      ...> Dict.get(d, :c)
      3

  """
  @spec update(t, key, value, (value -> value)) :: t
  def update(dict, key, initial, fun) do
    target(dict).update(dict, key, initial, fun)
  end

  @doc """
  Returns a tuple of two dicts, where the first dict contains only
  entries from `dict` with keys in `keys`, and the second dict
  contains only entries from `dict` with keys not in `keys`

  Any non-member keys are ignored.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> { d1, d2 } = Dict.split(d, [:a, :c])
      ...> { Dict.to_list(d1), Dict.to_list(d2) }
      { [a: 1], [b: 2] }

      iex> d = HashDict.new([])
      ...> { d1, d2 } = Dict.split(d, [:a, :c])
      ...> { Dict.to_list(d1), Dict.to_list(d2) }
      { [], [] }

      iex> d = HashDict.new([a: 1, b: 2])
      ...> { d1, d2 } = Dict.split(d, [:a, :b, :c])
      ...> { Dict.to_list(d1), Dict.to_list(d2) }
      { [a: 1, b: 2], [] }

  """
  @spec split(t, keys) :: {t, t}
  def split(dict, keys) do
    target(dict).split(dict, keys)
  end

  @doc """
  Returns a new dict where the the given `keys` a removed from `dict`.
  Any non-member keys are ignored.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.drop(d, [:a, :c, :d])
      ...> Dict.to_list(d)
      [b: 2]

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.drop(d, [:c, :d])
      ...> Dict.to_list(d)
      [a: 1, b: 2]

  """
  @spec drop(t, keys) :: t
  def drop(dict, keys) do
    target(dict).drop(dict, keys)
  end

  @doc """
  Returns a new dict where only the keys in `keys` from `dict` are
  included. Any non-member keys are ignored.

  ## Examples

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.take(d, [:a, :c, :d])
      ...> Dict.to_list(d)
      [a: 1]

      iex> d = HashDict.new([a: 1, b: 2])
      ...> d = Dict.take(d, [:c, :d])
      ...> Dict.to_list(d)
      []

  """
  @spec take(t, keys) :: t
  def take(dict, keys) do
    target(dict).take(dict, keys)
  end

  @doc """
  Returns an empty dict of the same type as `dict`.
  """
  @spec empty(t) :: t
  def empty(dict) do
    target(dict).empty(dict)
  end

  @doc """
  Check if two dicts are equal, if the dicts are of different types they're
  first converted to lists.

  ## Examples

      iex> a = HashDict.new(a: 2, b: 3, f: 5, c: 123)
      ...> b = ListDict.new(a: 2, b: 3, f: 5, c: 123)
      ...> Dict.equal?(a, b)
      true

      iex> a = HashDict.new(a: 2, b: 3, f: 5, c: 123)
      ...> b = []
      ...> Dict.equal?(a, b)
      false

  """
  @spec equal?(t, t) :: boolean
  def equal?(a, b) do
    a_target = target(a)
    b_target = target(b)

    cond do
      a_target == b_target ->
        a_target.equal?(a, b)

      a_target.size(a) == b_target.size(b) ->
        ListDict.equal?(a_target.to_list(a), b_target.to_list(b))

      true ->
        false
    end
  end

  @doc """
  Returns a list of key-value pairs stored in `dict`.
  No particular order is enforced.
  """
  @spec to_list(t) :: list
  def to_list(dict) do
    target(dict).to_list(dict)
  end
end
