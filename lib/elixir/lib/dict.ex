defmodule Dict do
  @moduledoc ~S"""
  This module specifies the Dict API expected to be
  implemented by different dictionaries. It also provides
  functions that redirect to the underlying Dict, allowing
  a developer to work with different Dict implementations
  using one API.

  To create a new dict, use the `new` functions defined
  by each dict type:

      HashDict.new  #=> creates an empty HashDict

  In the examples below, `dict_impl` means a specific
  `Dict` implementation, for example `HashDict` or `Map`.

  ## Protocols

  Besides implementing the functions in this module, all
  dictionaries are required to implement the `Access`
  protocol:

      iex> dict = dict_impl.new
      iex> dict = Dict.put(dict, :hello, :world)
      iex> dict[:hello]
      :world

  As well as the `Enumerable` and `Collectable` protocols.

  ## Match

  Dictionaries are required to implement all operations
  using the match (`===`) operator.
  """

  use Behaviour

  @type key :: any
  @type value :: any
  @type t :: tuple | list | map

  defcallback new :: t
  defcallback delete(t, key) :: t
  defcallback drop(t, Enum.t) :: t
  defcallback empty(t) :: t
  defcallback equal?(t, t) :: boolean
  defcallback get(t, key) :: value
  defcallback get(t, key, value) :: value
  defcallback fetch(t, key) :: { :ok, value } | :error
  defcallback fetch!(t, key) :: value | no_return
  defcallback has_key?(t, key) :: boolean
  defcallback keys(t) :: [key]
  defcallback merge(t, t) :: t
  defcallback merge(t, t, (key, value, value -> value)) :: t
  defcallback pop(t, key) :: {value, t}
  defcallback pop(t, key, value) :: {value, t}
  defcallback put(t, key, value) :: t
  defcallback put_new(t, key, value) :: t
  defcallback size(t) :: non_neg_integer()
  defcallback split(t, Enum.t) :: {t, t}
  defcallback take(t, Enum.t) :: t
  defcallback to_list(t) :: list()
  defcallback update(t, key, value, (value -> value)) :: t
  defcallback update!(t, key, (value -> value)) :: t | no_return
  defcallback values(t) :: list(value)

  defmacrop target(dict) do
    quote do
      case unquote(dict) do
        %{__struct__: x} when is_atom(x) ->
          x
        %{} ->
          Map
        x when is_tuple(x) ->
          elem(x, 0)
        x when is_list(x) ->
          ListDict
        x ->
          unsupported_dict(x)
      end
    end
  end

  @doc """
  Returns a list of all keys in `dict`.
  The keys are not guaranteed to be in any order.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> Enum.sort(Dict.keys(d))
      [:a,:b]

  """
  @spec keys(t) :: [key]
  def keys(dict) do
    target(dict).keys(dict)
  end

  @doc """
  Returns a list of all values in `dict`.
  The values are not guaranteed to be in any order.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> Enum.sort(Dict.values(d))
      [1,2]

  """
  @spec values(t) :: [value]
  def values(dict) do
    target(dict).values(dict)
  end

  @doc """
  Returns the number of elements in `dict`.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> Dict.size(d)
      2

  """
  @spec size(t) :: non_neg_integer
  def size(dict) do
    target(dict).size(dict)
  end

  @doc """
  Returns whether the given `key` exists in the given `dict`.

  ## Examples

      iex> d = Enum.into([a: 1], dict_impl.new)
      iex> Dict.has_key?(d, :a)
      true
      iex> Dict.has_key?(d, :b)
      false

  """
  @spec has_key?(t, key) :: boolean
  def has_key?(dict, key) do
    target(dict).has_key?(dict, key)
  end

  @doc """
  Returns the value associated with `key` in `dict`. If `dict` does not
  contain `key`, returns `default` (or `nil` if not provided).

  ## Examples

      iex> d = Enum.into([a: 1], dict_impl.new)
      iex> Dict.get(d, :a)
      1
      iex> Dict.get(d, :b)
      nil
      iex> Dict.get(d, :b, 3)
      3
  """
  @spec get(t, key, value) :: value
  def get(dict, key, default \\ nil) do
    target(dict).get(dict, key, default)
  end

  @doc """
  Returns `{ :ok, value }` associated with `key` in `dict`.
  If `dict` does not contain `key`, returns `:error`.

  ## Examples

      iex> d = Enum.into([a: 1], dict_impl.new)
      iex> Dict.fetch(d, :a)
      { :ok, 1 }
      iex> Dict.fetch(d, :b)
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

      iex> d = Enum.into([a: 1], dict_impl.new)
      iex> Dict.fetch!(d, :a)
      1

  """
  @spec fetch!(t, key) :: value | no_return
  def fetch!(dict, key) do
    target(dict).fetch!(dict, key)
  end

  @doc """
  Stores the given `value` under `key` in `dict`.
  If `dict` already has `key`, the stored value is replaced by the new one.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.put(d, :a, 3)
      iex> Dict.get(d, :a)
      3

  """
  @spec put(t, key, value) :: t
  def put(dict, key, val) do
    target(dict).put(dict, key, val)
  end

  @doc """
  Puts the given `value` under `key` in `dict` unless `key` already exists.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.put_new(d, :a, 3)
      iex> Dict.get(d, :a)
      1

  """
  @spec put_new(t, key, value) :: t
  def put_new(dict, key, val) do
    target(dict).put_new(dict, key, val)
  end

  @doc """
  Removes the entry stored under the given `key` from `dict`.
  If `dict` does not contain `key`, returns the dictionary unchanged.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.delete(d, :a)
      iex> Dict.get(d, :a)
      nil

      iex> d = Enum.into([b: 2], dict_impl.new)
      iex> Dict.delete(d, :a) == d
      true

  """
  @spec delete(t, key) :: t
  def delete(dict, key) do
    target(dict).delete(dict, key)
  end

  @doc """
  Merges the dict `b` into dict `a`.

  If one of the dict `b` entries already exists in the `dict`,
  the functions in entries in `b` have higher precedence unless a
  function is given to resolve conflicts.

  Notice this function is polymorphic as it merges dicts of any
  type. Each dict implementation also provides a `merge` function,
  but they can only merge dicts of the same type.

  ## Examples

      iex> d1 = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d2 = Enum.into([a: 3, d: 4], dict_impl.new)
      iex> d = Dict.merge(d1, d2)
      iex> [a: Dict.get(d, :a), b: Dict.get(d, :b), d: Dict.get(d, :d)]
      [a: 3, b: 2, d: 4]

      iex> d1 = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d2 = Enum.into([a: 3, d: 4], dict_impl.new)
      iex> d = Dict.merge(d1, d2, fn(_k, v1, v2) ->
      ...>   v1 + v2
      ...> end)
      iex> [a: Dict.get(d, :a), b: Dict.get(d, :b), d: Dict.get(d, :d)]
      [a: 4, b: 2, d: 4]

  """
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(dict1, dict2, fun \\ fn(_k, _v1, v2) -> v2 end) do
    target1 = target(dict1)
    target2 = target(dict2)

    if target1 == target2 do
      target1.merge(dict1, dict2, fun)
    else
      Enumerable.reduce(dict2, { :cont, dict1 }, fn({ k, v }, acc) ->
        { :cont, target1.update(acc, k, v, fn(other) -> fun.(k, other, v) end) }
      end) |> elem(1)
    end
  end

  @doc """
  Returns the value associated with `key` in `dict` as
  well as the `dict` without `key`.

  ## Examples

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> {v, d} = Dict.pop dict, :a
      iex> {v, Enum.sort(d)}
      {1,[]}

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> {v, d} = Dict.pop dict, :b
      iex> {v, Enum.sort(d)}
      {nil,[a: 1]}

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> {v, d} = Dict.pop dict, :b, 3
      iex> {v, Enum.sort(d)}
      {3,[a: 1]}

  """
  @spec pop(t, key, value) :: {value, t}
  def pop(dict, key, default \\ nil) do
    target(dict).pop(dict, key, default)
  end

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new
  value. An exception is generated if `key` is not present in the dict.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.update!(d, :a, fn(val) -> -val end)
      iex> Dict.get(d, :a)
      -1

  """
  @spec update!(t, key, (value -> value)) :: t
  def update!(dict, key, fun) do
    target(dict).update!(dict, key, fun)
  end

  @doc """
  Update a value in `dict` by calling `fun` on the value to get a new value. If
  `key` is not present in `dict` then `initial` will be stored as the first
  value.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.update(d, :c, 3, fn(val) -> -val end)
      iex> Dict.get(d, :c)
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

      iex> d = Enum.into([a: 1, b: 2, c: 3, d: 4], dict_impl.new)
      iex> { d1, d2 } = Dict.split(d, [:a, :c, :e])
      iex> { Dict.to_list(d1) |> Enum.sort, Dict.to_list(d2) |> Enum.sort }
      { [a: 1, c: 3], [b: 2, d: 4] }

      iex> d = Enum.into([], dict_impl.new)
      iex> { d1, d2 } = Dict.split(d, [:a, :c])
      iex> { Dict.to_list(d1), Dict.to_list(d2) }
      { [], [] }

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> { d1, d2 } = Dict.split(d, [:a, :b, :c])
      iex> { Dict.to_list(d1) |> Enum.sort, Dict.to_list(d2) }
      { [a: 1, b: 2], [] }

  """
  @spec split(t, [key]) :: {t, t}
  def split(dict, keys) do
    target(dict).split(dict, keys)
  end

  @doc """
  Returns a new dict where the given `keys` are removed from `dict`.
  Any non-member keys are ignored.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.drop(d, [:a, :c, :d])
      iex> Dict.to_list(d)
      [b: 2]

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.drop(d, [:c, :d])
      iex> Dict.to_list(d) |> Enum.sort
      [a: 1, b: 2]

  """
  @spec drop(t, [key]) :: t
  def drop(dict, keys) do
    target(dict).drop(dict, keys)
  end

  @doc """
  Returns a new dict where only the keys in `keys` from `dict` are included.

  Any non-member keys are ignored.

  ## Examples

      iex> d = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> d = Dict.take(d, [:a, :c, :d])
      iex> Dict.to_list(d)
      [a: 1]
      iex> d = Dict.take(d, [:c, :d])
      iex> Dict.to_list(d)
      []

  """
  @spec take(t, [key]) :: t
  def take(dict, keys) do
    target(dict).take(dict, keys)
  end

  @doc false
  @spec empty(t) :: t
  def empty(dict) do
    target(dict).empty(dict)
  end

  @doc """
  Check if two dicts are equal using `===`.

  Notice this function is polymorphic as it compares dicts of any
  type. Each dict implementation also provides an `equal?` function,
  but they can only compare dicts of the same type.

  ## Examples

      iex> a = Enum.into([a: 2, b: 3, f: 5, c: 123], dict_impl.new)
      iex> b = [a: 2, b: 3, f: 5, c: 123]
      iex> Dict.equal?(a, b)
      true

      iex> a = Enum.into([a: 2, b: 3, f: 5, c: 123], dict_impl.new)
      iex> b = []
      iex> Dict.equal?(a, b)
      false

  """
  @spec equal?(t, t) :: boolean
  def equal?(dict1, dict2) do
    target1 = target(dict1)
    target2 = target(dict2)

    cond do
      target1 == target2 ->
        target1.equal?(dict1, dict2)

      target1.size(dict1) == target2.size(dict2) ->
        Enumerable.reduce(dict2, { :cont, true }, fn({ k, v }, _acc) ->
          case target1.fetch(dict1, k) do
            { :ok, ^v } -> { :cont, true }
            _           -> { :halt, false }
          end
        end) |> elem(1)

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

  defp unsupported_dict(dict) do
    raise ArgumentError, message: "unsupported dict: #{inspect dict}"
  end
end
