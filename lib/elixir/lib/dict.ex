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

  ## Default implementation

  Default implementations for some functions in the `Dict` module
  are provided via `use Dict`.

  For example:

      defmodule MyDict do
        use Dict

        # implement required functions (see below)
        # override default implementations if optimization
        # is needed
      end

  The client module must contain the following functions:

    * `delete/2`
    * `fetch/2`
    * `put/3`
    * `reduce/3`
    * `size/1`

  All functions, except `reduce/3`, are required by the Dict behaviour.
  `reduce/3` must be implemented as per the Enumerable protocol.

  Based on these functions, `Dict` generates default implementations
  for the following functions:

    * `drop/2`
    * `equal?/2`
    * `fetch!/2`
    * `get/2`
    * `get/3`
    * `get_lazy/3`
    * `get_and_update/3`
    * `has_key?/2`
    * `keys/1`
    * `merge/2`
    * `merge/3`
    * `pop/2`
    * `pop/3`
    * `pop_lazy/3`
    * `put_new/3`
    * `put_new_lazy/3`
    * `split/2`
    * `take/2`
    * `to_list/1`
    * `update/4`
    * `update!/3`
    * `values/1`

  All of these functions are defined as overridable, so you can provide
  your own implementation if needed.

  Note you can also test your custom module via `Dict`'s doctests:

      defmodule MyDict do
        # ...
      end

      defmodule MyTests do
        use ExUnit.Case
        doctest Dict
        defp dict_impl, do: MyDict
      end

  """

  use Behaviour

  @type key :: any
  @type value :: any
  @type t :: list | map

  defcallback new :: t
  defcallback delete(t, key) :: t
  defcallback drop(t, Enum.t) :: t
  defcallback equal?(t, t) :: boolean
  defcallback get(t, key) :: value
  defcallback get(t, key, value) :: value
  defcallback get_lazy(t, key, (() -> value)) :: value
  defcallback get_and_update(t, key, (value -> {value, value})) :: {value, t}
  defcallback fetch(t, key) :: {:ok, value} | :error
  defcallback fetch!(t, key) :: value | no_return
  defcallback has_key?(t, key) :: boolean
  defcallback keys(t) :: [key]
  defcallback merge(t, t) :: t
  defcallback merge(t, t, (key, value, value -> value)) :: t
  defcallback pop(t, key) :: {value, t}
  defcallback pop(t, key, value) :: {value, t}
  defcallback pop_lazy(t, key, (() -> value)) :: {value, t}
  defcallback put(t, key, value) :: t
  defcallback put_new(t, key, value) :: t
  defcallback put_new_lazy(t, key, (() -> value)) :: t
  defcallback size(t) :: non_neg_integer()
  defcallback split(t, Enum.t) :: {t, t}
  defcallback take(t, Enum.t) :: t
  defcallback to_list(t) :: list()
  defcallback update(t, key, value, (value -> value)) :: t
  defcallback update!(t, key, (value -> value)) :: t | no_return
  defcallback values(t) :: list(value)

  defmacro __using__(_) do
    # Use this import to guarantee proper code expansion
    import Kernel, except: [size: 1]

    quote do
      @behaviour Dict

      def get(dict, key, default \\ nil) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> default
        end
      end

      def get_lazy(dict, key, fun) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> fun.()
        end
      end

      def get_and_update(dict, key, fun) do
        current_value = get(dict, key)
        {get, new_value} = fun.(current_value)
        {get, put(dict, key, new_value)}
      end

      def fetch!(dict, key) do
        case fetch(dict, key) do
          {:ok, value} -> value
          :error -> raise KeyError, key: key, term: dict
        end
      end

      def has_key?(dict, key) do
        match? {:ok, _}, fetch(dict, key)
      end

      def put_new(dict, key, value) do
        case has_key?(dict, key) do
          true  -> dict
          false -> put(dict, key, value)
        end
      end

      def put_new_lazy(dict, key, fun) do
        case has_key?(dict, key) do
          true  -> dict
          false -> put(dict, key, fun.())
        end
      end

      def drop(dict, keys) do
        Enum.reduce(keys, dict, &delete(&2, &1))
      end

      def take(dict, keys) do
        Enum.reduce(keys, new, fn key, acc ->
          case fetch(dict, key) do
            {:ok, value} -> put(acc, key, value)
            :error -> acc
          end
        end)
      end

      def to_list(dict) do
        reduce(dict, {:cont, []}, fn
          kv, acc -> {:cont, [kv|acc]}
        end) |> elem(1) |> :lists.reverse
      end

      def keys(dict) do
        reduce(dict, {:cont, []}, fn
          {k, _}, acc -> {:cont, [k|acc]}
        end) |> elem(1) |> :lists.reverse
      end

      def values(dict) do
        reduce(dict, {:cont, []}, fn
          {_, v}, acc -> {:cont, [v|acc]}
        end) |> elem(1) |> :lists.reverse
      end

      def equal?(dict1, dict2) do
        # Use this import to avoid conflicts in the user code
        import Kernel, except: [size: 1]

        case size(dict1) == size(dict2) do
          false -> false
          true  ->
            reduce(dict1, {:cont, true}, fn({k, v}, _acc) ->
              case fetch(dict2, k) do
                {:ok, ^v} -> {:cont, true}
                _ -> {:halt, false}
              end
            end) |> elem(1)
        end
      end

      def merge(dict1, dict2, fun \\ fn(_k, _v1, v2) -> v2 end) do
        # Use this import to avoid conflicts in the user code
        import Kernel, except: [size: 1]

        if size(dict1) < size(dict2) do
          reduce(dict1, {:cont, dict2}, fn {k, v1}, acc ->
            {:cont, update(acc, k, v1, &fun.(k, v1, &1))}
          end)
        else
          reduce(dict2, {:cont, dict1}, fn {k, v2}, acc ->
            {:cont, update(acc, k, v2, &fun.(k, &1, v2))}
          end)
        end |> elem(1)
      end

      def update(dict, key, initial, fun) do
        case fetch(dict, key) do
          {:ok, value} ->
            put(dict, key, fun.(value))
          :error ->
            put(dict, key, initial)
        end
      end

      def update!(dict, key, fun) do
        case fetch(dict, key) do
          {:ok, value} ->
            put(dict, key, fun.(value))
          :error ->
            raise KeyError, key: key, term: dict
        end
      end

      def pop(dict, key, default \\ nil) do
        case fetch(dict, key) do
          {:ok, value} ->
            {value, delete(dict, key)}
          :error ->
            {default, dict}
        end
      end

      def pop_lazy(dict, key, fun) do
        case fetch(dict, key) do
          {:ok, value} ->
            {value, delete(dict, key)}
          :error ->
            {fun.(), dict}
        end
      end

      def split(dict, keys) do
        Enum.reduce(keys, {new, dict}, fn key, {inc, exc} = acc ->
          case fetch(exc, key) do
            {:ok, value} ->
              {put(inc, key, value), delete(exc, key)}
            :error ->
              acc
          end
        end)
      end

      defoverridable merge: 2, merge: 3, equal?: 2, to_list: 1, keys: 1,
                     values: 1, take: 2, drop: 2, get: 2, get: 3, fetch!: 2,
                     has_key?: 2, put_new: 3, pop: 2, pop: 3, split: 2,
                     update: 4, update!: 3, get_and_update: 3, get_lazy: 3,
                     pop_lazy: 3, put_new_lazy: 3
    end
  end


  defmacrop target(dict) do
    quote do
      case unquote(dict) do
        %{__struct__: x} when is_atom(x) ->
          x
        %{} ->
          Map
        x when is_list(x) ->
          Keyword
        x ->
          unsupported_dict(x)
      end
    end
  end

  @doc """
  Returns a list of all keys in `dict`.
  The keys are not guaranteed to be in any order.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> Enum.sort(Dict.keys(dict))
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

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> Enum.sort(Dict.values(dict))
      [1,2]

  """
  @spec values(t) :: [value]
  def values(dict) do
    target(dict).values(dict)
  end

  @doc """
  Returns the number of elements in `dict`.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> Dict.size(dict)
      2

  """
  @spec size(t) :: non_neg_integer
  def size(dict) do
    target(dict).size(dict)
  end

  @doc """
  Returns whether the given `key` exists in the given `dict`.

  ## Examples

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> Dict.has_key?(dict, :a)
      true
      iex> Dict.has_key?(dict, :b)
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

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> Dict.get(dict, :a)
      1
      iex> Dict.get(dict, :b)
      nil
      iex> Dict.get(dict, :b, 3)
      3
  """
  @spec get(t, key, value) :: value
  def get(dict, key, default \\ nil) do
    target(dict).get(dict, key, default)
  end

  @doc """
  Returns the value associated with `key` in `dict`. If `dict` does not
  contain `key`, it lazily evaluates `fun` and returns its result.

  This is useful if the default value is very expensive to calculate or
  generally difficult to set-up and tear-down again.

  ## Examples

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   :result
      ...> end
      iex> Dict.get_lazy(dict, :a, fun)
      1
      iex> Dict.get_lazy(dict, :b, fun)
      :result

  """
  @spec get_lazy(t, key, (() -> value)) :: value
  def get_lazy(dict, key, fun) do
    target(dict).get_lazy(dict, key, fun)
  end

  @doc """
  Gets a value from `dict` and updates the value at `key` in one pass.

  This `fun` argument receives the value of `key` in `dict` (or `nil` if `key`
  is not present) and must return a two-elements tuple: the "get" value (the
  value retrieved from the dict which can be operated on before being returned)
  and the new value to be stored under `key` in `dict`.

  The returned value is a tuple with the "get" value returned by `fun` and a new
  dict with the updated value under `key`.

  ## Examples

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> {get, new_dict} = Dict.get_and_update dict, :a, fn(current_value) ->
      ...>   {current_value + 1, "foo"}
      ...> end
      iex> get
      2
      iex> Dict.get(new_dict, :a)
      "foo"

  """
  @spec get_and_update(t, key, (value -> {value, value})) :: {value, t}
  def get_and_update(dict, key, fun) do
    target(dict).get_and_update(dict, key, fun)
  end

  @doc """
  Returns `{:ok, value}` associated with `key` in `dict`.
  If `dict` does not contain `key`, returns `:error`.

  ## Examples

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> Dict.fetch(dict, :a)
      {:ok, 1}
      iex> Dict.fetch(dict, :b)
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

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> Dict.fetch!(dict, :a)
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

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.put(dict, :a, 3)
      iex> Dict.get(dict, :a)
      3

  """
  @spec put(t, key, value) :: t
  def put(dict, key, val) do
    target(dict).put(dict, key, val)
  end

  @doc """
  Puts the given `value` under `key` in `dict` unless `key` is already present.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.put_new(dict, :a, 3)
      iex> Dict.get(dict, :a)
      1

  """
  @spec put_new(t, key, value) :: t
  def put_new(dict, key, val) do
    target(dict).put_new(dict, key, val)
  end

  @doc """
  Evaluates `fun` and puts the result under `key` in `dict` unless `key`
  is already present.

  This is useful if the value is very expensive to calculate or generally
  difficult to set-up and tear-down again.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   3
      ...> end
      iex> dict = Dict.put_new_lazy(dict, :a, fun)
      iex> Dict.get(dict, :a)
      1
      iex> dict = Dict.put_new_lazy(dict, :c, fun)
      iex> Dict.get(dict, :c)
      3

  """
  @spec put_new_lazy(t, key, (() -> value)) :: t
  def put_new_lazy(dict, key, fun) do
    target(dict).put_new_lazy(dict, key, fun)
  end

  @doc """
  Removes the entry stored under the given `key` from `dict`.
  If `dict` does not contain `key`, returns the dictionary unchanged.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.delete(dict, :a)
      iex> Dict.get(dict, :a)
      nil

      iex> dict = Enum.into([b: 2], dict_impl.new)
      iex> Dict.delete(dict, :a) == dict
      true

  """
  @spec delete(t, key) :: t
  def delete(dict, key) do
    target(dict).delete(dict, key)
  end

  @doc """
  Merges the dict `dict2` into dict `dict1`.

  If one of the `dict2` entries is found in `dict1`, the
  conflicting entries in `dict2` have higher precedence unless a
  function is given to resolve conflicts.

  Notice this function is polymorphic as it merges dicts of any
  type. Each dict implementation also provides a `merge` function,
  but they can only merge dicts of the same type.

  ## Examples

      iex> dict1 = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict2 = Enum.into([a: 3, d: 4], dict_impl.new)
      iex> dict = Dict.merge(dict1, dict2)
      iex> [a: Dict.get(dict, :a), b: Dict.get(dict, :b), d: Dict.get(dict, :d)]
      [a: 3, b: 2, d: 4]

      iex> dict1 = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict2 = Enum.into([a: 3, d: 4], dict_impl.new)
      iex> dict = Dict.merge(dict1, dict2, fn(_k, v1, v2) ->
      ...>   v1 + v2
      ...> end)
      iex> [a: Dict.get(dict, :a), b: Dict.get(dict, :b), d: Dict.get(dict, :d)]
      [a: 4, b: 2, d: 4]

  """
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(dict1, dict2, fun \\ fn(_k, _v1, v2) -> v2 end) do
    target1 = target(dict1)
    target2 = target(dict2)

    if target1 == target2 do
      target1.merge(dict1, dict2, fun)
    else
      Enumerable.reduce(dict2, {:cont, dict1}, fn({k, v}, acc) ->
        {:cont, target1.update(acc, k, v, fn(other) -> fun.(k, other, v) end)}
      end) |> elem(1)
    end
  end

  @doc """
  Returns the value associated with `key` in `dict` as
  well as the `dict` without `key`.

  If `key` is not present in `dict`, then the `dict` will
  be returned unmodified.

  ## Examples

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> {v, dict} = Dict.pop dict, :a
      iex> {v, Enum.sort(dict)}
      {1,[]}

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> {v, dict} = Dict.pop dict, :b
      iex> {v, Enum.sort(dict)}
      {nil,[a: 1]}

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> {v, dict} = Dict.pop dict, :b, 3
      iex> {v, Enum.sort(dict)}
      {3,[a: 1]}

  """
  @spec pop(t, key, value) :: {value, t}
  def pop(dict, key, default \\ nil) do
    target(dict).pop(dict, key, default)
  end

  @doc """
  Returns the value associated with `key` in `dict` as
  well as the `dict` without `key`.

  If `key` is not present in `dict`, then the `dict` will
  be returned unmodified, and it will lazily evaluate `fun`
  and return its result instead of the missing value.

  This is useful if the default value is very expensive to calculate or
  generally difficult to set-up and tear-down again.

  ## Examples

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   :result
      ...> end
      iex> {v, dict} = Dict.pop_lazy dict, :a, fun
      iex> {v, Enum.sort(dict)}
      {1,[]}

      iex> dict = Enum.into([a: 1], dict_impl.new)
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   :result
      ...> end
      iex> {v, dict} = Dict.pop_lazy dict, :b, fun
      iex> {v, Enum.sort(dict)}
      {:result,[a: 1]}

  """
  @spec pop_lazy(t, key, (() -> value)) :: {value, t}
  def pop_lazy(dict, key, fun) do
    target(dict).pop_lazy(dict, key, fun)
  end

  @doc """
  Updates a value in `dict` by calling `fun` on the value to get a new
  value. An exception is generated if `key` is not present in the dict.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.update!(dict, :a, fn(val) -> -val end)
      iex> Dict.get(dict, :a)
      -1

  """
  @spec update!(t, key, (value -> value)) :: t
  def update!(dict, key, fun) do
    target(dict).update!(dict, key, fun)
  end

  @doc """
  Updates a value in `dict` by calling `fun` on the value to get a new value. If
  `key` is not present in `dict` then `initial` will be stored as the first
  value.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.update(dict, :c, 3, fn(val) -> -val end)
      iex> Dict.get(dict, :c)
      3

  """
  @spec update(t, key, value, (value -> value)) :: t
  def update(dict, key, initial, fun) do
    target(dict).update(dict, key, initial, fun)
  end

  @doc """
  Returns a tuple of two dicts, where the first dict contains only
  entries from `dict` with keys in `keys`, and the second dict
  contains only entries from `dict` with keys not in `keys`.

  All non-member keys are ignored.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2, c: 3, d: 4], dict_impl.new)
      iex> {dict1, dict2} = Dict.split(dict, [:a, :c, :e])
      iex> {Dict.to_list(dict1) |> Enum.sort, Dict.to_list(dict2) |> Enum.sort}
      {[a: 1, c: 3], [b: 2, d: 4]}

      iex> dict = Enum.into([], dict_impl.new)
      iex> {dict1, dict2} = Dict.split(dict, [:a, :c])
      iex> {Dict.to_list(dict1), Dict.to_list(dict2)}
      {[], []}

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> {dict1, dict2} = Dict.split(dict, [:a, :b, :c])
      iex> {Dict.to_list(dict1) |> Enum.sort, Dict.to_list(dict2)}
      {[a: 1, b: 2], []}

  """
  @spec split(t, [key]) :: {t, t}
  def split(dict, keys) do
    target(dict).split(dict, keys)
  end

  @doc """
  Returns a new dict where the given `keys` are removed from `dict`.
  All non-member keys are ignored.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.drop(dict, [:a, :c, :d])
      iex> Dict.to_list(dict)
      [b: 2]

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.drop(dict, [:c, :d])
      iex> Dict.to_list(dict) |> Enum.sort
      [a: 1, b: 2]

  """
  @spec drop(t, [key]) :: t
  def drop(dict, keys) do
    target(dict).drop(dict, keys)
  end

  @doc """
  Returns a new dict where only the keys in `keys` from `dict` are included.

  All non-member keys are ignored.

  ## Examples

      iex> dict = Enum.into([a: 1, b: 2], dict_impl.new)
      iex> dict = Dict.take(dict, [:a, :c, :d])
      iex> Dict.to_list(dict)
      [a: 1]
      iex> dict = Dict.take(dict, [:c, :d])
      iex> Dict.to_list(dict)
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
  Checks if two dicts are equal using `===`.

  Notice this function is polymorphic as it compares dicts of any
  type. Each dict implementation also provides an `equal?` function,
  but they can only compare dicts of the same type.

  ## Examples

      iex> dict1 = Enum.into([a: 2, b: 3, f: 5, c: 123], dict_impl.new)
      iex> dict2 = [a: 2, b: 3, f: 5, c: 123]
      iex> Dict.equal?(dict1, dict2)
      true

      iex> dict1 = Enum.into([a: 2, b: 3, f: 5, c: 123], dict_impl.new)
      iex> dict2 = []
      iex> Dict.equal?(dict1, dict2)
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
        Enumerable.reduce(dict2, {:cont, true}, fn({k, v}, _acc) ->
          case target1.fetch(dict1, k) do
            {:ok, ^v} -> {:cont, true}
            _           -> {:halt, false}
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
    raise ArgumentError, "unsupported dict: #{inspect dict}"
  end
end
