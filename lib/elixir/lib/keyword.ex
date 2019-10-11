defmodule Keyword do
  @moduledoc """
  Keyword lists are lists of two-element tuples, where the first
  element of the tuple is an atom and the second element can be any
  value, used mostly to work with optional values.

  For example, the following is a keyword list:

      [{:exit_on_close, true}, {:active, :once}, {:packet_size, 1024}]

  Elixir provides a special and more concise syntax for keyword lists
  that looks like this:

      [exit_on_close: true, active: :once, packet_size: 1024]

  This is also the syntax that Elixir uses to inspect keyword lists:

      iex> [{:active, :once}]
      [active: :once]

  The two syntaxes are completely equivalent. Like atoms, keywords
  must be composed of Unicode characters such as letters, numbers,
  underscore, and `@`. If the keyword has a character that does not
  belong to the category above, such as spaces, you can wrap it in
  quotes:

      iex> ["exit on close": true]
      ["exit on close": true]

  Wrapping a keyword in quotes does not make it a string. Keywords are
  always atoms. If you use quotes when all characters are a valid part
  of a keyword without quotes, Elixir will warn.

  Note that when keyword lists are passed as the last argument to a function,
  if the shorthand syntax is used then the square brackets around the keyword
  list can be omitted as well. For example, the following:

      String.split("1-0", "-", trim: true, parts: 2)

  is equivalent to:

      String.split("1-0", "-", [trim: true, parts: 2])

  A keyword may have duplicated keys so it is not strictly
  a key-value store. However most of the functions in this module
  behave exactly as a key-value so they work similarly to
  the functions you would find in the `Map` module.

  For example, `Keyword.get/3` will get the first entry matching
  the given key, regardless if duplicated entries exist.
  Similarly, `Keyword.put/3` and `Keyword.delete/2` ensure all
  duplicated entries for a given key are removed when invoked.
  Note that operations that require keys to be found in the keyword
  list (like `Keyword.get/3`) need to traverse the list in order
  to find keys, so these operations may be slower than their map
  counterparts.

  A handful of functions exist to handle duplicated keys, in
  particular, `Enum.into/2` allows creating new keywords without
  removing duplicated keys, `get_values/2` returns all values for
  a given key and `delete_first/2` deletes just one of the existing
  entries.

  The functions in `Keyword` do not guarantee any property when
  it comes to ordering. However, since a keyword list is simply a
  list, all the operations defined in `Enum` and `List` can be
  applied too, especially when ordering is required.

  Most of the functions in this module work in linear time. This means
  that, the time it takes to perform an operation grows at the same
  rate as the length of the list.
  """

  @compile :inline_list_funcs

  @type key :: atom
  @type value :: any

  @type t :: [{key, value}]
  @type t(value) :: [{key, value}]

  @doc """
  Returns `true` if `term` is a keyword list; otherwise returns `false`.

  ## Examples

      iex> Keyword.keyword?([])
      true
      iex> Keyword.keyword?(a: 1)
      true
      iex> Keyword.keyword?([{Foo, 1}])
      true
      iex> Keyword.keyword?([{}])
      false
      iex> Keyword.keyword?([:key])
      false
      iex> Keyword.keyword?(%{})
      false

  """
  @spec keyword?(term) :: boolean
  def keyword?(term)

  def keyword?([{key, _value} | rest]) when is_atom(key), do: keyword?(rest)
  def keyword?([]), do: true
  def keyword?(_other), do: false

  @doc """
  Returns an empty keyword list, i.e. an empty list.

  ## Examples

      iex> Keyword.new()
      []

  """
  @spec new :: []
  def new, do: []

  @doc """
  Creates a keyword list from an enumerable.

  Duplicated entries are removed, the latest one prevails.
  Unlike `Enum.into(enumerable, [])`, `Keyword.new(enumerable)`
  guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([{:b, 1}, {:a, 2}])
      [b: 1, a: 2]

      iex> Keyword.new([{:a, 1}, {:a, 2}, {:a, 3}])
      [a: 3]

  """
  @spec new(Enum.t()) :: t
  def new(pairs) do
    new(pairs, fn pair -> pair end)
  end

  @doc """
  Creates a keyword list from an enumerable via the transformation function.

  Duplicated entries are removed, the latest one prevails.
  Unlike `Enum.into(enumerable, [], fun)`,
  `Keyword.new(enumerable, fun)` guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([:a, :b], fn x -> {x, x} end)
      [a: :a, b: :b]

  """
  @spec new(Enum.t(), (term -> {key, value})) :: t
  def new(pairs, transform) when is_function(transform, 1) do
    fun = fn el, acc ->
      {k, v} = transform.(el)
      put_new(acc, k, v)
    end

    :lists.foldl(fun, [], Enum.reverse(pairs))
  end

  @doc """
  Gets the value for a specific `key`.

  If `key` does not exist, return the default value
  (`nil` if no default value).

  If duplicated entries exist, the first one is returned.
  Use `get_values/2` to retrieve all entries.

  ## Examples

      iex> Keyword.get([], :a)
      nil
      iex> Keyword.get([a: 1], :a)
      1
      iex> Keyword.get([a: 1], :b)
      nil
      iex> Keyword.get([a: 1], :b, 3)
      3

  With duplicated keys:

      iex> Keyword.get([a: 1, a: 2], :a, 3)
      1
      iex> Keyword.get([a: 1, a: 2], :b, 3)
      3

  """
  @spec get(t, key, value) :: value
  def get(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> default
    end
  end

  @doc """
  Gets the value for a specific `key`.

  If `key` does not exist, lazily evaluates `fun` and returns its result.

  This is useful if the default value is very expensive to calculate or
  generally difficult to setup and teardown again.

  If duplicated entries exist, the first one is returned.
  Use `get_values/2` to retrieve all entries.

  ## Examples

      iex> keyword = [a: 1]
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   13
      ...> end
      iex> Keyword.get_lazy(keyword, :a, fun)
      1
      iex> Keyword.get_lazy(keyword, :b, fun)
      13

  """
  @spec get_lazy(t, key, (() -> value)) :: value
  def get_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> fun.()
    end
  end

  @doc """
  Gets the value from `key` and updates it, all in one pass.

  This `fun` argument receives the value of `key` (or `nil` if `key`
  is not present) and must return a two-element tuple: the "get" value
  (the retrieved value, which can be operated on before being returned)
  and the new value to be stored under `key`. The `fun` may also
  return `:pop`, implying the current value shall be removed from the
  keyword list and returned.

  The returned value is a tuple with the "get" value returned by
  `fun` and a new keyword list with the updated value under `key`.

  ## Examples

      iex> Keyword.get_and_update([a: 1], :a, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      {1, [a: "new value!"]}

      iex> Keyword.get_and_update([a: 1], :b, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      {nil, [b: "new value!", a: 1]}

      iex> Keyword.get_and_update([a: 1], :a, fn _ -> :pop end)
      {1, []}

      iex> Keyword.get_and_update([a: 1], :b, fn _ -> :pop end)
      {nil, [a: 1]}

  """
  @spec get_and_update(t, key, (value -> {get, value} | :pop)) :: {get, t} when get: term
  def get_and_update(keywords, key, fun)
      when is_list(keywords) and is_atom(key),
      do: get_and_update(keywords, [], key, fun)

  defp get_and_update([{key, current} | t], acc, key, fun) do
    case fun.(current) do
      {get, value} ->
        {get, :lists.reverse(acc, [{key, value} | t])}

      :pop ->
        {current, :lists.reverse(acc, t)}

      other ->
        raise "the given function must return a two-element tuple or :pop, got: #{inspect(other)}"
    end
  end

  defp get_and_update([{_, _} = h | t], acc, key, fun), do: get_and_update(t, [h | acc], key, fun)

  defp get_and_update([], acc, key, fun) do
    case fun.(nil) do
      {get, update} ->
        {get, [{key, update} | :lists.reverse(acc)]}

      :pop ->
        {nil, :lists.reverse(acc)}

      other ->
        raise "the given function must return a two-element tuple or :pop, got: #{inspect(other)}"
    end
  end

  @doc """
  Gets the value from `key` and updates it. Raises if there is no `key`.

  This `fun` argument receives the value of `key` and must return a
  two-element tuple: the "get" value (the retrieved value, which can be
  operated on before being returned) and the new value to be stored under
  `key`.

  The returned value is a tuple with the "get" value returned by `fun` and a new
  keyword list with the updated value under `key`.

  ## Examples

      iex> Keyword.get_and_update!([a: 1], :a, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      {1, [a: "new value!"]}

      iex> Keyword.get_and_update!([a: 1], :b, fn current_value ->
      ...>   {current_value, "new value!"}
      ...> end)
      ** (KeyError) key :b not found in: [a: 1]

      iex> Keyword.get_and_update!([a: 1], :a, fn _ ->
      ...>   :pop
      ...> end)
      {1, []}

  """
  @spec get_and_update!(t, key, (value -> {get, value})) :: {get, t} when get: term
  def get_and_update!(keywords, key, fun) do
    get_and_update!(keywords, key, fun, [])
  end

  defp get_and_update!([{key, value} | keywords], key, fun, acc) do
    case fun.(value) do
      {get, value} ->
        {get, :lists.reverse(acc, [{key, value} | delete(keywords, key)])}

      :pop ->
        {value, :lists.reverse(acc, keywords)}

      other ->
        raise "the given function must return a two-element tuple or :pop, got: #{inspect(other)}"
    end
  end

  defp get_and_update!([{_, _} = e | keywords], key, fun, acc) do
    get_and_update!(keywords, key, fun, [e | acc])
  end

  defp get_and_update!([], key, _fun, acc) when is_atom(key) do
    raise(KeyError, key: key, term: acc)
  end

  @doc """
  Fetches the value for a specific `key` and returns it in a tuple.

  If the `key` does not exist, returns `:error`.

  ## Examples

      iex> Keyword.fetch([a: 1], :a)
      {:ok, 1}
      iex> Keyword.fetch([a: 1], :b)
      :error

  """
  @spec fetch(t, key) :: {:ok, value} | :error
  def fetch(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> {:ok, value}
      false -> :error
    end
  end

  @doc """
  Fetches the value for specific `key`.

  If `key` does not exist, a `KeyError` is raised.

  ## Examples

      iex> Keyword.fetch!([a: 1], :a)
      1
      iex> Keyword.fetch!([a: 1], :b)
      ** (KeyError) key :b not found in: [a: 1]

  """
  @spec fetch!(t, key) :: value
  def fetch!(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> raise(KeyError, key: key, term: keywords)
    end
  end

  @doc """
  Gets all values for a specific `key`.

  ## Examples

      iex> Keyword.get_values([], :a)
      []
      iex> Keyword.get_values([a: 1], :a)
      [1]
      iex> Keyword.get_values([a: 1, a: 2], :a)
      [1, 2]

  """
  @spec get_values(t, key) :: [value]
  def get_values(keywords, key) when is_list(keywords) and is_atom(key) do
    get_values(keywords, key, [])
  end

  defp get_values([{key, value} | tail], key, values), do: get_values(tail, key, [value | values])
  defp get_values([{_, _} | tail], key, values), do: get_values(tail, key, values)
  defp get_values([], _key, values), do: :lists.reverse(values)

  @doc """
  Returns all keys from the keyword list.

  Duplicated keys appear duplicated in the final list of keys.

  ## Examples

      iex> Keyword.keys(a: 1, b: 2)
      [:a, :b]
      iex> Keyword.keys(a: 1, b: 2, a: 3)
      [:a, :b, :a]

  """
  @spec keys(t) :: [key]
  def keys(keywords) when is_list(keywords) do
    :lists.map(fn {k, _} -> k end, keywords)
  end

  @doc """
  Returns all values from the keyword list.

  Values from duplicated keys will be kept in the final list of values.

  ## Examples

      iex> Keyword.values(a: 1, b: 2)
      [1, 2]
      iex> Keyword.values(a: 1, b: 2, a: 3)
      [1, 2, 3]

  """
  @spec values(t) :: [value]
  def values(keywords) when is_list(keywords) do
    :lists.map(fn {_, v} -> v end, keywords)
  end

  @doc false
  @deprecated "Use Keyword.fetch/2 + Keyword.delete/2 instead"
  def delete(keywords, key, value) when is_list(keywords) and is_atom(key) do
    case :lists.keymember(key, 1, keywords) do
      true -> delete_key_value(keywords, key, value)
      _ -> keywords
    end
  end

  defp delete_key_value([{key, value} | tail], key, value) do
    delete_key_value(tail, key, value)
  end

  defp delete_key_value([{_, _} = pair | tail], key, value) do
    [pair | delete_key_value(tail, key, value)]
  end

  defp delete_key_value([], _key, _value) do
    []
  end

  @doc """
  Deletes the entries in the keyword list for a specific `key`.

  If the `key` does not exist, returns the keyword list unchanged.
  Use `delete_first/2` to delete just the first entry in case of
  duplicated keys.

  ## Examples

      iex> Keyword.delete([a: 1, b: 2], :a)
      [b: 2]
      iex> Keyword.delete([a: 1, b: 2, a: 3], :a)
      [b: 2]
      iex> Keyword.delete([b: 2], :a)
      [b: 2]

  """
  @spec delete(t, key) :: t
  @compile {:inline, delete: 2}
  def delete(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keymember(key, 1, keywords) do
      true -> delete_key(keywords, key)
      _ -> keywords
    end
  end

  defp delete_key([{key, _} | tail], key), do: delete_key(tail, key)
  defp delete_key([{_, _} = pair | tail], key), do: [pair | delete_key(tail, key)]
  defp delete_key([], _key), do: []

  @doc """
  Deletes the first entry in the keyword list for a specific `key`.

  If the `key` does not exist, returns the keyword list unchanged.

  ## Examples

      iex> Keyword.delete_first([a: 1, b: 2, a: 3], :a)
      [b: 2, a: 3]
      iex> Keyword.delete_first([b: 2], :a)
      [b: 2]

  """
  @spec delete_first(t, key) :: t
  def delete_first(keywords, key) when is_list(keywords) and is_atom(key) do
    case :lists.keymember(key, 1, keywords) do
      true -> delete_first_key(keywords, key)
      _ -> keywords
    end
  end

  defp delete_first_key([{key, _} | tail], key) do
    tail
  end

  defp delete_first_key([{_, _} = pair | tail], key) do
    [pair | delete_first_key(tail, key)]
  end

  defp delete_first_key([], _key) do
    []
  end

  @doc """
  Puts the given `value` under `key`.

  If a previous value is already stored, all entries are
  removed and the value is overridden.

  ## Examples

      iex> Keyword.put([a: 1], :b, 2)
      [b: 2, a: 1]
      iex> Keyword.put([a: 1, b: 2], :a, 3)
      [a: 3, b: 2]
      iex> Keyword.put([a: 1, b: 2, a: 4], :a, 3)
      [a: 3, b: 2]

  """
  @spec put(t, key, value) :: t
  def put(keywords, key, value) when is_list(keywords) and is_atom(key) do
    [{key, value} | delete(keywords, key)]
  end

  @doc """
  Evaluates `fun` and puts the result under `key`
  in keyword list unless `key` is already present.

  This is useful if the value is very expensive to calculate or
  generally difficult to setup and teardown again.

  ## Examples

      iex> keyword = [a: 1]
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   3
      ...> end
      iex> Keyword.put_new_lazy(keyword, :a, fun)
      [a: 1]
      iex> Keyword.put_new_lazy(keyword, :b, fun)
      [b: 3, a: 1]

  """
  @spec put_new_lazy(t, key, (() -> value)) :: t
  def put_new_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, _} -> keywords
      false -> [{key, fun.()} | keywords]
    end
  end

  @doc """
  Puts the given `value` under `key` unless the entry `key`
  already exists.

  ## Examples

      iex> Keyword.put_new([a: 1], :b, 2)
      [b: 2, a: 1]
      iex> Keyword.put_new([a: 1, b: 2], :a, 3)
      [a: 1, b: 2]

  """
  @spec put_new(t, key, value) :: t
  def put_new(keywords, key, value) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, _} -> keywords
      false -> [{key, value} | keywords]
    end
  end

  @doc false
  @deprecated "Use Keyword.fetch/2 + Keyword.put/3 instead"
  def replace(keywords, key, value) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, _} -> [{key, value} | delete(keywords, key)]
      false -> keywords
    end
  end

  @doc """
  Alters the value stored under `key` to `value`, but only
  if the entry `key` already exists in `keywords`.

  If `key` is not present in `keywords`, a `KeyError` exception is raised.

  ## Examples

      iex> Keyword.replace!([a: 1, b: 2, a: 3], :a, :new)
      [a: :new, b: 2]
      iex> Keyword.replace!([a: 1, b: 2, c: 3, b: 4], :b, :new)
      [a: 1, b: :new, c: 3]

      iex> Keyword.replace!([a: 1], :b, 2)
      ** (KeyError) key :b not found in: [a: 1]

  """
  @doc since: "1.5.0"
  @spec replace!(t, key, value) :: t
  def replace!(keywords, key, value) when is_list(keywords) and is_atom(key) do
    replace!(keywords, key, value, keywords)
  end

  defp replace!([{key, _} | keywords], key, value, _original) do
    [{key, value} | delete(keywords, key)]
  end

  defp replace!([{_, _} = e | keywords], key, value, original) do
    [e | replace!(keywords, key, value, original)]
  end

  defp replace!([], key, _value, original) when is_atom(key) do
    raise(KeyError, key: key, term: original)
  end

  @doc """
  Checks if two keywords are equal.

  Two keywords are considered to be equal if they contain
  the same keys and those keys contain the same values.

  ## Examples

      iex> Keyword.equal?([a: 1, b: 2], [b: 2, a: 1])
      true
      iex> Keyword.equal?([a: 1, b: 2], [b: 1, a: 2])
      false
      iex> Keyword.equal?([a: 1, b: 2, a: 3], [b: 2, a: 3, a: 1])
      true

  """
  @spec equal?(t, t) :: boolean
  def equal?(left, right) when is_list(left) and is_list(right) do
    :lists.sort(left) == :lists.sort(right)
  end

  @doc """
  Merges two keyword lists into one.

  All keys, including duplicated keys, given in `keywords2` will be added
  to `keywords1`, overriding any existing one.

  There are no guarantees about the order of keys in the returned keyword.

  ## Examples

      iex> Keyword.merge([a: 1, b: 2], [a: 3, d: 4])
      [b: 2, a: 3, d: 4]

      iex> Keyword.merge([a: 1, b: 2], [a: 3, d: 4, a: 5])
      [b: 2, a: 3, d: 4, a: 5]

      iex> Keyword.merge([a: 1], [2, 3])
      ** (ArgumentError) expected a keyword list as the second argument, got: [2, 3]

  """
  @spec merge(t, t) :: t
  def merge(keywords1, keywords2)

  def merge(keywords1, []) when is_list(keywords1), do: keywords1
  def merge([], keywords2) when is_list(keywords2), do: keywords2

  def merge(keywords1, keywords2) when is_list(keywords1) and is_list(keywords2) do
    if keyword?(keywords2) do
      fun = fn
        {key, _value} when is_atom(key) ->
          not has_key?(keywords2, key)

        _ ->
          raise ArgumentError,
                "expected a keyword list as the first argument, got: #{inspect(keywords1)}"
      end

      :lists.filter(fun, keywords1) ++ keywords2
    else
      raise ArgumentError,
            "expected a keyword list as the second argument, got: #{inspect(keywords2)}"
    end
  end

  @doc """
  Merges two keyword lists into one.

  All keys, including duplicated keys, given in `keywords2` will be added
  to `keywords1`. The given function will be invoked to solve conflicts.

  If `keywords2` has duplicate keys, the given function will be invoked
  for each matching pair in `keywords1`.

  There are no guarantees about the order of keys in the returned keyword.

  ## Examples

      iex> Keyword.merge([a: 1, b: 2], [a: 3, d: 4], fn _k, v1, v2 ->
      ...>   v1 + v2
      ...> end)
      [b: 2, a: 4, d: 4]

      iex> Keyword.merge([a: 1, b: 2], [a: 3, d: 4, a: 5], fn :a, v1, v2 ->
      ...>   v1 + v2
      ...> end)
      [b: 2, a: 4, d: 4, a: 5]

      iex> Keyword.merge([a: 1, b: 2, a: 3], [a: 3, d: 4, a: 5], fn :a, v1, v2 ->
      ...>   v1 + v2
      ...> end)
      [b: 2, a: 4, d: 4, a: 8]

      iex> Keyword.merge([a: 1, b: 2], [:a, :b], fn :a, v1, v2 ->
      ...>   v1 + v2
      ...> end)
      ** (ArgumentError) expected a keyword list as the second argument, got: [:a, :b]

  """
  @spec merge(t, t, (key, value, value -> value)) :: t
  def merge(keywords1, keywords2, fun)
      when is_list(keywords1) and is_list(keywords2) and is_function(fun, 3) do
    if keyword?(keywords1) do
      do_merge(keywords2, [], keywords1, keywords1, fun, keywords2)
    else
      raise ArgumentError,
            "expected a keyword list as the first argument, got: #{inspect(keywords1)}"
    end
  end

  defp do_merge([{key, value2} | tail], acc, rest, original, fun, keywords2) when is_atom(key) do
    case :lists.keyfind(key, 1, original) do
      {^key, value1} ->
        acc = [{key, fun.(key, value1, value2)} | acc]
        original = :lists.keydelete(key, 1, original)
        do_merge(tail, acc, delete(rest, key), original, fun, keywords2)

      false ->
        do_merge(tail, [{key, value2} | acc], rest, original, fun, keywords2)
    end
  end

  defp do_merge([], acc, rest, _original, _fun, _keywords2) do
    rest ++ :lists.reverse(acc)
  end

  defp do_merge(_other, _acc, _rest, _original, _fun, keywords2) do
    raise ArgumentError,
          "expected a keyword list as the second argument, got: #{inspect(keywords2)}"
  end

  @doc """
  Returns whether a given `key` exists in the given `keywords`.

  ## Examples

      iex> Keyword.has_key?([a: 1], :a)
      true
      iex> Keyword.has_key?([a: 1], :b)
      false

  """
  @spec has_key?(t, key) :: boolean
  def has_key?(keywords, key) when is_list(keywords) and is_atom(key) do
    :lists.keymember(key, 1, keywords)
  end

  @doc """
  Updates the `key` with the given function.

  If the `key` does not exist, raises `KeyError`.

  If there are duplicated keys, they are all removed and only the first one
  is updated.

  ## Examples

      iex> Keyword.update!([a: 1, b: 2, a: 3], :a, &(&1 * 2))
      [a: 2, b: 2]
      iex> Keyword.update!([a: 1, b: 2, c: 3], :b, &(&1 * 2))
      [a: 1, b: 4, c: 3]

      iex> Keyword.update!([a: 1], :b, &(&1 * 2))
      ** (KeyError) key :b not found in: [a: 1]

  """
  @spec update!(t, key, (value -> value)) :: t
  def update!(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 1) do
    update!(keywords, key, fun, keywords)
  end

  defp update!([{key, value} | keywords], key, fun, _original) do
    [{key, fun.(value)} | delete(keywords, key)]
  end

  defp update!([{_, _} = e | keywords], key, fun, original) do
    [e | update!(keywords, key, fun, original)]
  end

  defp update!([], key, _fun, original) when is_atom(key) do
    raise(KeyError, key: key, term: original)
  end

  @doc """
  Updates the `key` in `keywords` with the given function.

  If the `key` does not exist, inserts the given `initial` value.

  If there are duplicated keys, they are all removed and only the first one
  is updated.

  ## Examples

      iex> Keyword.update([a: 1], :a, 13, &(&1 * 2))
      [a: 2]
      iex> Keyword.update([a: 1, a: 2], :a, 13, &(&1 * 2))
      [a: 2]
      iex> Keyword.update([a: 1], :b, 11, &(&1 * 2))
      [a: 1, b: 11]

  """
  @spec update(t, key, value, (value -> value)) :: t
  def update(keywords, key, initial, fun)

  def update([{key, value} | keywords], key, _initial, fun) do
    [{key, fun.(value)} | delete(keywords, key)]
  end

  def update([{_, _} = e | keywords], key, initial, fun) do
    [e | update(keywords, key, initial, fun)]
  end

  def update([], key, initial, _fun) when is_atom(key) do
    [{key, initial}]
  end

  @doc """
  Takes all entries corresponding to the given keys and extracts them into a
  separate keyword list.

  Returns a tuple with the new list and the old list with removed keys.

  Keys for which there are no entries in the keyword list are ignored.

  Entries with duplicated keys end up in the same keyword list.

  ## Examples

      iex> Keyword.split([a: 1, b: 2, c: 3], [:a, :c, :e])
      {[a: 1, c: 3], [b: 2]}
      iex> Keyword.split([a: 1, b: 2, c: 3, a: 4], [:a, :c, :e])
      {[a: 1, c: 3, a: 4], [b: 2]}

  """
  @spec split(t, [key]) :: {t, t}
  def split(keywords, keys) when is_list(keywords) and is_list(keys) do
    fun = fn {k, v}, {take, drop} ->
      case k in keys do
        true -> {[{k, v} | take], drop}
        false -> {take, [{k, v} | drop]}
      end
    end

    acc = {[], []}
    {take, drop} = :lists.foldl(fun, acc, keywords)
    {:lists.reverse(take), :lists.reverse(drop)}
  end

  @doc """
  Takes all entries corresponding to the given keys and returns them in a new
  keyword list.

  Duplicated keys are preserved in the new keyword list.

  ## Examples

      iex> Keyword.take([a: 1, b: 2, c: 3], [:a, :c, :e])
      [a: 1, c: 3]
      iex> Keyword.take([a: 1, b: 2, c: 3, a: 5], [:a, :c, :e])
      [a: 1, c: 3, a: 5]

  """
  @spec take(t, [key]) :: t
  def take(keywords, keys) when is_list(keywords) and is_list(keys) do
    :lists.filter(fn {k, _} -> k in keys end, keywords)
  end

  @doc """
  Drops the given keys from the keyword list.

  Duplicated keys are preserved in the new keyword list.

  ## Examples

      iex> Keyword.drop([a: 1, b: 2, c: 3], [:b, :d])
      [a: 1, c: 3]
      iex> Keyword.drop([a: 1, b: 2, b: 3, c: 3, a: 5], [:b, :d])
      [a: 1, c: 3, a: 5]

  """
  @spec drop(t, [key]) :: t
  def drop(keywords, keys) when is_list(keywords) and is_list(keys) do
    :lists.filter(fn {key, _} -> key not in keys end, keywords)
  end

  @doc """
  Returns the first value for `key` and removes all associated entries in the keyword list.

  It returns a tuple where the first element is the first value for `key` and the
  second element is a keyword list with all entries associated with `key` removed.
  If the `key` is not present in the keyword list, `{default, keyword_list}` is
  returned.

  If you don't want to remove all the entries associated with `key` use `pop_first/3`
  instead, that function will remove only the first entry.

  ## Examples

      iex> Keyword.pop([a: 1], :a)
      {1, []}
      iex> Keyword.pop([a: 1], :b)
      {nil, [a: 1]}
      iex> Keyword.pop([a: 1], :b, 3)
      {3, [a: 1]}
      iex> Keyword.pop([a: 1, a: 2], :a)
      {1, []}

  """
  @spec pop(t, key, value) :: {value, t}
  def pop(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case fetch(keywords, key) do
      {:ok, value} -> {value, delete(keywords, key)}
      :error -> {default, keywords}
    end
  end

  @doc """
  Returns the first value for `key` and removes all associated antries in the keyword list,
  raising if `key` is not present.

  This function behaves like `pop/3`, but raises in cases the `key` is not present in the
  given `keywords`.

  ## Examples

      iex> Keyword.pop!([a: 1], :a)
      {1, []}
      iex> Keyword.pop!([a: 1, a: 2], :a)
      {1, []}
      iex> Keyword.pop!([a: 1], :b)
      ** (KeyError) key :b not found in: [a: 1]

  """
  @doc since: "1.10.0"
  @spec pop!(t, key) :: {value, t}
  def pop!(keywords, key) when is_list(keywords) and is_atom(key) do
    case fetch(keywords, key) do
      {:ok, value} -> {value, delete(keywords, key)}
      :error -> raise KeyError, key: key, term: keywords
    end
  end

  @doc """
  Returns all values for `key` and removes all associated entries in the keyword list.

  It returns a tuple where the first element is a list of values for `key` and the
  second element is a keyword list with all entries associated with `key` removed.
  If the `key` is not present in the keyword list, `{[], keyword_list}` is
  returned.

  If you don't want to remove all the entries associated with `key` use `pop_first/3`
  instead, that function will remove only the first entry.

  ## Examples

      iex> Keyword.pop_values([a: 1], :a)
      {[1], []}
      iex> Keyword.pop_values([a: 1], :b)
      {[], [a: 1]}
      iex> Keyword.pop_values([a: 1, a: 2], :a)
      {[1, 2], []}

  """
  @doc since: "1.10.0"
  @spec pop_values(t, key) :: {[value], t}
  def pop_values(keywords, key) when is_list(keywords) and is_atom(key) do
    pop_values(:lists.reverse(keywords), key, [], [])
  end

  defp pop_values([{key, value} | tail], key, values, acc),
    do: pop_values(tail, key, [value | values], acc)

  defp pop_values([{_, _} = pair | tail], key, values, acc),
    do: pop_values(tail, key, values, [pair | acc])

  defp pop_values([], _key, values, acc),
    do: {values, acc}

  @doc """
  Lazily returns and removes all values associated with `key` in the keyword list.

  This is useful if the default value is very expensive to calculate or
  generally difficult to setup and teardown again.

  All duplicated keys are removed. See `pop_first/3` for
  removing only the first entry.

  ## Examples

      iex> keyword = [a: 1]
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   13
      ...> end
      iex> Keyword.pop_lazy(keyword, :a, fun)
      {1, []}
      iex> Keyword.pop_lazy(keyword, :b, fun)
      {13, [a: 1]}

  """
  @spec pop_lazy(t, key, (() -> value)) :: {value, t}
  def pop_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case fetch(keywords, key) do
      {:ok, value} ->
        {value, delete(keywords, key)}

      :error ->
        {fun.(), keywords}
    end
  end

  @doc """
  Returns and removes the first value associated with `key` in the keyword list.

  Duplicated keys are not removed.

  ## Examples

      iex> Keyword.pop_first([a: 1], :a)
      {1, []}
      iex> Keyword.pop_first([a: 1], :b)
      {nil, [a: 1]}
      iex> Keyword.pop_first([a: 1], :b, 3)
      {3, [a: 1]}
      iex> Keyword.pop_first([a: 1, a: 2], :a)
      {1, [a: 2]}

  """
  @spec pop_first(t, key, value) :: {value, t}
  def pop_first(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case :lists.keytake(key, 1, keywords) do
      {:value, {^key, value}, rest} -> {value, rest}
      false -> {default, keywords}
    end
  end

  @doc """
  Returns the keyword list itself.

  ## Examples

      iex> Keyword.to_list(a: 1)
      [a: 1]

  """
  @spec to_list(t) :: t
  def to_list(keyword) when is_list(keyword) do
    keyword
  end

  @doc false
  @deprecated "Use Kernel.length/1 instead"
  def size(keyword) do
    length(keyword)
  end
end
