defmodule Keyword do
  @moduledoc """
  A keyword list is a list that consists exclusively of two-element tuples.

  The first element of these tuples is known as the *key*, and it must be an atom.
  The second element, known as the *value*, can be any term.

  Keywords are mostly used to work with optional values. For a general introduction
  to keywords and how they compare with maps, see our [Keyword and Maps](keywords-and-maps.md)
  guide.

  ## Examples

  For example, the following is a keyword list:

      [{:exit_on_close, true}, {:active, :once}, {:packet_size, 1024}]

  Elixir provides a special and more concise syntax for keyword lists:

      [exit_on_close: true, active: :once, packet_size: 1024]

  The two syntaxes return the exact same value.

  A *key* can be any atom, consisting of Unicode letters, numbers,
  an underscore or the `@` sign. If the *key* should have any other
  characters, such as spaces, you can wrap it in quotes:

      iex> ["exit on close": true]
      ["exit on close": true]

  Wrapping an atom in quotes does not make it a string. Keyword list
  *keys* are always atoms. Quotes should only be used when necessary
  or Elixir will issue a warning.

  ## Duplicate keys and ordering

  A keyword may have duplicate keys so it is not strictly a key-value
  data type. However, most of the functions in this module work on a
  key-value structure and behave similar to the functions you would
  find in the `Map` module. For example, `Keyword.get/3` will get the first
  entry matching the given key, regardless if duplicate entries exist.
  Similarly, `Keyword.put/3` and `Keyword.delete/2` ensure all duplicate
  entries for a given key are removed when invoked. Note, however, that
  keyword list operations need to traverse the whole list in order to find
  keys, so these operations are slower than their map counterparts.

  A handful of functions exist to handle duplicate keys, for example,
  `get_values/2` returns all values for a given key and `delete_first/2`
  deletes just the first entry of the existing ones.

  Even though lists preserve the existing order, the functions in
  `Keyword` do not guarantee any ordering. For example, if you invoke
  `Keyword.put(opts, new_key, new_value)`, there is no guarantee for
  where `new_key` will be added to (the front, the end or anywhere else).

  Given ordering is not guaranteed, it is not recommended to pattern
  match on keyword lists either. For example, a function such as:

      def my_function([some_key: value, another_key: another_value])

  will match

      my_function([some_key: :foo, another_key: :bar])

  but it won't match

      my_function([another_key: :bar, some_key: :foo])

  Most of the functions in this module work in linear time. This means
  that the time it takes to perform an operation grows at the same
  rate as the length of the list.

  ## Call syntax

  When keyword lists are passed as the last argument to a function,
  the square brackets around the keyword list can be omitted. For
  example, the keyword list syntax:

      String.split("1-0", "-", [trim: true, parts: 2])

  can be written without the enclosing brackets whenever it is the last
  argument of a function call:

      String.split("1-0", "-", trim: true, parts: 2)

  Since tuples, lists and maps are treated similarly to function
  arguments in Elixir syntax, this property is also available to them:

      iex> {1, 2, foo: :bar}
      {1, 2, [{:foo, :bar}]}

      iex> [1, 2, foo: :bar]
      [1, 2, {:foo, :bar}]

      iex> %{1 => 2, foo: :bar}
      %{1 => 2, :foo => :bar}

  """

  @compile :inline_list_funcs

  @type key :: atom
  @type value :: any

  @typedoc since: "1.17.0"
  @type default :: any

  @type t :: [{key, value}]
  @type t(value) :: [{key, value}]

  @doc """
  Builds a keyword from the given `keys` and the fixed `value`.

  ## Examples

      iex> Keyword.from_keys([:foo, :bar, :baz], :atom)
      [foo: :atom, bar: :atom, baz: :atom]
      iex> Keyword.from_keys([], :atom)
      []

  """
  @doc since: "1.14.0"
  @spec from_keys([key], value) :: t(value)
  def from_keys(keys, value) when is_list(keys) do
    :lists.map(&{&1, value}, keys)
  end

  @doc """
  Returns `true` if `term` is a keyword list, otherwise `false`.

  When `term` is a list it is traversed to the end.

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

  Removes duplicate entries and the last one prevails.
  Unlike `Enum.into(enumerable, [])`, `Keyword.new(enumerable)`
  guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([{:b, 1}, {:a, 2}])
      [b: 1, a: 2]

      iex> Keyword.new([{:a, 1}, {:a, 2}, {:a, 3}])
      [a: 3]

  """
  @spec new(Enumerable.t()) :: t
  def new(pairs) do
    new(pairs, fn pair -> pair end)
  end

  @doc """
  Creates a keyword list from an enumerable via the transformation function.

  Removes duplicate entries and the last one prevails.
  Unlike `Enum.into(enumerable, [], fun)`,
  `Keyword.new(enumerable, fun)` guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([:a, :b], fn x -> {x, x} end)
      [a: :a, b: :b]

  """
  @spec new(Enumerable.t(), (term -> {key, value})) :: t
  def new(pairs, transform) when is_function(transform, 1) do
    fun = fn el, acc ->
      {k, v} = transform.(el)
      put_new(acc, k, v)
    end

    :lists.foldl(fun, [], Enum.reverse(pairs))
  end

  @doc """
  Ensures the given `keyword` has only the keys given in `values`.

  The second argument must be a list of atoms, specifying
  a given key, or tuples specifying a key and a default value.

  If the keyword list has only the given keys, it returns
  `{:ok, keyword}` with default values applied. Otherwise it
  returns `{:error, invalid_keys}` with invalid keys.

  See also: `validate!/2`.

  ## Examples

      iex> {:ok, result} = Keyword.validate([], [one: 1, two: 2])
      iex> Enum.sort(result)
      [one: 1, two: 2]

      iex> {:ok, result} = Keyword.validate([two: 3], [one: 1, two: 2])
      iex> Enum.sort(result)
      [one: 1, two: 3]

  If atoms are given, they are supported as keys but do not
  provide a default value:

      iex> {:ok, result} = Keyword.validate([], [:one, two: 2])
      iex> Enum.sort(result)
      [two: 2]

      iex> {:ok, result} = Keyword.validate([one: 1], [:one, two: 2])
      iex> Enum.sort(result)
      [one: 1, two: 2]

  Passing unknown keys returns an error:

      iex> Keyword.validate([three: 3, four: 4], [one: 1, two: 2])
      {:error, [:four, :three]}

  Passing the same key multiple times also errors:

      iex> Keyword.validate([one: 1, two: 2, one: 1], [:one, :two])
      {:error, [:one]}

  """
  @doc since: "1.13.0"
  @spec validate(keyword(), values :: [atom() | {atom(), term()}]) ::
          {:ok, keyword()} | {:error, [atom]}
  def validate(keyword, values) when is_list(keyword) and is_list(values) do
    validate(keyword, values, [], [], [])
  end

  defp validate([{key, _} = pair | keyword], values1, values2, acc, bad_keys) when is_atom(key) do
    case find_key!(key, values1, values2) do
      {values1, values2} ->
        validate(keyword, values1, values2, [pair | acc], bad_keys)

      :error ->
        case find_key!(key, values2, values1) do
          {values1, values2} ->
            validate(keyword, values1, values2, [pair | acc], bad_keys)

          :error ->
            validate(keyword, values1, values2, acc, [key | bad_keys])
        end
    end
  end

  defp validate([], values1, values2, acc, []) do
    {:ok, move_pairs!(values1, move_pairs!(values2, acc))}
  end

  defp validate([], _values1, _values2, _acc, bad_keys) do
    {:error, bad_keys}
  end

  defp validate([pair | _], _values1, _values2, _acc, []) do
    raise ArgumentError,
          "expected a keyword list as first argument, got invalid entry: #{inspect(pair)}"
  end

  defp find_key!(key, [key | rest], acc), do: {rest, acc}
  defp find_key!(key, [{key, _} | rest], acc), do: {rest, acc}
  defp find_key!(key, [head | tail], acc), do: find_key!(key, tail, [head | acc])
  defp find_key!(_key, [], _acc), do: :error

  defp move_pairs!([key | rest], acc) when is_atom(key),
    do: move_pairs!(rest, acc)

  defp move_pairs!([{key, _} = pair | rest], acc) when is_atom(key),
    do: move_pairs!(rest, [pair | acc])

  defp move_pairs!([], acc),
    do: acc

  defp move_pairs!([other | _], _) do
    raise ArgumentError,
          "expected the second argument to be a list of atoms or tuples, got: #{inspect(other)}"
  end

  @doc """
  Similar to `validate/2` but returns the keyword or raises an error.

  ## Examples

      iex> Keyword.validate!([], [one: 1, two: 2]) |> Enum.sort()
      [one: 1, two: 2]
      iex> Keyword.validate!([two: 3], [one: 1, two: 2]) |> Enum.sort()
      [one: 1, two: 3]

  If atoms are given, they are supported as keys but do not
  provide a default value:

      iex> Keyword.validate!([], [:one, two: 2]) |> Enum.sort()
      [two: 2]
      iex> Keyword.validate!([one: 1], [:one, two: 2]) |> Enum.sort()
      [one: 1, two: 2]

  Passing unknown keys raises an error:

      iex> Keyword.validate!([three: 3], [one: 1, two: 2])
      ** (ArgumentError) unknown keys [:three] in [three: 3], the allowed keys are: [:one, :two]

  Passing the same key multiple times also errors:

      iex> Keyword.validate!([one: 1, two: 2, one: 1], [:one, :two])
      ** (ArgumentError) duplicate keys [:one] in [one: 1, two: 2, one: 1]

  """
  @doc since: "1.13.0"
  @spec validate!(keyword(), values :: [atom() | {atom(), term()}]) :: keyword()
  def validate!(keyword, values) do
    case validate(keyword, values) do
      {:ok, kw} ->
        kw

      {:error, invalid_keys} ->
        keys =
          for value <- values,
              do: if(is_atom(value), do: value, else: elem(value, 0))

        message =
          case Enum.split_with(invalid_keys, &(&1 in keys)) do
            {_, [_ | _] = unknown} ->
              "unknown keys #{inspect(unknown)} in #{inspect(keyword)}, " <>
                "the allowed keys are: #{inspect(keys)}"

            {[_ | _] = known, _} ->
              "duplicate keys #{inspect(known)} in #{inspect(keyword)}"
          end

        raise ArgumentError, message
    end
  end

  @doc """
  Gets the value under the given `key`.

  Returns the default value if `key` does not exist
  (`nil` if no default value is provided).

  If duplicate entries exist, it returns the first one.
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

  With duplicate keys:

      iex> Keyword.get([a: 1, a: 2], :a, 3)
      1
      iex> Keyword.get([a: 1, a: 2], :b, 3)
      3

  """
  @spec get(t, key, default) :: value | default
  def get(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> default
    end
  end

  @doc """
  Gets the value under the given `key`.

  If `key` does not exist, lazily evaluates `fun` and returns its result.

  This is useful if the default value is very expensive to calculate or
  generally difficult to set up and tear down again.

  If duplicate entries exist, it returns the first one.
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
  @spec get_lazy(t, key, (-> value)) :: value
  def get_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> fun.()
    end
  end

  @doc """
  Gets the value from `key` and updates it, all in one pass.

  The `fun` argument receives the value of `key` (or `nil` if `key`
  is not present) and must return a two-element tuple: the current value
  (the retrieved value, which can be operated on before being returned)
  and the new value to be stored under `key`. The `fun` may also
  return `:pop`, implying the current value shall be removed from the
  keyword list and returned.

  Returns a tuple that contains the current value returned by
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

      iex> Keyword.get_and_update([a: 2], :a, fn number ->
      ...>   {2 * number, 3 * number}
      ...> end)
      {4, [a: 6]}

      iex> Keyword.get_and_update([a: 1], :a, fn _ -> :pop end)
      {1, []}

      iex> Keyword.get_and_update([a: 1], :b, fn _ -> :pop end)
      {nil, [a: 1]}

  """
  @spec get_and_update(t, key, (value | nil -> {current_value, new_value :: value} | :pop)) ::
          {current_value, new_keywords :: t}
        when current_value: value
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
  Gets the value under `key` and updates it. Raises if there is no `key`.

  The `fun` argument receives the value under `key` and must return a
  two-element tuple: the current value (the retrieved value, which can be
  operated on before being returned) and the new value to be stored under
  `key`.

  Returns a tuple that contains the current value returned by
  `fun` and a new keyword list with the updated value under `key`.

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
  @spec get_and_update!(t, key, (value -> {current_value, new_value :: value} | :pop)) ::
          {current_value, new_keywords :: t}
        when current_value: value
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
    raise KeyError, key: key, term: acc
  end

  @doc """
  Fetches the value for a specific `key` and returns it in a tuple.

  If the `key` does not exist, it returns `:error`.

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

  If the `key` does not exist, it raises a `KeyError`.

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
      false -> raise KeyError, key: key, term: keywords
    end
  end

  @doc """
  Gets all values under a specific `key`.

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

  Keeps duplicate keys in the resulting list of keys.

  ## Examples

      iex> Keyword.keys(a: 1, b: 2)
      [:a, :b]

      iex> Keyword.keys(a: 1, b: 2, a: 3)
      [:a, :b, :a]

      iex> Keyword.keys([{:a, 1}, {"b", 2}, {:c, 3}])
      ** (ArgumentError) expected a keyword list, but an entry in the list is not a two-element tuple with an atom as its first element, got: {"b", 2}

  """
  @spec keys(t) :: [key]
  def keys(keywords) when is_list(keywords) do
    :lists.map(
      fn
        {key, _} when is_atom(key) -> key
        element -> throw(element)
      end,
      keywords
    )
  catch
    element ->
      raise ArgumentError,
            "expected a keyword list, but an entry in the list is not a two-element tuple " <>
              "with an atom as its first element, got: #{inspect(element)}"
  end

  @doc """
  Returns all values from the keyword list.

  Keeps values from duplicate keys in the resulting list of values.

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
  Deletes the entries in the keyword list under a specific `key`.

  If the `key` does not exist, it returns the keyword list unchanged.
  Use `delete_first/2` to delete just the first entry in case of
  duplicate keys.

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
  Deletes the first entry in the keyword list under a specific `key`.

  If the `key` does not exist, it returns the keyword list unchanged.

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
  Puts the given `value` under the specified `key`.

  If a value under `key` already exists, it overrides the value
  and removes all duplicate entries.

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
  generally difficult to set up and tear down again.

  ## Examples

      iex> keyword = [a: 1]
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   13
      ...> end
      iex> Keyword.put_new_lazy(keyword, :a, fun)
      [a: 1]
      iex> Keyword.put_new_lazy(keyword, :b, fun)
      [b: 13, a: 1]

  """
  @spec put_new_lazy(t, key, (-> value)) :: t
  def put_new_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, _} -> keywords
      false -> [{key, fun.()} | keywords]
    end
  end

  @doc """
  Puts the given `value` under `key`, unless the entry `key` already exists.

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

  @doc """
  Puts a value under `key` only if the `key` already exists in `keywords`.

  In case a key exists multiple times in the keyword list,
  it removes later occurrences.

  ## Examples

      iex> Keyword.replace([a: 1, b: 2, a: 4], :a, 3)
      [a: 3, b: 2]

      iex> Keyword.replace([a: 1], :b, 2)
      [a: 1]

  """
  @doc since: "1.11.0"
  @spec replace(t, key, value) :: t
  def replace(keywords, key, value) when is_list(keywords) and is_atom(key) do
    do_replace(keywords, key, value)
  end

  defp do_replace([{key, _} | keywords], key, value) do
    [{key, value} | delete(keywords, key)]
  end

  defp do_replace([{_, _} = e | keywords], key, value) do
    [e | do_replace(keywords, key, value)]
  end

  defp do_replace([], _key, _value) do
    []
  end

  @doc """
  Puts a value under `key` only if the `key` already exists in `keywords`.

  If `key` is not present in `keywords`, it raises a `KeyError`.

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

  defp replace!([], key, _value, original) do
    raise KeyError, key: key, term: original
  end

  @doc """
  Replaces the value under `key` using the given function only if
  `key` already exists in `keywords`.

  In comparison to `replace/3`, this can be useful when it's expensive to calculate the value.

  If `key` does not exist, the original keyword list is returned unchanged.

  ## Examples

      iex> Keyword.replace_lazy([a: 1, b: 2], :a, fn v -> v * 4 end)
      [a: 4, b: 2]

      iex> Keyword.replace_lazy([a: 2, b: 2, a: 1], :a, fn v -> v * 4 end)
      [a: 8, b: 2]

      iex> Keyword.replace_lazy([a: 1, b: 2], :c, fn v -> v * 4 end)
      [a: 1, b: 2]

  """
  @doc since: "1.14.0"
  @spec replace_lazy(t, key, (existing_value :: value -> new_value :: value)) :: t
  def replace_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 1) do
    do_replace_lazy(keywords, key, fun)
  end

  defp do_replace_lazy([{key, value} | keywords], key, fun) do
    [{key, fun.(value)} | delete(keywords, key)]
  end

  defp do_replace_lazy([{_, _} = e | keywords], key, fun) do
    [e | do_replace_lazy(keywords, key, fun)]
  end

  defp do_replace_lazy([], _key, _value), do: []

  @doc """
  Checks if two keywords are equal.

  Considers two keywords to be equal if they contain
  the same keys and those keys contain the same values.

  ## Examples

      iex> Keyword.equal?([a: 1, b: 2], [b: 2, a: 1])
      true
      iex> Keyword.equal?([a: 1, b: 2], [b: 1, a: 2])
      false
      iex> Keyword.equal?([a: 1, b: 2, a: 3], [b: 2, a: 3, a: 1])
      true

  Comparison between values is done with `===/3`,
  which means integers are not equivalent to floats:

      iex> Keyword.equal?([a: 1.0], [a: 1])
      false

  """
  @spec equal?(t, t) :: boolean
  def equal?(left, right) when is_list(left) and is_list(right) do
    :lists.sort(left) === :lists.sort(right)
  end

  @doc """
  Intersects two keyword lists, returning a keyword with the common keys.

  By default, it returns the values of the intersected keys in `keyword2`.
  The keys are returned in the order found in `keyword1`.

  ## Examples

      iex> Keyword.intersect([a: 1, b: 2], [b: "b", c: "c"])
      [b: "b"]

      iex> Keyword.intersect([a: 1, b: 2], [b: 2, c: 3], fn _k, v1, v2 ->
      ...>   v1 + v2
      ...> end)
      [b: 4]

  """
  @doc since: "1.17.0"
  @spec intersect(keyword, keyword, (key, value, value -> value)) :: keyword
  def intersect(keyword1, keyword2, fun \\ fn _key, _v1, v2 -> v2 end)

  def intersect([{k, v1} | keyword1], keyword2, fun) do
    case :lists.keyfind(k, 1, keyword2) do
      {_, v2} -> [{k, fun.(k, v1, v2)} | intersect(keyword1, keyword2, fun)]
      false -> intersect(keyword1, keyword2, fun)
    end
  end

  def intersect([], _keyword2, _fun), do: []

  @doc """
  Merges two keyword lists into one.

  Adds all keys, including duplicate keys, given in `keywords2`
  to `keywords1`, overriding any existing ones.

  There are no guarantees about the order of the keys in the returned keyword.

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

  Adds all keys, including duplicate keys, given in `keywords2`
  to `keywords1`. Invokes the given function to solve conflicts.

  If `keywords2` has duplicate keys, it invokes the given function
  for each matching pair in `keywords1`.

  There are no guarantees about the order of the keys in the returned keyword.

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
  Updates the value under `key` using the given function.

  Raises `KeyError` if the `key` does not exist.

  Removes all duplicate keys and only updates the first one.

  ## Examples

      iex> Keyword.update!([a: 1, b: 2, a: 3], :a, &(&1 * 2))
      [a: 2, b: 2]
      iex> Keyword.update!([a: 1, b: 2, c: 3], :b, &(&1 * 2))
      [a: 1, b: 4, c: 3]

      iex> Keyword.update!([a: 1], :b, &(&1 * 2))
      ** (KeyError) key :b not found in: [a: 1]

  """
  @spec update!(t, key, (current_value :: value -> new_value :: value)) :: t
  def update!(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 1) do
    update!(keywords, key, fun, keywords)
  end

  defp update!([{key, value} | keywords], key, fun, _original) do
    [{key, fun.(value)} | delete(keywords, key)]
  end

  defp update!([{_, _} = pair | keywords], key, fun, original) do
    [pair | update!(keywords, key, fun, original)]
  end

  defp update!([], key, _fun, original) do
    raise KeyError, key: key, term: original
  end

  @doc """
  Updates the value under `key` in `keywords` using the given function.

  If the `key` does not exist, it inserts the given `default` value.
  Does not pass the `default` value through the update function.

  Removes all duplicate keys and only updates the first one.

  ## Examples

      iex> Keyword.update([a: 1], :a, 13, fn existing_value -> existing_value * 2 end)
      [a: 2]

      iex> Keyword.update([a: 1, a: 2], :a, 13, fn existing_value -> existing_value * 2 end)
      [a: 2]

      iex> Keyword.update([a: 1], :b, 11, fn existing_value -> existing_value * 2 end)
      [a: 1, b: 11]

  """
  @spec update(t, key, default :: value, (existing_value :: value -> new_value :: value)) :: t
  def update(keywords, key, default, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 1) do
    update_guarded(keywords, key, default, fun)
  end

  defp update_guarded([{key, value} | keywords], key, _default, fun) do
    [{key, fun.(value)} | delete(keywords, key)]
  end

  defp update_guarded([{_, _} = pair | keywords], key, default, fun) do
    [pair | update_guarded(keywords, key, default, fun)]
  end

  defp update_guarded([], key, default, _fun) do
    [{key, default}]
  end

  @doc """
  Takes all entries corresponding to the given `keys` and extracts them into a
  separate keyword list.

  Returns a tuple with the new list and the old list with removed keys.

  Ignores keys for which there are no entries in the keyword list.

  Entries with duplicate keys end up in the same keyword list.

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
  Splits the `keywords` into two keyword lists according to the given function
  `fun`.

  The provided `fun` receives each `{key, value}` pair in the `keywords` as its only
  argument. Returns a tuple with the first keyword list containing all the
  elements in `keywords` for which applying `fun` returned a truthy value, and
  a second keyword list with all the elements for which applying `fun` returned
  a falsy value (`false` or `nil`).

  ## Examples

      iex> Keyword.split_with([a: 1, b: 2, c: 3], fn {_k, v} -> rem(v, 2) == 0 end)
      {[b: 2], [a: 1, c: 3]}

      iex> Keyword.split_with([a: 1, b: 2, c: 3, b: 4], fn {_k, v} -> rem(v, 2) == 0 end)
      {[b: 2, b: 4], [a: 1, c: 3]}

      iex> Keyword.split_with([a: 1, b: 2, c: 3, b: 4], fn {k, v} -> k in [:a, :c] and rem(v, 2) == 0 end)
      {[], [a: 1, b: 2, c: 3, b: 4]}

      iex> Keyword.split_with([], fn {_k, v} -> rem(v, 2) == 0 end)
      {[], []}

  """
  @doc since: "1.15.0"
  @spec split_with(t, ({key, value} -> as_boolean(term))) :: {t, t}
  def split_with(keywords, fun) when is_list(keywords) and is_function(fun, 1) do
    fun = fn key_value_pair, {while_true, while_false} ->
      if fun.(key_value_pair) do
        {[key_value_pair | while_true], while_false}
      else
        {while_true, [key_value_pair | while_false]}
      end
    end

    :lists.foldr(fun, {[], []}, keywords)
  end

  @doc """
  Takes all entries corresponding to the given `keys` and returns them as a new
  keyword list.

  Preserves duplicate keys in the new keyword list.

  ## Examples

      iex> Keyword.take([a: 1, b: 2, c: 3], [:a, :c, :e])
      [a: 1, c: 3]
      iex> Keyword.take([a: 1, b: 2, c: 3, a: 5], [:a, :c, :e])
      [a: 1, c: 3, a: 5]

  """
  @spec take(t, [key]) :: t
  def take(keywords, keys) when is_list(keywords) and is_list(keys) do
    :lists.filter(fn {k, _} -> :lists.member(k, keys) end, keywords)
  end

  @doc """
  Drops the given `keys` from the keyword list.

  Removes duplicate keys from the new keyword list.

  ## Examples

      iex> Keyword.drop([a: 1, a: 2], [:a])
      []
      iex> Keyword.drop([a: 1, b: 2, c: 3], [:b, :d])
      [a: 1, c: 3]
      iex> Keyword.drop([a: 1, b: 2, b: 3, c: 3, a: 5], [:b, :d])
      [a: 1, c: 3, a: 5]

  """
  @spec drop(t, [key]) :: t
  def drop(keywords, keys) when is_list(keywords) and is_list(keys) do
    :lists.filter(fn {k, _} -> k not in keys end, keywords)
  end

  @doc """
  Returns the first value for `key` and removes all associated entries in the keyword list.

  It returns a tuple where the first element is the first value for `key` and the
  second element is a keyword list with all entries associated with `key` removed.
  If the `key` is not present in the keyword list, it returns `{default, keyword_list}`.

  If you don't want to remove all the entries associated with `key` use `pop_first/3`
  instead, which will remove only the first entry.

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
  @spec pop(t, key, default) :: {value | default, t}
  def pop(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case fetch(keywords, key) do
      {:ok, value} -> {value, delete(keywords, key)}
      :error -> {default, keywords}
    end
  end

  @doc """
  Returns the first value for `key` and removes all associated entries in the keyword list,
  raising if `key` is not present.

  This function behaves like `pop/3`, but raises in case the `key` is not present in the
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
  If the `key` is not present in the keyword list, it returns `{[], keyword_list}`.

  If you don't want to remove all the entries associated with `key` use `pop_first/3`
  instead, which will remove only the first entry.

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
  generally difficult to set up and tear down again.

  Removes all duplicate keys. See `pop_first/3` for removing only the first entry.

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
  @spec pop_lazy(t, key, (-> value)) :: {value, t}
  def pop_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case fetch(keywords, key) do
      {:ok, value} -> {value, delete(keywords, key)}
      :error -> {fun.(), keywords}
    end
  end

  @doc """
  Returns and removes the first value associated with `key` in the keyword list.

  Keeps duplicate keys in the resulting keyword list.

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
  @spec pop_first(t, key, default) :: {value | default, t}
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
  def to_list(keywords) when is_list(keywords) do
    keywords
  end

  @doc false
  @deprecated "Use Kernel.length/1 instead"
  def size(keywords) do
    length(keywords)
  end

  @doc """
  Returns a keyword list containing only the entries from `keywords`
  for which the function `fun` returns a truthy value.

  See also `reject/2` which discards all entries where the function
  returns a truthy value.

  ## Examples

      iex> Keyword.filter([one: 1, two: 2, three: 3], fn {_key, val} -> rem(val, 2) == 1 end)
      [one: 1, three: 3]

  """
  @doc since: "1.13.0"
  @spec filter(t, ({key, value} -> as_boolean(term))) :: t
  def filter(keywords, fun) when is_list(keywords) and is_function(fun, 1) do
    do_filter(keywords, fun)
  end

  defp do_filter([], _fun), do: []

  defp do_filter([{_, _} = entry | entries], fun) do
    if fun.(entry) do
      [entry | do_filter(entries, fun)]
    else
      do_filter(entries, fun)
    end
  end

  @doc """
  Returns a keyword list excluding the entries from `keywords`
  for which the function `fun` returns a truthy value.

  See also `filter/2`.

  ## Examples

      iex> Keyword.reject([one: 1, two: 2, three: 3], fn {_key, val} -> rem(val, 2) == 1 end)
      [two: 2]

  """
  @doc since: "1.13.0"
  @spec reject(t, ({key, value} -> as_boolean(term))) :: t
  def reject(keywords, fun) when is_list(keywords) and is_function(fun, 1) do
    do_reject(keywords, fun)
  end

  defp do_reject([], _fun), do: []

  defp do_reject([{_, _} = entry | entries], fun) do
    if fun.(entry) do
      do_reject(entries, fun)
    else
      [entry | do_reject(entries, fun)]
    end
  end

  @doc false
  @deprecated "Use Keyword.new/2 instead"
  def map(keywords, fun) when is_list(keywords) do
    Enum.map(keywords, fn {k, v} -> {k, fun.({k, v})} end)
  end
end
