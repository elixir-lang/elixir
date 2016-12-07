defmodule Access do
  @moduledoc """
  Key-based access to data structures using the `data[key]` syntax.

  Elixir provides two syntaxes for accessing values. `user[:name]`
  is used by dynamic structures, like maps and keywords, while
  `user.name` is used by structs. The main difference is that
  `user[:name]` won't raise if the key `:name` is missing but
  `user.name` will raise if there is no `:name` key.

  Besides the cases above, this module provides convenience
  functions for accessing other structures, like `at/1` for
  lists and `elem/1` for tuples. Those functions can be used
  by the nested update functions in `Kernel`, such as
  `Kernel.get_in/2`, `Kernel.put_in/3`, `Kernel.update_in/3`,
  `Kernel.get_and_update_in/3` and friends.

  ## Dynamic lookups

  Out of the box, `Access` works with `Keyword` and `Map`:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:a]
      1

      iex> map = %{a: 1, b: 2}
      iex> map[:a]
      1

      iex> star_ratings = %{1.0 => "★", 1.5 => "★☆", 2.0 => "★★"}
      iex> star_ratings[1.5]
      "★☆"

  Note that the dynamic lookup syntax (`term[key]`) roughly translates to
  `Access.get(term, key, nil)`.

  `Access` can be combined with `Kernel.put_in/3` to put a value
  in a given key:

      iex> map = %{a: 1, b: 2}
      iex> put_in map[:a], 3
      %{a: 3, b: 2}

  This syntax is very convenient as it can be nested arbitrarily:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in users["john"][:age], 28
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  Furthermore, `Access` transparently ignores `nil` values:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:c][:unknown]
      nil

  Since `Access` is a behaviour, it can be implemented for key-value
  data structures. The implementation should be added to the
  module that defines the struct being accessed. `Access` requires the
  key comparison to be implemented using the `===` operator.

  ## Static lookups

  The `Access` syntax (`foo[bar]`) cannot be used to access fields in
  structs, since structs do not implement the `Access` behaviour by
  default. It is also a design decision: the dynamic access lookup
  is meant to be used for dynamic key-value structures, like maps
  and keywords, and not by static ones like structs (where fields are
  known and not dynamic).

  Therefore Elixir provides a static lookup for struct fields and for atom
  fields in maps. Imagine a struct named `User` with a `:name` field.
  The following would raise:

      user = %User{name: "John"}
      user[:name]
      # ** (UndefinedFunctionError) undefined function User.fetch/2
      #    (User does not implement the Access behaviour)

  Structs instead use the `user.name` syntax to access fields:

      user.name
      #=> "John"

  The same `user.name` syntax can also be used by `Kernel.put_in/2`
  to for updating structs fields:

      put_in user.name, "Mary"
      #=> %User{name: "Mary"}

  Differently from `user[:name]`, `user.name` is not extensible via
  a behaviour and is restricted only to structs and atom keys in maps.

  As mentioned above, this works for atom keys in maps as well. Refer to the
  `Map` module for more information on this.

  Summing up:

    * `user[:name]` is used by dynamic structures, is extensible and
      does not raise on missing keys
    * `user.name` is used by static structures, it is not extensible
      and it will raise on missing keys

  ## Accessors

  While Elixir provides built-in syntax only for traversing dynamic
  and static key-value structures, this module provides convenience
  functions for traversing other structures, like tuples and lists,
  to be used alongside `Kernel.put_in/2` in others.

  For instance, given a user with a list of languages, here is how to
  deeply traverse the map and convert all language names to uppercase:

      iex> user = %{name: "john",
      ...>          languages: [%{name: "elixir", type: :functional},
      ...>                      %{name: "c", type: :procedural}]}
      iex> update_in user, [:languages, Access.all(), :name], &String.upcase/1
      %{name: "john",
        languages: [%{name: "ELIXIR", type: :functional},
                    %{name: "C", type: :procedural}]}

  See the functions `key/1`, `key!/1`, `elem/1`, and `all/0` for some of the
  available accessors.

  ## Implementing the Access behaviour for custom data structures

  In order to be able to use the `Access` protocol with custom data structures
  (which have to be structs), such structures have to implement the `Access`
  behaviour. For example, for a `User` struct, this would have to be done:

      defmodule User do
        defstruct [:name, :email]

        @behaviour Access
        # Implementation of the Access callbacks...
      end

  """

  @type t :: list | map | nil | any
  @type key :: any
  @type value :: any

  @doc """
  Invoked in order to access the value stored under `key` in the given term `term`.

  This function should return `{:ok, value}` where `value` is the value under
  `key` if it succeeded, or `:error` if the key does not exist in the structure.

  Many of the functions defined in the `Access` module internally call this
  function. This function is also used when the square-brackets access syntax
  (`structure[key]`) is used: the `fetch/2` callback implemented by the module
  that defines the `structure` struct is invoked and if it returns `{:ok,
  value}` then `value` is returned, or if it returns `:error` then `nil` is
  returned.


  See the `Map.fetch/2` and `Keyword.fetch/2` implementations for examples of
  how to implement this callback.
  """
  @callback fetch(term :: t, key) :: {:ok, value} | :error

  @doc """
  Invoked in order to access the value stored under `key` in the given term `term`,
  defaulting to `default` if not present.

  This function should return the value under the key `key` in `term` if there's
  such key, otherwise `default`.

  For most data structures, this can be implemented using `fetch/2` internally;
  for example:

      def get(structure, key, default) do
        case fetch(structure, key) do
          {:ok, value} -> value
          :error       -> default
        end
      end

  See the `Map.get/3` and `Keyword.get/3` implementations for more examples.
  """
  @callback get(term :: t, key, default :: value) :: value

  @doc """
  Invoked in order to access the value under `key` and update it at the same time.

  The implementation of this callback should invoke the passed function with the
  value under key `key` in the passed structure, or `nil` if the key is not
  present. This function should return either `{value_to_return, new_value}` or
  `:pop`.

  If it returns `{value_to_return, new_value}`, the return value of this
  callback should be `{value_to_return, new_term}` where `new_term` is `term`
  after updating the value of `key` with `new_value`.

  If it returns `:pop`, the return value of this callback should be `{value,
  new_term}` where `value` is the value under `key` or `nil` if not present, and
  `new_term` is `term` without the key `key`.

  See the implementations of `Map.get_and_update/3` or `Keyword.get_and_update/3`
  for more examples.
  """
  @callback get_and_update(term :: t, key, (value -> {value, value} | :pop)) :: {value, t}

  @doc """
  Invoked to "pop" the value under `key` out of the given term.

  When the key `key` exists in the given `term`, the implementation should
  return a `{value, new_term}` tuple where `value` is the value that was under
  `key` and `new_term` is `term` without `key`.

  When the key `key` is not present in the given `term`, a tuple `{value, term}`
  should be returned, where `value` is implementation-defined.

  See the implementations for `Map.pop/3` or `Keyword.pop/3` for more examples.
  """
  @callback pop(term :: t, key) :: {value, t}

  defmacrop raise_undefined_behaviour(e, struct, top) do
    quote do
      stacktrace = System.stacktrace
      e =
        case stacktrace do
          [unquote(top) | _] ->
            %{unquote(e) | reason: "#{inspect unquote(struct)} does not implement the Access behaviour"}
          _ ->
            unquote(e)
        end
      reraise e, stacktrace
    end
  end

  @doc """
  Fetches the value for the given key in a container (a map, keyword
  list, or struct that implements the `Access` behaviour).
  """
  @spec fetch(t, term) :: {:ok, term} | :error
  def fetch(container, key)

  def fetch(%{__struct__: struct} = container, key) do
    struct.fetch(container, key)
  rescue
    e in UndefinedFunctionError ->
      raise_undefined_behaviour e, struct, {^struct, :fetch, [^container, ^key], _}
  end

  def fetch(map, key) when is_map(map) do
    Map.fetch(map, key)
  end

  def fetch(list, key) when is_list(list) and is_atom(key) do
    Keyword.fetch(list, key)
  end

  def fetch(list, key) when is_list(list) do
    raise ArgumentError,
      "the Access calls for keywords expect the key to be an atom, got: " <> inspect(key)
  end

  def fetch(nil, _key) do
    :error
  end

  @doc """
  Gets the value for the given key in a container (a map, keyword
  list, or struct that implements the `Access` behaviour).
  """
  @spec get(t, term, term) :: term
  def get(container, key, default \\ nil) do
    case fetch(container, key) do
      {:ok, value} -> value
      :error -> default
    end
  end

  @doc """
  Gets and updates the given key in a container (a map, keyword
  list, or struct that implements the `Access` behaviour).

  This `fun` argument receives the value of `key` (or `nil` if `key`
  is not present) and must return a two-element tuple: the "get" value
  (the retrieved value, which can be operated on before being returned)
  and the new value to be stored under `key`. The `fun` may also
  return `:pop`, implying the current value shall be removed
  from the container and returned.

  The returned value is a two-element tuple with the "get" value returned by
  `fun` and a new container with the updated value under `key`.
  """
  @spec get_and_update(container :: t, key, (value -> {get_value, update_value} | :pop)) ::
        {get_value, container :: t} when get_value: var, update_value: value
  def get_and_update(container, key, fun)

  def get_and_update(%{__struct__: struct} = container, key, fun) do
    struct.get_and_update(container, key, fun)
  rescue
    e in UndefinedFunctionError ->
      raise_undefined_behaviour e, struct, {^struct, :get_and_update, [^container, ^key, ^fun], _}
  end

  def get_and_update(map, key, fun) when is_map(map) do
    Map.get_and_update(map, key, fun)
  end

  def get_and_update(list, key, fun) when is_list(list) do
    Keyword.get_and_update(list, key, fun)
  end

  def get_and_update(nil, key, _fun) do
    raise ArgumentError,
      "could not put/update key #{inspect key} on a nil value"
  end

  @doc """
  Removes the entry with a given key from a container (a map, keyword
  list, or struct that implements the `Access` behaviour).

  Returns a tuple containing the value associated with the key and the
  updated container. `nil` is returned for the value if the key isn't
  in the container.

  ## Examples

  With a map:

      iex> Access.pop(%{name: "Elixir", creator: "Valim"}, :name)
      {"Elixir", %{creator: "Valim"}}

  A keyword list:

      iex> Access.pop([name: "Elixir", creator: "Valim"], :name)
      {"Elixir", [creator: "Valim"]}

  An unknown key:

      iex> Access.pop(%{name: "Elixir", creator: "Valim"}, :year)
      {nil, %{creator: "Valim", name: "Elixir"}}

  """
  def pop(%{__struct__: struct} = container, key) do
    struct.pop(container, key)
  rescue
    e in UndefinedFunctionError ->
      raise_undefined_behaviour e, struct, {^struct, :pop, [^container, ^key], _}
  end

  def pop(map, key) when is_map(map) do
    Map.pop(map, key)
  end

  def pop(list, key) when is_list(list) do
    Keyword.pop(list, key)
  end

  def pop(nil, key) do
    raise ArgumentError,
      "could not pop key #{inspect key} on a nil value"
  end

  ## Accessors

  @doc false
  def key(key) do
    IO.warn "Access.key/1 is deprecated due to erratic behaviour for missing keys, " <>
            "please use Access.key/2 instead with proper default values " <>
            "(or Access.key!/1 if you expect the key to always be available)"

    fn
      :get, data, next ->
        next.(Map.get(to_map(data), key))
      :get_and_update, data, next ->
        value = Map.get(to_map(data), key)
        case next.(value) do
          {get, update} -> {get, Map.put(data, key, update)}
          :pop -> {value, Map.delete(data, key)}
        end
    end
  end

  defp to_map(nil), do: %{}
  defp to_map(%{} = map), do: map
  defp to_map(data), do: raise "Access.key/1 expected a map/struct or nil, got: #{inspect data}"

  @doc """
  Returns a function that accesses the given key in a map/struct.

  The returned function is typically passed as an accessor to `Kernel.get_in/2`,
  `Kernel.get_and_update_in/3`, and friends.

  The returned function uses the default value if the key does not exist.
  This can be used to specify defaults and safely traverse missing keys:

      iex> get_in(%{}, [Access.key(:user, %{}), Access.key(:name, nil)])
      nil

  Such is also useful when using update functions, allowing us to introduce
  values as we traverse the data-structure for updates:

      iex> put_in(%{}, [Access.key(:user, %{}), Access.key(:name, nil)], "Mary")
      %{user: %{name: "Mary"}}

  ## Examples

      iex> map = %{user: %{name: "john"}}
      iex> get_in(map, [Access.key(:unknown, %{}), Access.key(:name, "john")])
      "john"
      iex> get_and_update_in(map, [Access.key!(:user), Access.key!(:name)], fn
      ...>   prev -> {prev, String.upcase(prev)}
      ...> end)
      {"john", %{user: %{name: "JOHN"}}}
      iex> pop_in(map, [Access.key!(:user), Access.key!(:name)])
      {"john", %{user: %{}}}

  An error is raised if the accessed structure is not a map or a struct:

      iex> get_in(nil, [Access.key(:foo, nil)])
      ** (BadMapError) expected a map, got: nil

      iex> get_in([], [Access.key(:foo, nil)])
      ** (BadMapError) expected a map, got: []

  """
  def key(key, default) do
    fn
      :get, data, next ->
        next.(Map.get(data, key, default))
      :get_and_update, data, next ->
        value = Map.get(data, key, default)
        case next.(value) do
          {get, update} -> {get, Map.put(data, key, update)}
          :pop -> {value, Map.delete(data, key)}
        end
    end
  end

  @doc """
  Returns a function that accesses the given key in a map/struct.

  The returned function is typically passed as an accessor to `Kernel.get_in/2`,
  `Kernel.get_and_update_in/3`, and friends.

  Raises if the key does not exist.

  ## Examples

      iex> map = %{user: %{name: "john"}}
      iex> get_in(map, [Access.key!(:user), Access.key!(:name)])
      "john"
      iex> get_and_update_in(map, [Access.key!(:user), Access.key!(:name)], fn
      ...>   prev -> {prev, String.upcase(prev)}
      ...> end)
      {"john", %{user: %{name: "JOHN"}}}
      iex> pop_in(map, [Access.key!(:user), Access.key!(:name)])
      {"john", %{user: %{}}}
      iex> get_in(map, [Access.key!(:user), Access.key!(:unknown)])
      ** (KeyError) key :unknown not found in: %{name: \"john\"}

  An error is raised if the accessed structure is not a map/struct:

      iex> get_in([], [Access.key!(:foo)])
      ** (RuntimeError) Access.key!/1 expected a map/struct, got: []

  """
  def key!(key) do
    fn
      :get, %{} = data, next ->
        next.(Map.fetch!(data, key))
      :get_and_update, %{} = data, next ->
        value = Map.fetch!(data, key)
        case next.(value) do
          {get, update} -> {get, Map.put(data, key, update)}
          :pop -> {value, Map.delete(data, key)}
        end
      _op, data, _next ->
        raise "Access.key!/1 expected a map/struct, got: #{inspect data}"
    end
  end

  @doc ~S"""
  Returns a function that accesses the element at the given index in a tuple.

  The returned function is typically passed as an accessor to `Kernel.get_in/2`,
  `Kernel.get_and_update_in/3`, and friends.

  Raises if the index is out of bounds.

  ## Examples

      iex> map = %{user: {"john", 27}}
      iex> get_in(map, [:user, Access.elem(0)])
      "john"
      iex> get_and_update_in(map, [:user, Access.elem(0)], fn
      ...>   prev -> {prev, String.upcase(prev)}
      ...> end)
      {"john", %{user: {"JOHN", 27}}}
      iex> pop_in(map, [:user, Access.elem(0)])
      ** (RuntimeError) cannot pop data from a tuple

  An error is raised if the accessed structure is not a tuple:

      iex> get_in(%{}, [Access.elem(0)])
      ** (RuntimeError) Access.elem/1 expected a tuple, got: %{}

  """
  def elem(index) when is_integer(index) do
    pos = index + 1

    fn
      :get, data, next when is_tuple(data) ->
        next.(:erlang.element(pos, data))
      :get_and_update, data, next when is_tuple(data) ->
        value = :erlang.element(pos, data)
        case next.(value) do
          {get, update} -> {get, :erlang.setelement(pos, data, update)}
          :pop -> raise "cannot pop data from a tuple"
        end
      _op, data, _next ->
        raise "Access.elem/1 expected a tuple, got: #{inspect data}"
    end
  end

  @doc ~S"""
  Returns a function that accesses all the elements in a list.

  The returned function is typically passed as an accessor to `Kernel.get_in/2`,
  `Kernel.get_and_update_in/3`, and friends.

  ## Examples

      iex> list = [%{name: "john"}, %{name: "mary"}]
      iex> get_in(list, [Access.all(), :name])
      ["john", "mary"]
      iex> get_and_update_in(list, [Access.all(), :name], fn
      ...>   prev -> {prev, String.upcase(prev)}
      ...> end)
      {["john", "mary"], [%{name: "JOHN"}, %{name: "MARY"}]}
      iex> pop_in(list, [Access.all(), :name])
      {["john", "mary"], [%{}, %{}]}

  Here is an example that traverses the list dropping even
  numbers and multipling odd numbers by 2:

      iex> require Integer
      iex> get_and_update_in([1, 2, 3, 4, 5], [Access.all], fn
      ...>   num -> if Integer.is_even(num), do: :pop, else: {num, num * 2}
      ...> end)
      {[1, 2, 3, 4, 5], [2, 6, 10]}

  An error is raised if the accessed structure is not a list:

      iex> get_in(%{}, [Access.all()])
      ** (RuntimeError) Access.all/0 expected a list, got: %{}

  """
  def all() do
    &all/3
  end

  defp all(:get, data, next) when is_list(data) do
    Enum.map(data, next)
  end

  defp all(:get_and_update, data, next) when is_list(data) do
    all(data, next, [], [])
  end

  defp all(_op, data, _next) do
    raise "Access.all/0 expected a list, got: #{inspect data}"
  end

  defp all([head | rest], next, gets, updates) do
    case next.(head) do
      {get, update} -> all(rest, next, [get | gets], [update | updates])
      :pop -> all(rest, next, [head | gets], updates)
    end
  end

  defp all([], _next, gets, updates) do
    {:lists.reverse(gets), :lists.reverse(updates)}
  end

  @doc ~S"""
  Returns a function that accesses the element at `index` (zero based) of a list.

  The returned function is typically passed as an accessor to `Kernel.get_in/2`,
  `Kernel.get_and_update_in/3`, and friends.

  ## Examples

      iex> list = [%{name: "john"}, %{name: "mary"}]
      iex> get_in(list, [Access.at(1), :name])
      "mary"
      iex> get_and_update_in(list, [Access.at(0), :name], fn
      ...>   prev -> {prev, String.upcase(prev)}
      ...> end)
      {"john", [%{name: "JOHN"}, %{name: "mary"}]}

  `at/1` can also be used to pop elements out of a list or
  a key inside of a list:

      iex> list = [%{name: "john"}, %{name: "mary"}]
      iex> pop_in(list, [Access.at(0)])
      {%{name: "john"}, [%{name: "mary"}]}
      iex> pop_in(list, [Access.at(0), :name])
      {"john", [%{}, %{name: "mary"}]}

  When the index is out of bounds, `nil` is returned and the update function is never called:

      iex> list = [%{name: "john"}, %{name: "mary"}]
      iex> get_in(list, [Access.at(10), :name])
      nil
      iex> get_and_update_in(list, [Access.at(10), :name], fn
      ...>   prev -> {prev, String.upcase(prev)}
      ...> end)
      {nil, [%{name: "john"}, %{name: "mary"}]}

  An error is raised for negative indexes:

      iex> get_in([], [Access.at(-1)])
      ** (FunctionClauseError) no function clause matching in Access.at/1

  An error is raised if the accessed structure is not a list:

      iex> get_in(%{}, [Access.at(1)])
      ** (RuntimeError) Access.at/1 expected a list, got: %{}
  """
  def at(index) when index >= 0 do
    fn(op, data, next) -> at(op, data, index, next) end
  end

  defp at(:get, data, index, next) when is_list(data) do
    data |> Enum.at(index) |> next.()
  end

  defp at(:get_and_update, data, index, next) when is_list(data) do
    get_and_update_at(data, index, next, [])
  end

  defp at(_op, data, _index, _next) do
    raise "Access.at/1 expected a list, got: #{inspect data}"
  end

  defp get_and_update_at([head | rest], 0, next, updates) do
    case next.(head) do
      {get, update} -> {get, :lists.reverse([update | updates], rest)}
      :pop -> {head, :lists.reverse(updates, rest)}
    end
  end

  defp get_and_update_at([head | rest], index, next, updates) do
    get_and_update_at(rest, index - 1, next, [head | updates])
  end

  defp get_and_update_at([], _index, _next, updates) do
    {nil, :lists.reverse(updates)}
  end
end
