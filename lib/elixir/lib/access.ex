defmodule Access do
  @moduledoc """
  Key-based access to data structures via the `foo[bar]` bracket syntax.

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

  ## Key-based lookups

  Out of the box, Access works with `Keyword` and `Map`:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:a]
      1

      iex> map = %{a: 1, b: 2}
      iex> map[:a]
      1

      iex> star_ratings = %{1.0 => "★", 1.5 => "★☆", 2.0 => "★★"}
      iex> star_ratings[1.5]
      "★☆"

  Access can be combined with `Kernel.put_in/3` to put a value
  in a given key:

      iex> map = %{a: 1, b: 2}
      iex> put_in map[:a], 3
      %{a: 3, b: 2}

  This syntax is very convenient as it can be nested arbitrarily:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in users["john"][:age], 28
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  Furthermore, Access transparently ignores `nil` values:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:c][:unknown]
      nil

  Since Access is a behaviour, it can be implemented to key-value
  data structures. Access requires the key comparison to be
  implemented using the `===` operator.

  ## Field-based lookups

  The Access syntax (`foo[bar]`) cannot be used to access fields in
  structs. That's by design, as Access is meant to be used for
  dynamic key-value structures, like maps and keywords, and not
  by static ones like structs.

  Therefore Elixir provides a field-based lookup for structs.
  Imagine a struct named `User` with name and age fields. The
  following would raise:

      user = %User{name: "john"}
      user[:name]
      ** (UndefinedFunctionError) undefined function User.fetch/2
         (User does not implement the Access behaviour)

  Structs instead use the `user.name` syntax:

      user.name
      #=> "john"

  The same `user.name` syntax can also be used by `Kernel.put_in/2`
  to for updating structs fields:

      put_in user.name, "mary"
      %User{name: "mary"}

  Differently from `user[:name]`, `user.name` is not extensible and
  is restricted to only maps and structs.

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

  **TODO: implement `field/1`, `key/1`, `at/1`, `elem/1`.**

  """

  @type t :: list | map | nil
  @type key :: any
  @type value :: any

  @callback fetch(t, key) :: {:ok, value} | :error
  @callback get_and_update(t, key, (value -> {value, value} | :pop)) :: {value, t}
  @callback pop(t, key) :: {value, t}

  defmacrop raise_undefined_behaviour(e, struct, top) do
    quote do
      stacktrace = System.stacktrace
      e =
        case stacktrace do
          [unquote(top)|_] ->
            %{unquote(e) | reason: "#{inspect unquote(struct)} does not implement the Access behaviour"}
          _ ->
            unquote(e)
        end
      reraise e, stacktrace
    end
  end

  @doc """
  Fetches the container's value for the given key.
  """
  @spec fetch(t, term) :: {:ok, term} | :error
  def fetch(container, key)

  def fetch(%{__struct__: struct} = container, key) do
    struct.fetch(container, key)
  rescue
    e in UndefinedFunctionError ->
      raise_undefined_behaviour e, struct, {^struct, :fetch, [^container, ^key], _}
  end

  def fetch(%{} = map, key) do
    :maps.find(key, map)
  end

  def fetch(list, key) when is_list(list) and is_atom(key) do
    case :lists.keyfind(key, 1, list) do
      {^key, value} -> {:ok, value}
      false -> :error
    end
  end

  def fetch(list, key) when is_list(list) do
    raise ArgumentError,
      "the Access calls for keywords expect the key to be an atom, got: " <> inspect(key)
  end

  def fetch(nil, _key) do
    :error
  end

  @doc """
  Gets the container's value for the given key.
  """
  @spec get(t, term, term) :: term
  def get(container, key, default \\ nil) do
    case fetch(container, key) do
      {:ok, value} -> value
      :error -> default
    end
  end

  @doc """
  Gets and updates the container's value for the given key, in a single pass.

  The argument function `fun` must receive the value for the given `key` (or
  `nil` if the key doesn't exist in `container`). It must return a tuple
  containing the `get` value and the new value to be stored in the `container`.

  This function returns a two-element tuple.
  The first element is the `get` value, as returned by `fun`.
  The second element is the container, updated with the value returned by `fun`.
  """
  @spec get_and_update(t, term, (term -> {get, term})) :: {get, t} when get: var
  def get_and_update(container, key, fun)

  def get_and_update(%{__struct__: struct} = container, key, fun) do
    struct.get_and_update(container, key, fun)
  rescue
    e in UndefinedFunctionError ->
      raise_undefined_behaviour e, struct, {^struct, :get_and_update, [^container, ^key, ^fun], _}
  end

  def get_and_update(%{} = map, key, fun) do
    Map.get_and_update(map, key, fun)
  end

  def get_and_update(list, key, fun) when is_list(list) do
    Keyword.get_and_update(list, key, fun)
  end

  def get_and_update(nil, key, _fun) do
    raise ArgumentError,
      "could not put/update key #{inspect key} on a nil value"
  end

  def pop(%{__struct__: struct} = container, key) do
    struct.pop(container, key)
  rescue
    e in UndefinedFunctionError ->
      raise_undefined_behaviour e, struct, {^struct, :pop, [^container, ^key], _}
  end
  def pop(list, key) when is_list(list), do: Keyword.pop(list, key)
  def pop(map, key) when is_map(map) do
    case map do
      %{^key => value} -> {value, :maps.remove(key, map)}
      %{} -> {nil, map}
    end
  end
  def pop(nil, key) do
    raise ArgumentError,
      "could not pop key #{inspect key} on a nil value"
  end
end
