defmodule Access do
  @moduledoc """
  Dictionary-like access to data structures via the `foo[bar]` syntax.

  This module also empowers `Kernel`s nested update functions
  `Kernel.get_in/2`, `Kernel.put_in/3`, `Kernel.update_in/3` and
  `Kernel.get_and_update_in/3`.

  ## Examples

  Out of the box, Access works with built-in dictionaries: `Keyword`
  and `Map`:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:a]
      1

      iex> map = %{a: 1, b: 2}
      iex> map[:a]
      1

      iex> star_ratings = %{1.0 => "★", 1.5 => "★☆", 2.0 => "★★"}
      iex> star_ratings[1.5]
      "★☆"

  Furthermore, Access transparently ignores `nil` values:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:c][:unknown]
      nil

  The key comparison must be implemented using the `===` operator.
  """

  @type t :: list | map | nil
  @type key :: any
  @type value :: any

  @callback fetch(t, key) :: {:ok, value} | :error
  @callback get_and_update(t, key, (value -> {value, value})) :: {value, t}

  @doc """
  Fetches the container's value for the given key.
  """
  @spec fetch(t, term) :: {:ok, term} | :error
  def fetch(container, key)

  def fetch(%{__struct__: struct} = container, key) do
    struct.fetch(container, key)
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
  end

  def get_and_update(%{} = map, key, fun) do
    current_value = case :maps.find(key, map) do
      {:ok, value} -> value
      :error -> nil
    end

    {get, update} = fun.(current_value)
    {get, :maps.put(key, update, map)}
  end

  def get_and_update(list, key, fun) when is_list(list) do
    Keyword.get_and_update(list, key, fun)
  end

  def get_and_update(nil, key, _fun) do
    raise ArgumentError,
      "could not put/update key #{inspect key} on a nil value"
  end
end
