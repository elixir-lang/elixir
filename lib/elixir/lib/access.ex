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

  @callback get(t, key, value) :: value
  @callback get_and_update(t, key, (value -> {value, value})) :: {value, t}

  @doc """
  Gets the container's value for the given key.
  """
  @spec get(t, term, term) :: term
  def get(container, key, default \\ nil)

  def get(%{__struct__: struct} = container, key, default) do
    struct.get(container, key, default)
  end

  def get(%{} = map, key, default) do
    case :maps.find(key, map) do
      {:ok, value} -> value
      :error -> default
    end
  end

  def get(list, key, default) when is_list(list) do
    case :lists.keyfind(key, 1, list) do
      {^key, value} -> value
      false -> default
    end
  end

  def get(nil, _key, default) do
    default
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

# Callbacks invoked when inlining code for *_in in Kernel.
# TODO: Remove me on 1.2
defmodule Access.Map do
  @moduledoc false

  def update!(%{} = map, key, fun) do
    case :maps.find(key, map) do
      {:ok, value} ->
        :maps.put(key, fun.(value), map)
      :error ->
        raise KeyError, key: key, term: map
    end
  end

  def update!(other, key, _fun) do
    raise ArgumentError,
      "could not put/update key #{inspect key}. Expected map/struct, got: #{inspect other}"
  end

  def get_and_update!(%{} = map, key, fun) do
    case :maps.find(key, map) do
      {:ok, value} ->
        {get, update} = fun.(value)
        {get, :maps.put(key, update, map)}
      :error ->
        raise KeyError, key: key, term: map
    end
  end

  def get_and_update!(other, key, _fun) do
    raise ArgumentError,
      "could not put/update key #{inspect key}. Expected map/struct, got: #{inspect other}"
  end
end
