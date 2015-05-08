defprotocol Access do
  @moduledoc """
  Dictionary-like access to data structures via the `foo[bar]` syntax.

  This module also empowers `Kernel`s nested update functions
  `Kernel.get_in/2`, `Kernel.put_in/3`, `Kernel.update_in/3` and
  `Kernel.get_and_update_in/3`.

  ## Deprecated

  Currently, the Access protocol is deprecated as there are performance
  concerns in the current implementation. Since Elixir v1.1, instead of
  using a protocol, `foo[bar]` will dispatch directly to the `Dict`
  module. Therefore, while `foo[bar]` will continue to work, extension
  of the syntax should be done via a custom `Dict` implementation.

  ## Examples

  Out of the box, Access works all built-in dictionaries: `Keyword`,
  `Map` and `HashDict`:

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

  @doc """
  Gets the container's value for the given key.
  """
  @spec get(t, term) :: t
  def get(container, key)

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
end

defimpl Access, for: List do
  def get(dict, key) when is_atom(key) do
    case :lists.keyfind(key, 1, dict) do
      {^key, value} -> value
      false -> nil
    end
  end

  def get(_dict, key) do
    raise ArgumentError,
      "the access protocol for lists expect the key to be an atom, got: #{inspect key}"
  end

  def get_and_update(dict, key, fun) when is_atom(key) do
    get_and_update(dict, [], key, fun)
  end

  defp get_and_update([{key, value}|t], acc, key, fun) do
    {get, update} = fun.(value)
    {get, :lists.reverse(acc, [{key, update}|t])}
  end

  defp get_and_update([h|t], acc, key, fun) do
    get_and_update(t, [h|acc], key, fun)
  end

  defp get_and_update([], acc, key, fun) do
    {get, update} = fun.(nil)
    {get, [{key, update}|:lists.reverse(acc)]}
  end
end

defimpl Access, for: [Map, Any] do
  def get(map, key) do
    case :maps.find(key, map) do
      {:ok, value} -> value
      :error -> nil
    end
  end

  def get_and_update(map, key, fun) do
    value =
      case :maps.find(key, map) do
        {:ok, value} -> value
        :error -> nil
      end

    {get, update} = fun.(value)
    {get, :maps.put(key, update, map)}
  end

  def get!(%{} = map, key) do
    case :maps.find(key, map) do
      {:ok, value} -> value
      :error -> raise KeyError, key: key, term: map
    end
  end

  def get!(other, key) do
    raise ArgumentError,
      "could not get key #{inspect key}. Expected map/struct, got: #{inspect other}"
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

defimpl Access, for: Atom do
  def get(nil, _) do
    nil
  end

  def get(atom, _) do
    undefined(atom)
  end

  def get_and_update(nil, key, _fun) do
    raise ArgumentError,
      "could not put/update key #{inspect key} on a nil value"
  end

  def get_and_update(atom, _key, _fun) do
    undefined(atom)
  end

  defp undefined(atom) do
    raise Protocol.UndefinedError,
      protocol: @protocol,
      value: atom,
      description: "only the nil atom is supported"
  end
end
