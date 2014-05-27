defprotocol Access do
  @moduledoc """
  The Access protocol is used by `foo[bar]` and also
  empowers the nested update functions in Kernel.

  For instance, `foo[bar]` translates `Access.get(foo, bar)`.
  `Kernel.get_in/2`, `Kernel.put_in/3` and `Kernel.update_in/3`
  are also all powered by the Access protocol.

  This protocol is implemented by default for keywords, maps
  and dictionary like types:

      iex> keywords = [a: 1, b: 2]
      iex> keywords[:a]
      1

      iex> map = %{a: 1, b: 2}
      iex> map[:a]
      1

      iex> star_ratings = %{1.0 => "★", 1.5 => "★☆", 2.0 => "★★"}
      iex> star_ratings[1.5]
      "★☆"

  The key access must be implemented using the `===` operator.
  """

  @doc """
  Accesses the given key in the container.
  """
  def get(container, key)

  @doc """
  Updates the given key in container with a function.

  In case the key is not set, invokes the function passing nil.
  """
  def update(container, key, fun)

  @doc false
  Kernel.def access(container, key) do
    get(container, key)
  end
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

  def update([{key, v}|t], key, fun) when is_atom(key) do
    [{key, fun.(v)}|t]
  end

  def update([h|t], key, fun) do
    [h|update(t, key, fun)]
  end

  def update([], key, fun) when is_atom(key) do
    [{key, fun.(nil)}]
  end
end

defimpl Access, for: Map do
  def get(map, key) do
    case :maps.find(key, map) do
      {:ok, value} -> value
      :error -> nil
    end
  end

  def update(map, key, fun) do
    value =
      case :maps.find(key, map) do
        {:ok, value} -> value
        :error -> nil
      end

    :maps.put(key, fun.(value), map)
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

  def update!(%{} = map, key, fun) do
    case :maps.find(key, map) do
      {:ok, value} -> :maps.put(key, fun.(value), map)
      :error -> raise KeyError, key: key, term: map
    end
  end

  def update!(other, key, _fun) do
    raise ArgumentError,
      "could not update key #{inspect key}. Expected map/struct, got: #{inspect other}"
  end
end

defimpl Access, for: Atom do
  def get(nil, _) do
    nil
  end

  def get(atom, _) do
    undefined(atom)
  end

  def update(nil, _, fun) do
    fun.(nil)
  end

  def update(atom, _key, _fun) do
    undefined(atom)
  end

  defp undefined(atom) do
    raise Protocol.UndefinedError,
      protocol: @protocol,
      value: atom,
      description: "only the nil atom is supported"
  end
end
