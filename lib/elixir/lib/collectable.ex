defprotocol Collectable do
  @moduledoc """
  A protocol to traverse data structures.

  The `Enum.into/2` function uses this protocol to insert an
  enumerable into a collection:

      iex> Enum.into([a: 1, b: 2], %{})
      %{a: 1, b: 2}

  ## Why Collectable?

  The `Enumerable` protocol is useful to take values out of a collection.
  In order to support a wide range of values, the functions provided by
  the `Enumerable` protocol do not keep shape. For example, passing a
  map to `Enum.map/2` always returns a list.

  This design is intentional. `Enumerable` was designed to support infinite
  collections, resources and other structures with fixed shape. For example,
  it doesn't make sense to insert values into a range, as it has a fixed
  shape where just the range limits are stored.

  The `Collectable` module was designed to fill the gap left by the
  `Enumerable` protocol. `into/1` can be seen as the opposite of
  `Enumerable.reduce/3`. If `Enumerable` is about taking values out,
  `Collectable.into/1` is about collecting those values into a structure.
  """

  @type command :: {:cont, term} | :done | :halt

  @doc """
  Returns a function that collects values alongside
  the initial accumulation value.

  The returned function receives a collectable and injects a given
  value into it for every `{:cont, term}` instruction.

  `:done` is passed when no further values will be injected, useful
  for closing resources and normalizing values. A collectable must
  be returned on `:done`.

  If injection is suddenly interrupted, `:halt` is passed and it can
  return any value, as it won't be used.
  """
  @spec into(t) :: {term, (term, command -> t | term)}
  def into(collectable)
end

defimpl Collectable, for: List do
  def into(original) do
    {[], fn
      list, {:cont, x} -> [x | list]
      list, :done -> original ++ :lists.reverse(list)
      _, :halt -> :ok
    end}
  end
end

defimpl Collectable, for: BitString do
  def into(original) do
    {original, fn
      acc, {:cont, x} when is_bitstring(x) -> [acc | x]
      acc, :done -> IO.iodata_to_binary(acc)
      _, :halt -> :ok
    end}
  end
end

defimpl Collectable, for: Map do
  def into(original) do
    {original, fn
      map, {:cont, {k, v}} -> :maps.put(k, v, map)
      map, :done -> map
      _, :halt -> :ok
    end}
  end
end
