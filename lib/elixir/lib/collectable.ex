defprotocol Collectable do
  @moduledoc """
  A protocol to traverse data structures.

  The `Enum.into/2` function uses this protocol to insert an
  enumerable into a collection:

      iex> Enum.into([a: 1, b: 2], %{})
      %{a: 1, b: 2}

  If a collection implements both `Enumerable` and `Collectable`, both
  operations can be combined with `Enum.traverse/2`:

      iex> Enum.traverse(%{ a: 1, b: 2 }, fn { k, v } -> { k, v * 2 } end)
      %{a: 2, b: 4}

  ## Why Collectable?

  The `Enumerable` protocol is useful to take values out of a collection.
  In order to support a wide range of values, the functions provided by
  the `Enumerable` protocol do not keep shape. For example, passing a
  dictionary to `Enum.map/2` always returns a list.

  This design is intentional. `Enumerable` was designed to support infinite
  collections, resources and other structures with fixed shape. For example,
  it doesn't make sense to insert values into a range, as it has a fixed
  shape where just the range limits are stored.

  The `Collectable` module was designed to fill the gap left by the
  `Enumerable` protocol. It provides two functions: `into/1` and `empty/1`.

  `into/1` can be seen as the opposite of `Enumerable.reduce/3`. If
  `Enumerable` is about taking values out, `Collectable.into/1` is about
  collecting those values into a structure.

  `empty/1` receives a collectable and returns an empty version of the
  same collectable. By combining the enumerable functionality with `into/1`
  and `empty/1`, one can, for example, implement a traversal mechanism.
  """

  @type command :: { :cont, term } | :done | :halt

  @doc """
  Receives a collectable structure and returns an empty one.
  """
  @spec empty(t) :: t
  def empty(collectable)

  @doc """
  Returns a function that collects values alongside
  the initial accumulation value.

  The returned function receives a collectable and injects a given
  value into it for every `{ :cont, term }` instruction.

  `:done` is passed when no further values will be injected, useful
  for closing resources and normalizing values. A collectable must
  be returned on `:done`.

  If injection is suddenly interrupted, `:halt` is passed and it can
  return any value, as it won't be used.
  """
  @spec into(t) :: { term, (term, command -> t | term) }
  def into(collectable)
end

defimpl Collectable, for: List do
  def empty(_list) do
    []
  end

  def into(original) do
    { [], fn
      list, { :cont, x } -> [x|list]
      list, :done -> original ++ :lists.reverse(list)
      _, :halt -> :ok
    end }
  end
end

defimpl Collectable, for: BitString do
  def empty(_bitstring) do
    ""
  end

  def into(original) do
    { original, fn
      bitstring, { :cont, x } -> <<bitstring :: bits, x :: bits>>
      bitstring, :done -> bitstring
      _, :halt -> :ok
    end }
  end
end

defimpl Collectable, for: Function do
  def empty(function) do
    function
  end

  def into(function) do
    { function, function }
  end
end

defimpl Collectable, for: Map do
  def empty(_map) do
    %{}
  end

  def into(original) do
    { original, fn
      map, { :cont, { k, v } } -> :maps.put(k, v, map)
      map, :done -> map
      _, :halt -> :ok
    end }
  end
end
