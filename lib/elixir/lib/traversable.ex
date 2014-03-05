defprotocol Traversable do
  @moduledoc """
  A protocol to traverse data structures.

  The `Enum.into/2` function uses this protocol to insert an
  enumerable into a collection:

      iex> Enum.into([a: 1, b: 2], %{})
      %{a: 1, b: 2}

  If a collection implements both `Enumerable` and `Traversable`, both
  operations can be combined with `Enum.traverse/2`:

      iex> Enum.traverse(%{ a: 1, b: 2 }, fn { k, v } -> { k, v * 2 } end)
      %{a: 2, b: 4}

  ## Why Traversable?

  The `Enumerable` protocol is useful to take values out of a collection.
  In order to support a wide range of values, the functions provided by
  the `Enumerable` protocol do not keep shape. For example, passing a
  dictionary to `Enum.map/2` always returns a list.

  This design is intentional. `Enumerable` was designed to support infinite
  collections, resources and other structures with fixed shape. For example,
  it doesn't make sense to insert values into a range, as it has a fixed
  shape where just the range limits are stored.

  The `Traversable` module was designed to fill the gap left by the
  `Enumerable` protocol. It provides two functions: `into/2` and `empty/1`.

  `into/1` can be seen as the opposite of `Enumerable.reduce/3`. If
  `Enumerable` is about taking values out, `Traversable.into/1` is about
  putting values into a structure.

  `empty/1` receives a traversable and returns an empty version of the
  same traversable. By combining the enumerable functionality with `into/1`
  and `empty/1`, one can implement a traversal mechanism.
  """

  @type command :: { :cont, term } | :done | :halt

  @doc """
  Receives a traversable structure and returns an empty one.
  """
  @spec empty(t) :: t
  def empty(traversable)

  @doc """
  Returns a function that injects values into a traversable alongside
  the initial accumulation value.

  The returned function receives a traversable and injects a given
  value into it for every `{ :cont, term }` instruction.

  `:done` is passed when no further values will be injected, useful
  for closing resources and normalizing values. A traversable must
  be returned on `:done`.

  If injection is suddently interrupted, `:halt` is passed and it can
  return any value, as it won't be used.
  """
  @spec into(t) :: { term, (term, command -> t | term) }
  def into(traversable)
end

defimpl Traversable, for: List do
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

defimpl Traversable, for: BitString do
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

defimpl Traversable, for: Function do
  def empty(function) do
    function
  end
  
  def into(function) do
    { function, function }
  end
end

defimpl Traversable, for: Map do
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
