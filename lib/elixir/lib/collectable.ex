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

  ## Examples

  To show how to manually use the `Collectable` protocol, let's play with its
  implementation for `MapSet`.

      iex> {initial_acc, collector_fun} = Collectable.into(MapSet.new())
      iex> updated_acc = Enum.reduce([1, 2, 3], initial_acc, fn elem, acc ->
      ...>   collector_fun.(acc, {:cont, elem})
      ...> end)
      iex> collector_fun.(updated_acc, :done)
      #MapSet<[1, 2, 3]>

  To show how the protocol can be implemented, we can take again a look at the
  implementation for `MapSet`. In this implementation "collecting" elements
  simply means inserting them in the set through `MapSet.put/2`.

      defimpl Collectable do
        def into(original) do
          collector_fun = fn
            set, {:cont, elem} -> MapSet.put(set, elem)
            set, :done -> set
            _set, :halt -> :ok
          end

          {original, collector_fun}
        end
      end

  """

  @type command :: {:cont, term} | :done | :halt

  @doc """
  Returns an initial accumulator and a "collector" function.

  The returned function receives a term and a command and injects the term into
  the collectable on every `{:cont, term}` command.

  `:done` is passed as a command when no further values will be injected. This
  is useful when there's a need to close resources or normalizing values. A
  collectable must be returned when the command is `:done`.

  If injection is suddenly interrupted, `:halt` is passed and the function
  can return any value as it won't be used.

  For examples on how to use the `Collectable` protocol and `into/1` see the
  module documentation.
  """
  @spec into(t) :: {term, (term, command -> t | term)}
  def into(collectable)
end

defimpl Collectable, for: List do
  def into(original) do
    if original != [] do
      IO.warn(
        "the Collectable protocol is deprecated for non-empty lists. The behaviour of " <>
          "things like Enum.into/2 or \"for\" comprehensions with an :into option is incorrect " <>
          "when collecting into non-empty lists. If you're collecting into a non-empty keyword " <>
          "list, consider using Keyword.merge/2 instead. If you're collecting into a non-empty " <>
          "list, consider concatenating the two lists with the ++ operator."
      )
    end

    fun = fn
      list, {:cont, x} -> [x | list]
      list, :done -> original ++ Enum.reverse(list)
      _, :halt -> :ok
    end

    {[], fun}
  end
end

defimpl Collectable, for: BitString do
  def into(original) when is_binary(original) do
    fun = fn
      acc, {:cont, x} when is_binary(x) and is_list(acc) ->
        [acc | x]

      acc, {:cont, x} when is_bitstring(x) and is_bitstring(acc) ->
        <<acc::bitstring, x::bitstring>>

      acc, {:cont, x} when is_bitstring(x) ->
        <<IO.iodata_to_binary(acc)::bitstring, x::bitstring>>

      acc, :done when is_bitstring(acc) ->
        acc

      acc, :done ->
        IO.iodata_to_binary(acc)

      _, :halt ->
        :ok
    end

    {[original], fun}
  end

  def into(original) when is_bitstring(original) do
    fun = fn
      acc, {:cont, x} when is_bitstring(x) ->
        <<acc::bitstring, x::bitstring>>

      acc, :done ->
        acc

      _, :halt ->
        :ok
    end

    {original, fun}
  end
end

defimpl Collectable, for: Map do
  def into(original) do
    fun = fn
      map, {:cont, {k, v}} -> :maps.put(k, v, map)
      map, :done -> map
      _, :halt -> :ok
    end

    {original, fun}
  end
end
