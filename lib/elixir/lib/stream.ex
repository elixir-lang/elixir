defmodule Stream do
  @moduledoc """
  Module for creating and composing streams.

  Streams are composable, lazy enumerables. Any enumerable that generates
  items one by one during enumeration are called streams. For example,
  Elixir's `Range` is a stream:

      iex> range = 1..5
      1..5
      iex> Enum.map range, &1 * 2
      [2,4,6,8,10]

  In the example above, as we mapped over the range, the elements being
  enumerated were created one by one, during enumeration. This module
  allows us to create lazy computations on top of streams:

      iex> range = 1..3
      iex> stream = Stream.map(range, &1 * 2)
      iex> Enum.map(stream, &1 + 1)
      [3,5,7]

  Notice we started with a range and then we created a stream that is
  meant to multiply each item in the range by 2. At this point, no
  computation was done yet. Just when `Enum.map/2` is called that we
  enumerate each item in the range, multiplying it per 2 and adding 1.
  We say the functions in `Stream` are lazy and the functions in `Enum`
  are eager.

  Due to its laziness, streams are useful when working with large collections
  or even infinite collections. When chaining many operations with `Enum`,
  intermediary lists are created, while `Stream` creates a recipe of
  computations that are executed just at a later moment. Let's see another
  example:

      iex> 1..3 |>
      ...>  Enum.map(IO.inspect(&1)) |>
      ...>  Enum.map(&1 * 2) |>
      ...>  Enum.map(IO.inspect(&1))
      1
      2
      3
      2
      4
      6
      [2,4,6]

  Notice that we first printed each item in the list, then multiplied each
  element by 2 and finally printed each new value. In this example, the list
  was iterated three times. Let's see an example with streams:

      iex> stream = 1..3 |>
      ...>  Stream.map(IO.inspect(&1)) |>
      ...>  Stream.map(&1 * 2) |>
      ...>  Stream.map(IO.inspect(&1))
      iex> Enum.to_list(stream)
      1
      2
      2
      4
      3
      6
      [2,4,6]

  Although the end result is the same, the order in which the iterms were
  printed changed! With streams, we print the first item and then print
  its double. In this example, the list was iterated just once!

  That's what we meant when we first said that streams are composable,
  lazy enumerables. Notice we could call `Stream.map/2` multiple times,
  effectively composing the streams and they are lazy. The computations
  are performed only when you call a function from the `Enum` module.

  ## Creating Streams

  There are many functions in Elixir's standard library that returns
  streams, some examples are:

  * `IO.lines_stream/1` - It streams input lines, one by one;
  * `URI.query_decoder/1` - Decodes a query string, pair by pair;

  This module also allows us to create streams from any enumerable:

      iex> stream = Stream.map([1,2,3], &1 * 2)
      iex> Enum.map(stream, &1 + 1)
      [3,5,7]

  By simply passing a list (which is an enumerable) as first argument
  to `Stream.map/2`, we have automatically created a stream that will
  multiply the items in the list by 2 on enumeration.

  This module also provides other functions for creating streams, like
  `Stream.cycle/1`.
  """

  defrecord Lazy, [:enumerable, :fun, :acc]

  defimpl Enumerable, for: Lazy do
    def reduce(Lazy[] = lazy, acc, fun) do
      do_reduce(lazy, acc, fun)
    end

    def count(Lazy[] = lazy) do
      do_reduce(lazy, 0, fn _, acc -> acc + 1 end)
    end

    def member?(Lazy[] = lazy, value) do
      do_reduce(lazy, false, fn(entry, _) ->
        if entry === value, do: throw(:function_member?), else: false
      end)
    catch
      :function_member? -> true
    end

    defp do_reduce(Lazy[enumerable: enumerable, fun: f1, acc: nil], acc, fun) do
      do_reduce(enumerable, acc, f1.(fun))
    end

    defp do_reduce(Lazy[enumerable: enumerable, fun: f1, acc: side], acc, fun) do
      do_reduce(enumerable, { acc, side }, f1.(fun)) |> elem(0)
    end

    defp do_reduce(enumerable, acc, fun) do
      Enumerable.reduce(enumerable, acc, fun)
    end
  end

  @type element :: any
  @type index :: non_neg_integer
  @type default :: any

  @doc """
  Creates a stream that cycles through the given enumerable,
  undefnitely.

  ## Examples

      iex> stream = Stream.cycle([1,2,3])
      iex> Enum.take(stream, 5)
      [1,2,3,1,2]

  """
  @spec cycle(Enumerable.t) :: Lazy.t
  def cycle(enumerable) do
    do_cycle(enumerable, &1, &2)
  end

  defp do_cycle(enumerable, acc, fun) do
    acc = Enumerable.reduce(enumerable, acc, fun)
    do_cycle(enumerable, acc, fun)
  end

  @doc """
  Creates a stream that will filter elements according to
  the given function on enumeration.

  ## Examples

      iex> stream = Stream.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      iex> Enum.to_list(stream)
      [2]

  """
  @spec filter(Enumerable.t, (element -> as_boolean(term))) :: Lazy.t
  def filter(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1) ->
           fn(entry, acc) ->
             if f.(entry), do: f1.(entry, acc), else: acc
           end
         end]
  end

  @doc """
  Creates a stream that will apply the given function on
  enumeration.

  ## Examples

      iex> stream = Stream.map([1, 2, 3], fn(x) -> x * 2 end)
      iex> Enum.to_list(stream)
      [2,4,6]

  """
  @spec map(Enumerable.t, (element -> any)) :: Lazy.t
  def map(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1) ->
           fn(entry, acc) ->
             f1.(f.(entry), acc)
           end
         end]
  end

  @doc """
  Creates a stream where each item in the enumerable will
  be accompanied by its index.

  ## Examples

      iex> stream = Stream.with_index([1, 2, 3])
      iex> Enum.to_list(stream)
      [{1,0},{2,1},{3,2}]

  """
  @spec with_index(Enumerable.t) :: Lazy.t
  def with_index(enumerable) do
    Lazy[enumerable: enumerable,
         fun: fn(f1) ->
           fn(entry, { acc, counter }) ->
             acc = f1.({ entry, counter }, acc)
             { acc, counter + 1}
           end
         end,
         acc: 0]
  end
end
