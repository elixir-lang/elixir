defmodule Stream do
  @moduledoc """
  Module for creating and composing streams.

  Streams are composable, lazy enumerables. Any enumerable that generates
  items one by one during enumeration is called a stream. For example,
  Elixir's `Range` is a stream:

      iex> range = 1..5
      1..5
      iex> Enum.map range, &(&1 * 2)
      [2,4,6,8,10]

  In the example above, as we mapped over the range, the elements being
  enumerated were created one by one, during enumeration. The `Stream`
  module allows us to map the range, without triggering its enumeration:

      iex> range = 1..3
      iex> stream = Stream.map(range, &(&1 * 2))
      iex> Enum.map(stream, &(&1 + 1))
      [3,5,7]

  Notice we started with a range and then we created a stream that is
  meant to multiply each item in the range by 2. At this point, no
  computation was done yet. Just when `Enum.map/2` is called we
  enumerate over each item in the range, multiplying it by 2 and adding 1.
  We say the functions in `Stream` are *lazy* and the functions in `Enum`
  are *eager*.

  Due to their laziness, streams are useful when working with large
  (or even infinite) collections. When chaining many operations with `Enum`,
  intermediate lists are created, while `Stream` creates a recipe of
  computations that are executed at a later moment. Let's see another
  example:

      1..3 |>
        Enum.map(&IO.inspect(&1)) |>
        Enum.map(&(&1 * 2)) |>
        Enum.map(&IO.inspect(&1))
      1
      2
      3
      2
      4
      6
      #=> [2,4,6]

  Notice that we first printed each item in the list, then multiplied each
  element by 2 and finally printed each new value. In this example, the list
  was iterated three times. Let's see an example with streams:

      stream = 1..3 |>
        Stream.map(&IO.inspect(&1)) |>
        Stream.map(&(&1 * 2)) |>
        Stream.map(&IO.inspect(&1))
      Enum.to_list(stream)
      1
      2
      2
      4
      3
      6
      #=> [2,4,6]

  Although the end result is the same, the order in which the items were
  printed changed! With streams, we print the first item and then print
  its double. In this example, the list was iterated just once!

  That's what we meant when we first said that streams are composable,
  lazy enumerables. Notice we could call `Stream.map/2` multiple times,
  effectively composing the streams and they are lazy. The computations
  are performed only when you call a function from the `Enum` module.

  ## Creating Streams

  There are many functions in Elixir's standard library that return
  streams, some examples are:

  * `IO.stream/1` - Streams input lines, one by one;
  * `URI.query_decoder/1` - Decodes a query string, pair by pair;

  This module also allows us to create streams from any enumerable:

      iex> stream = Stream.map([1,2,3], &(&1 * 2))
      iex> Enum.map(stream, &(&1 + 1))
      [3,5,7]

  By simply passing a list (which is an enumerable) as the first argument
  to `Stream.map/2`, we have automatically created a stream that will
  multiply the items in the list by 2 on enumeration.

  This module also provides other functions for creating streams, such as
  `Stream.cycle/1`.
  """

  defrecord Lazy, [:enumerable, :fun, :acc]

  defimpl Enumerable, for: Lazy do
    def reduce(Lazy[] = lazy, acc, fun) do
      do_reduce(lazy, acc, fun, 0)
    end

    def count(Lazy[] = lazy) do
      do_reduce(lazy, 0, fn _, acc -> acc + 1 end, 0)
    end

    def member?(Lazy[] = lazy, value) do
      do_reduce(lazy, false, fn(entry, _) ->
        if entry === value, do: throw({ :stream_lazy, 0, true }), else: false
      end, 0)
    end

    defp do_reduce(Lazy[enumerable: enumerable, fun: f1, acc: nil], acc, fun, nesting) do
      do_reduce(enumerable, acc, f1.(fun), nesting)
    end

    defp do_reduce(Lazy[enumerable: enumerable, fun: f1, acc: side], acc, fun, nesting) do
      do_reduce(enumerable, { acc, side }, f1.(fun, nesting), nesting + 1)
    end

    defp do_reduce(enumerable, acc, fun, nesting) do
      Enumerable.reduce(enumerable, acc, fun) |> remove_nesting(nesting)
    catch
      { :stream_lazy, nesting, res } -> remove_nesting(res, nesting)
    end

    defp remove_nesting(acc, 0),       do: acc
    defp remove_nesting(acc, nesting), do: remove_nesting(elem(acc, 0), nesting - 1)
  end

  @type t :: Lazy.t | (acc, (element, acc -> acc) -> acc)
  @type acc :: any
  @type element :: any
  @type index :: non_neg_integer
  @type default :: any

  @doc """
  Creates a stream that enumerates each enumerable in an enumerable.

  ## Examples

      iex> stream = Stream.concat([1..3, 4..6, 7..9])
      iex> Enum.to_list(stream)
      [1,2,3,4,5,6,7,8,9]

  """
  @spec concat(Enumerable.t) :: t
  def concat(enumerables) do
    &do_concat(enumerables, &1, &2)
  end

  @doc """
  Creates a stream that enumerates the first argument, followed by the second.

  ## Examples

      iex> stream = Stream.concat(1..3, 4..6)
      iex> Enum.to_list(stream)
      [1,2,3,4,5,6]

      iex> stream1 = Stream.cycle([1, 2, 3])
      iex> stream2 = Stream.cycle([4, 5, 6])
      iex> stream = Stream.concat(stream1, stream2)
      iex> Enum.take(stream, 6)
      [1,2,3,1,2,3]

  """
  @spec concat(Enumerable.t, Enumerable.t) :: t
  def concat(first, second) do
    &do_concat([first, second], &1, &2)
  end

  defp do_concat(enumerables, acc, fun) do
    Enumerable.reduce(enumerables, acc, &Enumerable.reduce(&1, &2, fun))
  end

  @doc """
  Creates a stream that cycles through the given enumerable,
  infinitely.

  ## Examples

      iex> stream = Stream.cycle([1,2,3])
      iex> Enum.take(stream, 5)
      [1,2,3,1,2]

  """
  @spec cycle(Enumerable.t) :: t
  def cycle(enumerable) do
    &do_cycle(enumerable, &1, &2)
  end

  defp do_cycle(enumerable, acc, fun) do
    acc = Enumerable.reduce(enumerable, acc, fun)
    do_cycle(enumerable, acc, fun)
  end

  @doc """
  Lazily drops the next `n` items from the enumerable.

  ## Examples

      iex> stream = Stream.drop(1..10, 5)
      iex> Enum.to_list(stream)
      [6,7,8,9,10]

  """
  @spec drop(Enumerable.t, non_neg_integer) :: t
  def drop(enumerable, n) when n >= 0 do
    Lazy[enumerable: enumerable,
         fun: fn(f1, _) ->
           fn
             _entry, { acc, n } when n > 0 ->
               { acc, n - 1 }
             entry, { acc, n } ->
               { f1.(entry, acc), n }
           end
         end,
         acc: n]
  end

  @doc """
  Lazily drops elements of the enumerable while the given
  function returns true.

  ## Examples

      iex> stream = Stream.drop_while(1..10, &(&1 <= 5))
      iex> Enum.to_list(stream)
      [6,7,8,9,10]

  """
  @spec drop_while(Enumerable.t, (element -> as_boolean(term))) :: t
  def drop_while(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1, _) ->
           fn
             entry, { acc, true } ->
               if f.(entry), do: { acc, true }, else: { f1.(entry, acc), false }
             entry, { acc, false } ->
               { f1.(entry, acc), false }
           end
         end,
         acc: true]
  end

  @doc """
  Creates a stream that will filter elements according to
  the given function on enumeration.

  ## Examples

      iex> stream = Stream.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      iex> Enum.to_list(stream)
      [2]

  """
  @spec filter(Enumerable.t, (element -> as_boolean(term))) :: t
  def filter(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1) ->
           fn(entry, acc) ->
             if f.(entry), do: f1.(entry, acc), else: acc
           end
         end]
  end

  @doc """
  Emit a sequence of values, starting with `start_value`. Successive
  values are generated by calling `next_fun` on the previous value.

  ## Examples

      iex> Stream.iterate(0, &(&1+1)) |> Enum.take(5)
      [0,1,2,3,4]

  """

  @spec iterate(element, (element -> element)) :: t
  def iterate(start_value, next_fun) do
    fn acc, fun ->
      do_iterate(start_value, next_fun, fun.(start_value, acc), fun)
    end
  end

  defp do_iterate(value, next_fun, acc, fun) do
    next = next_fun.(value)
    do_iterate(next, next_fun, fun.(next, acc), fun)
  end

  @doc """
  Creates a stream that will apply the given function on
  enumeration.

  ## Examples

      iex> stream = Stream.map([1, 2, 3], fn(x) -> x * 2 end)
      iex> Enum.to_list(stream)
      [2,4,6]

  """
  @spec map(Enumerable.t, (element -> any)) :: t
  def map(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1) ->
           fn(entry, acc) ->
             f1.(f.(entry), acc)
           end
         end]
  end

  @doc """
  Creates a stream that will apply the given function on enumeration and
  flatten the result.

  ## Examples

      iex> stream = Stream.flat_map([1, 2, 3], fn(x) -> [x, x * 2] end)
      iex> Enum.to_list(stream)
      [1, 2, 2, 4, 3, 6]

  """

  @spec flat_map(Enumerable.t, (element -> any)) :: t
  def flat_map(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1) ->
           fn(entry, acc) ->
             Enumerable.reduce(f.(entry), acc, f1)
           end
         end]
  end

  @doc """
  Creates a stream that will reject elements according to
  the given function on enumeration.

  ## Examples

      iex> stream = Stream.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      iex> Enum.to_list(stream)
      [1,3]

  """
  @spec reject(Enumerable.t, (element -> as_boolean(term))) :: t
  def reject(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1) ->
           fn(entry, acc) ->
             unless f.(entry), do: f1.(entry, acc), else: acc
           end
         end]
  end


  @doc """
  Returns a stream generated by calling `generator_fun` repeatedly.

  ## Examples

      iex> Stream.repeatedly(&:random.uniform/0) |> Enum.take(3)
      [0.4435846174457203, 0.7230402056221108, 0.94581636451987]

  """
  @spec repeatedly((() -> element)) :: t
  def repeatedly(generator_fun)
  when is_function(generator_fun, 0) do
    &do_repeatedly(generator_fun, &1, &2)
  end

  defp do_repeatedly(generator_fun, acc, fun) do
    do_repeatedly(generator_fun, fun.(generator_fun.(), acc), fun)
  end

  @doc """
  Lazily takes the next `n` items from the enumerable and stops
  enumeration.

  ## Examples

      iex> stream = Stream.take(1..100, 5)
      iex> Enum.to_list(stream)
      [1,2,3,4,5]

      iex> stream = Stream.cycle([1, 2, 3]) |> Stream.take(5)
      iex> Enum.to_list(stream)
      [1,2,3,1,2]

  """
  @spec take(Enumerable.t, non_neg_integer) :: t
  def take(_enumerable, 0), do: Lazy[enumerable: [], fun: &(&1)]

  def take(enumerable, n) when n > 0 do
    Lazy[enumerable: enumerable,
         fun: fn(f1, nesting) ->
           fn(entry, { acc, n }) ->
             res = f1.(entry, acc)
             if n > 1, do: { res, n-1 }, else: throw { :stream_lazy, nesting, res }
           end
         end,
         acc: n]
  end

  @doc """
  Lazily takes elements of the enumerable while the given
  function returns true.

  ## Examples

      iex> stream = Stream.take_while(1..100, &(&1 <= 5))
      iex> Enum.to_list(stream)
      [1,2,3,4,5]

  """
  @spec take_while(Enumerable.t, (element -> as_boolean(term))) :: t
  def take_while(enumerable, f) do
    Lazy[enumerable: enumerable,
         fun: fn(f1, nesting) ->
           fn(entry, { acc, true }) ->
             if f.(entry) do
               { f1.(entry, acc), true }
             else
               throw { :stream_lazy, nesting, acc }
             end
           end
         end,
         acc: true]
  end

  @doc """
  Emit a sequence of values and accumulators. Successive values are generated by
  calling `next_fun` with the previous accumulator.

  If the return value is nil iteration ends.

  ## Examples

      iex> Stream.unfold(5, fn 0 -> nil; n -> {n, n-1} end) |> Enum.to_list()
      [5, 4, 3, 2, 1]
  """
  @spec unfold(acc, (acc -> { element, acc } | nil)) :: t
  def unfold(acc, f) do
    fn acc1, f1 ->
      do_unfold(acc, f, acc1, f1)
    end
  end

  defp do_unfold(gen_acc, gen_fun, acc, fun) do
    case gen_fun.(gen_acc) do
      nil                -> acc
      { v, new_gen_acc } -> do_unfold(new_gen_acc, gen_fun, fun.(v, acc), fun)
    end
  end

  @doc """
  Creates a stream where each item in the enumerable will
  be accompanied by its index.

  ## Examples

      iex> stream = Stream.with_index([1, 2, 3])
      iex> Enum.to_list(stream)
      [{1,0},{2,1},{3,2}]

  """
  @spec with_index(Enumerable.t) :: t
  def with_index(enumerable) do
    Lazy[enumerable: enumerable,
         fun: fn(f1, _) ->
           fn(entry, { acc, counter }) ->
             acc = f1.({ entry, counter }, acc)
             { acc, counter + 1 }
           end
         end,
         acc: 0]
  end
end
