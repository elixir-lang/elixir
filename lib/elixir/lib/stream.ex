defmodule Stream do
  @moduledoc """
  Module for creating and composing streams.

  Streams are composable, lazy enumerables. Any enumerable that generates
  items one by one during enumeration is called a stream. For example,
  Elixir's `Range` is a stream:

      iex> range = 1..5
      1..5
      iex> Enum.map range, &(&1 * 2)
      [2, 4, 6, 8, 10]

  In the example above, as we mapped over the range, the elements being
  enumerated were created one by one, during enumeration. The `Stream`
  module allows us to map the range, without triggering its enumeration:

      iex> range = 1..3
      iex> stream = Stream.map(range, &(&1 * 2))
      iex> Enum.map(stream, &(&1 + 1))
      [3, 5, 7]

  Notice we started with a range and then we created a stream that is
  meant to multiply each item in the range by 2. At this point, no
  computation was done. Only when `Enum.map/2` is called we actually
  enumerate over each item in the range, multiplying it by 2 and adding 1.
  We say the functions in `Stream` are *lazy* and the functions in `Enum`
  are *eager*.

  Due to their laziness, streams are useful when working with large
  (or even infinite) collections. When chaining many operations with `Enum`,
  intermediate lists are created, while `Stream` creates a recipe of
  computations that are executed at a later moment. Let's see another
  example:

      1..3
      |> Enum.map(&IO.inspect(&1))
      |> Enum.map(&(&1 * 2))
      |> Enum.map(&IO.inspect(&1))
      1
      2
      3
      2
      4
      6
      #=> [2, 4, 6]

  Notice that we first printed each item in the list, then multiplied each
  element by 2 and finally printed each new value. In this example, the list
  was enumerated three times. Let's see an example with streams:

      stream = 1..3
      |> Stream.map(&IO.inspect(&1))
      |> Stream.map(&(&1 * 2))
      |> Stream.map(&IO.inspect(&1))
      Enum.to_list(stream)
      1
      2
      2
      4
      3
      6
      #=> [2, 4, 6]

  Although the end result is the same, the order in which the items were
  printed changed! With streams, we print the first item and then print
  its double. In this example, the list was enumerated just once!

  That's what we meant when we said earlier that streams are composable,
  lazy enumerables. Notice we could call `Stream.map/2` multiple times,
  effectively composing the streams and keeping them lazy. The computations
  are only performed when you call a function from the `Enum` module.

  ## Creating Streams

  There are many functions in Elixir's standard library that return
  streams, some examples are:

    * `IO.stream/2`         - streams input lines, one by one
    * `URI.query_decoder/1` - decodes a query string, pair by pair

  This module also provides many convenience functions for creating streams,
  like `Stream.cycle/1`, `Stream.unfold/2`, `Stream.resource/3` and more.

  Note the functions in this module are guaranteed to return enumerables.
  Since enumerables can have different shapes (structs, anonymous functions,
  and so on), the functions in this module may return any of those shapes
  and that this may change at any time. For example, a function that today
  returns an anonymous function may return a struct in future releases.
  """

  @doc false
  defstruct enum: nil, funs: [], accs: [], done: nil

  @type acc     :: any
  @type element :: any
  @type index   :: non_neg_integer
  @type default :: any

  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop skip(acc) do
    {:cont, acc}
  end

  defmacrop next(fun, entry, acc) do
    quote do: unquote(fun).(unquote(entry), unquote(acc))
  end

  defmacrop acc(head, state, tail) do
    quote do: [unquote(head), unquote(state) | unquote(tail)]
  end

  defmacrop next_with_acc(fun, entry, head, state, tail) do
    quote do
      {reason, [head | tail]} = unquote(fun).(unquote(entry), [unquote(head) | unquote(tail)])
      {reason, [head, unquote(state) | tail]}
    end
  end

  ## Transformers

  @doc """
  Shortcut to `chunk(enum, n, n)`.
  """
  @spec chunk(Enumerable.t, pos_integer) :: Enumerable.t
  def chunk(enum, n), do: chunk(enum, n, n, nil)

  @doc """
  Streams the enumerable in chunks, containing `n` items each, where
  each new chunk starts `step` elements into the enumerable.

  `step` is optional and, if not passed, defaults to `n`, i.e.
  chunks do not overlap. If the final chunk does not have `n`
  elements to fill the chunk, elements are taken as necessary
  from `leftover` if it was passed. If `leftover` is passed and
  does not have enough elements to fill the chunk, then the chunk is
  returned anyway with less than `n` elements. If `leftover` is not
  passed at all or is `nil`, then the partial chunk is discarded
  from the result.

  ## Examples

      iex> Stream.chunk([1, 2, 3, 4, 5, 6], 2) |> Enum.to_list
      [[1, 2], [3, 4], [5, 6]]

      iex> Stream.chunk([1, 2, 3, 4, 5, 6], 3, 2) |> Enum.to_list
      [[1, 2, 3], [3, 4, 5]]

      iex> Stream.chunk([1, 2, 3, 4, 5, 6], 3, 2, [7]) |> Enum.to_list
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

      iex> Stream.chunk([1, 2, 3, 4, 5, 6], 3, 3, []) |> Enum.to_list
      [[1, 2, 3], [4, 5, 6]]

  """
  @spec chunk(Enumerable.t, pos_integer, pos_integer) :: Enumerable.t
  @spec chunk(Enumerable.t, pos_integer, pos_integer, Enumerable.t | nil) :: Enumerable.t
  def chunk(enum, n, step, leftover \\ nil)
      when is_integer(n) and n > 0 and is_integer(step) and step > 0 do
    limit = :erlang.max(n, step)
    if is_nil(leftover) do
      lazy enum, {[], 0}, fn(f1) -> R.chunk(n, step, limit, f1) end
    else
      lazy enum, {[], 0},
           fn(f1) -> R.chunk(n, step, limit, f1) end,
           &do_chunk(&1, n, leftover, &2)
     end
  end

  defp do_chunk(acc(_, {_, 0}, _) = acc, _, _, _) do
    {:cont, acc}
  end

  defp do_chunk(acc(h, {buffer, count} = old, t), n, leftover, f1) do
    buffer = :lists.reverse(buffer, Enum.take(leftover, n - count))
    next_with_acc(f1, buffer, h, old, t)
  end

  @doc """
  Chunks the `enum` by buffering elements for which `fun` returns
  the same value and only emit them when `fun` returns a new value
  or the `enum` finishes.

  ## Examples

      iex> stream = Stream.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))
      iex> Enum.to_list(stream)
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

  """
  @spec chunk_by(Enumerable.t, (element -> any)) :: Enumerable.t
  def chunk_by(enum, fun) do
    lazy enum, nil,
         fn(f1) -> R.chunk_by(fun, f1) end,
         &do_chunk_by(&1, &2)
  end

  defp do_chunk_by(acc(_, nil, _) = acc, _f1) do
    {:cont, acc}
  end

  defp do_chunk_by(acc(h, {buffer, _}, t), f1) do
    next_with_acc(f1, :lists.reverse(buffer), h, nil, t)
  end


  @doc """
  Creates a stream that only emits elements if they are different from the last emitted element.

  This function only ever needs to store the last emitted element.

  Elements are compared using `===`.

  ## Examples

      iex> Stream.dedup([1, 2, 3, 3, 2, 1]) |> Enum.to_list
      [1, 2, 3, 2, 1]

  """
  @spec dedup(Enumerable.t) :: Enumerable.t
  def dedup(enum) do
    dedup_by(enum, fn x -> x end)
  end

  @doc """
  Creates a stream that only emits elements if the result of calling `fun` on the element is
  different from the (stored) result of calling `fun` on the last emitted element.

  ## Examples

      iex> Stream.dedup_by([{1, :x}, {2, :y}, {2, :z}, {1, :x}], fn {x, _} -> x end) |> Enum.to_list
      [{1, :x}, {2, :y}, {1, :x}]

  """
  @spec dedup_by(Enumerable.t, (element -> term)) :: Enumerable.t
  def dedup_by(enum, fun) when is_function(fun, 1) do
    lazy enum, nil, fn f1 -> R.dedup(fun, f1) end
  end

  @doc """
  Lazily drops the next `n` items from the enumerable.

  If a negative `n` is given, it will drop the last `n` items from
  the collection. Note that the mechanism by which this is implemented
  will delay the emission of any item until `n` additional items have
  been emitted by the enum.

  ## Examples

      iex> stream = Stream.drop(1..10, 5)
      iex> Enum.to_list(stream)
      [6, 7, 8, 9, 10]

      iex> stream = Stream.drop(1..10, -5)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

  """
  @spec drop(Enumerable.t, non_neg_integer) :: Enumerable.t
  def drop(enum, n) when n >= 0 do
    lazy enum, n, fn(f1) -> R.drop(f1) end
  end

  def drop(enum, n) when n < 0 do
    n = abs(n)

    lazy enum, {0, [], []}, fn(f1) ->
      fn
        entry, [h, {count, buf1, []} | t] ->
          do_drop(:cont, n, entry, h, count, buf1, [], t)
        entry, [h, {count, buf1, [next | buf2]} | t] ->
          {reason, [h | t]} = f1.(next, [h | t])
          do_drop(reason, n, entry, h, count, buf1, buf2, t)
      end
    end
  end

  defp do_drop(reason, n, entry, h, count, buf1, buf2, t) do
    buf1  = [entry | buf1]
    count = count + 1
    if count == n do
      {reason, [h, {0, [], :lists.reverse(buf1)} | t]}
    else
      {reason, [h, {count, buf1, buf2} | t]}
    end
  end

  @doc """
  Creates a stream that drops every `nth` item from the enumerable.

  The first item is always dropped, unless `nth` is 0.

  `nth` must be a non-negative integer.

  ## Examples

      iex> stream = Stream.drop_every(1..10, 2)
      iex> Enum.to_list(stream)
      [2, 4, 6, 8, 10]

      iex> stream = Stream.drop_every(1..1000, 1)
      iex> Enum.to_list(stream)
      []

      iex> stream = Stream.drop_every([1, 2, 3, 4, 5], 0)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

  """
  @spec drop_every(Enumerable.t, non_neg_integer) :: Enumerable.t
  def drop_every(enum, nth)
  def drop_every(enum, 0), do: %Stream{enum: enum}
  def drop_every([], _nth), do: %Stream{enum: []}

  def drop_every(enum, nth) when is_integer(nth) and nth > 0 do
    lazy enum, nth, fn(f1) -> R.drop_every(nth, f1) end
  end

  @doc """
  Lazily drops elements of the enumerable while the given
  function returns `true`.

  ## Examples

      iex> stream = Stream.drop_while(1..10, &(&1 <= 5))
      iex> Enum.to_list(stream)
      [6, 7, 8, 9, 10]

  """
  @spec drop_while(Enumerable.t, (element -> as_boolean(term))) :: Enumerable.t
  def drop_while(enum, fun) do
    lazy enum, true, fn(f1) -> R.drop_while(fun, f1) end
  end

  @doc """
  Executes the given function for each item.

  Useful for adding side effects (like printing) to a stream.

  ## Examples

      iex> stream = Stream.each([1, 2, 3], fn(x) -> send self(), x end)
      iex> Enum.to_list(stream)
      iex> receive do: (x when is_integer(x) -> x)
      1
      iex> receive do: (x when is_integer(x) -> x)
      2
      iex> receive do: (x when is_integer(x) -> x)
      3

  """
  @spec each(Enumerable.t, (element -> term)) :: Enumerable.t
  def each(enum, fun) when is_function(fun, 1) do
    lazy enum, fn(f1) ->
      fn(x, acc) ->
        fun.(x)
        f1.(x, acc)
      end
    end
  end

  @doc """
  Creates a stream that will apply the given function on enumeration and
  flatten the result, but only one level deep.

  ## Examples

      iex> stream = Stream.flat_map([1, 2, 3], fn(x) -> [x, x * 2] end)
      iex> Enum.to_list(stream)
      [1, 2, 2, 4, 3, 6]

      iex> stream = Stream.flat_map([1, 2, 3], fn(x) -> [[x]] end)
      iex> Enum.to_list(stream)
      [[1], [2], [3]]

  """
  @spec flat_map(Enumerable.t, (element -> Enumerable.t)) :: Enumerable.t
  def flat_map(enum, mapper) do
    transform(enum, nil, fn val, nil -> {mapper.(val), nil} end)
  end

  @doc """
  Creates a stream that filters elements according to
  the given function on enumeration.

  ## Examples

      iex> stream = Stream.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      iex> Enum.to_list(stream)
      [2]

  """
  @spec filter(Enumerable.t, (element -> as_boolean(term))) :: Enumerable.t
  def filter(enum, fun) do
    lazy enum, fn(f1) -> R.filter(fun, f1) end
  end

  @doc """
  Creates a stream that filters and then maps elements according
  to given functions.

  Exists for symmetry with `Enum.filter_map/3`.

  ## Examples

      iex> stream = Stream.filter_map(1..6, fn(x) -> rem(x, 2) == 0 end, &(&1 * 2))
      iex> Enum.to_list(stream)
      [4, 8, 12]

  """
  @spec filter_map(Enumerable.t, (element -> as_boolean(term)), (element -> any)) :: Enumerable.t
  def filter_map(enum, filter, mapper) do
    lazy enum, fn(f1) -> R.filter_map(filter, mapper, f1) end
  end

  @doc """
  Creates a stream that emits a value after the given period `n`
  in milliseconds.

  The values emitted are an increasing counter starting at `0`.
  This operation will block the caller by the given interval
  every time a new item is streamed.

  Do not use this function to generate a sequence of numbers.
  If blocking the caller process is not necessary, use
  `Stream.iterate(0, & &1 + 1)` instead.

  ## Examples

      iex> Stream.interval(10) |> Enum.take(10)
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

  """
  @spec interval(non_neg_integer) :: Enumerable.t
  def interval(n) do
    unfold 0, fn(count) ->
      Process.sleep(n)
      {count, count + 1}
    end
  end

  @doc """
  Injects the stream values into the given collectable as a side-effect.

  This function is often used with `run/1` since any evaluation
  is delayed until the stream is executed. See `run/1` for an example.
  """
  @spec into(Enumerable.t, Collectable.t, (term -> term)) :: Enumerable.t
  def into(enum, collectable, transform \\ fn x -> x end) when is_function(transform, 1) do
    &do_into(enum, collectable, transform, &1, &2)
  end

  defp do_into(enum, collectable, transform, acc, fun) do
    {initial, into} = Collectable.into(collectable)
    composed = fn x, [acc | collectable] ->
      collectable = into.(collectable, {:cont, transform.(x)})
      {reason, acc} = fun.(x, acc)
      {reason, [acc | collectable]}
    end
    do_into(&Enumerable.reduce(enum, &1, composed), initial, into, acc)
  end

  defp do_into(reduce, collectable, into, {command, acc}) do
    try do
      reduce.({command, [acc | collectable]})
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        into.(collectable, :halt)
        :erlang.raise(kind, reason, stacktrace)
    else
      {:suspended, [acc | collectable], continuation} ->
        {:suspended, acc, &do_into(continuation, collectable, into, &1)}
      {reason, [acc | collectable]} ->
        into.(collectable, :done)
        {reason, acc}
    end
  end

  @doc """
  Creates a stream that will apply the given function on
  enumeration.

  ## Examples

      iex> stream = Stream.map([1, 2, 3], fn(x) -> x * 2 end)
      iex> Enum.to_list(stream)
      [2, 4, 6]

  """
  @spec map(Enumerable.t, (element -> any)) :: Enumerable.t
  def map(enum, fun) do
    lazy enum, fn(f1) -> R.map(fun, f1) end
  end

  @doc """
  Creates a stream that will apply the given function on
  every `nth` item from the enumerable.

  The first item is always passed to the given function.

  `nth` must be a non-negative integer.

  ## Examples

      iex> stream = Stream.map_every(1..10, 2, fn(x) -> x * 2 end)
      iex> Enum.to_list(stream)
      [2, 2, 6, 4, 10, 6, 14, 8, 18, 10]

      iex> stream = Stream.map_every([1, 2, 3, 4, 5], 1, fn(x) -> x * 2 end)
      iex> Enum.to_list(stream)
      [2, 4, 6, 8, 10]

      iex> stream = Stream.map_every(1..5, 0, fn(x) -> x * 2 end)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

  """
  @spec map_every(Enumerable.t, non_neg_integer, (element -> any)) :: Enumerable.t
  def map_every(enum, nth, fun)

  def map_every(enum, 1, fun), do: map(enum, fun)
  def map_every(enum, 0, _fun), do: %Stream{enum: enum}
  def map_every([], _nth, _fun), do: %Stream{enum: []}

  def map_every(enum, nth, fun) when is_integer(nth) and nth > 0 do
    lazy enum, nth, fn(f1) -> R.map_every(nth, fun, f1) end
  end


  @doc """
  Creates a stream that will reject elements according to
  the given function on enumeration.

  ## Examples

      iex> stream = Stream.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      iex> Enum.to_list(stream)
      [1, 3]

  """
  @spec reject(Enumerable.t, (element -> as_boolean(term))) :: Enumerable.t
  def reject(enum, fun) do
    lazy enum, fn(f1) -> R.reject(fun, f1) end
  end

  @doc """
  Runs the given stream.

  This is useful when a stream needs to be run, for side effects,
  and there is no interest in its return result.

  ## Examples

  Open up a file, replace all `#` by `%` and stream to another file
  without loading the whole file in memory:

      stream = File.stream!("code")
      |> Stream.map(&String.replace(&1, "#", "%"))
      |> Stream.into(File.stream!("new"))
      |> Stream.run

  No computation will be done until we call one of the Enum functions
  or `Stream.run/1`.
  """
  @spec run(Enumerable.t) :: :ok
  def run(stream) do
    _ = Enumerable.reduce(stream, {:cont, nil}, fn(_, _) -> {:cont, nil} end)
    :ok
  end

  @doc """
  Creates a stream that applies the given function to each
  element, emits the result and uses the same result as the accumulator
  for the next computation.

  ## Examples

      iex> stream = Stream.scan(1..5, &(&1 + &2))
      iex> Enum.to_list(stream)
      [1, 3, 6, 10, 15]

  """
  @spec scan(Enumerable.t, (element, acc -> any)) :: Enumerable.t
  def scan(enum, fun) do
    lazy enum, :first, fn(f1) -> R.scan2(fun, f1) end
  end

  @doc """
  Creates a stream that applies the given function to each
  element, emits the result and uses the same result as the accumulator
  for the next computation. Uses the given `acc` as the starting value.

  ## Examples

      iex> stream = Stream.scan(1..5, 0, &(&1 + &2))
      iex> Enum.to_list(stream)
      [1, 3, 6, 10, 15]

  """
  @spec scan(Enumerable.t, acc, (element, acc -> any)) :: Enumerable.t
  def scan(enum, acc, fun) do
    lazy enum, acc, fn(f1) -> R.scan3(fun, f1) end
  end

  @doc """
  Lazily takes the next `count` items from the enumerable and stops
  enumeration.

  If a negative `count` is given, the last `count` values will be taken.
  For such, the collection is fully enumerated keeping up to `2 * count`
  elements in memory. Once the end of the collection is reached,
  the last `count` elements will be executed. Therefore, using
  a negative `count` on an infinite collection will never return.

  ## Examples

      iex> stream = Stream.take(1..100, 5)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

      iex> stream = Stream.take(1..100, -5)
      iex> Enum.to_list(stream)
      [96, 97, 98, 99, 100]

      iex> stream = Stream.cycle([1, 2, 3]) |> Stream.take(5)
      iex> Enum.to_list(stream)
      [1, 2, 3, 1, 2]

  """
  @spec take(Enumerable.t, integer) :: Enumerable.t
  def take(_enum, 0), do: %Stream{enum: []}
  def take([], _count), do: %Stream{enum: []}

  def take(enum, count) when is_integer(count) and count > 0 do
    lazy enum, count, fn(f1) -> R.take(f1) end
  end

  def take(enum, count) when is_integer(count) and count < 0 do
    &Enumerable.reduce(Enum.take(enum, count), &1, &2)
  end

  @doc """
  Creates a stream that takes every `nth` item from the enumerable.

  The first item is always included, unless `nth` is 0.

  `nth` must be a non-negative integer.

  ## Examples

      iex> stream = Stream.take_every(1..10, 2)
      iex> Enum.to_list(stream)
      [1, 3, 5, 7, 9]

      iex> stream = Stream.take_every([1, 2, 3, 4, 5], 1)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

      iex> stream = Stream.take_every(1..1000, 0)
      iex> Enum.to_list(stream)
      []

  """
  @spec take_every(Enumerable.t, non_neg_integer) :: Enumerable.t
  def take_every(enum, nth)
  def take_every(_enum, 0), do: %Stream{enum: []}
  def take_every([], _nth), do: %Stream{enum: []}

  def take_every(enum, nth) when is_integer(nth) and nth > 0 do
    lazy enum, nth, fn(f1) -> R.take_every(nth, f1) end
  end

  @doc """
  Lazily takes elements of the enumerable while the given
  function returns `true`.

  ## Examples

      iex> stream = Stream.take_while(1..100, &(&1 <= 5))
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

  """
  @spec take_while(Enumerable.t, (element -> as_boolean(term))) :: Enumerable.t
  def take_while(enum, fun) do
    lazy enum, fn(f1) -> R.take_while(fun, f1) end
  end

  @doc """
  Creates a stream that emits a single value after `n` milliseconds.

  The value emitted is `0`. This operation will block the caller by
  the given time until the item is streamed.

  ## Examples

      iex> Stream.timer(10) |> Enum.to_list
      [0]

  """
  @spec timer(non_neg_integer) :: Enumerable.t
  def timer(n) do
    take(interval(n), 1)
  end

  @doc """
  Transforms an existing stream.

  It expects an accumulator and a function that receives each stream item
  and an accumulator, and must return a tuple containing a new stream
  (often a list) with the new accumulator or a tuple with `:halt` as first
  element and the accumulator as second.

  Note: this function is similar to `Enum.flat_map_reduce/3` except the
  latter returns both the flat list and accumulator, while this one returns
  only the stream.

  ## Examples

  `Stream.transform/3` is useful as it can be used as the basis to implement
  many of the functions defined in this module. For example, we can implement
  `Stream.take(enum, n)` as follows:

      iex> enum = 1..100
      iex> n = 3
      iex> stream = Stream.transform(enum, 0, fn i, acc ->
      ...>   if acc < n, do: {[i], acc + 1}, else: {:halt, acc}
      ...> end)
      iex> Enum.to_list(stream)
      [1, 2, 3]

  """
  @spec transform(Enumerable.t, acc, fun) :: Enumerable.t
        when fun: (element, acc -> {Enumerable.t, acc} | {:halt, acc}),
             acc: any
  def transform(enum, acc, reducer) when is_function(reducer, 2) do
    &do_transform(enum, fn -> acc end, reducer, &1, &2, nil)
  end

  @doc """
  Transforms an existing stream with function-based start and finish.

  The accumulator is only calculated when transformation starts. It also
  allows an after function to be given which is invoked when the stream
  halts or completes.

  This function can be seen as a combination of `Stream.resource/3` with
  `Stream.transform/3`.
  """
  @spec transform(Enumerable.t, (() -> acc), fun, (acc -> term)) :: Enumerable.t
        when fun: (element, acc -> {Enumerable.t, acc} | {:halt, acc}),
             acc: any
  def transform(enum, start_fun, reducer, after_fun)
      when is_function(start_fun, 0) and is_function(reducer, 2) and is_function(after_fun, 1) do
    &do_transform(enum, start_fun, reducer, &1, &2, after_fun)
  end

  defp do_transform(enumerables, user_acc, user, inner_acc, fun, after_fun) do
    inner = &do_transform_each(&1, &2, fun)
    step  = &do_transform_step(&1, &2)
    next  = &Enumerable.reduce(enumerables, &1, step)
    do_transform(user_acc.(), user, fun, :cont, next, inner_acc, inner, after_fun)
  end

  defp do_transform(user_acc, _user, _fun, _next_op, next, {:halt, inner_acc}, _inner, after_fun) do
    next.({:halt, []})
    do_after(after_fun, user_acc)
    {:halted, inner_acc}
  end

  defp do_transform(user_acc, user, fun, next_op, next, {:suspend, inner_acc}, inner, after_fun) do
    {:suspended, inner_acc, &do_transform(user_acc, user, fun, next_op, next, &1, inner, after_fun)}
  end

  defp do_transform(user_acc, _user, _fun, :halt, _next, {_, inner_acc}, _inner, after_fun) do
    do_after(after_fun, user_acc)
    {:halted, inner_acc}
  end

  defp do_transform(user_acc, user, fun, :cont, next, inner_acc, inner, after_fun) do
    try do
      next.({:cont, []})
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        do_after(after_fun, user_acc)
        :erlang.raise(kind, reason, stacktrace)
    else
      {:suspended, [val], next} ->
        do_transform_user(val, user_acc, user, fun, :cont, next, inner_acc, inner, after_fun)
      {_, [val]} ->
        do_transform_user(val, user_acc, user, fun, :halt, next, inner_acc, inner, after_fun)
      {_, []} ->
        do_transform(user_acc, user, fun, :halt, next, inner_acc, inner, after_fun)
    end
  end

  defp do_transform_user(val, user_acc, user, fun, next_op, next, inner_acc, inner, after_fun) do
    user.(val, user_acc)
  catch
    kind, reason ->
      stacktrace = System.stacktrace
      next.({:halt, []})
      do_after(after_fun, user_acc)
      :erlang.raise(kind, reason, stacktrace)
  else
    {[], user_acc} ->
      do_transform(user_acc, user, fun, next_op, next, inner_acc, inner, after_fun)
    {list, user_acc} when is_list(list) ->
      do_list_transform(user_acc, user, fun, next_op, next, inner_acc, inner,
                        &Enumerable.List.reduce(list, &1, fun), after_fun)
    {:halt, user_acc} ->
      next.({:halt, []})
      do_after(after_fun, user_acc)
      {:halted, elem(inner_acc, 1)}
    {other, user_acc} ->
      do_enum_transform(user_acc, user, fun, next_op, next, inner_acc, inner,
                        &Enumerable.reduce(other, &1, inner), after_fun)
  end

  defp do_list_transform(user_acc, user, fun, next_op, next, inner_acc, inner, reduce, after_fun) do
    try do
      reduce.(inner_acc)
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        next.({:halt, []})
        do_after(after_fun, user_acc)
        :erlang.raise(kind, reason, stacktrace)
    else
      {:done, acc} ->
        do_transform(user_acc, user, fun, next_op, next, {:cont, acc}, inner, after_fun)
      {:halted, acc} ->
        next.({:halt, []})
        do_after(after_fun, user_acc)
        {:halted, acc}
      {:suspended, acc, c} ->
        {:suspended, acc, &do_list_transform(user_acc, user, fun, next_op, next, &1, inner, c, after_fun)}
    end
  end

  defp do_enum_transform(user_acc, user, fun, next_op, next, {op, inner_acc}, inner, reduce, after_fun) do
    try do
      reduce.({op, [:outer | inner_acc]})
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        next.({:halt, []})
        do_after(after_fun, user_acc)
        :erlang.raise(kind, reason, stacktrace)
    else
      # Only take into account outer halts when the op is not halt itself.
      # Otherwise, we were the ones wishing to halt, so we should just stop.
      {:halted, [:outer | acc]} when op != :halt ->
        do_transform(user_acc, user, fun, next_op, next, {:cont, acc}, inner, after_fun)
      {:halted, [_ | acc]} ->
        next.({:halt, []})
        do_after(after_fun, user_acc)
        {:halted, acc}
      {:done, [_ | acc]} ->
        do_transform(user_acc, user, fun, next_op, next, {:cont, acc}, inner, after_fun)
      {:suspended, [_ | acc], c} ->
        {:suspended, acc, &do_enum_transform(user_acc, user, fun, next_op, next, &1, inner, c, after_fun)}
    end
  end

  defp do_after(nil, _user_acc), do: :ok
  defp do_after(fun, user_acc),  do: fun.(user_acc)

  defp do_transform_each(x, [:outer | acc], f) do
    case f.(x, acc) do
      {:halt, res} -> {:halt, [:inner | res]}
      {op, res}    -> {op, [:outer | res]}
    end
  end

  defp do_transform_step(x, acc) do
    {:suspend, [x | acc]}
  end

  @doc """
  Creates a stream that only emits elements if they are unique.

  Keep in mind that, in order to know if an element is unique
  or not, this function needs to store all unique values emitted
  by the stream. Therefore, if the stream is infinite, the number
  of items stored will grow infinitely, never being garbage collected.

  ## Examples

      iex> Stream.uniq([1, 2, 3, 3, 2, 1]) |> Enum.to_list
      [1, 2, 3]

  """
  @spec uniq(Enumerable.t) :: Enumerable.t
  def uniq(enum) do
    uniq_by(enum, fn x -> x end)
  end

  @doc false
  def uniq(enum, fun) do
    uniq_by(enum, fun)
  end

  @doc """
  Creates a stream that only emits elements if they are unique, by removing the
  elements for which function `fun` returned duplicate items.

  The function `fun` maps every element to a term which is used to
  determine if two elements are duplicates.

  Keep in mind that, in order to know if an element is unique
  or not, this function needs to store all unique values emitted
  by the stream. Therefore, if the stream is infinite, the number
  of items stored will grow infinitely, never being garbage collected.

  ## Example

      iex> Stream.uniq_by([{1, :x}, {2, :y}, {1, :z}], fn {x, _} -> x end) |> Enum.to_list
      [{1, :x}, {2, :y}]

      iex> Stream.uniq_by([a: {:tea, 2}, b: {:tea, 2}, c: {:coffee, 1}], fn {_, y} -> y end) |> Enum.to_list
      [a: {:tea, 2}, c: {:coffee, 1}]

  """
  @spec uniq_by(Enumerable.t, (element -> term)) :: Enumerable.t
  def uniq_by(enum, fun) do
    lazy enum, %{}, fn f1 -> R.uniq_by(fun, f1) end
  end

  @doc """
  Creates a stream where each item in the enumerable will
  be wrapped in a tuple alongside its index.

  If an `offset` is given, we will index from the given offset instead of from zero.

  ## Examples

      iex> stream = Stream.with_index([1, 2, 3])
      iex> Enum.to_list(stream)
      [{1, 0}, {2, 1}, {3, 2}]

      iex> stream = Stream.with_index([1, 2, 3], 3)
      iex> Enum.to_list(stream)
      [{1, 3}, {2, 4}, {3, 5}]

  """
  @spec with_index(Enumerable.t) :: Enumerable.t
  @spec with_index(Enumerable.t, integer) :: Enumerable.t
  def with_index(enum, offset \\ 0) do
    lazy enum, offset, fn(f1) -> R.with_index(f1) end
  end

  ## Combiners

  @doc """
  Creates a stream that enumerates each enumerable in an enumerable.

  ## Examples

      iex> stream = Stream.concat([1..3, 4..6, 7..9])
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5, 6, 7, 8, 9]

  """
  @spec concat(Enumerable.t) :: Enumerable.t
  def concat(enumerables) do
    flat_map(enumerables, &(&1))
  end

  @doc """
  Creates a stream that enumerates the first argument, followed by the second.

  ## Examples

      iex> stream = Stream.concat(1..3, 4..6)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5, 6]

      iex> stream1 = Stream.cycle([1, 2, 3])
      iex> stream2 = Stream.cycle([4, 5, 6])
      iex> stream = Stream.concat(stream1, stream2)
      iex> Enum.take(stream, 6)
      [1, 2, 3, 1, 2, 3]

  """
  @spec concat(Enumerable.t, Enumerable.t) :: Enumerable.t
  def concat(first, second) do
    flat_map([first, second], &(&1))
  end

  @doc """
  Zips two collections together, lazily.

  The zipping finishes as soon as any enumerable completes.

  ## Examples

      iex> concat = Stream.concat(1..3, 4..6)
      iex> cycle  = Stream.cycle([:a, :b, :c])
      iex> Stream.zip(concat, cycle) |> Enum.to_list
      [{1, :a}, {2, :b}, {3, :c}, {4, :a}, {5, :b}, {6, :c}]

  """
  @spec zip(Enumerable.t, Enumerable.t) :: Enumerable.t
  def zip(left, right), do: zip([left, right])

  @doc """
  Zips corresponding elements from a collection of enumerables
  into one stream of tuples.

  The zipping finishes as soon as any enumerable completes.

  ## Examples

      iex> concat = Stream.concat(1..3, 4..6)
      iex> cycle = Stream.cycle(["foo", "bar", "baz"])
      iex> Stream.zip([concat, [:a, :b, :c], cycle]) |> Enum.to_list
      [{1, :a, "foo"}, {2, :b, "bar"}, {3, :c, "baz"}]

  """
  @spec zip([Enumerable.t]) :: Enumerable.t
  def zip(enumerables) do
    step      = &do_zip_step(&1, &2)
    enum_funs = Enum.map(enumerables, fn enum ->
      {&Enumerable.reduce(enum, &1, step), :cont}
    end)

    &do_zip(enum_funs, &1, &2)
  end

  # This implementation of do_zip/3 works for any number of
  # streams to zip, even if right now zip/2 only zips two streams.

  defp do_zip(zips, {:halt, acc}, _fun) do
    do_zip_close(zips)
    {:halted, acc}
  end

  defp do_zip(zips, {:suspend, acc}, fun) do
    {:suspended, acc, &do_zip(zips, &1, fun)}
  end

  defp do_zip(zips, {:cont, acc}, callback) do
    try do
      do_zip_next_tuple(zips, acc, callback, [], [])
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        do_zip_close(zips)
        :erlang.raise(kind, reason, stacktrace)
    else
      {:next, buffer, acc} ->
        do_zip(buffer, acc, callback)
      {:done, _acc} = other ->
        other
    end
  end

  # do_zip_next_tuple/5 computes the next tuple formed by
  # the next element of each zipped stream.

  defp do_zip_next_tuple([{_, :halt} | zips], acc, _callback, _yielded_elems, buffer) do
    do_zip_close(:lists.reverse(buffer, zips))
    {:done, acc}
  end

  defp do_zip_next_tuple([{fun, :cont} | zips], acc, callback, yielded_elems, buffer) do
    case fun.({:cont, []}) do
      {:suspended, [elem], fun} ->
        do_zip_next_tuple(zips, acc, callback, [elem | yielded_elems], [{fun, :cont} | buffer])
      {_, [elem]} ->
        do_zip_next_tuple(zips, acc, callback, [elem | yielded_elems], [{fun, :halt} | buffer])
      {_, []} ->
        # The current zipped stream terminated, so we close all the streams
        # and return {:halted, acc} (which is returned as is by do_zip/3).
        do_zip_close(:lists.reverse(buffer, zips))
        {:done, acc}
    end
  end

  defp do_zip_next_tuple([] = _zips, acc, callback, yielded_elems, buffer) do
    # "yielded_elems" is a reversed list of results for the current iteration of
    # zipping: it needs to be reversed and converted to a tuple to have the next
    # tuple in the list resulting from zipping.
    zipped = List.to_tuple(:lists.reverse(yielded_elems))
    {:next, :lists.reverse(buffer), callback.(zipped, acc)}
  end

  defp do_zip_close(zips) do
    :lists.foreach(fn {fun, _} -> fun.({:halt, []}) end, zips)
  end

  defp do_zip_step(x, []) do
    {:suspend, [x]}
  end

  ## Sources

  @doc """
  Creates a stream that cycles through the given enumerable,
  infinitely.

  ## Examples

      iex> stream = Stream.cycle([1, 2, 3])
      iex> Enum.take(stream, 5)
      [1, 2, 3, 1, 2]

  """
  @spec cycle(Enumerable.t) :: Enumerable.t
  def cycle(enumerable)

  def cycle(enumerable) when is_list(enumerable) do
    unfold {enumerable, enumerable}, fn
      {source, [h | t]}      -> {h, {source, t}}
      {source = [h | t], []} -> {h, {source, t}}
    end
  end

  def cycle(enumerable) do
    fn acc, fun ->
      inner = &do_cycle_each(&1, &2, fun)
      outer = &Enumerable.reduce(enumerable, &1, inner)
      do_cycle(outer, outer, acc)
    end
  end

  defp do_cycle(_reduce, _cycle, {:halt, acc}) do
    {:halted, acc}
  end

  defp do_cycle(reduce, cycle, {:suspend, acc}) do
    {:suspended, acc, &do_cycle(reduce, cycle, &1)}
  end

  defp do_cycle(reduce, cycle, acc) do
    try do
      reduce.(acc)
    catch
      {:stream_cycle, acc} ->
        {:halted, acc}
    else
      {state, acc} when state in [:done, :halted] ->
        do_cycle(cycle, cycle, {:cont, acc})
      {:suspended, acc, continuation} ->
        {:suspended, acc, &do_cycle(continuation, cycle, &1)}
    end
  end

  defp do_cycle_each(x, acc, f) do
    case f.(x, acc) do
      {:halt, h} -> throw({:stream_cycle, h})
      {_, _} = o -> o
    end
  end

  @doc """
  Emits a sequence of values, starting with `start_value`. Successive
  values are generated by calling `next_fun` on the previous value.

  ## Examples

      iex> Stream.iterate(0, &(&1+1)) |> Enum.take(5)
      [0, 1, 2, 3, 4]

  """
  @spec iterate(element, (element -> element)) :: Enumerable.t
  def iterate(start_value, next_fun) do
    unfold({:ok, start_value}, fn
      {:ok, value} ->
        {value, {:next, value}}
      {:next, value} ->
        next = next_fun.(value)
        {next, {:next, next}}
    end)
  end

  @doc """
  Returns a stream generated by calling `generator_fun` repeatedly.

  ## Examples

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsplus, {1, 2, 3})
      iex> Stream.repeatedly(&:rand.uniform/0) |> Enum.take(3)
      [0.40502929729990744, 0.45336720247823126, 0.04094511692041057]

  """
  @spec repeatedly((() -> element)) :: Enumerable.t
  def repeatedly(generator_fun) when is_function(generator_fun, 0) do
    &do_repeatedly(generator_fun, &1, &2)
  end

  defp do_repeatedly(generator_fun, {:suspend, acc}, fun) do
    {:suspended, acc, &do_repeatedly(generator_fun, &1, fun)}
  end

  defp do_repeatedly(_generator_fun, {:halt, acc}, _fun) do
    {:halted, acc}
  end

  defp do_repeatedly(generator_fun, {:cont, acc}, fun) do
    do_repeatedly(generator_fun, fun.(generator_fun.(), acc), fun)
  end

  @doc """
  Emits a sequence of values for the given resource.

  Similar to `transform/3` but the initial accumulated value is
  computed lazily via `start_fun` and executes an `after_fun` at
  the end of enumeration (both in cases of success and failure).

  Successive values are generated by calling `next_fun` with the
  previous accumulator (the initial value being the result returned
  by `start_fun`) and it must return a tuple containing a list
  of items to be emitted and the next accumulator. The enumeration
  finishes if it returns `{:halt, acc}`.

  As the name says, this function is useful to stream values from
  resources.

  ## Examples

      Stream.resource(fn -> File.open!("sample") end,
                      fn file ->
                        case IO.read(file, :line) do
                          data when is_binary(data) -> {[data], file}
                          _ -> {:halt, file}
                        end
                      end,
                      fn file -> File.close(file) end)

  """
  @spec resource((() -> acc), (acc -> {[element], acc} | {:halt, acc}), (acc -> term)) :: Enumerable.t
  def resource(start_fun, next_fun, after_fun) do
    &do_resource(start_fun.(), next_fun, &1, &2, after_fun)
  end

  defp do_resource(next_acc, next_fun, {:suspend, acc}, fun, after_fun) do
    {:suspended, acc, &do_resource(next_acc, next_fun, &1, fun, after_fun)}
  end

  defp do_resource(next_acc, _next_fun, {:halt, acc}, _fun, after_fun) do
    after_fun.(next_acc)
    {:halted, acc}
  end

  defp do_resource(next_acc, next_fun, {:cont, acc}, fun, after_fun) do
    try do
      # Optimize the most common cases
      case next_fun.(next_acc) do
        {[], next_acc}  -> {:opt, {:cont, acc}, next_acc}
        {[v], next_acc} -> {:opt, fun.(v, acc), next_acc}
        {_, _} = other  -> other
      end
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        after_fun.(next_acc)
        :erlang.raise(kind, reason, stacktrace)
    else
      {:opt, acc, next_acc} ->
        do_resource(next_acc, next_fun, acc, fun, after_fun)
      {:halt, next_acc} ->
        do_resource(next_acc, next_fun, {:halt, acc}, fun, after_fun)
      {list, next_acc} when is_list(list) ->
        do_list_resource(next_acc, next_fun, {:cont, acc}, fun, after_fun,
                         &Enumerable.List.reduce(list, &1, fun))
      {enum, next_acc} ->
        inner = &do_resource_each(&1, &2, fun)
        do_enum_resource(next_acc, next_fun, {:cont, acc}, fun, after_fun,
                         &Enumerable.reduce(enum, &1, inner))
    end
  end

  defp do_list_resource(next_acc, next_fun, acc, fun, after_fun, reduce) do
    try do
      reduce.(acc)
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        after_fun.(next_acc)
        :erlang.raise(kind, reason, stacktrace)
    else
      {:done, acc} ->
        do_resource(next_acc, next_fun, {:cont, acc}, fun, after_fun)
      {:halted, acc} ->
        do_resource(next_acc, next_fun, {:halt, acc}, fun, after_fun)
      {:suspended, acc, c} ->
        {:suspended, acc, &do_list_resource(next_acc, next_fun, &1, fun, after_fun, c)}
    end
  end

  defp do_enum_resource(next_acc, next_fun, {op, acc}, fun, after_fun, reduce) do
    try do
      reduce.({op, [:outer | acc]})
    catch
      kind, reason ->
        stacktrace = System.stacktrace
        after_fun.(next_acc)
        :erlang.raise(kind, reason, stacktrace)
    else
      {:halted, [:outer | acc]} ->
        do_resource(next_acc, next_fun, {:cont, acc}, fun, after_fun)
      {:halted, [:inner | acc]} ->
        do_resource(next_acc, next_fun, {:halt, acc}, fun, after_fun)
      {:done, [_ | acc]} ->
        do_resource(next_acc, next_fun, {:cont, acc}, fun, after_fun)
      {:suspended, [_ | acc], c} ->
        {:suspended, acc, &do_enum_resource(next_acc, next_fun, &1, fun, after_fun, c)}
    end
  end

  defp do_resource_each(x, [:outer | acc], f) do
    case f.(x, acc) do
      {:halt, res} -> {:halt, [:inner | res]}
      {op, res}    -> {op, [:outer | res]}
    end
  end

  @doc """
  Emits a sequence of values for the given accumulator.

  Successive values are generated by calling `next_fun` with the previous
  accumulator and it must return a tuple with the current value and next
  accumulator. The enumeration finishes if it returns `nil`.

  ## Examples

      iex> Stream.unfold(5, fn 0 -> nil; n -> {n, n-1} end) |> Enum.to_list()
      [5, 4, 3, 2, 1]

  """
  @spec unfold(acc, (acc -> {element, acc} | nil)) :: Enumerable.t
  def unfold(next_acc, next_fun) do
    &do_unfold(next_acc, next_fun, &1, &2)
  end

  defp do_unfold(next_acc, next_fun, {:suspend, acc}, fun) do
    {:suspended, acc, &do_unfold(next_acc, next_fun, &1, fun)}
  end

  defp do_unfold(_next_acc, _next_fun, {:halt, acc}, _fun) do
    {:halted, acc}
  end

  defp do_unfold(next_acc, next_fun, {:cont, acc}, fun) do
    case next_fun.(next_acc) do
      nil           -> {:done, acc}
      {v, next_acc} -> do_unfold(next_acc, next_fun, fun.(v, acc), fun)
    end
  end

  ## Helpers

  @compile {:inline, lazy: 2, lazy: 3, lazy: 4}

  defp lazy(%Stream{done: nil, funs: funs} = lazy, fun),
    do: %{lazy | funs: [fun | funs] }
  defp lazy(enum, fun),
    do: %Stream{enum: enum, funs: [fun]}

  defp lazy(%Stream{done: nil, funs: funs, accs: accs} = lazy, acc, fun),
    do: %{lazy | funs: [fun | funs], accs: [acc | accs] }
  defp lazy(enum, acc, fun),
    do: %Stream{enum: enum, funs: [fun], accs: [acc]}

  defp lazy(%Stream{done: nil, funs: funs, accs: accs} = lazy, acc, fun, done),
    do: %{lazy | funs: [fun | funs], accs: [acc | accs], done: done}
  defp lazy(enum, acc, fun, done),
    do: %Stream{enum: enum, funs: [fun], accs: [acc], done: done}
end

defimpl Enumerable, for: Stream do
  @compile :inline_list_funs

  def reduce(lazy, acc, fun) do
    do_reduce(lazy, acc, fn x, [acc] ->
      {reason, acc} = fun.(x, acc)
      {reason, [acc]}
    end)
  end

  def count(_lazy) do
    {:error, __MODULE__}
  end

  def member?(_lazy, _value) do
    {:error, __MODULE__}
  end

  defp do_reduce(%Stream{enum: enum, funs: funs, accs: accs, done: done}, acc, fun) do
    composed = :lists.foldl(fn fun, acc -> fun.(acc) end, fun, funs)
    do_each(&Enumerable.reduce(enum, &1, composed),
            done && {done, fun}, :lists.reverse(accs), acc)
  end

  defp do_each(reduce, done, accs, {command, acc}) do
    case reduce.({command, [acc | accs]}) do
      {:suspended, [acc | accs], continuation} ->
        {:suspended, acc, &do_each(continuation, done, accs, &1)}
      {:halted, accs} ->
        do_done {:halted, accs}, done
      {:done, accs} ->
        do_done {:done, accs}, done
    end
  end

  defp do_done({reason, [acc | _]}, nil), do: {reason, acc}
  defp do_done({reason, [acc | t]}, {done, fun}) do
    [h | _] = Enum.reverse(t)
    case done.([acc, h], fun) do
      {:cont, [acc | _]}    -> {reason, acc}
      {:halt, [acc | _]}    -> {:halted, acc}
      {:suspend, [acc | _]} -> {:suspended, acc, &({:done, elem(&1, 1)})}
    end
  end
end

defimpl Inspect, for: Stream do
  import Inspect.Algebra

  def inspect(%{enum: enum, funs: funs}, opts) do
    inner = [enum: enum, funs: Enum.reverse(funs)]
    concat ["#Stream<", to_doc(inner, opts), ">"]
  end
end
