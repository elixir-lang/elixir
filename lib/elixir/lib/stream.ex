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
  was enumerated three times. Let's see an example with streams:

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
  its double. In this example, the list was enumerated just once!

  That's what we meant when we first said that streams are composable,
  lazy enumerables. Notice we could call `Stream.map/2` multiple times,
  effectively composing the streams and they are lazy. The computations
  are performed only when you call a function from the `Enum` module.

  ## Creating Streams

  There are many functions in Elixir's standard library that return
  streams, some examples are:

  * `IO.stream/1` - Streams input lines, one by one;
  * `URI.query_decoder/1` - Decodes a query string, pair by pair;

  This module also provides many convenience functions for creating streams,
  like `Stream.cycle/1`, `Stream.unfold/2`, `Stream.resource/3` and more.
  """

  defrecord Lazy, enum: nil, funs: [], accs: [], done: nil

  defimpl Enumerable, for: Lazy do
    @compile :inline_list_funs

    def reduce(lazy, acc, fun) do
      do_reduce(lazy, acc, fn x, [acc] ->
        { reason, acc } = fun.(x, acc)
        { reason, [acc] }
      end)
    end

    def count(_lazy) do
      { :error, __MODULE__ }
    end

    def member?(_lazy, _value) do
      { :error, __MODULE__ }
    end

    defp do_reduce(Lazy[enum: enum, funs: funs, accs: accs, done: done], acc, fun) do
      composed = :lists.foldl(fn fun, acc -> fun.(acc) end, fun, funs)
      do_each(&Enumerable.reduce(enum, &1, composed), done && { done, fun }, :lists.reverse(accs), acc)
    end

    defp do_each(_reduce, _done, _accs, { :halt, acc }) do
      { :halted, acc }
    end

    defp do_each(reduce, done, accs, { :suspend, acc }) do
      { :suspended, acc, &do_each(reduce, done, accs, &1) }
    end

    defp do_each(reduce, done, accs, { :cont, acc }) do
      case reduce.({ :cont, [acc|accs] }) do
        { :suspended, [acc|accs], continuation } ->
          { :suspended, acc, &do_each(continuation, done, accs, &1) }
        { :halted, [acc|_] } ->
          { :halted, acc }
        { :done, [acc|_] = accs } ->
          case done do
            nil ->
              { :done, acc }
            { done, fun } ->
              case done.(fun).(accs) do
                { :cont, [acc|_] }    -> { :done, acc }
                { :halt, [acc|_] }    -> { :halted, acc }
                { :suspend, [acc|_] } -> { :suspended, acc, &({ :done, &1 |> elem(1) }) }
              end
          end
      end
    end
  end

  @type acc :: any
  @type element :: any
  @type index :: non_neg_integer
  @type default :: any

  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop cont(f, entry, acc) do
    quote do: unquote(f).(unquote(entry), unquote(acc))
  end

  defmacrop acc(h, n, t) do
    quote do: [unquote(h),unquote(n)|unquote(t)]
  end

  defmacrop cont_with_acc(f, entry, h, n, t) do
    quote do
      { reason, [h|t] } = unquote(f).(unquote(entry), [unquote(h)|unquote(t)])
      { reason, [h,unquote(n)|t] }
    end
  end

  ## Transformers

  @doc """
  Shortcut to `chunks(coll, n, n)`.
  """
  @spec chunks(Enumerable.t, non_neg_integer) :: Enumerable.t
  def chunks(coll, n), do: chunks(coll, n, n, nil)

  @doc """
  Streams the enumerable in chunks, containing `n` items each, where
  each new chunk starts `step` elements into the enumerable.

  `step` is optional and, if not passed, defaults to `n`, i.e.
  chunks do not overlap. If the final chunk does not have `n`
  elements to fill the chunk, elements are taken as necessary
  from `pad` if it was passed. If `pad` is passed and does not
  have enough elements to fill the chunk, then the chunk is
  returned anyway with less than `n` elements. If `pad` is not
  passed at all or is nil, then the partial chunk is discarded
  from the result.

  ## Examples

      iex> Stream.chunks([1, 2, 3, 4, 5, 6], 2) |> Enum.to_list
      [[1, 2], [3, 4], [5, 6]]
      iex> Stream.chunks([1, 2, 3, 4, 5, 6], 3, 2) |> Enum.to_list
      [[1, 2, 3], [3, 4, 5]]
      iex> Stream.chunks([1, 2, 3, 4, 5, 6], 3, 2, [7]) |> Enum.to_list
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]
      iex> Stream.chunks([1, 2, 3, 4, 5, 6], 3, 3, []) |> Enum.to_list
      [[1, 2, 3], [4, 5, 6]]

  """
  @spec chunks(Enumerable.t, non_neg_integer, non_neg_integer) :: Enumerable.t
  @spec chunks(Enumerable.t, non_neg_integer, non_neg_integer, Enumerable.t | nil) :: Enumerable.t
  def chunks(enum, n, step, pad // nil) when n > 0 and step > 0 do
    limit = :erlang.max(n, step)
    lazy enum, { [], 0 },
         fn(f1) -> R.chunks(n, step, limit, f1) end,
         fn(f1) -> &do_chunks(&1, n, pad, f1) end
  end

  defp do_chunks(acc(h, { buffer, count } = old, t) = acc, n, pad, f1) do
    if nil?(pad) || count == 0 do
      { :cont, acc }
    else
      buffer = :lists.reverse(buffer) ++ Enum.take(pad, n - count)
      cont_with_acc(f1, buffer, h, old, t)
    end
  end

  @doc """
  Chunks the `enum` by buffering elements for which `fun` returns
  the same value and only emit them when `fun` returns a new value
  or the `enum` finishes,

  ## Examples

      iex> stream = Stream.chunks_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))
      iex> Enum.to_list(stream)
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

  """
  @spec chunks_by(Enumerable.t, (element -> any)) :: Enumerable.t
  def chunks_by(enum, fun) do
    lazy enum, nil,
         fn(f1) -> R.chunks_by(fun, f1) end,
         fn(f1) -> &do_chunks_by(&1, f1) end
  end

  defp do_chunks_by(acc(_, nil, _) = acc, _f1) do
    { :cont, acc }
  end

  defp do_chunks_by(acc(h, { buffer, _ }, t), f1) do
    cont_with_acc(f1, :lists.reverse(buffer), h, nil, t)
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
      [6,7,8,9,10]

      iex> stream = Stream.drop(1..10, -5)
      iex> Enum.to_list(stream)
      [1,2,3,4,5]

  """
  @spec drop(Enumerable.t, non_neg_integer) :: Enumerable.t
  def drop(enum, n) when n >= 0 do
    lazy enum, n, fn(f1) -> R.drop(f1) end
  end

  def drop(enum, n) when n < 0 do
    n = abs(n)

    lazy enum, { 0, [], [] }, fn(f1) ->
      fn
        entry, [h, { count, buf1, [] } | t] ->
          do_drop(:cont, n, entry, h, count, buf1, [], t)
        entry, [h, { count, buf1, [next|buf2] } | t] ->
          { reason, [h|t] } = f1.(next, [h|t])
          do_drop(reason, n, entry, h, count, buf1, buf2, t)
      end
    end
  end

  defp do_drop(reason, n, entry, h, count, buf1, buf2, t) do
    buf1  = [entry|buf1]
    count = count + 1
    if count == n do
      { reason, [h, { 0, [], :lists.reverse(buf1) }|t] }
    else
      { reason, [h, { count, buf1, buf2 }|t] }
    end
  end

  @doc """
  Lazily drops elements of the enumerable while the given
  function returns true.

  ## Examples

      iex> stream = Stream.drop_while(1..10, &(&1 <= 5))
      iex> Enum.to_list(stream)
      [6,7,8,9,10]

  """
  @spec drop_while(Enumerable.t, (element -> as_boolean(term))) :: Enumerable.t
  def drop_while(enum, fun) do
    lazy enum, true, fn(f1) -> R.drop_while(fun, f1) end
  end

  @doc """
  Execute the given function for each item.

  Useful for adding side effects (like printing) to a stream.

  ## Examples

      iex> stream = Stream.each([1, 2, 3], fn(x) -> IO.puts x end)
      iex> Enum.to_list(stream)
      1
      2
      3
      [1,2,3]

  """
  @spec each(Enumerable.t, (element -> term)) :: Enumerable.t
  def each(enum, fun) do
    lazy enum, fn(f1) ->
      fn(x, acc) ->
        fun.(x)
        f1.(x, acc)
      end
    end
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
  Creates a stream that filters and then map elements according
  to given funtions.

  It exists for simmetry with Enum.filter_map/3.

  ## Examples

      iex> stream = Stream.filter_map(1..6, fn(x) -> rem(x, 2) == 0 end, &(&1 * 2))
      iex> Enum.to_list(stream)
      [4,8,12]

  """
  @spec filter_map(Enumerable.t, (element -> as_boolean(term)), (element -> any)) :: Enumerable.t
  def filter_map(enum, filter, mapper) do
    lazy enum, fn(f1) -> R.filter_map(filter, mapper, f1) end
  end

  @doc """
  Creates a stream that will apply the given function on
  enumeration.

  ## Examples

      iex> stream = Stream.map([1, 2, 3], fn(x) -> x * 2 end)
      iex> Enum.to_list(stream)
      [2,4,6]

  """
  @spec map(Enumerable.t, (element -> any)) :: Enumerable.t
  def map(enum, fun) do
    lazy enum, fn(f1) -> R.map(fun, f1) end
  end

  @doc """
  Creates a stream that applies the given function to each
  element, emits the result and uses the same result as accumulator
  for the next computation.

  ## Examples

      iex> stream = Stream.scan(1..5, &(&1 + &2))
      iex> Enum.to_list(stream)
      [1,3,6,10,15]

  """
  @spec scan(Enumerable.t, (element, acc -> any)) :: Enumerable.t
  def scan(enum, fun) do
    lazy enum, :first, fn(f1) -> R.scan_2(fun, f1) end
  end

  @doc """
  Creates a stream that applies the given function to each
  element, emits the result and uses the same result as accumulator
  for the next computation. Uses the given `acc` as starting value.

  ## Examples

      iex> stream = Stream.scan(1..5, 0, &(&1 + &2))
      iex> Enum.to_list(stream)
      [1,3,6,10,15]

  """
  @spec scan(Enumerable.t, acc, (element, acc -> any)) :: Enumerable.t
  def scan(enum, acc, fun) do
    lazy enum, acc, fn(f1) -> R.scan_3(fun, f1) end
  end

  @doc """
  Creates a stream that will apply the given function on enumeration and
  flatten the result.

  ## Examples

      iex> stream = Stream.flat_map([1, 2, 3], fn(x) -> [x, x * 2] end)
      iex> Enum.to_list(stream)
      [1, 2, 2, 4, 3, 6]

  """
  @spec flat_map(Enumerable.t, (element -> any)) :: Enumerable.t
  def flat_map(enum, mapper) do
    &do_flat_map(enum, mapper, &1, &2)
  end

  defp do_flat_map(enumerables, mapper, acc, fun) do
    fun  = &do_flat_map_each(fun, &1, &2)
    step = &do_flat_map_step/2
    next = &Enumerable.reduce(enumerables, &1, step)
    do_flat_map([], next, mapper, acc, fun)
  end

  defp do_flat_map(next_acc, next, mapper, acc, fun) do
    case next.({ :cont, next_acc }) do
      { :suspended, [val|next_acc], next } ->
        enum = mapper.(val)
        do_flat_map(next_acc, next, mapper, acc, fun, &Enumerable.reduce(enum, &1, fun))
      { reason, _ } ->
        { reason, elem(acc, 1) }
    end
  end

  defp do_flat_map(next_acc, next, mapper, acc, fun, reduce) do
    try do
      reduce.(acc)
    catch
      { :stream_flat_map, h } -> { :halted, h }
    else
      { _, acc }              -> do_flat_map(next_acc, next, mapper, { :cont, acc }, fun)
      { :suspended, acc, c }  -> { :suspended, acc, &do_flat_map(next_acc, next, mapper, &1, fun, c) }
    end
  end

  defp do_flat_map_each(f, x, acc) do
    case f.(x, acc) do
      { :halt, h } -> throw({ :stream_flat_map, h })
      { _, _ } = o -> o
    end
  end

  defp do_flat_map_step(x, acc) do
    { :suspend, [x|acc] }
  end

  @doc """
  Creates a stream that will reject elements according to
  the given function on enumeration.

  ## Examples

      iex> stream = Stream.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      iex> Enum.to_list(stream)
      [1,3]

  """
  @spec reject(Enumerable.t, (element -> as_boolean(term))) :: Enumerable.t
  def reject(enum, fun) do
    lazy enum, fn(f1) -> R.reject(fun, f1) end
  end

  @doc """
  Lazily takes the next `n` items from the enumerable and stops
  enumeration.

  If a negative `n` is given, the last `n` values will be taken.
  For such, the collection is fully enumerated keeping up to `2 * n`
  elements in memory. Once the end of the collection is reached,
  the last `count` elements will be executed. Therefore, using
  a negative `n` in an infinite collection will never return.

  ## Examples

      iex> stream = Stream.take(1..100, 5)
      iex> Enum.to_list(stream)
      [1,2,3,4,5]

      iex> stream = Stream.take(1..100, -5)
      iex> Enum.to_list(stream)
      [96,97,98,99,100]

      iex> stream = Stream.cycle([1, 2, 3]) |> Stream.take(5)
      iex> Enum.to_list(stream)
      [1,2,3,1,2]

  """
  @spec take(Enumerable.t, non_neg_integer) :: Enumerable.t
  def take(_enum, 0), do: Lazy[enum: []]

  def take(enum, n) when n > 0 do
    lazy enum, n, fn(f1) -> R.take(f1) end
  end

  def take(enum, n) when n < 0 do
    &do_take(enum, abs(n), &1, &2)
  end

  defp do_take(enum, n, acc, f) do
    { _, { _count, buf1, buf2 } } =
      Enumerable.reduce(enum, { :cont, { 0, [], [] } }, fn
        entry, { count, buf1, buf2 } ->
          buf1  = [entry|buf1]
          count = count + 1
          if count == n do
            { :cont, { 0, [], buf1 } }
          else
            { :cont, { count, buf1, buf2 } }
          end
      end)

    Enumerable.reduce(do_take_last(buf1, buf2, n, []), acc, f)
  end

  defp do_take_last(_buf1, _buf2, 0, acc),
    do: acc
  defp do_take_last([], [], _, acc),
    do: acc
  defp do_take_last([], [h|t], n, acc),
    do: do_take_last([], t, n-1, [h|acc])
  defp do_take_last([h|t], buf2, n, acc),
    do: do_take_last(t, buf2, n-1, [h|acc])

  @doc """
  Creates a stream that takes every `n` item from the enumerable.

  The first item is always included, unless n is 0.

  ## Examples

      iex> stream = Stream.take_every(1..10, 2)
      iex> Enum.to_list(stream)
      [1,3,5,7,9]

  """
  @spec take(Enumerable.t, non_neg_integer) :: Enumerable.t
  def take_every(enum, n) when n > 0 do
    lazy enum, n, fn(f1) -> R.take_every(n, f1) end
  end

  def take_every(_enum, 0), do: Lazy[enum: []]

  @doc """
  Lazily takes elements of the enumerable while the given
  function returns true.

  ## Examples

      iex> stream = Stream.take_while(1..100, &(&1 <= 5))
      iex> Enum.to_list(stream)
      [1,2,3,4,5]

  """
  @spec take_while(Enumerable.t, (element -> as_boolean(term))) :: Enumerable.t
  def take_while(enum, fun) do
    lazy enum, fn(f1) -> R.take_while(fun, f1) end
  end

  @doc """
  Creates a stream that only emits elements if they are uniq.

  Keep in mind that, in order to know if an element is unique
  or not, this function needs to store all uniq values emitted
  by the stream. Therefore, if the stream is infinite, the number
  of items stored will grow infinitely, never being garbage collected.

  ## Examples

      iex> Stream.uniq([1, 2, 3, 2, 1]) |> Enum.to_list
      [1, 2, 3]

      iex> Stream.uniq([{1, :x}, {2, :y}, {1, :z}], fn {x, _} -> x end) |> Enum.to_list
      [{1,:x}, {2,:y}]

  """
  @spec uniq(Enumerable.t) :: Enumerable.t
  @spec uniq(Enumerable.t, (element -> term)) :: Enumerable.t
  def uniq(enum, fun // fn x -> x end) do
    lazy enum, [], fn f1 -> R.uniq(fun, f1) end
  end

  @doc """
  Creates a stream where each item in the enumerable will
  be accompanied by its index.

  ## Examples

      iex> stream = Stream.with_index([1, 2, 3])
      iex> Enum.to_list(stream)
      [{1,0},{2,1},{3,2}]

  """
  @spec with_index(Enumerable.t) :: Enumerable.t
  def with_index(enum) do
    lazy enum, 0, fn(f1) -> R.with_index(f1) end
  end

  ## Combiners

  @doc """
  Creates a stream that enumerates each enumerable in an enumerable.

  ## Examples

      iex> stream = Stream.concat([1..3, 4..6, 7..9])
      iex> Enum.to_list(stream)
      [1,2,3,4,5,6,7,8,9]

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
      [1,2,3,4,5,6]

      iex> stream1 = Stream.cycle([1, 2, 3])
      iex> stream2 = Stream.cycle([4, 5, 6])
      iex> stream = Stream.concat(stream1, stream2)
      iex> Enum.take(stream, 6)
      [1,2,3,1,2,3]

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
      [{1,:a},{2,:b},{3,:c},{4,:a},{5,:b},{6,:c}]

  """
  @spec zip(Enumerable.t, Enumerable.t) :: Enumerable.t
  def zip(left, right) do
    step      = &do_zip_step(&1, &2)
    left_fun  = &Enumerable.reduce(left, &1, step)
    right_fun = &Enumerable.reduce(right, &1, step)

    # Return a function as a lazy enumerator.
    &do_zip(left_fun, [], right_fun, [], &1, &2)
  end

  defp do_zip(_left_fun, _left_acc, _right_fun, _right_acc, { :halt, acc }, _fun) do
    { :halted, acc }
  end

  defp do_zip(left_fun, left_acc, right_fun, right_acc, { :suspend, acc }, fun) do
    { :suspended, acc, &do_zip(left_fun, left_acc, right_fun, right_acc, &1, fun) }
  end

  defp do_zip(left_fun, left_acc, right_fun, right_acc, { :cont, acc }, callback) do
    case left_fun.({ :cont, left_acc }) do
      { :suspended, [x|left_acc], left_fun } ->
        case right_fun.({ :cont, right_acc }) do
          { :suspended, [y|right_acc], right_fun } ->
            do_zip(left_fun, left_acc, right_fun, right_acc, callback.({ x, y }, acc), callback)
          { reason, _ } ->
            { reason, acc }
        end
      { reason, _ } ->
        { reason, acc }
    end
  end

  defp do_zip_step(x, acc) do
    { :suspend, [x|acc] }
  end

  ## Sources

  @doc """
  Creates a stream that cycles through the given enumerable,
  infinitely.

  ## Examples

      iex> stream = Stream.cycle([1,2,3])
      iex> Enum.take(stream, 5)
      [1,2,3,1,2]

  """
  @spec cycle(Enumerable.t) :: Enumerable.t
  def cycle(enumerable) do
    fn acc, fun ->
      reduce = &Enumerable.reduce(enumerable, &1, fun)
      do_cycle(reduce, reduce, acc)
    end
  end

  defp do_cycle(_reduce, _cycle, { :halt, acc }) do
    { :halted, acc }
  end

  defp do_cycle(reduce, cycle, { :suspend, acc }) do
    { :suspended, acc, &do_cycle(reduce, cycle, &1) }
  end

  defp do_cycle(reduce, cycle, acc) do
    case reduce.(acc) do
      { :done, acc } ->
        do_cycle(cycle, cycle, { :cont, acc })
      { :halted, acc } ->
        { :halted, acc }
      { :suspended, acc, continuation } ->
        { :suspended, acc, &do_cycle(continuation, cycle, &1) }
    end
  end

  @doc """
  Emit a sequence of values, starting with `start_value`. Successive
  values are generated by calling `next_fun` on the previous value.

  ## Examples

      iex> Stream.iterate(0, &(&1+1)) |> Enum.take(5)
      [0,1,2,3,4]

  """
  @spec iterate(element, (element -> element)) :: Enumerable.t
  def iterate(start_value, next_fun) do
    unfold({ :ok, start_value}, fn
      { :ok, value } ->
        { value, { :next, value } }
      { :next, value } ->
        next = next_fun.(value)
        { next, { :next, next } }
    end)
  end

  @doc """
  Returns a stream generated by calling `generator_fun` repeatedly.

  ## Examples

      iex> Stream.repeatedly(&:random.uniform/0) |> Enum.take(3)
      [0.4435846174457203, 0.7230402056221108, 0.94581636451987]

  """
  @spec repeatedly((() -> element)) :: Enumerable.t
  def repeatedly(generator_fun) when is_function(generator_fun, 0) do
    &do_repeatedly(generator_fun, &1, &2)
  end

  defp do_repeatedly(generator_fun, { :suspend, acc }, fun) do
    { :suspended, acc, &do_repeatedly(generator_fun, &1, fun) }
  end

  defp do_repeatedly(_generator_fun, { :halt, acc }, _fun) do
    { :halted, acc }
  end

  defp do_repeatedly(generator_fun, { :cont, acc }, fun) do
    do_repeatedly(generator_fun, fun.(generator_fun.(), acc), fun)
  end

  @doc """
  Emits a sequence of values for the given resource.

  Similar to `unfold/2` but the initial value is computed lazily via
  `start_fun` and executes an `after_fun` at the end of enumeration
  (both in cases of success and failure).

  Successive values are generated by calling `next_fun` with the
  previous accumulator (the initial value being the result returned
  by `start_fun`) and it must return a tuple with the current and
  next accumulator. The enumeration finishes if it returns nil.

  As the name says, this function is useful to stream values from
  resources.

  ## Examples

      Stream.resource(fn -> File.open("sample") end,
                      fn file ->
                        case IO.read(file, :line) do
                          data when is_binary(data) -> { data, file }
                          _ -> nil
                        end
                      end,
                      fn file -> File.close!(file) end)

  """
  @spec resource((() -> acc), (acc -> { element, acc } | nil), (acc -> term)) :: Enumerable.t
  def resource(start_fun, next_fun, after_fun) do
    fn acc, fun ->
      next_acc = start_fun.()
      try do
        do_unfold(next_acc, next_fun, acc, fun)
      after
        after_fun.(next_acc)
      end
    end
  end

  @doc """
  Emits a sequence of values for the given accumulator.

  Successive values are generated by calling `next_fun` with the previous
  accumulator and it must return a tuple with the current and next
  accumulator. The enumeration finishes if it returns nil.

  ## Examples

      iex> Stream.unfold(5, fn 0 -> nil; n -> {n, n-1} end) |> Enum.to_list()
      [5, 4, 3, 2, 1]
  """
  @spec unfold(acc, (acc -> { element, acc } | nil)) :: Enumerable.t
  def unfold(next_acc, next_fun) do
    &do_unfold(next_acc, next_fun, &1, &2)
  end

  defp do_unfold(next_acc, next_fun, { :suspend, acc }, fun) do
    { :suspended, acc, &do_unfold(next_acc, next_fun, &1, fun) }
  end

  defp do_unfold(_next_acc, _next_fun, { :halt, acc }, _fun) do
    { :halted, acc }
  end

  defp do_unfold(next_acc, next_fun, { :cont, acc }, fun) do
    case next_fun.(next_acc) do
      nil             -> { :done, acc }
      { v, next_acc } -> do_unfold(next_acc, next_fun, fun.(v, acc), fun)
    end
  end

  ## Helpers

  @compile { :inline, lazy: 2, lazy: 3, lazy: 4 }

  defp lazy(Lazy[funs: funs] = lazy, fun),
    do: lazy.funs([fun|funs])
  defp lazy(enum, fun),
    do: Lazy[enum: enum, funs: [fun]]

  defp lazy(Lazy[funs: funs, accs: accs] = lazy, acc, fun),
    do: lazy.funs([fun|funs]).accs([acc|accs])
  defp lazy(enum, acc, fun),
    do: Lazy[enum: enum, funs: [fun], accs: [acc]]

  defp lazy(Lazy[done: nil, funs: funs, accs: accs] = lazy, acc, fun, done),
    do: lazy.funs([fun|funs]).accs([acc|accs]).done(done)
  defp lazy(enum, acc, fun, done),
    do: Lazy[enum: enum, funs: [fun], accs: [acc], done: done]
end
