defprotocol Enumerable do
  @moduledoc """
  Enumerable protocol used by `Enum` and `Stream` modules.

  When you invoke a function in the `Enum` module, the first argument
  is usually a collection that must implement this protocol.
  For example, the expression `Enum.map([1, 2, 3], &(&1 * 2))`
  invokes `Enumerable.reduce/3` to perform the reducing operation that
  builds a mapped list by calling the mapping function `&(&1 * 2)` on
  every element in the collection and consuming the element with an
  accumulated list.

  Internally, `Enum.map/2` is implemented as follows:

      def map(enumerable, fun) do
        reducer = fn x, acc -> {:cont, [fun.(x) | acc]} end
        Enumerable.reduce(enumerable, {:cont, []}, reducer) |> elem(1) |> :lists.reverse()
      end

  Note that the user-supplied function is wrapped into a `t:reducer/0` function.
  The `t:reducer/0` function must return a tagged tuple after each step,
  as described in the `t:acc/0` type. At the end, `Enumerable.reduce/3`
  returns `t:result/0`.

  This protocol uses tagged tuples to exchange information between the
  reducer function and the data type that implements the protocol. This
  allows enumeration of resources, such as files, to be done efficiently
  while also guaranteeing the resource will be closed at the end of the
  enumeration. This protocol also allows suspension of the enumeration,
  which is useful when interleaving between many enumerables is required
  (as in the `zip/1` and `zip/2` functions).

  This protocol requires four functions to be implemented, `reduce/3`,
  `count/1`, `member?/2`, and `slice/1`. The core of the protocol is the
  `reduce/3` function. All other functions exist as optimizations paths
  for data structures that can implement certain properties in better
  than linear time.
  """

  @typedoc """
  An enumerable of elements of type `element`.

  This type is equivalent to `t:t/0` but is especially useful for documentation.

  For example, imagine you define a function that expects an enumerable of
  integers and returns an enumerable of strings:

      @spec integers_to_strings(Enumerable.t(integer())) :: Enumerable.t(String.t())
      def integers_to_strings(integers) do
        Stream.map(integers, &Integer.to_string/1)
      end

  """
  @typedoc since: "1.14.0"
  @type t(_element) :: t()

  @typedoc """
  The accumulator value for each step.

  It must be a tagged tuple with one of the following "tags":

    * `:cont`    - the enumeration should continue
    * `:halt`    - the enumeration should halt immediately
    * `:suspend` - the enumeration should be suspended immediately

  Depending on the accumulator value, the result returned by
  `Enumerable.reduce/3` will change. Please check the `t:result/0`
  type documentation for more information.

  In case a `t:reducer/0` function returns a `:suspend` accumulator,
  it must be explicitly handled by the caller and never leak.
  """
  @type acc :: {:cont, term} | {:halt, term} | {:suspend, term}

  @typedoc """
  The reducer function.

  Should be called with the `enumerable` element and the
  accumulator contents.

  Returns the accumulator for the next enumeration step.
  """
  @type reducer :: (element :: term, current_acc :: acc -> updated_acc :: acc)

  @typedoc """
  The result of the reduce operation.

  It may be *done* when the enumeration is finished by reaching
  its end, or *halted*/*suspended* when the enumeration was halted
  or suspended by the tagged accumulator.

  In case the tagged `:halt` accumulator is given, the `:halted` tuple
  with the accumulator must be returned. Functions like `Enum.take_while/2`
  use `:halt` underneath and can be used to test halting enumerables.

  In case the tagged `:suspend` accumulator is given, the caller must
  return the `:suspended` tuple with the accumulator and a continuation.
  The caller is then responsible of managing the continuation and the
  caller must always call the continuation, eventually halting or continuing
  until the end. `Enum.zip/2` uses suspension, so it can be used to test
  whether your implementation handles suspension correctly. You can also use
  `Stream.zip/2` with `Enum.take_while/2` to test the combination of
  `:suspend` with `:halt`.
  """
  @type result ::
          {:done, term}
          | {:halted, term}
          | {:suspended, term, continuation}

  @typedoc """
  A partially applied reduce function.

  The continuation is the closure returned as a result when
  the enumeration is suspended. When invoked, it expects
  a new accumulator and it returns the result.

  A continuation can be trivially implemented as long as the reduce
  function is defined in a tail recursive fashion. If the function
  is tail recursive, all the state is passed as arguments, so
  the continuation is the reducing function partially applied.
  """
  @type continuation :: (acc -> result)

  @typedoc """
  A slicing function that receives the initial position,
  the number of elements in the slice, and the step.

  The `start` position is a number `>= 0` and guaranteed to
  exist in the `enumerable`. The length is a number `>= 1`
  in a way that `start + length * step <= count`, where
  `count` is the maximum amount of elements in the enumerable.

  The function should return a non empty list where
  the amount of elements is equal to `length`.
  """
  @type slicing_fun ::
          (start :: non_neg_integer, length :: pos_integer, step :: pos_integer -> [term()])

  @typedoc """
  Receives an enumerable and returns a list.
  """
  @type to_list_fun :: (t -> [term()])

  @doc """
  Reduces the `enumerable` into an element.

  Most of the operations in `Enum` are implemented in terms of reduce.
  This function should apply the given `t:reducer/0` function to each
  element in the `enumerable` and proceed as expected by the returned
  accumulator.

  See the documentation of the types `t:result/0` and `t:acc/0` for
  more information.

  ## Examples

  As an example, here is the implementation of `reduce` for lists:

      def reduce(_list, {:halt, acc}, _fun), do: {:halted, acc}
      def reduce(list, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
      def reduce([], {:cont, acc}, _fun), do: {:done, acc}
      def reduce([head | tail], {:cont, acc}, fun), do: reduce(tail, fun.(head, acc), fun)

  """
  @spec reduce(t, acc, reducer) :: result
  def reduce(enumerable, acc, fun)

  @doc """
  Retrieves the number of elements in the `enumerable`.

  It should return `{:ok, count}` if you can count the number of elements
  in `enumerable` in a faster way than fully traversing it.

  Otherwise it should return `{:error, __MODULE__}` and a default algorithm
  built on top of `reduce/3` that runs in linear time will be used.
  """
  @spec count(t) :: {:ok, non_neg_integer} | {:error, module}
  def count(enumerable)

  @doc """
  Checks if an `element` exists within the `enumerable`.

  It should return `{:ok, boolean}` if you can check the membership of a
  given element in `enumerable` with `===/2` without traversing the whole
  of it.

  Otherwise it should return `{:error, __MODULE__}` and a default algorithm
  built on top of `reduce/3` that runs in linear time will be used.

  When called outside guards, the [`in`](`in/2`) and [`not in`](`in/2`)
  operators work by using this function.
  """
  @spec member?(t, term) :: {:ok, boolean} | {:error, module}
  def member?(enumerable, element)

  @doc """
  Returns a function that slices the data structure contiguously.

  It should return either:

    * `{:ok, size, slicing_fun}` - if the `enumerable` has a known
      bound and can access a position in the `enumerable` without
      traversing all previous elements. The `slicing_fun` will receive
      a `start` position, the `amount` of elements to fetch, and a
      `step`.

    * `{:ok, size, to_list_fun}` - if the `enumerable` has a known bound
      and can access a position in the `enumerable` by first converting
      it to a list via `to_list_fun`.

    * `{:error, __MODULE__}` - the enumerable cannot be sliced efficiently
      and a default algorithm built on top of `reduce/3` that runs in
      linear time will be used.

  ## Differences to `count/1`

  The `size` value returned by this function is used for boundary checks,
  therefore it is extremely important that this function only returns `:ok`
  if retrieving the `size` of the `enumerable` is cheap, fast, and takes
  constant time. Otherwise the simplest of operations, such as
  `Enum.at(enumerable, 0)`, will become too expensive.

  On the other hand, the `count/1` function in this protocol should be
  implemented whenever you can count the number of elements in the collection
  without traversing it.
  """
  @spec slice(t) ::
          {:ok, size :: non_neg_integer(), slicing_fun() | to_list_fun()}
          | {:error, module()}
  def slice(enumerable)
end

defmodule Enum do
  import Kernel, except: [max: 2, min: 2]

  @moduledoc """
  Functions for working with collections (known as enumerables).

  In Elixir, an enumerable is any data type that implements the
  `Enumerable` protocol. `List`s (`[1, 2, 3]`), `Map`s (`%{foo: 1, bar: 2}`)
  and `Range`s (`1..3`) are common data types used as enumerables:

      iex> Enum.map([1, 2, 3], fn x -> x * 2 end)
      [2, 4, 6]

      iex> Enum.sum([1, 2, 3])
      6

      iex> Enum.map(1..3, fn x -> x * 2 end)
      [2, 4, 6]

      iex> Enum.sum(1..3)
      6

      iex> map = %{"a" => 1, "b" => 2}
      iex> Enum.map(map, fn {k, v} -> {k, v * 2} end)
      [{"a", 2}, {"b", 4}]

  However, many other enumerables exist in the language, such as `MapSet`s
  and the data type returned by `File.stream!/3` which allows a file to be
  traversed as if it was an enumerable.

  The functions in this module work in linear time. This means that, the
  time it takes to perform an operation grows at the same rate as the length
  of the enumerable. This is expected on operations such as `Enum.map/2`.
  After all, if we want to traverse every element on a list, the longer the
  list, the more elements we need to traverse, and the longer it will take.

  This linear behaviour should also be expected on operations like `count/1`,
  `member?/2`, `at/2` and similar. While Elixir does allow data types to
  provide performant variants for such operations, you should not expect it
  to always be available, since the `Enum` module is meant to work with a
  large variety of data types and not all data types can provide optimized
  behaviour.

  Finally, note the functions in the `Enum` module are eager: they will
  traverse the enumerable as soon as they are invoked. This is particularly
  dangerous when working with infinite enumerables. In such cases, you should
  use the `Stream` module, which allows you to lazily express computations,
  without traversing collections, and work with possibly infinite collections.
  See the `Stream` module for examples and documentation.
  """

  @compile :inline_list_funcs

  @type t :: Enumerable.t()
  @type acc :: any
  @type element :: any

  @typedoc "Zero-based index. It can also be a negative integer."
  @type index :: integer

  @type default :: any

  require Stream.Reducers, as: R

  defmacrop skip(acc) do
    acc
  end

  defmacrop next(_, entry, acc) do
    quote(do: [unquote(entry) | unquote(acc)])
  end

  defmacrop acc(head, state, _) do
    quote(do: {unquote(head), unquote(state)})
  end

  defmacrop next_with_acc(_, entry, head, state, _) do
    quote do
      {[unquote(entry) | unquote(head)], unquote(state)}
    end
  end

  @doc """
  Returns `true` if all elements in `enumerable` are truthy.

  When an element has a falsy value (`false` or `nil`) iteration stops immediately
  and `false` is returned. In all other cases `true` is returned.

  ## Examples

      iex> Enum.all?([1, 2, 3])
      true

      iex> Enum.all?([1, nil, 3])
      false

      iex> Enum.all?([])
      true

  """
  @spec all?(t) :: boolean
  def all?(enumerable) when is_list(enumerable) do
    all_list(enumerable)
  end

  def all?(enumerable) do
    Enumerable.reduce(enumerable, {:cont, true}, fn entry, _ ->
      if entry, do: {:cont, true}, else: {:halt, false}
    end)
    |> elem(1)
  end

  @doc """
  Returns `true` if `fun.(element)` is truthy for all elements in `enumerable`.

  Iterates over `enumerable` and invokes `fun` on each element. If `fun` ever
  returns a falsy value (`false` or `nil`), iteration stops immediately and
  `false` is returned. Otherwise, `true` is returned.

  ## Examples

      iex> Enum.all?([2, 4, 6], fn x -> rem(x, 2) == 0 end)
      true

      iex> Enum.all?([2, 3, 4], fn x -> rem(x, 2) == 0 end)
      false

      iex> Enum.all?([], fn _ -> nil end)
      true

  As the last example shows, `Enum.all?/2` returns `true` if `enumerable` is
  empty, regardless of `fun`. In an empty enumerable there is no element for
  which `fun` returns a falsy value, so the result must be `true`. This is a
  well-defined logical argument for empty collections.

  """
  @spec all?(t, (element -> as_boolean(term))) :: boolean
  def all?(enumerable, fun) when is_list(enumerable) do
    all_list(enumerable, fun)
  end

  def all?(enumerable, fun) do
    Enumerable.reduce(enumerable, {:cont, true}, fn entry, _ ->
      if fun.(entry), do: {:cont, true}, else: {:halt, false}
    end)
    |> elem(1)
  end

  @doc """
  Returns `true` if at least one element in `enumerable` is truthy.

  When an element has a truthy value (neither `false` nor `nil`) iteration stops
  immediately and `true` is returned. In all other cases `false` is returned.

  ## Examples

      iex> Enum.any?([false, false, false])
      false

      iex> Enum.any?([false, true, false])
      true

      iex> Enum.any?([])
      false

  """
  @spec any?(t) :: boolean
  def any?(enumerable) when is_list(enumerable) do
    any_list(enumerable)
  end

  def any?(enumerable) do
    Enumerable.reduce(enumerable, {:cont, false}, fn entry, _ ->
      if entry, do: {:halt, true}, else: {:cont, false}
    end)
    |> elem(1)
  end

  @doc """
  Returns `true` if `fun.(element)` is truthy for at least one element in `enumerable`.

  Iterates over the `enumerable` and invokes `fun` on each element. When an invocation
  of `fun` returns a truthy value (neither `false` nor `nil`) iteration stops
  immediately and `true` is returned. In all other cases `false` is returned.

  ## Examples

      iex> Enum.any?([2, 4, 6], fn x -> rem(x, 2) == 1 end)
      false

      iex> Enum.any?([2, 3, 4], fn x -> rem(x, 2) == 1 end)
      true

      iex> Enum.any?([], fn x -> x > 0 end)
      false

  """
  @spec any?(t, (element -> as_boolean(term))) :: boolean
  def any?(enumerable, fun) when is_list(enumerable) do
    any_list(enumerable, fun)
  end

  def any?(enumerable, fun) do
    Enumerable.reduce(enumerable, {:cont, false}, fn entry, _ ->
      if fun.(entry), do: {:halt, true}, else: {:cont, false}
    end)
    |> elem(1)
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Returns `default` if `index` is out of bounds.

  A negative `index` can be passed, which means the `enumerable` is
  enumerated once and the `index` is counted from the end (for example,
  `-1` finds the last element).

  ## Examples

      iex> Enum.at([2, 4, 6], 0)
      2

      iex> Enum.at([2, 4, 6], 2)
      6

      iex> Enum.at([2, 4, 6], 4)
      nil

      iex> Enum.at([2, 4, 6], 4, :none)
      :none

  """
  @spec at(t, index, default) :: element | default
  def at(enumerable, index, default \\ nil) when is_integer(index) do
    case slice_forward(enumerable, index, 1, 1) do
      [value] -> value
      [] -> default
    end
  end

  @doc false
  @deprecated "Use Enum.chunk_every/2 instead"
  def chunk(enumerable, count), do: chunk(enumerable, count, count, nil)

  @doc false
  @deprecated "Use Enum.chunk_every/3 instead"
  def chunk(enum, n, step) do
    chunk_every(enum, n, step, :discard)
  end

  @doc false
  @deprecated "Use Enum.chunk_every/4 instead"
  def chunk(enumerable, count, step, leftover) do
    chunk_every(enumerable, count, step, leftover || :discard)
  end

  @doc """
  Shortcut to `chunk_every(enumerable, count, count)`.
  """
  @doc since: "1.5.0"
  @spec chunk_every(t, pos_integer) :: [list]
  def chunk_every(enumerable, count), do: chunk_every(enumerable, count, count, [])

  @doc """
  Returns list of lists containing `count` elements each, where
  each new chunk starts `step` elements into the `enumerable`.

  `step` is optional and, if not passed, defaults to `count`, i.e.
  chunks do not overlap. Chunking will stop as soon as the collection
  ends or when we emit an incomplete chunk.

  If the last chunk does not have `count` elements to fill the chunk,
  elements are taken from `leftover` to fill in the chunk. If `leftover`
  does not have enough elements to fill the chunk, then a partial chunk
  is returned with less than `count` elements.

  If `:discard` is given in `leftover`, the last chunk is discarded
  unless it has exactly `count` elements.

  ## Examples

      iex> Enum.chunk_every([1, 2, 3, 4, 5, 6], 2)
      [[1, 2], [3, 4], [5, 6]]

      iex> Enum.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, :discard)
      [[1, 2, 3], [3, 4, 5]]

      iex> Enum.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, [7])
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

      iex> Enum.chunk_every([1, 2, 3, 4], 3, 3, [])
      [[1, 2, 3], [4]]

      iex> Enum.chunk_every([1, 2, 3, 4], 10)
      [[1, 2, 3, 4]]

      iex> Enum.chunk_every([1, 2, 3, 4, 5], 2, 3, [])
      [[1, 2], [4, 5]]

      iex> Enum.chunk_every([1, 2, 3, 4], 3, 3, Stream.cycle([0]))
      [[1, 2, 3], [4, 0, 0]]

  """
  @doc since: "1.5.0"
  @spec chunk_every(t, pos_integer, pos_integer, t | :discard) :: [list]
  def chunk_every(enumerable, count, step, leftover \\ [])
      when is_integer(count) and count > 0 and is_integer(step) and step > 0 do
    R.chunk_every(&chunk_while/4, enumerable, count, step, leftover)
  end

  @doc """
  Chunks the `enumerable` with fine grained control when every chunk is emitted.

  `chunk_fun` receives the current element and the accumulator and must return:

    * `{:cont, chunk, acc}` to emit a chunk and continue with the accumulator
    * `{:cont, acc}` to not emit any chunk and continue with the accumulator
    * `{:halt, acc}` to halt chunking over the `enumerable`.

  `after_fun` is invoked with the final accumulator when iteration is
  finished (or `halt`ed) to handle any trailing elements that were returned
  as part of an accumulator, but were not emitted as a chunk by `chunk_fun`.
  It must return:

    * `{:cont, chunk, acc}` to emit a chunk. The chunk will be appended to the
      list of already emitted chunks.
    * `{:cont, acc}` to not emit a chunk

  The `acc` in `after_fun` is required in order to mirror the tuple format
  from `chunk_fun` but it will be discarded since the traversal is complete.

  Returns a list of emitted chunks.

  ## Examples

      iex> chunk_fun = fn element, acc ->
      ...>   if rem(element, 2) == 0 do
      ...>     {:cont, Enum.reverse([element | acc]), []}
      ...>   else
      ...>     {:cont, [element | acc]}
      ...>   end
      ...> end
      iex> after_fun = fn
      ...>   [] -> {:cont, []}
      ...>   acc -> {:cont, Enum.reverse(acc), []}
      ...> end
      iex> Enum.chunk_while(1..10, [], chunk_fun, after_fun)
      [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
      iex> Enum.chunk_while([1, 2, 3, 5, 7], [], chunk_fun, after_fun)
      [[1, 2], [3, 5, 7]]

  """
  @doc since: "1.5.0"
  @spec chunk_while(
          t,
          acc,
          (element, acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          (acc -> {:cont, chunk, acc} | {:cont, acc})
        ) :: Enumerable.t()
        when chunk: any
  def chunk_while(enumerable, acc, chunk_fun, after_fun) do
    {_, {res, acc}} =
      Enumerable.reduce(enumerable, {:cont, {[], acc}}, fn entry, {buffer, acc} ->
        case chunk_fun.(entry, acc) do
          {:cont, chunk, acc} -> {:cont, {[chunk | buffer], acc}}
          {:cont, acc} -> {:cont, {buffer, acc}}
          {:halt, acc} -> {:halt, {buffer, acc}}
        end
      end)

    case after_fun.(acc) do
      {:cont, _acc} -> :lists.reverse(res)
      {:cont, chunk, _acc} -> :lists.reverse([chunk | res])
    end
  end

  @doc """
  Splits enumerable on every element for which `fun` returns a new
  value.

  Returns a list of lists.

  ## Examples

      iex> Enum.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

  """
  @spec chunk_by(t, (element -> any)) :: [list]
  def chunk_by(enumerable, fun) do
    R.chunk_by(&chunk_while/4, enumerable, fun)
  end

  @doc """
  Given an enumerable of enumerables, concatenates the `enumerables` into
  a single list.

  ## Examples

      iex> Enum.concat([1..3, 4..6, 7..9])
      [1, 2, 3, 4, 5, 6, 7, 8, 9]

      iex> Enum.concat([[1, [2], 3], [4], [5, 6]])
      [1, [2], 3, 4, 5, 6]

  """
  @spec concat(t) :: t
  def concat(enumerables)

  def concat(list) when is_list(list) do
    concat_list(list)
  end

  def concat(enums) do
    concat_enum(enums)
  end

  @doc """
  Concatenates the enumerable on the `right` with the enumerable on the
  `left`.

  This function produces the same result as the `++/2` operator
  for lists.

  ## Examples

      iex> Enum.concat(1..3, 4..6)
      [1, 2, 3, 4, 5, 6]

      iex> Enum.concat([1, 2, 3], [4, 5, 6])
      [1, 2, 3, 4, 5, 6]

  """
  @spec concat(t, t) :: t
  def concat(left, right) when is_list(left) and is_list(right) do
    left ++ right
  end

  def concat(left, right) do
    concat_enum([left, right])
  end

  @doc """
  Returns the size of the `enumerable`.

  ## Examples

      iex> Enum.count([1, 2, 3])
      3

  """
  @spec count(t) :: non_neg_integer
  def count(enumerable) when is_list(enumerable) do
    length(enumerable)
  end

  def count(enumerable) do
    case Enumerable.count(enumerable) do
      {:ok, value} when is_integer(value) ->
        value

      {:error, module} ->
        enumerable |> module.reduce({:cont, 0}, fn _, acc -> {:cont, acc + 1} end) |> elem(1)
    end
  end

  @doc """
  Returns the count of elements in the `enumerable` for which `fun` returns
  a truthy value.

  ## Examples

      iex> Enum.count([1, 2, 3, 4, 5], fn x -> rem(x, 2) == 0 end)
      2

  """
  @spec count(t, (element -> as_boolean(term))) :: non_neg_integer
  def count(enumerable, fun) do
    reduce(enumerable, 0, fn entry, acc ->
      if(fun.(entry), do: acc + 1, else: acc)
    end)
  end

  @doc """
  Counts the enumerable stopping at `limit`.

  This is useful for checking certain properties of the count of an enumerable
  without having to actually count the entire enumerable. For example, if you
  wanted to check that the count was exactly, at least, or more than a value.

  If the enumerable implements `c:Enumerable.count/1`, the enumerable is
  not traversed and we return the lower of the two numbers. To force
  enumeration, use `count_until/3` with `fn _ -> true end` as the second
  argument.

  ## Examples

      iex> Enum.count_until(1..20, 5)
      5
      iex> Enum.count_until(1..20, 50)
      20
      iex> Enum.count_until(1..10, 10) == 10 # At least 10
      true
      iex> Enum.count_until(1..11, 10 + 1) > 10 # More than 10
      true
      iex> Enum.count_until(1..5, 10) < 10 # Less than 10
      true
      iex> Enum.count_until(1..10, 10 + 1) == 10 # Exactly ten
      true

  """
  @doc since: "1.12.0"
  @spec count_until(t, pos_integer) :: non_neg_integer
  def count_until(enumerable, limit) when is_integer(limit) and limit > 0 do
    stop_at = limit - 1

    case Enumerable.count(enumerable) do
      {:ok, value} ->
        Kernel.min(value, limit)

      {:error, module} ->
        enumerable
        |> module.reduce(
          {:cont, 0},
          fn
            _, ^stop_at ->
              {:halt, limit}

            _, acc ->
              {:cont, acc + 1}
          end
        )
        |> elem(1)
    end
  end

  @doc """
  Counts the elements in the enumerable for which `fun` returns a truthy value, stopping at `limit`.

  See `count/2` and `count_until/2` for more information.

  ## Examples

      iex> Enum.count_until(1..20, fn x -> rem(x, 2) == 0 end, 7)
      7
      iex> Enum.count_until(1..20, fn x -> rem(x, 2) == 0 end, 11)
      10
  """
  @doc since: "1.12.0"
  @spec count_until(t, (element -> as_boolean(term)), pos_integer) :: non_neg_integer
  def count_until(enumerable, fun, limit) when is_integer(limit) and limit > 0 do
    stop_at = limit - 1

    Enumerable.reduce(enumerable, {:cont, 0}, fn
      entry, ^stop_at ->
        if fun.(entry) do
          {:halt, limit}
        else
          {:cont, stop_at}
        end

      entry, acc ->
        if fun.(entry) do
          {:cont, acc + 1}
        else
          {:cont, acc}
        end
    end)
    |> elem(1)
  end

  @doc """
  Enumerates the `enumerable`, returning a list where all consecutive
  duplicated elements are collapsed to a single element.

  Elements are compared using `===/2`.

  If you want to remove all duplicated elements, regardless of order,
  see `uniq/1`.

  ## Examples

      iex> Enum.dedup([1, 2, 3, 3, 2, 1])
      [1, 2, 3, 2, 1]

      iex> Enum.dedup([1, 1, 2, 2.0, :three, :three])
      [1, 2, 2.0, :three]

  """
  @spec dedup(t) :: list
  def dedup(enumerable) when is_list(enumerable) do
    dedup_list(enumerable, []) |> :lists.reverse()
  end

  def dedup(enumerable) do
    Enum.reduce(enumerable, [], fn x, acc ->
      case acc do
        [^x | _] -> acc
        _ -> [x | acc]
      end
    end)
    |> :lists.reverse()
  end

  @doc """
  Enumerates the `enumerable`, returning a list where all consecutive
  duplicated elements are collapsed to a single element.

  The function `fun` maps every element to a term which is used to
  determine if two elements are duplicates.

  ## Examples

      iex> Enum.dedup_by([{1, :a}, {2, :b}, {2, :c}, {1, :a}], fn {x, _} -> x end)
      [{1, :a}, {2, :b}, {1, :a}]

      iex> Enum.dedup_by([5, 1, 2, 3, 2, 1], fn x -> x > 2 end)
      [5, 1, 3, 2]

  """
  @spec dedup_by(t, (element -> term)) :: list
  def dedup_by(enumerable, fun) do
    {list, _} = reduce(enumerable, {[], []}, R.dedup(fun))
    :lists.reverse(list)
  end

  @doc """
  Drops the `amount` of elements from the `enumerable`.

  If a negative `amount` is given, the `amount` of last values will be dropped.
  The `enumerable` will be enumerated once to retrieve the proper index and
  the remaining calculation is performed from the end.

  ## Examples

      iex> Enum.drop([1, 2, 3], 2)
      [3]

      iex> Enum.drop([1, 2, 3], 10)
      []

      iex> Enum.drop([1, 2, 3], 0)
      [1, 2, 3]

      iex> Enum.drop([1, 2, 3], -1)
      [1, 2]

  """
  @spec drop(t, integer) :: list
  def drop(enumerable, amount)
      when is_list(enumerable) and is_integer(amount) and amount >= 0 do
    drop_list(enumerable, amount)
  end

  def drop(enumerable, 0) do
    to_list(enumerable)
  end

  def drop(enumerable, amount) when is_integer(amount) and amount > 0 do
    {result, _} = reduce(enumerable, {[], amount}, R.drop())
    if is_list(result), do: :lists.reverse(result), else: []
  end

  def drop(enumerable, amount) when is_integer(amount) and amount < 0 do
    {count, fun} = slice_count_and_fun(enumerable, 1)
    amount = Kernel.min(amount + count, count)

    if amount > 0 do
      fun.(0, amount, 1)
    else
      []
    end
  end

  @doc """
  Returns a list of every `nth` element in the `enumerable` dropped,
  starting with the first element.

  The first element is always dropped, unless `nth` is 0.

  The second argument specifying every `nth` element must be a non-negative
  integer.

  ## Examples

      iex> Enum.drop_every(1..10, 2)
      [2, 4, 6, 8, 10]

      iex> Enum.drop_every(1..10, 0)
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      iex> Enum.drop_every([1, 2, 3], 1)
      []

  """
  @spec drop_every(t, non_neg_integer) :: list
  def drop_every(enumerable, nth)

  def drop_every(_enumerable, 1), do: []
  def drop_every(enumerable, 0), do: to_list(enumerable)
  def drop_every([], nth) when is_integer(nth), do: []

  def drop_every(enumerable, nth) when is_integer(nth) and nth > 1 do
    {res, _} = reduce(enumerable, {[], :first}, R.drop_every(nth))
    :lists.reverse(res)
  end

  @doc """
  Drops elements at the beginning of the `enumerable` while `fun` returns a
  truthy value.

  ## Examples

      iex> Enum.drop_while([1, 2, 3, 2, 1], fn x -> x < 3 end)
      [3, 2, 1]

  """
  @spec drop_while(t, (element -> as_boolean(term))) :: list
  def drop_while(enumerable, fun) when is_list(enumerable) do
    drop_while_list(enumerable, fun)
  end

  def drop_while(enumerable, fun) do
    {res, _} = reduce(enumerable, {[], true}, R.drop_while(fun))
    :lists.reverse(res)
  end

  @doc """
  Invokes the given `fun` for each element in the `enumerable`.

  Returns `:ok`.

  ## Examples

      Enum.each(["some", "example"], fn x -> IO.puts(x) end)
      "some"
      "example"
      #=> :ok

  """
  @spec each(t, (element -> any)) :: :ok
  def each(enumerable, fun) when is_list(enumerable) do
    :lists.foreach(fun, enumerable)
  end

  def each(enumerable, fun) do
    reduce(enumerable, nil, fn entry, _ ->
      fun.(entry)
      nil
    end)

    :ok
  end

  @doc """
  Determines if the `enumerable` is empty.

  Returns `true` if `enumerable` is empty, otherwise `false`.

  ## Examples

      iex> Enum.empty?([])
      true

      iex> Enum.empty?([1, 2, 3])
      false

  """
  @spec empty?(t) :: boolean
  def empty?(enumerable) when is_list(enumerable) do
    enumerable == []
  end

  def empty?(enumerable) do
    case Enumerable.slice(enumerable) do
      {:ok, value, _} ->
        value == 0

      {:error, module} ->
        enumerable
        |> module.reduce({:cont, true}, fn _, _ -> {:halt, false} end)
        |> elem(1)
    end
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Returns `{:ok, element}` if found, otherwise `:error`.

  A negative `index` can be passed, which means the `enumerable` is
  enumerated once and the `index` is counted from the end (for example,
  `-1` fetches the last element).

  ## Examples

      iex> Enum.fetch([2, 4, 6], 0)
      {:ok, 2}

      iex> Enum.fetch([2, 4, 6], -3)
      {:ok, 2}

      iex> Enum.fetch([2, 4, 6], 2)
      {:ok, 6}

      iex> Enum.fetch([2, 4, 6], 4)
      :error

  """
  @spec fetch(t, index) :: {:ok, element} | :error
  def fetch(enumerable, index) when is_integer(index) do
    case slice_forward(enumerable, index, 1, 1) do
      [value] -> {:ok, value}
      [] -> :error
    end
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Raises `OutOfBoundsError` if the given `index` is outside the range of
  the `enumerable`.

  ## Examples

      iex> Enum.fetch!([2, 4, 6], 0)
      2

      iex> Enum.fetch!([2, 4, 6], 2)
      6

      iex> Enum.fetch!([2, 4, 6], 4)
      ** (Enum.OutOfBoundsError) out of bounds error

  """
  @spec fetch!(t, index) :: element
  def fetch!(enumerable, index) when is_integer(index) do
    case slice_forward(enumerable, index, 1, 1) do
      [value] -> value
      [] -> raise Enum.OutOfBoundsError
    end
  end

  @doc """
  Filters the `enumerable`, i.e. returns only those elements
  for which `fun` returns a truthy value.

  See also `reject/2` which discards all elements where the
  function returns a truthy value.

  ## Examples

      iex> Enum.filter([1, 2, 3], fn x -> rem(x, 2) == 0 end)
      [2]

  Keep in mind that `filter` is not capable of filtering and
  transforming an element at the same time. If you would like
  to do so, consider using `flat_map/2`. For example, if you
  want to convert all strings that represent an integer and
  discard the invalid one in one pass:

      strings = ["1234", "abc", "12ab"]

      Enum.flat_map(strings, fn string ->
        case Integer.parse(string) do
          # transform to integer
          {int, _rest} -> [int]
          # skip the value
          :error -> []
        end
      end)

  """
  @spec filter(t, (element -> as_boolean(term))) :: list
  def filter(enumerable, fun) when is_list(enumerable) do
    filter_list(enumerable, fun)
  end

  def filter(enumerable, fun) do
    reduce(enumerable, [], R.filter(fun)) |> :lists.reverse()
  end

  @doc false
  @deprecated "Use Enum.filter/2 + Enum.map/2 or for comprehensions instead"
  def filter_map(enumerable, filter, mapper) when is_list(enumerable) do
    for element <- enumerable, filter.(element), do: mapper.(element)
  end

  def filter_map(enumerable, filter, mapper) do
    enumerable
    |> reduce([], R.filter_map(filter, mapper))
    |> :lists.reverse()
  end

  @doc """
  Returns the first element for which `fun` returns a truthy value.
  If no such element is found, returns `default`.

  ## Examples

      iex> Enum.find([2, 3, 4], fn x -> rem(x, 2) == 1 end)
      3

      iex> Enum.find([2, 4, 6], fn x -> rem(x, 2) == 1 end)
      nil
      iex> Enum.find([2, 4, 6], 0, fn x -> rem(x, 2) == 1 end)
      0

  """
  @spec find(t, default, (element -> any)) :: element | default
  def find(enumerable, default \\ nil, fun)

  def find(enumerable, default, fun) when is_list(enumerable) do
    find_list(enumerable, default, fun)
  end

  def find(enumerable, default, fun) do
    Enumerable.reduce(enumerable, {:cont, default}, fn entry, default ->
      if fun.(entry), do: {:halt, entry}, else: {:cont, default}
    end)
    |> elem(1)
  end

  @doc """
  Similar to `find/3`, but returns the index (zero-based)
  of the element instead of the element itself.

  ## Examples

      iex> Enum.find_index([2, 4, 6], fn x -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find_index([2, 3, 4], fn x -> rem(x, 2) == 1 end)
      1

  """
  @spec find_index(t, (element -> any)) :: non_neg_integer | nil
  def find_index(enumerable, fun) when is_list(enumerable) do
    find_index_list(enumerable, 0, fun)
  end

  def find_index(enumerable, fun) do
    result =
      Enumerable.reduce(enumerable, {:cont, {:not_found, 0}}, fn entry, {_, index} ->
        if fun.(entry), do: {:halt, {:found, index}}, else: {:cont, {:not_found, index + 1}}
      end)

    case elem(result, 1) do
      {:found, index} -> index
      {:not_found, _} -> nil
    end
  end

  @doc """
  Similar to `find/3`, but returns the value of the function
  invocation instead of the element itself.

  The return value is considered to be found when the result is truthy
  (neither `nil` nor `false`).

  ## Examples

      iex> Enum.find_value([2, 3, 4], fn x ->
      ...>   if x > 2, do: x * x
      ...> end)
      9

      iex> Enum.find_value([2, 4, 6], fn x -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find_value([2, 3, 4], fn x -> rem(x, 2) == 1 end)
      true

      iex> Enum.find_value([1, 2, 3], "no bools!", &is_boolean/1)
      "no bools!"

  """
  @spec find_value(t, any, (element -> any)) :: any | nil
  def find_value(enumerable, default \\ nil, fun)

  def find_value(enumerable, default, fun) when is_list(enumerable) do
    find_value_list(enumerable, default, fun)
  end

  def find_value(enumerable, default, fun) do
    Enumerable.reduce(enumerable, {:cont, default}, fn entry, default ->
      fun_entry = fun.(entry)
      if fun_entry, do: {:halt, fun_entry}, else: {:cont, default}
    end)
    |> elem(1)
  end

  @doc """
  Maps the given `fun` over `enumerable` and flattens the result.

  This function returns a new enumerable built by appending the result of invoking `fun`
  on each element of `enumerable` together; conceptually, this is similar to a
  combination of `map/2` and `concat/1`.

  ## Examples

      iex> Enum.flat_map([:a, :b, :c], fn x -> [x, x] end)
      [:a, :a, :b, :b, :c, :c]

      iex> Enum.flat_map([{1, 3}, {4, 6}], fn {x, y} -> x..y end)
      [1, 2, 3, 4, 5, 6]

      iex> Enum.flat_map([:a, :b, :c], fn x -> [[x]] end)
      [[:a], [:b], [:c]]

  """
  @spec flat_map(t, (element -> t)) :: list
  def flat_map(enumerable, fun) when is_list(enumerable) do
    flat_map_list(enumerable, fun)
  end

  def flat_map(enumerable, fun) do
    reduce(enumerable, [], fn entry, acc ->
      case fun.(entry) do
        list when is_list(list) -> [list | acc]
        other -> [to_list(other) | acc]
      end
    end)
    |> flat_reverse([])
  end

  defp flat_reverse([h | t], acc), do: flat_reverse(t, h ++ acc)
  defp flat_reverse([], acc), do: acc

  @doc """
  Maps and reduces an `enumerable`, flattening the given results (only one level deep).

  It expects an accumulator and a function that receives each enumerable
  element, and must return a tuple containing a new enumerable (often a list)
  with the new accumulator or a tuple with `:halt` as first element and
  the accumulator as second.

  ## Examples

      iex> enumerable = 1..100
      iex> n = 3
      iex> Enum.flat_map_reduce(enumerable, 0, fn x, acc ->
      ...>   if acc < n, do: {[x], acc + 1}, else: {:halt, acc}
      ...> end)
      {[1, 2, 3], 3}

      iex> Enum.flat_map_reduce(1..5, 0, fn x, acc -> {[[x]], acc + x} end)
      {[[1], [2], [3], [4], [5]], 15}

  """
  @spec flat_map_reduce(t, acc, fun) :: {[any], acc}
        when fun: (element, acc -> {t, acc} | {:halt, acc})
  def flat_map_reduce(enumerable, acc, fun) do
    {_, {list, acc}} =
      Enumerable.reduce(enumerable, {:cont, {[], acc}}, fn entry, {list, acc} ->
        case fun.(entry, acc) do
          {:halt, acc} ->
            {:halt, {list, acc}}

          {[], acc} ->
            {:cont, {list, acc}}

          {[entry], acc} ->
            {:cont, {[entry | list], acc}}

          {entries, acc} ->
            {:cont, {reduce(entries, list, &[&1 | &2]), acc}}
        end
      end)

    {:lists.reverse(list), acc}
  end

  @doc """
  Returns a map with keys as unique elements of `enumerable` and values
  as the count of every element.

  ## Examples

      iex> Enum.frequencies(~w{ant buffalo ant ant buffalo dingo})
      %{"ant" => 3, "buffalo" => 2, "dingo" => 1}

  """
  @doc since: "1.10.0"
  @spec frequencies(t) :: map
  def frequencies(enumerable) do
    reduce(enumerable, %{}, fn key, acc ->
      case acc do
        %{^key => value} -> %{acc | key => value + 1}
        %{} -> Map.put(acc, key, 1)
      end
    end)
  end

  @doc """
  Returns a map with keys as unique elements given by `key_fun` and values
  as the count of every element.

  ## Examples

      iex> Enum.frequencies_by(~w{aa aA bb cc}, &String.downcase/1)
      %{"aa" => 2, "bb" => 1, "cc" => 1}

      iex> Enum.frequencies_by(~w{aaa aA bbb cc c}, &String.length/1)
      %{3 => 2, 2 => 2, 1 => 1}

  """
  @doc since: "1.10.0"
  @spec frequencies_by(t, (element -> any)) :: map
  def frequencies_by(enumerable, key_fun) when is_function(key_fun) do
    reduce(enumerable, %{}, fn entry, acc ->
      key = key_fun.(entry)

      case acc do
        %{^key => value} -> %{acc | key => value + 1}
        %{} -> Map.put(acc, key, 1)
      end
    end)
  end

  @doc """
  Splits the `enumerable` into groups based on `key_fun`.

  The result is a map where each key is given by `key_fun`
  and each value is a list of elements given by `value_fun`.
  The order of elements within each list is preserved from the `enumerable`.
  However, like all maps, the resulting map is unordered.

  ## Examples

      iex> Enum.group_by(~w{ant buffalo cat dingo}, &String.length/1)
      %{3 => ["ant", "cat"], 5 => ["dingo"], 7 => ["buffalo"]}

      iex> Enum.group_by(~w{ant buffalo cat dingo}, &String.length/1, &String.first/1)
      %{3 => ["a", "c"], 5 => ["d"], 7 => ["b"]}

  The key can be any Elixir value. For example, you may use a tuple
  to group by multiple keys:

      iex> collection = [
      ...>   %{id: 1, lang: "Elixir", seq: 1},
      ...>   %{id: 1, lang: "Java", seq: 1},
      ...>   %{id: 1, lang: "Ruby", seq: 2},
      ...>   %{id: 2, lang: "Python", seq: 1},
      ...>   %{id: 2, lang: "C#", seq: 2},
      ...>   %{id: 2, lang: "Haskell", seq: 2},
      ...> ]
      iex> Enum.group_by(collection, &{&1.id, &1.seq})
      %{
        {1, 1} => [%{id: 1, lang: "Elixir", seq: 1}, %{id: 1, lang: "Java", seq: 1}],
        {1, 2} => [%{id: 1, lang: "Ruby", seq: 2}],
        {2, 1} => [%{id: 2, lang: "Python", seq: 1}],
        {2, 2} => [%{id: 2, lang: "C#", seq: 2}, %{id: 2, lang: "Haskell", seq: 2}]
      }
      iex> Enum.group_by(collection, &{&1.id, &1.seq}, &{&1.id, &1.lang})
      %{
        {1, 1} => [{1, "Elixir"}, {1, "Java"}],
        {1, 2} => [{1, "Ruby"}],
        {2, 1} => [{2, "Python"}],
        {2, 2} => [{2, "C#"}, {2, "Haskell"}]
      }

  """
  @spec group_by(t, (element -> any), (element -> any)) :: map
  def group_by(enumerable, key_fun, value_fun \\ fn x -> x end)

  def group_by(enumerable, key_fun, value_fun) when is_function(key_fun) do
    reduce(reverse(enumerable), %{}, fn entry, acc ->
      key = key_fun.(entry)
      value = value_fun.(entry)

      case acc do
        %{^key => existing} -> %{acc | key => [value | existing]}
        %{} -> Map.put(acc, key, [value])
      end
    end)
  end

  def group_by(enumerable, dict, fun) do
    IO.warn(
      "Enum.group_by/3 with a map/dictionary as second element is deprecated. " <>
        "A map is used by default and it is no longer required to pass one to this function"
    )

    # Avoid warnings about Dict
    dict_module = Dict

    reduce(reverse(enumerable), dict, fn entry, categories ->
      dict_module.update(categories, fun.(entry), [entry], &[entry | &1])
    end)
  end

  @doc """
  Intersperses `separator` between each element of the enumeration.

  ## Examples

      iex> Enum.intersperse([1, 2, 3], 0)
      [1, 0, 2, 0, 3]

      iex> Enum.intersperse([1], 0)
      [1]

      iex> Enum.intersperse([], 0)
      []

  """
  @spec intersperse(t, element) :: list
  def intersperse(enumerable, separator) when is_list(enumerable) do
    case enumerable do
      [] -> []
      list -> intersperse_non_empty_list(list, separator)
    end
  end

  def intersperse(enumerable, separator) do
    list =
      enumerable
      |> reduce([], fn x, acc -> [x, separator | acc] end)
      |> :lists.reverse()

    # Head is a superfluous separator
    case list do
      [] -> []
      [_ | t] -> t
    end
  end

  @doc """
  Inserts the given `enumerable` into a `collectable`.

  Note that passing a non-empty list as the `collectable` is deprecated.
  If you're collecting into a non-empty keyword list, consider using
  `Keyword.merge(collectable, Enum.to_list(enumerable))`. If you're collecting
  into a non-empty list, consider something like `Enum.to_list(enumerable) ++ collectable`.

  ## Examples

      iex> Enum.into([1, 2], [])
      [1, 2]

      iex> Enum.into([a: 1, b: 2], %{})
      %{a: 1, b: 2}

      iex> Enum.into(%{a: 1}, %{b: 2})
      %{a: 1, b: 2}

      iex> Enum.into([a: 1, a: 2], %{})
      %{a: 2}

  """
  @spec into(Enumerable.t(), Collectable.t()) :: Collectable.t()
  def into(enumerable, collectable)

  def into(enumerable, []) do
    to_list(enumerable)
  end

  def into(%_{} = enumerable, collectable) do
    into_protocol(enumerable, collectable)
  end

  def into(enumerable, %_{} = collectable) do
    into_protocol(enumerable, collectable)
  end

  def into(enumerable, %{} = collectable) do
    if map_size(collectable) == 0 do
      into_map(enumerable)
    else
      into_map(enumerable, collectable)
    end
  end

  def into(enumerable, collectable) do
    into_protocol(enumerable, collectable)
  end

  defp into_map(%{} = enumerable), do: enumerable
  defp into_map(enumerable) when is_list(enumerable), do: :maps.from_list(enumerable)
  defp into_map(enumerable), do: enumerable |> Enum.to_list() |> :maps.from_list()

  defp into_map(%{} = enumerable, collectable),
    do: Map.merge(collectable, enumerable)

  defp into_map(enumerable, collectable) when is_list(enumerable),
    do: Map.merge(collectable, :maps.from_list(enumerable))

  defp into_map(enumerable, collectable),
    do: Enum.reduce(enumerable, collectable, fn {key, val}, acc -> Map.put(acc, key, val) end)

  defp into_protocol(enumerable, collectable) do
    {initial, fun} = Collectable.into(collectable)

    try do
      reduce_into_protocol(enumerable, initial, fun)
    catch
      kind, reason ->
        fun.(initial, :halt)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      acc -> fun.(acc, :done)
    end
  end

  defp reduce_into_protocol(enumerable, initial, fun) when is_list(enumerable) do
    :lists.foldl(fn x, acc -> fun.(acc, {:cont, x}) end, initial, enumerable)
  end

  defp reduce_into_protocol(enumerable, initial, fun) do
    enumerable
    |> Enumerable.reduce({:cont, initial}, fn x, acc ->
      {:cont, fun.(acc, {:cont, x})}
    end)
    |> elem(1)
  end

  @doc """
  Inserts the given `enumerable` into a `collectable` according to the
  transformation function.

  ## Examples

      iex> Enum.into([1, 2, 3], [], fn x -> x * 3 end)
      [3, 6, 9]

      iex> Enum.into(%{a: 1, b: 2}, %{c: 3}, fn {k, v} -> {k, v * 2} end)
      %{a: 2, b: 4, c: 3}

  """
  @spec into(Enumerable.t(), Collectable.t(), (term -> term)) :: Collectable.t()
  def into(enumerable, [], transform) do
    Enum.map(enumerable, transform)
  end

  def into(%_{} = enumerable, collectable, transform) do
    into_protocol(enumerable, collectable, transform)
  end

  def into(enumerable, %_{} = collectable, transform) do
    into_protocol(enumerable, collectable, transform)
  end

  def into(enumerable, %{} = collectable, transform) do
    if map_size(collectable) == 0 do
      enumerable |> Enum.map(transform) |> :maps.from_list()
    else
      Enum.reduce(enumerable, collectable, fn entry, acc ->
        {key, val} = transform.(entry)
        Map.put(acc, key, val)
      end)
    end
  end

  def into(enumerable, collectable, transform) do
    into_protocol(enumerable, collectable, transform)
  end

  defp into_protocol(enumerable, collectable, transform) do
    {initial, fun} = Collectable.into(collectable)

    try do
      reduce_into_protocol(enumerable, initial, transform, fun)
    catch
      kind, reason ->
        fun.(initial, :halt)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      acc -> fun.(acc, :done)
    end
  end

  defp reduce_into_protocol(enumerable, initial, transform, fun) when is_list(enumerable) do
    :lists.foldl(fn x, acc -> fun.(acc, {:cont, transform.(x)}) end, initial, enumerable)
  end

  defp reduce_into_protocol(enumerable, initial, transform, fun) do
    enumerable
    |> Enumerable.reduce({:cont, initial}, fn x, acc ->
      {:cont, fun.(acc, {:cont, transform.(x)})}
    end)
    |> elem(1)
  end

  @doc """
  Joins the given `enumerable` into a string using `joiner` as a
  separator.

  If `joiner` is not passed at all, it defaults to an empty string.

  All elements in the `enumerable` must be convertible to a string,
  otherwise an error is raised.

  ## Examples

      iex> Enum.join([1, 2, 3])
      "123"

      iex> Enum.join([1, 2, 3], " = ")
      "1 = 2 = 3"

      iex> Enum.join([["a", "b"], ["c", "d", "e", ["f", "g"]], "h", "i"], " ")
      "ab cdefg h i"

  """
  @spec join(t, String.t()) :: String.t()
  def join(enumerable, joiner \\ "")

  def join(enumerable, "") do
    enumerable
    |> map(&entry_to_string(&1))
    |> IO.iodata_to_binary()
  end

  def join(enumerable, joiner) when is_list(enumerable) and is_binary(joiner) do
    join_list(enumerable, joiner)
  end

  def join(enumerable, joiner) when is_binary(joiner) do
    reduced =
      reduce(enumerable, :first, fn
        entry, :first -> entry_to_string(entry)
        entry, acc -> [acc, joiner | entry_to_string(entry)]
      end)

    if reduced == :first do
      ""
    else
      IO.iodata_to_binary(reduced)
    end
  end

  @doc """
  Returns a list where each element is the result of invoking
  `fun` on each corresponding element of `enumerable`.

  For maps, the function expects a key-value tuple.

  ## Examples

      iex> Enum.map([1, 2, 3], fn x -> x * 2 end)
      [2, 4, 6]

      iex> Enum.map([a: 1, b: 2], fn {k, v} -> {k, -v} end)
      [a: -1, b: -2]

  """
  @spec map(t, (element -> any)) :: list
  def map(enumerable, fun)

  def map(enumerable, fun) when is_list(enumerable) do
    :lists.map(fun, enumerable)
  end

  def map(enumerable, fun) do
    reduce(enumerable, [], R.map(fun)) |> :lists.reverse()
  end

  @doc """
  Returns a list of results of invoking `fun` on every `nth`
  element of `enumerable`, starting with the first element.

  The first element is always passed to the given function, unless `nth` is `0`.

  The second argument specifying every `nth` element must be a non-negative
  integer.

  If `nth` is `0`, then `enumerable` is directly converted to a list,
  without `fun` being ever applied.

  ## Examples

      iex> Enum.map_every(1..10, 2, fn x -> x + 1000 end)
      [1001, 2, 1003, 4, 1005, 6, 1007, 8, 1009, 10]

      iex> Enum.map_every(1..10, 3, fn x -> x + 1000 end)
      [1001, 2, 3, 1004, 5, 6, 1007, 8, 9, 1010]

      iex> Enum.map_every(1..5, 0, fn x -> x + 1000 end)
      [1, 2, 3, 4, 5]

      iex> Enum.map_every([1, 2, 3], 1, fn x -> x + 1000 end)
      [1001, 1002, 1003]

  """
  @doc since: "1.4.0"
  @spec map_every(t, non_neg_integer, (element -> any)) :: list
  def map_every(enumerable, nth, fun)

  def map_every(enumerable, 1, fun), do: map(enumerable, fun)
  def map_every(enumerable, 0, _fun), do: to_list(enumerable)
  def map_every([], nth, _fun) when is_integer(nth) and nth > 1, do: []

  def map_every(enumerable, nth, fun) when is_integer(nth) and nth > 1 do
    {res, _} = reduce(enumerable, {[], :first}, R.map_every(nth, fun))
    :lists.reverse(res)
  end

  @doc """
  Maps and intersperses the given enumerable in one pass.

  ## Examples

      iex> Enum.map_intersperse([1, 2, 3], :a, &(&1 * 2))
      [2, :a, 4, :a, 6]
  """
  @doc since: "1.10.0"
  @spec map_intersperse(t, element(), (element -> any())) :: list()
  def map_intersperse(enumerable, separator, mapper)

  def map_intersperse(enumerable, separator, mapper) when is_list(enumerable) do
    map_intersperse_list(enumerable, separator, mapper)
  end

  def map_intersperse(enumerable, separator, mapper) do
    reduced =
      reduce(enumerable, :first, fn
        entry, :first -> [mapper.(entry)]
        entry, acc -> [mapper.(entry), separator | acc]
      end)

    if reduced == :first do
      []
    else
      :lists.reverse(reduced)
    end
  end

  @doc """
  Maps and joins the given `enumerable` in one pass.

  If `joiner` is not passed at all, it defaults to an empty string.

  All elements returned from invoking the `mapper` must be convertible to
  a string, otherwise an error is raised.

  ## Examples

      iex> Enum.map_join([1, 2, 3], &(&1 * 2))
      "246"

      iex> Enum.map_join([1, 2, 3], " = ", &(&1 * 2))
      "2 = 4 = 6"

  """
  @spec map_join(t, String.t(), (element -> String.Chars.t())) :: String.t()
  def map_join(enumerable, joiner \\ "", mapper) when is_binary(joiner) do
    enumerable
    |> map_intersperse(joiner, &entry_to_string(mapper.(&1)))
    |> IO.iodata_to_binary()
  end

  @doc """
  Invokes the given function to each element in the `enumerable` to reduce
  it to a single element, while keeping an accumulator.

  Returns a tuple where the first element is the mapped enumerable and
  the second one is the final accumulator.

  The function, `fun`, receives two arguments: the first one is the
  element, and the second one is the accumulator. `fun` must return
  a tuple with two elements in the form of `{result, accumulator}`.

  For maps, the first tuple element must be a `{key, value}` tuple.

  ## Examples

      iex> Enum.map_reduce([1, 2, 3], 0, fn x, acc -> {x * 2, x + acc} end)
      {[2, 4, 6], 6}

  """
  @spec map_reduce(t, acc, (element, acc -> {element, acc})) :: {list, acc}
  def map_reduce(enumerable, acc, fun) when is_list(enumerable) do
    :lists.mapfoldl(fun, acc, enumerable)
  end

  def map_reduce(enumerable, acc, fun) do
    {list, acc} =
      reduce(enumerable, {[], acc}, fn entry, {list, acc} ->
        {new_entry, acc} = fun.(entry, acc)
        {[new_entry | list], acc}
      end)

    {:lists.reverse(list), acc}
  end

  @doc false
  def max(list = [_ | _]), do: :lists.max(list)

  @doc false
  def max(list = [_ | _], empty_fallback) when is_function(empty_fallback, 0) do
    :lists.max(list)
  end

  @doc false
  @spec max(t, (-> empty_result)) :: element | empty_result when empty_result: any
  def max(enumerable, empty_fallback) when is_function(empty_fallback, 0) do
    max(enumerable, &>=/2, empty_fallback)
  end

  @doc """
  Returns the maximal element in the `enumerable` according
  to Erlang's term ordering.

  By default, the comparison is done with the `>=` sorter function.
  If multiple elements are considered maximal, the first one that
  was found is returned. If you want the last element considered
  maximal to be returned, the sorter function should not return true
  for equal elements.

  If the enumerable is empty, the provided `empty_fallback` is called.
  The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.max([1, 2, 3])
      3

  The fact this function uses Erlang's term ordering means that the comparison
  is structural and not semantic. For example:

      iex> Enum.max([~D[2017-03-31], ~D[2017-04-01]])
      ~D[2017-03-31]

  In the example above, `max/2` returned March 31st instead of April 1st
  because the structural comparison compares the day before the year.
  For this reason, most structs provide a "compare" function, such as
  `Date.compare/2`, which receives two structs and returns `:lt` (less-than),
  `:eq` (equal to), and `:gt` (greater-than). If you pass a module as the
  sorting function, Elixir will automatically use the `compare/2` function
  of said module:

      iex> Enum.max([~D[2017-03-31], ~D[2017-04-01]], Date)
      ~D[2017-04-01]

  Finally, if you don't want to raise on empty enumerables, you can pass
  the empty fallback:

      iex> Enum.max([], &>=/2, fn -> 0 end)
      0

  """
  @spec max(t, (element, element -> boolean) | module()) ::
          element | empty_result
        when empty_result: any
  @spec max(t, (element, element -> boolean) | module(), (-> empty_result)) ::
          element | empty_result
        when empty_result: any
  def max(enumerable, sorter \\ &>=/2, empty_fallback \\ fn -> raise Enum.EmptyError end) do
    aggregate(enumerable, max_sort_fun(sorter), empty_fallback)
  end

  defp max_sort_fun(sorter) when is_function(sorter, 2), do: sorter
  defp max_sort_fun(module) when is_atom(module), do: &(module.compare(&1, &2) != :lt)

  @doc false
  @spec max_by(
          t,
          (element -> any),
          (-> empty_result) | (element, element -> boolean) | module()
        ) :: element | empty_result
        when empty_result: any
  def max_by(enumerable, fun, empty_fallback)
      when is_function(fun, 1) and is_function(empty_fallback, 0) do
    max_by(enumerable, fun, &>=/2, empty_fallback)
  end

  @doc """
  Returns the maximal element in the `enumerable` as calculated
  by the given `fun`.

  By default, the comparison is done with the `>=` sorter function.
  If multiple elements are considered maximal, the first one that
  was found is returned. If you want the last element considered
  maximal to be returned, the sorter function should not return true
  for equal elements.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.max_by(["a", "aa", "aaa"], fn x -> String.length(x) end)
      "aaa"

      iex> Enum.max_by(["a", "aa", "aaa", "b", "bbb"], &String.length/1)
      "aaa"

  The fact this function uses Erlang's term ordering means that the
  comparison is structural and not semantic. Therefore, if you want
  to compare structs, most structs provide a "compare" function, such as
  `Date.compare/2`, which receives two structs and returns `:lt` (less-than),
  `:eq` (equal to), and `:gt` (greater-than). If you pass a module as the
  sorting function, Elixir will automatically use the `compare/2` function
  of said module:

      iex> users = [
      ...>   %{name: "Ellis", birthday: ~D[1943-05-11]},
      ...>   %{name: "Lovelace", birthday: ~D[1815-12-10]},
      ...>   %{name: "Turing", birthday: ~D[1912-06-23]}
      ...> ]
      iex> Enum.max_by(users, &(&1.birthday), Date)
      %{name: "Ellis", birthday: ~D[1943-05-11]}

  Finally, if you don't want to raise on empty enumerables, you can pass
  the empty fallback:

      iex> Enum.max_by([], &String.length/1, fn -> nil end)
      nil

  """
  @spec max_by(
          t,
          (element -> any),
          (element, element -> boolean) | module(),
          (-> empty_result)
        ) :: element | empty_result
        when empty_result: any
  def max_by(enumerable, fun, sorter \\ &>=/2, empty_fallback \\ fn -> raise Enum.EmptyError end)
      when is_function(fun, 1) do
    aggregate_by(enumerable, fun, max_sort_fun(sorter), empty_fallback)
  end

  @doc """
  Checks if `element` exists within the `enumerable`.

  Membership is tested with the match (`===/2`) operator.

  ## Examples

      iex> Enum.member?(1..10, 5)
      true
      iex> Enum.member?(1..10, 5.0)
      false

      iex> Enum.member?([1.0, 2.0, 3.0], 2)
      false
      iex> Enum.member?([1.0, 2.0, 3.0], 2.000)
      true

      iex> Enum.member?([:a, :b, :c], :d)
      false


  When called outside guards, the [`in`](`in/2`) and [`not in`](`in/2`)
  operators work by using this function.
  """
  @spec member?(t, element) :: boolean
  def member?(enumerable, element) when is_list(enumerable) do
    :lists.member(element, enumerable)
  end

  def member?(enumerable, element) do
    case Enumerable.member?(enumerable, element) do
      {:ok, element} when is_boolean(element) ->
        element

      {:error, module} ->
        module.reduce(enumerable, {:cont, false}, fn
          v, _ when v === element -> {:halt, true}
          _, _ -> {:cont, false}
        end)
        |> elem(1)
    end
  end

  @doc false
  def min(list = [_ | _]), do: :lists.min(list)

  @doc false
  def min(list = [_ | _], empty_fallback) when is_function(empty_fallback, 0) do
    :lists.min(list)
  end

  @doc false
  @spec min(t, (-> empty_result)) :: element | empty_result when empty_result: any
  def min(enumerable, empty_fallback) when is_function(empty_fallback, 0) do
    min(enumerable, &<=/2, empty_fallback)
  end

  @doc """
  Returns the minimal element in the `enumerable` according
  to Erlang's term ordering.

  By default, the comparison is done with the `<=` sorter function.
  If multiple elements are considered minimal, the first one that
  was found is returned. If you want the last element considered
  minimal to be returned, the sorter function should not return true
  for equal elements.

  If the enumerable is empty, the provided `empty_fallback` is called.
  The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min([1, 2, 3])
      1

  The fact this function uses Erlang's term ordering means that the comparison
  is structural and not semantic. For example:

      iex> Enum.min([~D[2017-03-31], ~D[2017-04-01]])
      ~D[2017-04-01]

  In the example above, `min/2` returned April 1st instead of March 31st
  because the structural comparison compares the day before the year.
  For this reason, most structs provide a "compare" function, such as
  `Date.compare/2`, which receives two structs and returns `:lt` (less-than),
  `:eq` (equal to), and `:gt` (greater-than). If you pass a module as the
  sorting function, Elixir will automatically use the `compare/2` function
  of said module:

      iex> Enum.min([~D[2017-03-31], ~D[2017-04-01]], Date)
      ~D[2017-03-31]

  Finally, if you don't want to raise on empty enumerables, you can pass
  the empty fallback:

      iex> Enum.min([], fn -> 0 end)
      0

  """
  @spec min(t, (element, element -> boolean) | module()) ::
          element | empty_result
        when empty_result: any
  @spec min(t, (element, element -> boolean) | module(), (-> empty_result)) ::
          element | empty_result
        when empty_result: any
  def min(enumerable, sorter \\ &<=/2, empty_fallback \\ fn -> raise Enum.EmptyError end) do
    aggregate(enumerable, min_sort_fun(sorter), empty_fallback)
  end

  defp min_sort_fun(sorter) when is_function(sorter, 2), do: sorter
  defp min_sort_fun(module) when is_atom(module), do: &(module.compare(&1, &2) != :gt)

  @doc false
  @spec min_by(
          t,
          (element -> any),
          (-> empty_result) | (element, element -> boolean) | module()
        ) :: element | empty_result
        when empty_result: any
  def min_by(enumerable, fun, empty_fallback)
      when is_function(fun, 1) and is_function(empty_fallback, 0) do
    min_by(enumerable, fun, &<=/2, empty_fallback)
  end

  @doc """
  Returns the minimal element in the `enumerable` as calculated
  by the given `fun`.

  By default, the comparison is done with the `<=` sorter function.
  If multiple elements are considered minimal, the first one that
  was found is returned. If you want the last element considered
  minimal to be returned, the sorter function should not return true
  for equal elements.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min_by(["a", "aa", "aaa"], fn x -> String.length(x) end)
      "a"

      iex> Enum.min_by(["a", "aa", "aaa", "b", "bbb"], &String.length/1)
      "a"

  The fact this function uses Erlang's term ordering means that the
  comparison is structural and not semantic. Therefore, if you want
  to compare structs, most structs provide a "compare" function, such as
  `Date.compare/2`, which receives two structs and returns `:lt` (less-than),
  `:eq` (equal to), and `:gt` (greater-than). If you pass a module as the
  sorting function, Elixir will automatically use the `compare/2` function
  of said module:

      iex> users = [
      ...>   %{name: "Ellis", birthday: ~D[1943-05-11]},
      ...>   %{name: "Lovelace", birthday: ~D[1815-12-10]},
      ...>   %{name: "Turing", birthday: ~D[1912-06-23]}
      ...> ]
      iex> Enum.min_by(users, &(&1.birthday), Date)
      %{name: "Lovelace", birthday: ~D[1815-12-10]}

  Finally, if you don't want to raise on empty enumerables, you can pass
  the empty fallback:

      iex> Enum.min_by([], &String.length/1, fn -> nil end)
      nil

  """
  @spec min_by(
          t,
          (element -> any),
          (element, element -> boolean) | module(),
          (-> empty_result)
        ) :: element | empty_result
        when empty_result: any
  def min_by(enumerable, fun, sorter \\ &<=/2, empty_fallback \\ fn -> raise Enum.EmptyError end)
      when is_function(fun, 1) do
    aggregate_by(enumerable, fun, min_sort_fun(sorter), empty_fallback)
  end

  @doc """
  Returns a tuple with the minimal and the maximal elements in the
  enumerable according to Erlang's term ordering.

  If multiple elements are considered maximal or minimal, the first one
  that was found is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min_max([2, 3, 1])
      {1, 3}

      iex> Enum.min_max([], fn -> {nil, nil} end)
      {nil, nil}

  """
  @spec min_max(t, (-> empty_result)) :: {element, element} | empty_result
        when empty_result: any
  def min_max(enumerable, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def min_max(first..last//step = range, empty_fallback) when is_function(empty_fallback, 0) do
    case Range.size(range) do
      0 ->
        empty_fallback.()

      _ ->
        last = last - rem(last - first, step)
        {Kernel.min(first, last), Kernel.max(first, last)}
    end
  end

  def min_max(enumerable, empty_fallback) when is_function(empty_fallback, 0) do
    first_fun = &[&1 | &1]

    reduce_fun = fn entry, [min | max] ->
      [Kernel.min(min, entry) | Kernel.max(max, entry)]
    end

    case reduce_by(enumerable, first_fun, reduce_fun) do
      :empty -> empty_fallback.()
      [min | max] -> {min, max}
    end
  end

  @doc false
  @spec min_max_by(t, (element -> any), (-> empty_result)) :: {element, element} | empty_result
        when empty_result: any
  def min_max_by(enumerable, fun, empty_fallback)
      when is_function(fun, 1) and is_function(empty_fallback, 0) do
    min_max_by(enumerable, fun, &</2, empty_fallback)
  end

  @doc """
  Returns a tuple with the minimal and the maximal elements in the
  enumerable as calculated by the given function.

  If multiple elements are considered maximal or minimal, the first one
  that was found is returned.

  ## Examples

      iex> Enum.min_max_by(["aaa", "bb", "c"], fn x -> String.length(x) end)
      {"c", "aaa"}

      iex> Enum.min_max_by(["aaa", "a", "bb", "c", "ccc"], &String.length/1)
      {"a", "aaa"}

      iex> Enum.min_max_by([], &String.length/1, fn -> {nil, nil} end)
      {nil, nil}

  The fact this function uses Erlang's term ordering means that the
  comparison is structural and not semantic. Therefore, if you want
  to compare structs, most structs provide a "compare" function, such as
  `Date.compare/2`, which receives two structs and returns `:lt` (less-than),
  `:eq` (equal to), and `:gt` (greater-than). If you pass a module as the
  sorting function, Elixir will automatically use the `compare/2` function
  of said module:

      iex> users = [
      ...>   %{name: "Ellis", birthday: ~D[1943-05-11]},
      ...>   %{name: "Lovelace", birthday: ~D[1815-12-10]},
      ...>   %{name: "Turing", birthday: ~D[1912-06-23]}
      ...> ]
      iex> Enum.min_max_by(users, &(&1.birthday), Date)
      {
        %{name: "Lovelace", birthday: ~D[1815-12-10]},
        %{name: "Ellis", birthday: ~D[1943-05-11]}
      }

  Finally, if you don't want to raise on empty enumerables, you can pass
  the empty fallback:

      iex> Enum.min_max_by([], &String.length/1, fn -> nil end)
      nil

  """
  @spec min_max_by(t, (element -> any), (element, element -> boolean) | module()) ::
          {element, element} | empty_result
        when empty_result: any
  @spec min_max_by(
          t,
          (element -> any),
          (element, element -> boolean) | module(),
          (-> empty_result)
        ) :: {element, element} | empty_result
        when empty_result: any
  def min_max_by(
        enumerable,
        fun,
        sorter_or_empty_fallback \\ &</2,
        empty_fallback \\ fn -> raise Enum.EmptyError end
      )

  def min_max_by(enumerable, fun, sorter, empty_fallback)
      when is_function(fun, 1) and is_atom(sorter) and is_function(empty_fallback, 0) do
    min_max_by(enumerable, fun, min_max_by_sort_fun(sorter), empty_fallback)
  end

  def min_max_by(enumerable, fun, sorter, empty_fallback)
      when is_function(fun, 1) and is_function(sorter, 2) and is_function(empty_fallback, 0) do
    first_fun = fn entry ->
      fun_entry = fun.(entry)
      {entry, entry, fun_entry, fun_entry}
    end

    reduce_fun = fn entry, {prev_min, prev_max, fun_min, fun_max} = acc ->
      fun_entry = fun.(entry)

      cond do
        sorter.(fun_entry, fun_min) ->
          {entry, prev_max, fun_entry, fun_max}

        sorter.(fun_max, fun_entry) ->
          {prev_min, entry, fun_min, fun_entry}

        true ->
          acc
      end
    end

    case reduce_by(enumerable, first_fun, reduce_fun) do
      :empty -> empty_fallback.()
      {min, max, _, _} -> {min, max}
    end
  end

  defp min_max_by_sort_fun(module) when is_atom(module), do: &(module.compare(&1, &2) == :lt)

  @doc """
  Splits the `enumerable` in two lists according to the given function `fun`.

  Splits the given `enumerable` in two lists by calling `fun` with each element
  in the `enumerable` as its only argument. Returns a tuple with the first list
  containing all the elements in `enumerable` for which applying `fun` returned
  a truthy value, and a second list with all the elements for which applying
  `fun` returned a falsy value (`false` or `nil`).

  The elements in both the returned lists are in the same relative order as they
  were in the original enumerable (if such enumerable was ordered, like a
  list). See the examples below.

  ## Examples

      iex> Enum.split_with([5, 4, 3, 2, 1, 0], fn x -> rem(x, 2) == 0 end)
      {[4, 2, 0], [5, 3, 1]}

      iex> Enum.split_with([a: 1, b: -2, c: 1, d: -3], fn {_k, v} -> v < 0 end)
      {[b: -2, d: -3], [a: 1, c: 1]}

      iex> Enum.split_with([a: 1, b: -2, c: 1, d: -3], fn {_k, v} -> v > 50 end)
      {[], [a: 1, b: -2, c: 1, d: -3]}

      iex> Enum.split_with([], fn {_k, v} -> v > 50 end)
      {[], []}

  """
  @doc since: "1.4.0"
  @spec split_with(t, (element -> as_boolean(term))) :: {list, list}
  def split_with(enumerable, fun) do
    {acc1, acc2} =
      reduce(enumerable, {[], []}, fn entry, {acc1, acc2} ->
        if fun.(entry) do
          {[entry | acc1], acc2}
        else
          {acc1, [entry | acc2]}
        end
      end)

    {:lists.reverse(acc1), :lists.reverse(acc2)}
  end

  @doc false
  @deprecated "Use Enum.split_with/2 instead"
  def partition(enumerable, fun) do
    split_with(enumerable, fun)
  end

  @doc """
  Returns a random element of an `enumerable`.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  This function uses Erlang's [`:rand` module](`:rand`) to calculate
  the random value. Check its documentation for setting a
  different random algorithm or a different seed.

  The implementation is based on the
  [reservoir sampling](https://en.wikipedia.org/wiki/Reservoir_sampling#Relation_to_Fisher-Yates_shuffle)
  algorithm.
  It assumes that the sample being returned can fit into memory;
  the input `enumerable` doesn't have to, as it is traversed just once.

  If a range is passed into the function, this function will pick a
  random value between the range limits, without traversing the whole
  range (thus executing in constant time and constant memory).

  ## Examples

  The examples below use the `:exsss` pseudorandom algorithm since it's
  the default from Erlang/OTP 22:

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsss, {100, 101, 102})
      iex> Enum.random([1, 2, 3])
      2
      iex> Enum.random([1, 2, 3])
      1
      iex> Enum.random(1..1_000)
      309

  """
  @spec random(t) :: element
  def random(enumerable)

  def random(enumerable) when is_list(enumerable) do
    case length(enumerable) do
      0 -> raise Enum.EmptyError
      length -> enumerable |> drop_list(random_integer(0, length - 1)) |> hd()
    end
  end

  def random(enumerable) do
    result =
      case Enumerable.slice(enumerable) do
        {:ok, 0, _} ->
          []

        {:ok, count, fun} when is_function(fun, 1) ->
          slice_list(fun.(enumerable), random_integer(0, count - 1), 1, 1)

        # TODO: Deprecate me in Elixir v1.18.
        {:ok, count, fun} when is_function(fun, 2) ->
          fun.(random_integer(0, count - 1), 1)

        {:ok, count, fun} when is_function(fun, 3) ->
          fun.(random_integer(0, count - 1), 1, 1)

        {:error, _} ->
          take_random(enumerable, 1)
      end

    case result do
      [] -> raise Enum.EmptyError
      [elem] -> elem
    end
  end

  @doc """
  Invokes `fun` for each element in the `enumerable` with the
  accumulator.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  The first element of the `enumerable` is used as the initial value
  of the accumulator. Then, the function is invoked with the next
  element and the accumulator. The result returned by the function
  is used as the accumulator for the next iteration, recursively.
  When the `enumerable` is done, the last accumulator is returned.

  Since the first element of the enumerable is used as the initial
  value of the accumulator, `fun` will only be executed `n - 1` times
  where `n` is the length of the enumerable. This function won't call
  the specified function for enumerables that are one-element long.

  If you wish to use another value for the accumulator, use
  `Enum.reduce/3`.

  ## Examples

      iex> Enum.reduce([1, 2, 3, 4], fn x, acc -> x * acc end)
      24

  """
  @spec reduce(t, (element, acc -> acc)) :: acc
  def reduce(enumerable, fun)

  def reduce([h | t], fun) do
    reduce(t, h, fun)
  end

  def reduce([], _fun) do
    raise Enum.EmptyError
  end

  def reduce(enumerable, fun) do
    Enumerable.reduce(enumerable, {:cont, :first}, fn
      x, {:acc, acc} -> {:cont, {:acc, fun.(x, acc)}}
      x, :first -> {:cont, {:acc, x}}
    end)
    |> elem(1)
    |> case do
      :first -> raise Enum.EmptyError
      {:acc, acc} -> acc
    end
  end

  @doc """
  Invokes `fun` for each element in the `enumerable` with the accumulator.

  The initial value of the accumulator is `acc`. The function is invoked for
  each element in the enumerable with the accumulator. The result returned
  by the function is used as the accumulator for the next iteration.
  The function returns the last accumulator.

  ## Examples

      iex> Enum.reduce([1, 2, 3], 0, fn x, acc -> x + acc end)
      6

  ## Reduce as a building block

  Reduce (sometimes called `fold`) is a basic building block in functional
  programming. Almost all of the functions in the `Enum` module can be
  implemented on top of reduce. Those functions often rely on other operations,
  such as `Enum.reverse/1`, which are optimized by the runtime.

  For example, we could implement `map/2` in terms of `reduce/3` as follows:

      def my_map(enumerable, fun) do
        enumerable
        |> Enum.reduce([], fn x, acc -> [fun.(x) | acc] end)
        |> Enum.reverse()
      end

  In the example above, `Enum.reduce/3` accumulates the result of each call
  to `fun` into a list in reverse order, which is correctly ordered at the
  end by calling `Enum.reverse/1`.

  Implementing functions like `map/2`, `filter/2` and others are a good
  exercise for understanding the power behind `Enum.reduce/3`. When an
  operation cannot be expressed by any of the functions in the `Enum`
  module, developers will most likely resort to `reduce/3`.
  """
  @spec reduce(t, acc, (element, acc -> acc)) :: acc
  def reduce(enumerable, acc, fun) when is_list(enumerable) do
    :lists.foldl(fun, acc, enumerable)
  end

  def reduce(first..last//step, acc, fun) do
    reduce_range(first, last, step, acc, fun)
  end

  def reduce(%_{} = enumerable, acc, fun) do
    reduce_enumerable(enumerable, acc, fun)
  end

  def reduce(%{} = enumerable, acc, fun) do
    :maps.fold(fn k, v, acc -> fun.({k, v}, acc) end, acc, enumerable)
  end

  def reduce(enumerable, acc, fun) do
    reduce_enumerable(enumerable, acc, fun)
  end

  @doc """
  Reduces `enumerable` until `fun` returns `{:halt, term}`.

  The return value for `fun` is expected to be

    * `{:cont, acc}` to continue the reduction with `acc` as the new
      accumulator or
    * `{:halt, acc}` to halt the reduction

  If `fun` returns `{:halt, acc}` the reduction is halted and the function
  returns `acc`. Otherwise, if the enumerable is exhausted, the function returns
  the accumulator of the last `{:cont, acc}`.

  ## Examples

      iex> Enum.reduce_while(1..100, 0, fn x, acc ->
      ...>   if x < 5 do
      ...>     {:cont, acc + x}
      ...>   else
      ...>     {:halt, acc}
      ...>   end
      ...> end)
      10
      iex> Enum.reduce_while(1..100, 0, fn x, acc ->
      ...>   if x > 0 do
      ...>     {:cont, acc + x}
      ...>   else
      ...>     {:halt, acc}
      ...>   end
      ...> end)
      5050

  """
  @spec reduce_while(t, any, (element, any -> {:cont, any} | {:halt, any})) :: any
  def reduce_while(enumerable, acc, fun) do
    Enumerable.reduce(enumerable, {:cont, acc}, fun) |> elem(1)
  end

  @doc """
  Returns a list of elements in `enumerable` excluding those for which the function `fun` returns
  a truthy value.

  See also `filter/2`.

  ## Examples

      iex> Enum.reject([1, 2, 3], fn x -> rem(x, 2) == 0 end)
      [1, 3]

  """
  @spec reject(t, (element -> as_boolean(term))) :: list
  def reject(enumerable, fun) when is_list(enumerable) do
    reject_list(enumerable, fun)
  end

  def reject(enumerable, fun) do
    reduce(enumerable, [], R.reject(fun)) |> :lists.reverse()
  end

  @doc """
  Returns a list of elements in `enumerable` in reverse order.

  ## Examples

      iex> Enum.reverse([1, 2, 3])
      [3, 2, 1]

  """
  @spec reverse(t) :: list
  def reverse(enumerable)

  def reverse([]), do: []
  def reverse([_] = list), do: list
  def reverse([element1, element2]), do: [element2, element1]
  def reverse([element1, element2 | rest]), do: :lists.reverse(rest, [element2, element1])
  def reverse(enumerable), do: reduce(enumerable, [], &[&1 | &2])

  @doc """
  Reverses the elements in `enumerable`, appends the `tail`, and returns
  it as a list.

  This is an optimization for
  `enumerable |> Enum.reverse() |> Enum.concat(tail)`.

  ## Examples

      iex> Enum.reverse([1, 2, 3], [4, 5, 6])
      [3, 2, 1, 4, 5, 6]

  """
  @spec reverse(t, t) :: list
  def reverse(enumerable, tail) when is_list(enumerable) do
    :lists.reverse(enumerable, to_list(tail))
  end

  def reverse(enumerable, tail) do
    reduce(enumerable, to_list(tail), fn entry, acc ->
      [entry | acc]
    end)
  end

  @doc """
  Reverses the `enumerable` in the range from initial `start_index`
  through `count` elements.

  If `count` is greater than the size of the rest of the `enumerable`,
  then this function will reverse the rest of the enumerable.

  ## Examples

      iex> Enum.reverse_slice([1, 2, 3, 4, 5, 6], 2, 4)
      [1, 2, 6, 5, 4, 3]

  """
  @spec reverse_slice(t, non_neg_integer, non_neg_integer) :: list
  def reverse_slice(enumerable, start_index, count)
      when is_integer(start_index) and start_index >= 0 and is_integer(count) and count >= 0 do
    list = reverse(enumerable)
    length = length(list)
    count = Kernel.min(count, length - start_index)

    if count > 0 do
      reverse_slice(list, length, start_index + count, count, [])
    else
      :lists.reverse(list)
    end
  end

  @doc """
  Slides a single or multiple elements given by `range_or_single_index` from `enumerable`
  to `insertion_index`.

  The semantics of the range to be moved match the semantics of `Enum.slice/2`.
  Specifically, that means:

   * Indices are normalized, meaning that negative indexes will be counted from the end
      (for example, -1 means the last element of the enumerable). This will result in *two*
      traversals of your enumerable on types like lists that don't provide a constant-time count.

    * If the normalized index range's `last` is out of bounds, the range is truncated to the last element.

    * If the normalized index range's `first` is out of bounds, the selected range for sliding
      will be empty, so you'll get back your input list.

    * Decreasing ranges (such as `5..0//1`) also select an empty range to be moved,
      so you'll get back your input list.

    * Ranges with any step but 1 will raise an error.

  ## Examples

      # Slide a single element
      iex> Enum.slide([:a, :b, :c, :d, :e, :f, :g], 5, 1)
      [:a, :f, :b, :c, :d, :e, :g]

      # Slide a range of elements backward
      iex> Enum.slide([:a, :b, :c, :d, :e, :f, :g], 3..5, 1)
      [:a, :d, :e, :f, :b, :c, :g]

      # Slide a range of elements forward
      iex> Enum.slide([:a, :b, :c, :d, :e, :f, :g], 1..3, 5)
      [:a, :e, :f, :b, :c, :d, :g]

      # Slide with negative indices (counting from the end)
      iex> Enum.slide([:a, :b, :c, :d, :e, :f, :g], 3..-1//1, 2)
      [:a, :b, :d, :e, :f, :g, :c]
      iex> Enum.slide([:a, :b, :c, :d, :e, :f, :g], -4..-2, 1)
      [:a, :d, :e, :f, :b, :c, :g]

      # Insert at negative indices (counting from the end)
      iex> Enum.slide([:a, :b, :c, :d, :e, :f, :g], 3, -1)
      [:a, :b, :c, :e, :f, :g, :d]

  """
  @doc since: "1.13.0"
  @spec slide(t, Range.t() | index, index) :: list
  def slide(enumerable, range_or_single_index, insertion_index)

  def slide(enumerable, single_index, insertion_index) when is_integer(single_index) do
    slide(enumerable, single_index..single_index, insertion_index)
  end

  # This matches the behavior of Enum.slice/2
  def slide(_, _.._//step = index_range, _insertion_index) when step != 1 do
    raise ArgumentError,
          "Enum.slide/3 does not accept ranges with custom steps, got: #{inspect(index_range)}"
  end

  # Normalize negative input ranges like Enum.slice/2
  def slide(enumerable, first..last, insertion_index)
      when first < 0 or last < 0 or insertion_index < 0 do
    count = Enum.count(enumerable)
    normalized_first = if first >= 0, do: first, else: Kernel.max(first + count, 0)
    normalized_last = if last >= 0, do: last, else: last + count

    normalized_insertion_index =
      if insertion_index >= 0, do: insertion_index, else: insertion_index + count

    if normalized_first < count and normalized_first != normalized_insertion_index do
      normalized_range = normalized_first..normalized_last//1
      slide(enumerable, normalized_range, normalized_insertion_index)
    else
      Enum.to_list(enumerable)
    end
  end

  def slide(enumerable, insertion_index.._, insertion_index) do
    Enum.to_list(enumerable)
  end

  def slide(_, first..last, insertion_index)
      when insertion_index > first and insertion_index <= last do
    raise ArgumentError,
          "insertion index for slide must be outside the range being moved " <>
            "(tried to insert #{first}..#{last} at #{insertion_index})"
  end

  def slide(enumerable, first..last, _insertion_index) when first > last do
    Enum.to_list(enumerable)
  end

  # Guarantees at this point: step size == 1 and first <= last and (insertion_index < first or insertion_index > last)
  def slide(enumerable, first..last, insertion_index) do
    impl = if is_list(enumerable), do: &slide_list_start/4, else: &slide_any/4

    cond do
      insertion_index <= first -> impl.(enumerable, insertion_index, first, last)
      insertion_index > last -> impl.(enumerable, first, last + 1, insertion_index)
    end
  end

  # Takes the range from middle..last and moves it to be in front of index start
  defp slide_any(enumerable, start, middle, last) do
    # We're going to deal with 4 "chunks" of the enumerable:
    # 0. "Head," before the start index
    # 1. "Slide back," between start (inclusive) and middle (exclusive)
    # 2. "Slide front," between middle (inclusive) and last (inclusive)
    # 3. "Tail," after last
    #
    # But, we're going to accumulate these into only two lists: pre and post.
    # We'll reverse-accumulate the head into our pre list, then "slide back" into post,
    # then "slide front" into pre, then "tail" into post.
    #
    # Then at the end, we're going to reassemble and reverse them, and end up with the
    # chunks in the correct order.
    {_size, pre, post} =
      Enum.reduce(enumerable, {0, [], []}, fn item, {index, pre, post} ->
        {pre, post} =
          cond do
            index < start -> {[item | pre], post}
            index >= start and index < middle -> {pre, [item | post]}
            index >= middle and index <= last -> {[item | pre], post}
            true -> {pre, [item | post]}
          end

        {index + 1, pre, post}
      end)

    :lists.reverse(pre, :lists.reverse(post))
  end

  # Like slide_any/4 above, this optimized implementation of slide for lists depends
  # on the indices being sorted such that we're moving middle..last to be in front of start.
  defp slide_list_start([h | t], start, middle, last)
       when start > 0 and start <= middle and middle <= last do
    [h | slide_list_start(t, start - 1, middle - 1, last - 1)]
  end

  defp slide_list_start(list, 0, middle, last), do: slide_list_middle(list, middle, last, [])
  defp slide_list_start([], _start, _middle, _last), do: []

  defp slide_list_middle([h | t], middle, last, acc) when middle > 0 do
    slide_list_middle(t, middle - 1, last - 1, [h | acc])
  end

  defp slide_list_middle(list, 0, last, start_to_middle) do
    {slid_range, tail} = slide_list_last(list, last + 1, [])
    slid_range ++ :lists.reverse(start_to_middle, tail)
  end

  # You asked for a middle index off the end of the list... you get what we've got
  defp slide_list_middle([], _, _, acc) do
    :lists.reverse(acc)
  end

  defp slide_list_last([h | t], last, acc) when last > 0 do
    slide_list_last(t, last - 1, [h | acc])
  end

  defp slide_list_last(rest, 0, acc) do
    {:lists.reverse(acc), rest}
  end

  defp slide_list_last([], _, acc) do
    {:lists.reverse(acc), []}
  end

  @doc """
  Applies the given function to each element in the `enumerable`,
  storing the result in a list and passing it as the accumulator
  for the next computation. Uses the first element in the `enumerable`
  as the starting value.

  ## Examples

      iex> Enum.scan(1..5, &(&1 + &2))
      [1, 3, 6, 10, 15]

  """
  @spec scan(t, (element, any -> any)) :: list
  def scan(enumerable, fun)

  def scan([], _fun), do: []

  def scan([elem | rest], fun) do
    scanned = scan_list(rest, elem, fun)
    [elem | scanned]
  end

  def scan(enumerable, fun) do
    {res, _} = reduce(enumerable, {[], :first}, R.scan2(fun))
    :lists.reverse(res)
  end

  @doc """
  Applies the given function to each element in the `enumerable`,
  storing the result in a list and passing it as the accumulator
  for the next computation. Uses the given `acc` as the starting value.

  ## Examples

      iex> Enum.scan(1..5, 0, &(&1 + &2))
      [1, 3, 6, 10, 15]

  """
  @spec scan(t, any, (element, any -> any)) :: list
  def scan(enumerable, acc, fun) when is_list(enumerable) do
    scan_list(enumerable, acc, fun)
  end

  def scan(enumerable, acc, fun) do
    {res, _} = reduce(enumerable, {[], acc}, R.scan3(fun))
    :lists.reverse(res)
  end

  @doc """
  Returns a list with the elements of `enumerable` shuffled.

  This function uses Erlang's [`:rand` module](`:rand`) to calculate
  the random value. Check its documentation for setting a
  different random algorithm or a different seed.

  ## Examples

  The examples below use the `:exsss` pseudorandom algorithm since it's
  the default from Erlang/OTP 22:

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsss, {1, 2, 3})
      iex> Enum.shuffle([1, 2, 3])
      [3, 2, 1]
      iex> Enum.shuffle([1, 2, 3])
      [2, 1, 3]

  """
  @spec shuffle(t) :: list
  def shuffle(enumerable) do
    randomized =
      reduce(enumerable, [], fn x, acc ->
        [{:rand.uniform(), x} | acc]
      end)

    shuffle_unwrap(:lists.keysort(1, randomized), [])
  end

  @doc """
  Returns a subset list of the given `enumerable` by `index_range`.

  `index_range` must be a `Range`. Given an `enumerable`, it drops
  elements before `index_range.first` (zero-base), then it takes elements
  until element `index_range.last` (inclusively).

  Indexes are normalized, meaning that negative indexes will be counted
  from the end (for example, `-1` means the last element of the `enumerable`).

  If `index_range.last` is out of bounds, then it is assigned as the index
  of the last element.

  If the normalized `index_range.first` is out of bounds of the given
  `enumerable`, or this one is greater than the normalized `index_range.last`,
  then `[]` is returned.

  If a step `n` (other than `1`) is used in `index_range`, then it takes
  every `n`th element from `index_range.first` to `index_range.last`
  (according to the same rules described above).

  ## Examples

      iex> Enum.slice([1, 2, 3, 4, 5], 1..3)
      [2, 3, 4]

      iex> Enum.slice([1, 2, 3, 4, 5], 3..10)
      [4, 5]

      # Last three elements (negative indexes)
      iex> Enum.slice([1, 2, 3, 4, 5], -3..-1)
      [3, 4, 5]

  For ranges where `start > stop`, you need to explicit
  mark them as increasing:

      iex> Enum.slice([1, 2, 3, 4, 5], 1..-2//1)
      [2, 3, 4]

  The step can be any positive number. For example, to
  get every 2 elements of the collection:

      iex> Enum.slice([1, 2, 3, 4, 5], 0..-1//2)
      [1, 3, 5]

  To get every third element of the first ten elements:

      iex> integers = Enum.to_list(1..20)
      iex> Enum.slice(integers, 0..9//3)
      [1, 4, 7, 10]

  If the first position is after the end of the enumerable
  or after the last position of the range, it returns an
  empty list:

      iex> Enum.slice([1, 2, 3, 4, 5], 6..10)
      []

      # first is greater than last
      iex> Enum.slice([1, 2, 3, 4, 5], 6..5)
      []

  """
  @doc since: "1.6.0"
  @spec slice(t, Range.t()) :: list
  def slice(enumerable, first..last//step = index_range) do
    # TODO: Deprecate negative steps on Elixir v1.16
    # TODO: Support negative steps as a reverse on Elixir v2.0.
    cond do
      step > 0 ->
        slice_range(enumerable, first, last, step)

      step == -1 and first > last ->
        slice_range(enumerable, first, last, 1)

      true ->
        raise ArgumentError,
              "Enum.slice/2 does not accept ranges with negative steps, got: #{inspect(index_range)}"
    end
  end

  # TODO: Remove me on v2.0
  def slice(enumerable, %{__struct__: Range, first: first, last: last} = index_range) do
    step = if first <= last, do: 1, else: -1
    slice(enumerable, Map.put(index_range, :step, step))
  end

  defp slice_range(enumerable, first, -1, step) when first >= 0 do
    if step == 1 do
      drop(enumerable, first)
    else
      enumerable |> drop(first) |> take_every_list(step - 1)
    end
  end

  defp slice_range(enumerable, first, last, step)
       when last >= first and last >= 0 and first >= 0 do
    slice_forward(enumerable, first, last - first + 1, step)
  end

  defp slice_range(enumerable, first, last, step) do
    {count, fun} = slice_count_and_fun(enumerable, step)
    first = if first >= 0, do: first, else: Kernel.max(first + count, 0)
    last = if last >= 0, do: last, else: last + count
    amount = last - first + 1

    if first < count and amount > 0 do
      amount = Kernel.min(amount, count - first)
      amount = if step == 1, do: amount, else: div(amount - 1, step) + 1
      fun.(first, amount, step)
    else
      []
    end
  end

  @doc """
  Returns a subset list of the given `enumerable`, from `start_index` (zero-based)
  with `amount` number of elements if available.

  Given an `enumerable`, it drops elements right before element `start_index`;
  then, it takes `amount` of elements, returning as many elements as possible if
  there are not enough elements.

  A negative `start_index` can be passed, which means the `enumerable` is
  enumerated once and the index is counted from the end (for example,
  `-1` starts slicing from the last element).

  It returns `[]` if `amount` is `0` or if `start_index` is out of bounds.

  ## Examples

      iex> Enum.slice(1..100, 5, 10)
      [6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

      # amount to take is greater than the number of elements
      iex> Enum.slice(1..10, 5, 100)
      [6, 7, 8, 9, 10]

      iex> Enum.slice(1..10, 5, 0)
      []

      # using a negative start index
      iex> Enum.slice(1..10, -6, 3)
      [5, 6, 7]
      iex> Enum.slice(1..10, -11, 5)
      [1, 2, 3, 4, 5]

      # out of bound start index
      iex> Enum.slice(1..10, 10, 5)
      []

  """
  @spec slice(t, index, non_neg_integer) :: list
  def slice(_enumerable, start_index, 0) when is_integer(start_index), do: []

  def slice(enumerable, start_index, amount)
      when is_integer(start_index) and start_index < 0 and is_integer(amount) and amount >= 0 do
    {count, fun} = slice_count_and_fun(enumerable, 1)
    start_index = Kernel.max(count + start_index, 0)
    amount = Kernel.min(amount, count - start_index)

    if amount > 0 do
      fun.(start_index, amount, 1)
    else
      []
    end
  end

  def slice(enumerable, start_index, amount)
      when is_integer(start_index) and is_integer(amount) and amount >= 0 do
    slice_forward(enumerable, start_index, amount, 1)
  end

  @doc """
  Sorts the `enumerable` according to Erlang's term ordering.

  This function uses the merge sort algorithm. Do not use this
  function to sort structs, see `sort/2` for more information.

  ## Examples

      iex> Enum.sort([3, 2, 1])
      [1, 2, 3]

  """
  @spec sort(t) :: list
  def sort(enumerable) when is_list(enumerable) do
    :lists.sort(enumerable)
  end

  def sort(enumerable) do
    sort(enumerable, &(&1 <= &2))
  end

  @doc """
  Sorts the `enumerable` by the given function.

  This function uses the merge sort algorithm. The given function should compare
  two arguments, and return `true` if the first argument precedes or is in the
  same place as the second one.

  ## Examples

      iex> Enum.sort([1, 2, 3], &(&1 >= &2))
      [3, 2, 1]

  The sorting algorithm will be stable as long as the given function
  returns `true` for values considered equal:

      iex> Enum.sort(["some", "kind", "of", "monster"], &(byte_size(&1) <= byte_size(&2)))
      ["of", "some", "kind", "monster"]

  If the function does not return `true` for equal values, the sorting
  is not stable and the order of equal terms may be shuffled.
  For example:

      iex> Enum.sort(["some", "kind", "of", "monster"], &(byte_size(&1) < byte_size(&2)))
      ["of", "kind", "some", "monster"]

  ## Ascending and descending (since v1.10.0)

  `sort/2` allows a developer to pass `:asc` or `:desc` as the sorter, which is a convenience for
  [`&<=/2`](`<=/2`) and [`&>=/2`](`>=/2`) respectively.

      iex> Enum.sort([2, 3, 1], :asc)
      [1, 2, 3]
      iex> Enum.sort([2, 3, 1], :desc)
      [3, 2, 1]

  ## Sorting structs

  Do not use `</2`, `<=/2`, `>/2`, `>=/2` and friends when sorting structs.
  That's because the built-in operators above perform structural comparison
  and not a semantic one. Imagine we sort the following list of dates:

      iex> dates = [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]
      iex> Enum.sort(dates)
      [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]

  Note that the returned result is incorrect, because `sort/1` by default uses
  `<=/2`, which will compare their structure. When comparing structures, the
  fields are compared in alphabetical order, which means the dates above will
  be compared by `day`, `month` and then `year`, which is the opposite of what
  we want.

  For this reason, most structs provide a "compare" function, such as
  `Date.compare/2`, which receives two structs and returns `:lt` (less-than),
  `:eq` (equal to), and `:gt` (greater-than). If you pass a module as the
  sorting function, Elixir will automatically use the `compare/2` function
  of said module:

      iex> dates = [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]
      iex> Enum.sort(dates, Date)
      [~D[2019-01-01], ~D[2019-06-06], ~D[2020-03-02]]

  To retrieve all dates in descending order, you can wrap the module in
  a tuple with `:asc` or `:desc` as first element:

      iex> dates = [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]
      iex> Enum.sort(dates, {:asc, Date})
      [~D[2019-01-01], ~D[2019-06-06], ~D[2020-03-02]]
      iex> dates = [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]
      iex> Enum.sort(dates, {:desc, Date})
      [~D[2020-03-02], ~D[2019-06-06], ~D[2019-01-01]]

  """
  @spec sort(
          t,
          (element, element -> boolean) | :asc | :desc | module() | {:asc | :desc, module()}
        ) :: list
  def sort(enumerable, sorter) when is_list(enumerable) do
    case sorter do
      :asc -> :lists.sort(enumerable)
      :desc -> :lists.sort(enumerable) |> :lists.reverse()
      _ -> :lists.sort(to_sort_fun(sorter), enumerable)
    end
  end

  def sort(enumerable, sorter) do
    fun = to_sort_fun(sorter)

    reduce(enumerable, [], &sort_reducer(&1, &2, fun))
    |> sort_terminator(fun)
  end

  defp to_sort_fun(sorter) when is_function(sorter, 2), do: sorter
  defp to_sort_fun(:asc), do: &<=/2
  defp to_sort_fun(:desc), do: &>=/2
  defp to_sort_fun(module) when is_atom(module), do: &(module.compare(&1, &2) != :gt)
  defp to_sort_fun({:asc, module}) when is_atom(module), do: &(module.compare(&1, &2) != :gt)
  defp to_sort_fun({:desc, module}) when is_atom(module), do: &(module.compare(&1, &2) != :lt)

  @doc """
  Sorts the mapped results of the `enumerable` according to the provided `sorter`
  function.

  This function maps each element of the `enumerable` using the
  provided `mapper` function. The enumerable is then sorted by
  the mapped elements using the `sorter`, which defaults to `:asc`
  and sorts the elements ascendingly.

  `sort_by/3` differs from `sort/2` in that it only calculates the
  comparison value for each element in the enumerable once instead of
  once for each element in each comparison. If the same function is
  being called on both elements, it's more efficient to use `sort_by/3`.

  ## Ascending and descending (since v1.10.0)

  `sort_by/3` allows a developer to pass `:asc` or `:desc` as the sorter,
  which is a convenience for [`&<=/2`](`<=/2`) and [`&>=/2`](`>=/2`) respectively:
      iex> Enum.sort_by([2, 3, 1], &(&1), :asc)
      [1, 2, 3]

      iex> Enum.sort_by([2, 3, 1], &(&1), :desc)
      [3, 2, 1]

  ## Examples

  Using the default `sorter` of `:asc` :

      iex> Enum.sort_by(["some", "kind", "of", "monster"], &byte_size/1)
      ["of", "some", "kind", "monster"]

  Sorting by multiple properties - first by size, then by first letter
  (this takes advantage of the fact that tuples are compared element-by-element):

      iex> Enum.sort_by(["some", "kind", "of", "monster"], &{byte_size(&1), String.first(&1)})
      ["of", "kind", "some", "monster"]

  Similar to `sort/2`, you can pass a custom sorter:

      iex> Enum.sort_by(["some", "kind", "of", "monster"], &byte_size/1, :desc)
      ["monster", "some", "kind", "of"]

  As in `sort/2`, avoid using the default sorting function to sort
  structs, as by default it performs structural comparison instead of
  a semantic one. In such cases, you shall pass a sorting function as
  third element or any module that implements a `compare/2` function.
  For example, to sort users by their birthday in both ascending and
  descending order respectively:

      iex> users = [
      ...>   %{name: "Ellis", birthday: ~D[1943-05-11]},
      ...>   %{name: "Lovelace", birthday: ~D[1815-12-10]},
      ...>   %{name: "Turing", birthday: ~D[1912-06-23]}
      ...> ]
      iex> Enum.sort_by(users, &(&1.birthday), Date)
      [
        %{name: "Lovelace", birthday: ~D[1815-12-10]},
        %{name: "Turing", birthday: ~D[1912-06-23]},
        %{name: "Ellis", birthday: ~D[1943-05-11]}
      ]
      iex> Enum.sort_by(users, &(&1.birthday), {:desc, Date})
      [
        %{name: "Ellis", birthday: ~D[1943-05-11]},
        %{name: "Turing", birthday: ~D[1912-06-23]},
        %{name: "Lovelace", birthday: ~D[1815-12-10]}
      ]

  ## Performance characteristics

  As detailed in the initial section, `sort_by/3` calculates the comparison
  value for each element in the enumerable once instead of once for each
  element in each comparison. This implies `sort_by/3` must do an initial
  pass on the data to compute those values.

  However, if those values are cheap to compute, for example, you have
  already extracted the field you want to sort by into a tuple, then those
  extra passes become overhead. In such cases, consider using `List.keysort/3`
  instead.

  Let's see an example. Imagine you have a list of products and you have a
  list of IDs. You want to keep all products that are in the given IDs and
  return their names sorted by their price. You could write it like this:

      for(
        product <- products,
        product.id in ids,
        do: product
      )
      |> Enum.sort_by(& &1.price)
      |> Enum.map(& &1.name)

  However, you could also write it like this:

      for(
        product <- products,
        product.id in ids,
        do: {product.name, product.price}
      )
      |> List.keysort(1)
      |> Enum.map(&elem(&1, 0))

  Using `List.keysort/3` will be a better choice for performance sensitive
  code as it avoids additional traversals.
  """
  @spec sort_by(
          t,
          (element -> mapped_element),
          (element, element -> boolean) | :asc | :desc | module() | {:asc | :desc, module()}
        ) ::
          list
        when mapped_element: element
  def sort_by(enumerable, mapper, sorter \\ :asc)

  def sort_by(enumerable, mapper, :desc) when is_function(mapper, 1) do
    enumerable
    |> Enum.reduce([], &[{&1, mapper.(&1)} | &2])
    |> List.keysort(1, :asc)
    |> List.foldl([], &[elem(&1, 0) | &2])
  end

  def sort_by(enumerable, mapper, sorter) when is_function(mapper, 1) do
    enumerable
    |> map(&{&1, mapper.(&1)})
    |> List.keysort(1, sorter)
    |> map(&elem(&1, 0))
  end

  @doc """
  Splits the `enumerable` into two enumerables, leaving `count`
  elements in the first one.

  If `count` is a negative number, it starts counting from the
  back to the beginning of the `enumerable`.

  Be aware that a negative `count` implies the `enumerable`
  will be enumerated twice: once to calculate the position, and
  a second time to do the actual splitting.

  ## Examples

      iex> Enum.split([1, 2, 3], 2)
      {[1, 2], [3]}

      iex> Enum.split([1, 2, 3], 10)
      {[1, 2, 3], []}

      iex> Enum.split([1, 2, 3], 0)
      {[], [1, 2, 3]}

      iex> Enum.split([1, 2, 3], -1)
      {[1, 2], [3]}

      iex> Enum.split([1, 2, 3], -5)
      {[], [1, 2, 3]}

  """
  @spec split(t, integer) :: {list, list}
  def split(enumerable, count) when is_list(enumerable) and is_integer(count) and count >= 0 do
    split_list(enumerable, count, [])
  end

  def split(enumerable, count) when is_integer(count) and count >= 0 do
    {_, list1, list2} =
      reduce(enumerable, {count, [], []}, fn entry, {counter, acc1, acc2} ->
        if counter > 0 do
          {counter - 1, [entry | acc1], acc2}
        else
          {counter, acc1, [entry | acc2]}
        end
      end)

    {:lists.reverse(list1), :lists.reverse(list2)}
  end

  def split(enumerable, count) when is_integer(count) and count < 0 do
    split_reverse_list(reverse(enumerable), -count, [])
  end

  @doc """
  Splits enumerable in two at the position of the element for which
  `fun` returns a falsy value (`false` or `nil`) for the first time.

  It returns a two-element tuple with two lists of elements.
  The element that triggered the split is part of the second list.

  ## Examples

      iex> Enum.split_while([1, 2, 3, 4], fn x -> x < 3 end)
      {[1, 2], [3, 4]}

      iex> Enum.split_while([1, 2, 3, 4], fn x -> x < 0 end)
      {[], [1, 2, 3, 4]}

      iex> Enum.split_while([1, 2, 3, 4], fn x -> x > 0 end)
      {[1, 2, 3, 4], []}

  """
  @spec split_while(t, (element -> as_boolean(term))) :: {list, list}
  def split_while(enumerable, fun) when is_list(enumerable) do
    split_while_list(enumerable, fun, [])
  end

  def split_while(enumerable, fun) do
    {list1, list2} =
      reduce(enumerable, {[], []}, fn
        entry, {acc1, []} ->
          if(fun.(entry), do: {[entry | acc1], []}, else: {acc1, [entry]})

        entry, {acc1, acc2} ->
          {acc1, [entry | acc2]}
      end)

    {:lists.reverse(list1), :lists.reverse(list2)}
  end

  @doc """
  Returns the sum of all elements.

  Raises `ArithmeticError` if `enumerable` contains a non-numeric value.

  ## Examples

      iex> Enum.sum([1, 2, 3])
      6

      iex> Enum.sum(1..10)
      55

      iex> Enum.sum(1..10//2)
      25

  """
  @spec sum(t) :: number
  def sum(enumerable)

  def sum(first..last//step = range) do
    range
    |> Range.size()
    |> Kernel.*(first + last - rem(last - first, step))
    |> div(2)
  end

  def sum(enumerable) do
    reduce(enumerable, 0, &+/2)
  end

  @doc """
  Returns the product of all elements.

  Raises `ArithmeticError` if `enumerable` contains a non-numeric value.

  ## Examples

      iex> Enum.product([])
      1
      iex> Enum.product([2, 3, 4])
      24
      iex> Enum.product([2.0, 3.0, 4.0])
      24.0

  """
  @doc since: "1.12.0"
  @spec product(t) :: number
  def product(enumerable) do
    reduce(enumerable, 1, &*/2)
  end

  @doc """
  Takes an `amount` of elements from the beginning or the end of the `enumerable`.

  If a positive `amount` is given, it takes the `amount` elements from the
  beginning of the `enumerable`.

  If a negative `amount` is given, the `amount` of elements will be taken from the end.
  The `enumerable` will be enumerated once to retrieve the proper index and
  the remaining calculation is performed from the end.

  If amount is `0`, it returns `[]`.

  ## Examples

      iex> Enum.take([1, 2, 3], 2)
      [1, 2]

      iex> Enum.take([1, 2, 3], 10)
      [1, 2, 3]

      iex> Enum.take([1, 2, 3], 0)
      []

      iex> Enum.take([1, 2, 3], -1)
      [3]

  """
  @spec take(t, integer) :: list
  def take(enumerable, amount)

  def take(_enumerable, 0), do: []

  def take(enumerable, amount)
      when is_list(enumerable) and is_integer(amount) and amount > 0 do
    take_list(enumerable, amount)
  end

  def take(enumerable, amount) when is_integer(amount) and amount > 0 do
    {_, {res, _}} =
      Enumerable.reduce(enumerable, {:cont, {[], amount}}, fn entry, {list, n} ->
        case n do
          1 -> {:halt, {[entry | list], n - 1}}
          _ -> {:cont, {[entry | list], n - 1}}
        end
      end)

    :lists.reverse(res)
  end

  def take(enumerable, amount) when is_integer(amount) and amount < 0 do
    {count, fun} = slice_count_and_fun(enumerable, 1)
    first = Kernel.max(amount + count, 0)
    fun.(first, count - first, 1)
  end

  @doc """
  Returns a list of every `nth` element in the `enumerable`,
  starting with the first element.

  The first element is always included, unless `nth` is 0.

  The second argument specifying every `nth` element must be a non-negative
  integer.

  ## Examples

      iex> Enum.take_every(1..10, 2)
      [1, 3, 5, 7, 9]

      iex> Enum.take_every(1..10, 0)
      []

      iex> Enum.take_every([1, 2, 3], 1)
      [1, 2, 3]

  """
  @spec take_every(t, non_neg_integer) :: list
  def take_every(enumerable, nth)

  def take_every(_enumerable, 0), do: []
  def take_every(enumerable, 1), do: to_list(enumerable)

  def take_every(list, nth) when is_list(list) and is_integer(nth) and nth > 1 do
    take_every_list(list, nth - 1)
  end

  def take_every(enumerable, nth) when is_integer(nth) and nth > 1 do
    {res, _} = reduce(enumerable, {[], :first}, R.take_every(nth))
    :lists.reverse(res)
  end

  @doc """
  Takes `count` random elements from `enumerable`.

  Note that this function will traverse the whole `enumerable` to
  get the random sublist.

  See `random/1` for notes on implementation and random seed.

  ## Examples

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsss, {1, 2, 3})
      iex> Enum.take_random(1..10, 2)
      [3, 1]
      iex> Enum.take_random(?a..?z, 5)
      'mikel'

  """
  @spec take_random(t, non_neg_integer) :: list
  def take_random(enumerable, count)
  def take_random(_enumerable, 0), do: []

  def take_random([], _), do: []
  def take_random([h | t], 1), do: take_random_list_one(t, h, 1)

  def take_random(enumerable, 1) do
    enumerable
    |> reduce([], fn
      x, [current | index] ->
        if :rand.uniform(index + 1) == 1 do
          [x | index + 1]
        else
          [current | index + 1]
        end

      x, [] ->
        [x | 1]
    end)
    |> case do
      [] -> []
      [current | _index] -> [current]
    end
  end

  def take_random(enumerable, count) when is_integer(count) and count in 0..128 do
    sample = Tuple.duplicate(nil, count)

    reducer = fn elem, {idx, sample} ->
      jdx = random_integer(0, idx)

      cond do
        idx < count ->
          value = elem(sample, jdx)
          {idx + 1, put_elem(sample, idx, value) |> put_elem(jdx, elem)}

        jdx < count ->
          {idx + 1, put_elem(sample, jdx, elem)}

        true ->
          {idx + 1, sample}
      end
    end

    {size, sample} = reduce(enumerable, {0, sample}, reducer)
    sample |> Tuple.to_list() |> take(Kernel.min(count, size))
  end

  def take_random(enumerable, count) when is_integer(count) and count >= 0 do
    reducer = fn elem, {idx, sample} ->
      jdx = random_integer(0, idx)

      cond do
        idx < count ->
          value = Map.get(sample, jdx)
          {idx + 1, Map.put(sample, idx, value) |> Map.put(jdx, elem)}

        jdx < count ->
          {idx + 1, Map.put(sample, jdx, elem)}

        true ->
          {idx + 1, sample}
      end
    end

    {size, sample} = reduce(enumerable, {0, %{}}, reducer)
    take_random(sample, Kernel.min(count, size), [])
  end

  defp take_random(_sample, 0, acc), do: acc

  defp take_random(sample, position, acc) do
    position = position - 1
    take_random(sample, position, [Map.get(sample, position) | acc])
  end

  defp take_random_list_one([h | t], current, index) do
    if :rand.uniform(index + 1) == 1 do
      take_random_list_one(t, h, index + 1)
    else
      take_random_list_one(t, current, index + 1)
    end
  end

  defp take_random_list_one([], current, _), do: [current]

  @doc """
  Takes the elements from the beginning of the `enumerable` while `fun` returns
  a truthy value.

  ## Examples

      iex> Enum.take_while([1, 2, 3], fn x -> x < 3 end)
      [1, 2]

  """
  @spec take_while(t, (element -> as_boolean(term))) :: list
  def take_while(enumerable, fun) when is_list(enumerable) do
    take_while_list(enumerable, fun)
  end

  def take_while(enumerable, fun) do
    {_, res} =
      Enumerable.reduce(enumerable, {:cont, []}, fn entry, acc ->
        if fun.(entry) do
          {:cont, [entry | acc]}
        else
          {:halt, acc}
        end
      end)

    :lists.reverse(res)
  end

  @doc """
  Converts `enumerable` to a list.

  ## Examples

      iex> Enum.to_list(1..3)
      [1, 2, 3]

  """
  @spec to_list(t) :: [element]
  def to_list(enumerable) when is_list(enumerable), do: enumerable
  def to_list(%{__struct__: Range} = range), do: Range.to_list(range)
  def to_list(%_{} = enumerable), do: reverse(enumerable) |> :lists.reverse()
  def to_list(%{} = enumerable), do: Map.to_list(enumerable)
  def to_list(enumerable), do: reverse(enumerable) |> :lists.reverse()

  @doc """
  Enumerates the `enumerable`, removing all duplicated elements.

  ## Examples

      iex> Enum.uniq([1, 2, 3, 3, 2, 1])
      [1, 2, 3]

  """
  @spec uniq(t) :: list
  def uniq(enumerable) do
    uniq_by(enumerable, fn x -> x end)
  end

  @doc false
  @deprecated "Use Enum.uniq_by/2 instead"
  def uniq(enumerable, fun) do
    uniq_by(enumerable, fun)
  end

  @doc """
  Enumerates the `enumerable`, by removing the elements for which
  function `fun` returned duplicate elements.

  The function `fun` maps every element to a term. Two elements are
  considered duplicates if the return value of `fun` is equal for
  both of them.

  The first occurrence of each element is kept.

  ## Example

      iex> Enum.uniq_by([{1, :x}, {2, :y}, {1, :z}], fn {x, _} -> x end)
      [{1, :x}, {2, :y}]

      iex> Enum.uniq_by([a: {:tea, 2}, b: {:tea, 2}, c: {:coffee, 1}], fn {_, y} -> y end)
      [a: {:tea, 2}, c: {:coffee, 1}]

  """
  @spec uniq_by(t, (element -> term)) :: list

  def uniq_by(enumerable, fun) when is_list(enumerable) do
    uniq_list(enumerable, %{}, fun)
  end

  def uniq_by(enumerable, fun) do
    {list, _} = reduce(enumerable, {[], %{}}, R.uniq_by(fun))
    :lists.reverse(list)
  end

  @doc """
  Opposite of `zip/2`. Extracts two-element tuples from the
  given `enumerable` and groups them together.

  It takes an `enumerable` with elements being two-element tuples and returns
  a tuple with two lists, each of which is formed by the first and
  second element of each tuple, respectively.

  This function fails unless `enumerable` is or can be converted into a
  list of tuples with *exactly* two elements in each tuple.

  ## Examples

      iex> Enum.unzip([{:a, 1}, {:b, 2}, {:c, 3}])
      {[:a, :b, :c], [1, 2, 3]}

      iex> Enum.unzip(%{a: 1, b: 2})
      {[:a, :b], [1, 2]}

  """
  @spec unzip(t) :: {[element], [element]}

  def unzip([_ | _] = list) do
    :lists.reverse(list) |> unzip([], [])
  end

  def unzip([]) do
    {[], []}
  end

  def unzip(enumerable) do
    {list1, list2} =
      reduce(enumerable, {[], []}, fn {el1, el2}, {list1, list2} ->
        {[el1 | list1], [el2 | list2]}
      end)

    {:lists.reverse(list1), :lists.reverse(list2)}
  end

  defp unzip([{el1, el2} | reversed_list], list1, list2) do
    unzip(reversed_list, [el1 | list1], [el2 | list2])
  end

  defp unzip([], list1, list2) do
    {list1, list2}
  end

  @doc """
  Returns the `enumerable` with each element wrapped in a tuple
  alongside its index.

  May receive a function or an integer offset.

  If an `offset` is given, it will index from the given offset instead of from
  zero.

  If a `function` is given, it will index by invoking the function for each
  element and index (zero-based) of the enumerable.

  ## Examples

      iex> Enum.with_index([:a, :b, :c])
      [a: 0, b: 1, c: 2]

      iex> Enum.with_index([:a, :b, :c], 3)
      [a: 3, b: 4, c: 5]

      iex> Enum.with_index([:a, :b, :c], fn element, index -> {index, element} end)
      [{0, :a}, {1, :b}, {2, :c}]

  """
  @spec with_index(t, integer) :: [{term, integer}]
  @spec with_index(t, (element, index -> value)) :: [value] when value: any
  def with_index(enumerable, fun_or_offset \\ 0)

  def with_index(enumerable, offset) when is_list(enumerable) and is_integer(offset) do
    with_index_list(enumerable, offset)
  end

  def with_index(enumerable, fun) when is_list(enumerable) and is_function(fun, 2) do
    with_index_list(enumerable, 0, fun)
  end

  def with_index(enumerable, offset) when is_integer(offset) do
    enumerable
    |> map_reduce(offset, fn x, i -> {{x, i}, i + 1} end)
    |> elem(0)
  end

  def with_index(enumerable, fun) when is_function(fun, 2) do
    enumerable
    |> map_reduce(0, fn x, i -> {fun.(x, i), i + 1} end)
    |> elem(0)
  end

  @doc """
  Zips corresponding elements from two enumerables into a list
  of tuples.

  The zipping finishes as soon as either enumerable completes.

  ## Examples

      iex> Enum.zip([1, 2, 3], [:a, :b, :c])
      [{1, :a}, {2, :b}, {3, :c}]

      iex> Enum.zip([1, 2, 3, 4, 5], [:a, :b, :c])
      [{1, :a}, {2, :b}, {3, :c}]

  """
  @spec zip(t, t) :: [{any, any}]
  def zip(enumerable1, enumerable2) when is_list(enumerable1) and is_list(enumerable2) do
    zip_list(enumerable1, enumerable2, [])
  end

  def zip(enumerable1, enumerable2) do
    zip([enumerable1, enumerable2])
  end

  @doc """
  Zips corresponding elements from a finite collection of enumerables
  into a list of tuples.

  The zipping finishes as soon as any enumerable in the given collection completes.

  ## Examples

      iex> Enum.zip([[1, 2, 3], [:a, :b, :c], ["foo", "bar", "baz"]])
      [{1, :a, "foo"}, {2, :b, "bar"}, {3, :c, "baz"}]

      iex> Enum.zip([[1, 2, 3, 4, 5], [:a, :b, :c]])
      [{1, :a}, {2, :b}, {3, :c}]

  """
  @doc since: "1.4.0"
  @spec zip(enumerables) :: [tuple()] when enumerables: [t()] | t()
  def zip([]), do: []

  def zip(enumerables) do
    zip_reduce(enumerables, [], &[List.to_tuple(&1) | &2])
    |> :lists.reverse()
  end

  @doc """
  Zips corresponding elements from two enumerables into a list, transforming them with
  the `zip_fun` function as it goes.

  The corresponding elements from each collection are passed to the provided two-arity `zip_fun`
  function in turn. Returns a list that contains the result of calling `zip_fun` for each pair of
  elements.

  The zipping finishes as soon as either enumerable runs out of elements.

  ## Zipping Maps

  It's important to remember that zipping inherently relies on order.
  If you zip two lists you get the element at the index from each list in turn.
  If we zip two maps together it's tempting to think that you will get the given
  key in the left map and the matching key in the right map, but there is no such
  guarantee because map keys are not ordered! Consider the following:

      left =  %{:a => 1, 1 => 3}
      right = %{:a => 1, :b => :c}
      Enum.zip(left, right)
      # [{{1, 3}, {:a, 1}}, {{:a, 1}, {:b, :c}}]

  As you can see `:a` does not get paired with `:a`. If this is what you want,
  you should use `Map.merge/3`.

  ## Examples

      iex> Enum.zip_with([1, 2], [3, 4], fn x, y -> x + y end)
      [4, 6]

      iex> Enum.zip_with([1, 2], [3, 4, 5, 6], fn x, y -> x + y end)
      [4, 6]

      iex> Enum.zip_with([1, 2, 5, 6], [3, 4], fn x, y -> x + y end)
      [4, 6]

  """
  @doc since: "1.12.0"
  @spec zip_with(t, t, (enum1_elem :: term, enum2_elem :: term -> term)) :: [term]
  def zip_with(enumerable1, enumerable2, zip_fun)
      when is_list(enumerable1) and is_list(enumerable2) and is_function(zip_fun, 2) do
    zip_with_list(enumerable1, enumerable2, zip_fun)
  end

  def zip_with(enumerable1, enumerable2, zip_fun) when is_function(zip_fun, 2) do
    zip_reduce(enumerable1, enumerable2, [], fn l, r, acc -> [zip_fun.(l, r) | acc] end)
    |> :lists.reverse()
  end

  @doc """
  Zips corresponding elements from a finite collection of enumerables
  into list, transforming them with the `zip_fun` function as it goes.

  The first element from each of the enums in `enumerables` will be put
  into a list which is then passed to the one-arity `zip_fun` function.
  Then, the second elements from each of the enums are put into a list
  and passed to `zip_fun`, and so on until any one of the enums in
  `enumerables` runs out of elements.

  Returns a list with all the results of calling `zip_fun`.

  ## Examples

      iex> Enum.zip_with([[1, 2], [3, 4], [5, 6]], fn [x, y, z] -> x + y + z end)
      [9, 12]

      iex> Enum.zip_with([[1, 2], [3, 4]], fn [x, y] -> x + y end)
      [4, 6]

  """
  @doc since: "1.12.0"
  @spec zip_with(t, ([term] -> term)) :: [term]
  def zip_with([], _fun), do: []

  def zip_with(enumerables, zip_fun) do
    zip_reduce(enumerables, [], fn values, acc -> [zip_fun.(values) | acc] end)
    |> :lists.reverse()
  end

  @doc """
  Reduces over two enumerables halting as soon as either enumerable is empty.

  In practice, the behaviour provided by this function can be achieved with:

      Enum.reduce(Stream.zip(left, right), acc, reducer)

  But `zip_reduce/4` exists for convenience purposes.

  ## Examples

      iex> Enum.zip_reduce([1, 2], [3, 4], 0, fn x, y, acc -> x + y + acc end)
      10

      iex> Enum.zip_reduce([1, 2], [3, 4], [], fn x, y, acc -> [x + y | acc] end)
      [6, 4]
  """
  @doc since: "1.12.0"
  @spec zip_reduce(t, t, acc, (enum1_elem :: term, enum2_elem :: term, acc -> acc)) :: acc
        when acc: term
  def zip_reduce(left, right, acc, reducer)
      when is_list(left) and is_list(right) and is_function(reducer, 3) do
    zip_reduce_list(left, right, acc, reducer)
  end

  def zip_reduce(left, right, acc, reducer) when is_function(reducer, 3) do
    reduce = fn [l, r], acc -> {:cont, reducer.(l, r, acc)} end
    Stream.zip_with([left, right], & &1).({:cont, acc}, reduce) |> elem(1)
  end

  @doc """
  Reduces over all of the given enumerables, halting as soon as any enumerable is
  empty.

  The reducer will receive 2 args: a list of elements (one from each enum) and the
  accumulator.

  In practice, the behaviour provided by this function can be achieved with:

      Enum.reduce(Stream.zip(enums), acc, reducer)

  But `zip_reduce/3` exists for convenience purposes.

  ## Examples

      iex> enums = [[1, 1], [2, 2], [3, 3]]
      ...>  Enum.zip_reduce(enums, [], fn elements, acc ->
      ...>    [List.to_tuple(elements) | acc]
      ...> end)
      [{1, 2, 3}, {1, 2, 3}]

      iex> enums = [[1, 2], %{a: 3, b: 4}, [5, 6]]
      ...> Enum.zip_reduce(enums, [], fn elements, acc ->
      ...>   [List.to_tuple(elements) | acc]
      ...> end)
      [{2, {:b, 4}, 6}, {1, {:a, 3}, 5}]
  """
  @doc since: "1.12.0"
  @spec zip_reduce(t, acc, ([term], acc -> acc)) :: acc when acc: term
  def zip_reduce([], acc, reducer) when is_function(reducer, 2), do: acc

  def zip_reduce(enums, acc, reducer) when is_function(reducer, 2) do
    Stream.zip_with(enums, & &1).({:cont, acc}, &{:cont, reducer.(&1, &2)}) |> elem(1)
  end

  ## Helpers

  @compile {:inline, entry_to_string: 1, reduce: 3, reduce_by: 3, reduce_enumerable: 3}

  defp entry_to_string(entry) when is_binary(entry), do: entry
  defp entry_to_string(entry), do: String.Chars.to_string(entry)

  defp aggregate([head | tail], fun, _empty) do
    aggregate_list(tail, head, fun)
  end

  defp aggregate([], _fun, empty) do
    empty.()
  end

  defp aggregate(first..last//step = range, fun, empty) do
    case Range.size(range) do
      0 ->
        empty.()

      _ ->
        last = last - rem(last - first, step)

        case fun.(first, last) do
          true -> first
          false -> last
        end
    end
  end

  defp aggregate(enumerable, fun, empty) do
    ref = make_ref()

    enumerable
    |> reduce(ref, fn
      element, ^ref ->
        element

      element, acc ->
        case fun.(acc, element) do
          true -> acc
          false -> element
        end
    end)
    |> case do
      ^ref -> empty.()
      result -> result
    end
  end

  defp aggregate_list([head | tail], acc, fun) do
    acc =
      case fun.(acc, head) do
        true -> acc
        false -> head
      end

    aggregate_list(tail, acc, fun)
  end

  defp aggregate_list([], acc, _fun), do: acc

  defp aggregate_by(enumerable, fun, sorter, empty_fallback) do
    first_fun = &[&1 | fun.(&1)]

    reduce_fun = fn entry, [_ | fun_ref] = old ->
      fun_entry = fun.(entry)

      case sorter.(fun_ref, fun_entry) do
        true -> old
        false -> [entry | fun_entry]
      end
    end

    case reduce_by(enumerable, first_fun, reduce_fun) do
      :empty -> empty_fallback.()
      [entry | _] -> entry
    end
  end

  defp reduce_by([head | tail], first, fun) do
    :lists.foldl(fun, first.(head), tail)
  end

  defp reduce_by([], _first, _fun) do
    :empty
  end

  defp reduce_by(enumerable, first, fun) do
    reduce(enumerable, :empty, fn
      element, :empty -> first.(element)
      element, acc -> fun.(element, acc)
    end)
  end

  defp random_integer(limit, limit) when is_integer(limit) do
    limit
  end

  defp random_integer(lower_limit, upper_limit) when upper_limit < lower_limit do
    random_integer(upper_limit, lower_limit)
  end

  defp random_integer(lower_limit, upper_limit) do
    lower_limit + :rand.uniform(upper_limit - lower_limit + 1) - 1
  end

  ## Implementations

  ## all?

  defp all_list([h | t]) do
    if h do
      all_list(t)
    else
      false
    end
  end

  defp all_list([]) do
    true
  end

  defp all_list([h | t], fun) do
    if fun.(h) do
      all_list(t, fun)
    else
      false
    end
  end

  defp all_list([], _) do
    true
  end

  ## any?

  defp any_list([h | t]) do
    if h do
      true
    else
      any_list(t)
    end
  end

  defp any_list([]) do
    false
  end

  defp any_list([h | t], fun) do
    if fun.(h) do
      true
    else
      any_list(t, fun)
    end
  end

  defp any_list([], _) do
    false
  end

  ## concat

  defp concat_list([h | t]) when is_list(h), do: h ++ concat_list(t)
  defp concat_list([h | t]), do: concat_enum([h | t])
  defp concat_list([]), do: []

  defp concat_enum(enum) do
    fun = &[&1 | &2]
    enum |> reduce([], &reduce(&1, &2, fun)) |> :lists.reverse()
  end

  # dedup

  defp dedup_list([value | tail], acc) do
    acc =
      case acc do
        [^value | _] -> acc
        _ -> [value | acc]
      end

    dedup_list(tail, acc)
  end

  defp dedup_list([], acc) do
    acc
  end

  ## drop

  defp drop_list(list, 0), do: list
  defp drop_list([_ | tail], counter), do: drop_list(tail, counter - 1)
  defp drop_list([], _), do: []

  ## drop_while

  defp drop_while_list([head | tail], fun) do
    if fun.(head) do
      drop_while_list(tail, fun)
    else
      [head | tail]
    end
  end

  defp drop_while_list([], _) do
    []
  end

  ## filter

  defp filter_list([head | tail], fun) do
    if fun.(head) do
      [head | filter_list(tail, fun)]
    else
      filter_list(tail, fun)
    end
  end

  defp filter_list([], _fun) do
    []
  end

  ## find

  defp find_list([head | tail], default, fun) do
    if fun.(head) do
      head
    else
      find_list(tail, default, fun)
    end
  end

  defp find_list([], default, _) do
    default
  end

  ## find_index

  defp find_index_list([head | tail], counter, fun) do
    if fun.(head) do
      counter
    else
      find_index_list(tail, counter + 1, fun)
    end
  end

  defp find_index_list([], _, _) do
    nil
  end

  ## find_value

  defp find_value_list([head | tail], default, fun) do
    fun.(head) || find_value_list(tail, default, fun)
  end

  defp find_value_list([], default, _) do
    default
  end

  ## flat_map

  defp flat_map_list([head | tail], fun) do
    case fun.(head) do
      list when is_list(list) -> list ++ flat_map_list(tail, fun)
      other -> to_list(other) ++ flat_map_list(tail, fun)
    end
  end

  defp flat_map_list([], _fun) do
    []
  end

  ## intersperse

  defp intersperse_non_empty_list([head], _separator), do: [head]

  defp intersperse_non_empty_list([head | rest], separator) do
    [head, separator | intersperse_non_empty_list(rest, separator)]
  end

  ## join

  defp join_list([], _joiner), do: ""

  defp join_list(list, joiner) do
    join_non_empty_list(list, joiner, [])
    |> :lists.reverse()
    |> IO.iodata_to_binary()
  end

  defp join_non_empty_list([first], _joiner, acc), do: [entry_to_string(first) | acc]

  defp join_non_empty_list([first | rest], joiner, acc) do
    join_non_empty_list(rest, joiner, [joiner, entry_to_string(first) | acc])
  end

  ## map_intersperse

  defp map_intersperse_list([], _, _),
    do: []

  defp map_intersperse_list([last], _, mapper),
    do: [mapper.(last)]

  defp map_intersperse_list([head | rest], separator, mapper),
    do: [mapper.(head), separator | map_intersperse_list(rest, separator, mapper)]

  ## reduce

  defp reduce_range(first, last, step, acc, fun)
       when step > 0 and first <= last
       when step < 0 and first >= last do
    reduce_range(first + step, last, step, fun.(first, acc), fun)
  end

  defp reduce_range(_first, _last, _step, acc, _fun) do
    acc
  end

  defp reduce_enumerable(enumerable, acc, fun) do
    Enumerable.reduce(enumerable, {:cont, acc}, fn x, acc -> {:cont, fun.(x, acc)} end) |> elem(1)
  end

  ## reject

  defp reject_list([head | tail], fun) do
    if fun.(head) do
      reject_list(tail, fun)
    else
      [head | reject_list(tail, fun)]
    end
  end

  defp reject_list([], _fun) do
    []
  end

  ## reverse_slice

  defp reverse_slice(rest, idx, idx, count, acc) do
    {slice, rest} = head_slice(rest, count, [])
    :lists.reverse(rest, :lists.reverse(slice, acc))
  end

  defp reverse_slice([elem | rest], idx, start, count, acc) do
    reverse_slice(rest, idx - 1, start, count, [elem | acc])
  end

  defp head_slice(rest, 0, acc), do: {acc, rest}

  defp head_slice([elem | rest], count, acc) do
    head_slice(rest, count - 1, [elem | acc])
  end

  ## scan

  defp scan_list([], _acc, _fun), do: []

  defp scan_list([elem | rest], acc, fun) do
    acc = fun.(elem, acc)
    [acc | scan_list(rest, acc, fun)]
  end

  ## shuffle

  defp shuffle_unwrap([{_, h} | enumerable], t) do
    shuffle_unwrap(enumerable, [h | t])
  end

  defp shuffle_unwrap([], t), do: t

  ## slice

  defp slice_forward(enumerable, start, amount, step) when start < 0 do
    {count, fun} = slice_count_and_fun(enumerable, step)
    start = count + start

    if start >= 0 do
      amount = Kernel.min(amount, count - start)
      amount = if step == 1, do: amount, else: div(amount - 1, step) + 1
      fun.(start, amount, step)
    else
      []
    end
  end

  defp slice_forward(list, start, amount, step) when is_list(list) do
    amount = if step == 1, do: amount, else: div(amount - 1, step) + 1
    slice_list(list, start, amount, step)
  end

  defp slice_forward(enumerable, start, amount, step) do
    case Enumerable.slice(enumerable) do
      {:ok, count, _} when start >= count ->
        []

      {:ok, count, fun} when is_function(fun, 1) ->
        amount = Kernel.min(amount, count - start)
        enumerable |> fun.() |> slice_exact(start, amount, step, count)

      # TODO: Deprecate me in Elixir v1.18.
      {:ok, count, fun} when is_function(fun, 2) ->
        amount = Kernel.min(amount, count - start)

        if step == 1 do
          fun.(start, amount)
        else
          fun.(start, Kernel.min(amount * step, count - start))
          |> take_every_list(amount, step - 1)
        end

      {:ok, count, fun} when is_function(fun, 3) ->
        amount = Kernel.min(amount, count - start)
        amount = if step == 1, do: amount, else: div(amount - 1, step) + 1
        fun.(start, amount, step)

      {:error, module} ->
        slice_enum(enumerable, module, start, amount, step)
    end
  end

  defp slice_list(list, start, amount, step) do
    if step == 1 do
      list |> drop_list(start) |> take_list(amount)
    else
      list |> drop_list(start) |> take_every_list(amount, step - 1)
    end
  end

  defp slice_enum(enumerable, module, start, amount, 1) do
    {_, {_, _, slice}} =
      module.reduce(enumerable, {:cont, {start, amount, []}}, fn
        _entry, {start, amount, _list} when start > 0 ->
          {:cont, {start - 1, amount, []}}

        entry, {start, amount, list} when amount > 1 ->
          {:cont, {start, amount - 1, [entry | list]}}

        entry, {start, amount, list} ->
          {:halt, {start, amount, [entry | list]}}
      end)

    :lists.reverse(slice)
  end

  defp slice_enum(enumerable, module, start, amount, step) do
    {_, {_, _, _, slice}} =
      module.reduce(enumerable, {:cont, {start, amount, 1, []}}, fn
        _entry, {start, amount, to_drop, _list} when start > 0 ->
          {:cont, {start - 1, amount, to_drop, []}}

        entry, {start, amount, to_drop, list} when amount > 1 ->
          case to_drop do
            1 -> {:cont, {start, amount - 1, step, [entry | list]}}
            _ -> {:cont, {start, amount - 1, to_drop - 1, list}}
          end

        entry, {start, amount, to_drop, list} ->
          case to_drop do
            1 -> {:halt, {start, amount, to_drop, [entry | list]}}
            _ -> {:halt, {start, amount, to_drop, list}}
          end
      end)

    :lists.reverse(slice)
  end

  defp slice_count_and_fun(list, _step) when is_list(list) do
    length = length(list)
    {length, &slice_exact(list, &1, &2, &3, length)}
  end

  defp slice_count_and_fun(enumerable, step) do
    case Enumerable.slice(enumerable) do
      {:ok, count, fun} when is_function(fun, 3) ->
        {count, fun}

      # TODO: Deprecate me in Elixir v1.18.
      {:ok, count, fun} when is_function(fun, 2) ->
        if step == 1 do
          {count, fn start, amount, 1 -> fun.(start, amount) end}
        else
          {count,
           fn start, amount, step ->
             fun.(start, Kernel.min(amount * step, count - start))
             |> take_every_list(amount, step - 1)
           end}
        end

      {:ok, count, fun} when is_function(fun, 1) ->
        {count, &slice_exact(fun.(enumerable), &1, &2, &3, count)}

      {:error, module} ->
        {list, count} =
          enumerable
          |> module.reduce({:cont, {[], 0}}, fn elem, {acc, count} ->
            {:cont, {[elem | acc], count + 1}}
          end)
          |> elem(1)

        {count,
         fn start, amount, step ->
           list |> :lists.reverse() |> slice_exact(start, amount, step, count)
         end}
    end
  end

  # Slice a list when we know the bounds
  defp slice_exact(_list, _start, 0, _step, _), do: []

  defp slice_exact(list, start, amount, 1, size) when start + amount == size,
    do: list |> drop_exact(start)

  defp slice_exact(list, start, amount, 1, _),
    do: list |> drop_exact(start) |> take_exact(amount)

  defp slice_exact(list, start, amount, step, _),
    do: list |> drop_exact(start) |> take_every_list(amount, step - 1)

  defp drop_exact(list, 0), do: list
  defp drop_exact([_ | tail], amount), do: drop_exact(tail, amount - 1)

  defp take_exact(_list, 0), do: []
  defp take_exact([head | tail], amount), do: [head | take_exact(tail, amount - 1)]

  ## sort

  defp sort_reducer(entry, {:split, y, x, r, rs, bool}, fun) do
    cond do
      fun.(y, entry) == bool ->
        {:split, entry, y, [x | r], rs, bool}

      fun.(x, entry) == bool ->
        {:split, y, entry, [x | r], rs, bool}

      r == [] ->
        {:split, y, x, [entry], rs, bool}

      true ->
        {:pivot, y, x, r, rs, entry, bool}
    end
  end

  defp sort_reducer(entry, {:pivot, y, x, r, rs, s, bool}, fun) do
    cond do
      fun.(y, entry) == bool ->
        {:pivot, entry, y, [x | r], rs, s, bool}

      fun.(x, entry) == bool ->
        {:pivot, y, entry, [x | r], rs, s, bool}

      fun.(s, entry) == bool ->
        {:split, entry, s, [], [[y, x | r] | rs], bool}

      true ->
        {:split, s, entry, [], [[y, x | r] | rs], bool}
    end
  end

  defp sort_reducer(entry, [x], fun) do
    {:split, entry, x, [], [], fun.(x, entry)}
  end

  defp sort_reducer(entry, acc, _fun) do
    [entry | acc]
  end

  defp sort_terminator({:split, y, x, r, rs, bool}, fun) do
    sort_merge([[y, x | r] | rs], fun, bool)
  end

  defp sort_terminator({:pivot, y, x, r, rs, s, bool}, fun) do
    sort_merge([[s], [y, x | r] | rs], fun, bool)
  end

  defp sort_terminator(acc, _fun) do
    acc
  end

  defp sort_merge(list, fun, true), do: reverse_sort_merge(list, [], fun, true)

  defp sort_merge(list, fun, false), do: sort_merge(list, [], fun, false)

  defp sort_merge([t1, [h2 | t2] | l], acc, fun, true),
    do: sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, false) | acc], fun, true)

  defp sort_merge([[h2 | t2], t1 | l], acc, fun, false),
    do: sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, false) | acc], fun, false)

  defp sort_merge([l], [], _fun, _bool), do: l

  defp sort_merge([l], acc, fun, bool),
    do: reverse_sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp sort_merge([], acc, fun, bool), do: reverse_sort_merge(acc, [], fun, bool)

  defp reverse_sort_merge([[h2 | t2], t1 | l], acc, fun, true),
    do: reverse_sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, true) | acc], fun, true)

  defp reverse_sort_merge([t1, [h2 | t2] | l], acc, fun, false),
    do: reverse_sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, true) | acc], fun, false)

  defp reverse_sort_merge([l], acc, fun, bool),
    do: sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp reverse_sort_merge([], acc, fun, bool), do: sort_merge(acc, [], fun, bool)

  defp sort_merge1([h1 | t1], h2, t2, m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge1([], h2, t2, m, _fun, _bool), do: :lists.reverse(t2, [h2 | m])

  defp sort_merge2(h1, t1, [h2 | t2], m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge2(h1, t1, [], m, _fun, _bool), do: :lists.reverse(t1, [h1 | m])

  ## split

  defp split_list([head | tail], counter, acc) when counter > 0 do
    split_list(tail, counter - 1, [head | acc])
  end

  defp split_list(list, 0, acc) do
    {:lists.reverse(acc), list}
  end

  defp split_list([], _, acc) do
    {:lists.reverse(acc), []}
  end

  defp split_reverse_list([head | tail], counter, acc) when counter > 0 do
    split_reverse_list(tail, counter - 1, [head | acc])
  end

  defp split_reverse_list(list, 0, acc) do
    {:lists.reverse(list), acc}
  end

  defp split_reverse_list([], _, acc) do
    {[], acc}
  end

  ## split_while

  defp split_while_list([head | tail], fun, acc) do
    if fun.(head) do
      split_while_list(tail, fun, [head | acc])
    else
      {:lists.reverse(acc), [head | tail]}
    end
  end

  defp split_while_list([], _, acc) do
    {:lists.reverse(acc), []}
  end

  ## take

  defp take_list(_list, 0), do: []
  defp take_list([head | tail], counter), do: [head | take_list(tail, counter - 1)]
  defp take_list([], _counter), do: []

  defp take_every_list([head | tail], to_drop),
    do: [head | tail |> drop_list(to_drop) |> take_every_list(to_drop)]

  defp take_every_list([], _to_drop), do: []

  defp take_every_list(_list, 0, _to_drop), do: []

  defp take_every_list([head | tail], counter, to_drop),
    do: [head | tail |> drop_list(to_drop) |> take_every_list(counter - 1, to_drop)]

  defp take_every_list([], _counter, _to_drop), do: []

  ## take_while

  defp take_while_list([head | tail], fun) do
    if fun.(head) do
      [head | take_while_list(tail, fun)]
    else
      []
    end
  end

  defp take_while_list([], _) do
    []
  end

  ## uniq

  defp uniq_list([head | tail], set, fun) do
    value = fun.(head)

    case set do
      %{^value => true} -> uniq_list(tail, set, fun)
      %{} -> [head | uniq_list(tail, Map.put(set, value, true), fun)]
    end
  end

  defp uniq_list([], _set, _fun) do
    []
  end

  ## with_index

  defp with_index_list([head | tail], offset) do
    [{head, offset} | with_index_list(tail, offset + 1)]
  end

  defp with_index_list([], _offset), do: []

  defp with_index_list([head | tail], offset, fun) do
    [fun.(head, offset) | with_index_list(tail, offset + 1, fun)]
  end

  defp with_index_list([], _offset, _fun), do: []

  ## zip

  defp zip_list([head1 | next1], [head2 | next2], acc) do
    zip_list(next1, next2, [{head1, head2} | acc])
  end

  defp zip_list([], _, acc), do: :lists.reverse(acc)
  defp zip_list(_, [], acc), do: :lists.reverse(acc)

  defp zip_with_list([head1 | next1], [head2 | next2], fun) do
    [fun.(head1, head2) | zip_with_list(next1, next2, fun)]
  end

  defp zip_with_list(_, [], _fun), do: []
  defp zip_with_list([], _, _fun), do: []

  defp zip_reduce_list([head1 | next1], [head2 | next2], acc, fun) do
    zip_reduce_list(next1, next2, fun.(head1, head2, acc), fun)
  end

  defp zip_reduce_list(_, [], acc, _fun), do: acc
  defp zip_reduce_list([], _, acc, _fun), do: acc
end

defimpl Enumerable, for: List do
  def count(list), do: {:ok, length(list)}

  def member?([], _value), do: {:ok, false}
  def member?(_list, _value), do: {:error, __MODULE__}

  def slice([]), do: {:ok, 0, fn _, _, _ -> [] end}
  def slice(_list), do: {:error, __MODULE__}

  def reduce(_list, {:halt, acc}, _fun), do: {:halted, acc}
  def reduce(list, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
  def reduce([], {:cont, acc}, _fun), do: {:done, acc}
  def reduce([head | tail], {:cont, acc}, fun), do: reduce(tail, fun.(head, acc), fun)
end

defimpl Enumerable, for: Map do
  def count(map) do
    {:ok, map_size(map)}
  end

  def member?(map, {key, value}) do
    {:ok, match?(%{^key => ^value}, map)}
  end

  def member?(_map, _other) do
    {:ok, false}
  end

  def slice(map) do
    size = map_size(map)
    {:ok, size, &:maps.to_list/1}
  end

  def reduce(map, acc, fun) do
    Enumerable.List.reduce(:maps.to_list(map), acc, fun)
  end
end

defimpl Enumerable, for: Function do
  def count(_function), do: {:error, __MODULE__}
  def member?(_function, _value), do: {:error, __MODULE__}
  def slice(_function), do: {:error, __MODULE__}

  def reduce(function, acc, fun) when is_function(function, 2), do: function.(acc, fun)

  def reduce(function, _acc, _fun) do
    raise Protocol.UndefinedError,
      protocol: @protocol,
      value: function,
      description: "only anonymous functions of arity 2 are enumerable"
  end
end
