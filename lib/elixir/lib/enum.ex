defprotocol Enumerable do
  @moduledoc """
  Enumerable protocol used by `Enum` and `Stream` modules.

  When you invoke a function in the `Enum` module, the first argument
  is usually a collection that must implement this protocol.
  For example, the expression:

      Enum.map([1, 2, 3], &(&1 * 2))

  invokes `Enumerable.reduce/3` to perform the reducing operation that
  builds a mapped list by calling the mapping function `&(&1 * 2)` on
  every element in the collection and consuming the element with an
  accumulated list.

  Internally, `Enum.map/2` is implemented as follows:

      def map(enumerable, fun) do
        reducer = fn x, acc -> {:cont, [fun.(x) | acc]} end
        Enumerable.reduce(enumerable, {:cont, []}, reducer) |> elem(1) |> :lists.reverse()
      end

  Notice the user-supplied function is wrapped into a `t:reducer/0` function.
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
  @type reducer :: (term, term -> acc)

  @typedoc """
  The result of the reduce operation.

  It may be *done* when the enumeration is finished by reaching
  its end, or *halted*/*suspended* when the enumeration was halted
  or suspended by the `t:reducer/0` function.

  In case a `t:reducer/0` function returns the `:suspend` accumulator, the
  `:suspended` tuple must be explicitly handled by the caller and
  never leak. In practice, this means regular enumeration functions
  just need to be concerned about `:done` and `:halted` results.

  Furthermore, a `:suspend` call must always be followed by another call,
  eventually halting or continuing until the end.
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
  A slicing function that receives the initial position and the
  number of elements in the slice.

  The `start` position is a number `>= 0` and guaranteed to
  exist in the `enumerable`. The length is a number `>= 1` in a way
  that `start + length <= count`, where `count` is the maximum
  amount of elements in the enumerable.

  The function should return a non empty list where
  the amount of elements is equal to `length`.
  """
  @type slicing_fun :: (start :: non_neg_integer, length :: pos_integer -> [term()])

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
  in the `enumerable`.

  Otherwise it should return `{:error, __MODULE__}` and a default algorithm
  built on top of `reduce/3` that runs in linear time will be used.
  """
  @spec count(t) :: {:ok, non_neg_integer} | {:error, module}
  def count(enumerable)

  @doc """
  Checks if an `element` exists within the `enumerable`.

  It should return `{:ok, boolean}` if you can check the membership of a
  given element in the `enumerable` with `===/2` without traversing the whole
  enumerable.

  Otherwise it should return `{:error, __MODULE__}` and a default algorithm
  built on top of `reduce/3` that runs in linear time will be used.
  """
  @spec member?(t, term) :: {:ok, boolean} | {:error, module}
  def member?(enumerable, element)

  @doc """
  Returns a function that slices the data structure contiguously.

  It should return `{:ok, size, slicing_fun}` if the `enumerable` has
  a known bound and can access a position in the `enumerable` without
  traversing all previous elements.

  Otherwise it should return `{:error, __MODULE__}` and a default
  algorithm built on top of `reduce/3` that runs in linear time will be
  used.

  ## Differences to `count/1`

  The `size` value returned by this function is used for boundary checks,
  therefore it is extremely important that this function only returns `:ok`
  if retrieving the `size` of the `enumerable` is cheap, fast and takes constant
  time. Otherwise the simplest of operations, such as `Enum.at(enumerable, 0)`,
  will become too expensive.

  On the other hand, the `count/1` function in this protocol should be
  implemented whenever you can count the number of elements in the collection.
  """
  @spec slice(t) ::
          {:ok, size :: non_neg_integer(), slicing_fun()}
          | {:error, module()}
  def slice(enumerable)
end

defmodule Enum do
  import Kernel, except: [max: 2, min: 2]

  @moduledoc """
  Provides a set of algorithms to work with enumerables.

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
  Returns `true` if `fun.(element)` is truthy for all elements in `enumerable`.

  Iterates over the `enumerable` and invokes `fun` on each element. When an invocation
  of `fun` returns a falsy value (`false` or `nil`) iteration stops immediately and
  `false` is returned. In all other cases `true` is returned.

  ## Examples

      iex> Enum.all?([2, 4, 6], fn x -> rem(x, 2) == 0 end)
      true

      iex> Enum.all?([2, 3, 4], fn x -> rem(x, 2) == 0 end)
      false

      iex> Enum.all?([], fn x -> x > 0 end)
      true

  If no function is given, the truthiness of each element is checked during iteration.
  When an element has a falsy value (`false` or `nil`) iteration stops immediately and
  `false` is returned. In all other cases `true` is returned.

      iex> Enum.all?([1, 2, 3])
      true

      iex> Enum.all?([1, nil, 3])
      false

      iex> Enum.all?([])
      true

  """
  @spec all?(t, (element -> as_boolean(term))) :: boolean

  def all?(enumerable, fun \\ fn x -> x end)

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

  If no function is given, the truthiness of each element is checked during iteration.
  When an element has a truthy value (neither `false` nor `nil`) iteration stops
  immediately and `true` is returned. In all other cases `false` is returned.

      iex> Enum.any?([false, false, false])
      false

      iex> Enum.any?([false, true, false])
      true

      iex> Enum.any?([])
      false

  """
  @spec any?(t, (element -> as_boolean(term))) :: boolean

  def any?(enumerable, fun \\ fn x -> x end)

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
  def at(enumerable, index, default \\ nil) do
    case slice_any(enumerable, index, 1) do
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
    chunk_every(enum, n, step, nil)
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
  chunks do not overlap.

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

  """
  @doc since: "1.5.0"
  @spec chunk_every(t, pos_integer, pos_integer, t | :discard) :: [list]
  def chunk_every(enumerable, count, step, leftover \\ [])
      when is_integer(count) and count > 0 and is_integer(step) and step > 0 do
    R.chunk_every(&chunk_while/4, enumerable, count, step, leftover)
  end

  @doc """
  Chunks the `enumerable` with fine grained control when every chunk is emitted.

  `chunk_fun` receives the current element and the accumulator and
  must return `{:cont, chunk, acc}` to emit the given chunk and
  continue with accumulator or `{:cont, acc}` to not emit any chunk
  and continue with the return accumulator.

  `after_fun` is invoked when iteration is done and must also return
  `{:cont, chunk, acc}` or `{:cont, acc}`.

  Returns a list of lists.

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
          {:cont, emit, acc} -> {:cont, {[emit | buffer], acc}}
          {:cont, acc} -> {:cont, {buffer, acc}}
          {:halt, acc} -> {:halt, {buffer, acc}}
        end
      end)

    case after_fun.(acc) do
      {:cont, _acc} -> :lists.reverse(res)
      {:cont, elem, _acc} -> :lists.reverse([elem | res])
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
  def concat(enumerables) do
    fun = &[&1 | &2]
    enumerables |> reduce([], &reduce(&1, &2, fun)) |> :lists.reverse()
  end

  @doc """
  Concatenates the enumerable on the `right` with the enumerable on the
  `left`.

  This function produces the same result as the `Kernel.++/2` operator
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
    concat([left, right])
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
  def dedup(enumerable) do
    dedup_by(enumerable, fn x -> x end)
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

  def drop(enumerable, amount) when is_integer(amount) and amount >= 0 do
    {result, _} = reduce(enumerable, {[], amount}, R.drop())
    if is_list(result), do: :lists.reverse(result), else: []
  end

  def drop(enumerable, amount) when is_integer(amount) and amount < 0 do
    {count, fun} = slice_count_and_fun(enumerable)
    amount = Kernel.min(amount + count, count)

    if amount > 0 do
      fun.(0, amount)
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
    :ok
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
  def fetch(enumerable, index) do
    case slice_any(enumerable, index, 1) do
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
  def fetch!(enumerable, index) do
    case slice_any(enumerable, index, 1) do
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

  ## Examples

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
        list when is_list(list) -> :lists.reverse(list, acc)
        other -> reduce(other, acc, &[&1 | &2])
      end
    end)
    |> :lists.reverse()
  end

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

  """
  @spec group_by(t, (element -> any), (element -> any)) :: map
  def group_by(enumerable, key_fun, value_fun \\ fn x -> x end)

  def group_by(enumerable, key_fun, value_fun) when is_function(key_fun) do
    reduce(reverse(enumerable), %{}, fn entry, acc ->
      key = key_fun.(entry)
      value = value_fun.(entry)

      case acc do
        %{^key => existing} -> Map.put(acc, key, [value | existing])
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
  Intersperses `element` between each element of the enumeration.

  ## Examples

      iex> Enum.intersperse([1, 2, 3], 0)
      [1, 0, 2, 0, 3]

      iex> Enum.intersperse([1], 0)
      [1]

      iex> Enum.intersperse([], 0)
      []

  """
  @spec intersperse(t, element) :: list
  def intersperse(enumerable, element) do
    list =
      enumerable
      |> reduce([], fn x, acc -> [x, element | acc] end)
      |> :lists.reverse()

    # Head is a superfluous intersperser element
    case list do
      [] -> []
      [_ | t] -> t
    end
  end

  @doc """
  Inserts the given `enumerable` into a `collectable`.

  Note that passing a non-empty list as the `collectable` is deprecated. If you're collecting
  into a non-empty keyword list, consider using `Keyword.merge/2`. If you're collecting into a
  non-empty list, consider something like `to_list(enumerable) ++ collectable`.

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

  def into(%{} = enumerable, %{} = collectable) do
    Map.merge(collectable, enumerable)
  end

  def into(enumerable, %{} = collectable) when is_list(enumerable) do
    Map.merge(collectable, :maps.from_list(enumerable))
  end

  def into(enumerable, %{} = collectable) do
    reduce(enumerable, collectable, fn {key, val}, acc ->
      Map.put(acc, key, val)
    end)
  end

  def into(enumerable, collectable) do
    into_protocol(enumerable, collectable)
  end

  defp into_protocol(enumerable, collectable) do
    {initial, fun} = Collectable.into(collectable)

    into(enumerable, initial, fun, fn entry, acc ->
      fun.(acc, {:cont, entry})
    end)
  end

  @doc """
  Inserts the given `enumerable` into a `collectable` according to the
  transformation function.

  ## Examples

      iex> Enum.into([2, 3], [3], fn x -> x * 3 end)
      [3, 6, 9]

      iex> Enum.into(%{a: 1, b: 2}, %{c: 3}, fn {k, v} -> {k, v * 2} end)
      %{a: 2, b: 4, c: 3}

  """
  @spec into(Enumerable.t(), Collectable.t(), (term -> term)) :: Collectable.t()

  def into(enumerable, collectable, transform) when is_list(collectable) do
    collectable ++ map(enumerable, transform)
  end

  def into(enumerable, collectable, transform) do
    {initial, fun} = Collectable.into(collectable)

    into(enumerable, initial, fun, fn entry, acc ->
      fun.(acc, {:cont, transform.(entry)})
    end)
  end

  defp into(enumerable, initial, fun, callback) do
    try do
      reduce(enumerable, initial, callback)
    catch
      kind, reason ->
        fun.(initial, :halt)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      acc -> fun.(acc, :done)
    end
  end

  @doc """
  Joins the given `enumerable` into a binary using `joiner` as a
  separator.

  If `joiner` is not passed at all, it defaults to the empty binary.

  All elements in the `enumerable` must be convertible to a binary,
  otherwise an error is raised.

  ## Examples

      iex> Enum.join([1, 2, 3])
      "123"

      iex> Enum.join([1, 2, 3], " = ")
      "1 = 2 = 3"

  """
  @spec join(t, String.t()) :: String.t()
  def join(enumerable, joiner \\ "")

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
  Maps and joins the given `enumerable` in one pass.

  `joiner` can be either a binary or a list and the result will be of
  the same type as `joiner`.
  If `joiner` is not passed at all, it defaults to an empty binary.

  All elements returned from invoking the `mapper` must be convertible to
  a binary, otherwise an error is raised.

  ## Examples

      iex> Enum.map_join([1, 2, 3], &(&1 * 2))
      "246"

      iex> Enum.map_join([1, 2, 3], " = ", &(&1 * 2))
      "2 = 4 = 6"

  """
  @spec map_join(t, String.t(), (element -> String.Chars.t())) :: String.t()
  def map_join(enumerable, joiner \\ "", mapper)

  def map_join(enumerable, joiner, mapper) when is_binary(joiner) do
    reduced =
      reduce(enumerable, :first, fn
        entry, :first -> entry_to_string(mapper.(entry))
        entry, acc -> [acc, joiner | entry_to_string(mapper.(entry))]
      end)

    if reduced == :first do
      ""
    else
      IO.iodata_to_binary(reduced)
    end
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

  @doc """
  Returns the maximal element in the `enumerable` according
  to Erlang's term ordering.

  If multiple elements are considered maximal, the first one that was found
  is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.max([1, 2, 3])
      3

      iex> Enum.max([], fn -> 0 end)
      0

  The fact this function uses Erlang's term ordering means that the comparison
  is structural and not semantic. For example:

      iex> Enum.max([~D[2017-03-31], ~D[2017-04-01]])
      ~D[2017-03-31]

  In the example above, `max/1` returned March 31st instead of April 1st
  because the structural comparison compares the day before the year. This
  can be addressed by using `max_by/3` and by relying on structures where
  the most significant digits come first. In this particular case, we can
  use `Date.to_erl/1` to get a tuple representation with year, month and day
  fields:

      iex> Enum.max_by([~D[2017-03-31], ~D[2017-04-01]], &Date.to_erl/1)
      ~D[2017-04-01]

  For selecting a maximum value out of two consider using `Kernel.max/2`.

  """
  @spec max(t, (() -> empty_result)) :: element | empty_result when empty_result: any
  def max(enumerable, empty_fallback \\ fn -> raise Enum.EmptyError end)
      when is_function(empty_fallback, 0) do
    aggregate(enumerable, &Kernel.max/2, empty_fallback)
  end

  @doc """
  Returns the maximal element in the `enumerable` as calculated
  by the given function.

  If multiple elements are considered maximal, the first one that was found
  is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.max_by(["a", "aa", "aaa"], fn x -> String.length(x) end)
      "aaa"

      iex> Enum.max_by(["a", "aa", "aaa", "b", "bbb"], &String.length/1)
      "aaa"

      iex> Enum.max_by([], &String.length/1, fn -> nil end)
      nil

  """
  @spec max_by(t, (element -> any), (() -> empty_result)) :: element | empty_result
        when empty_result: any
  def max_by(enumerable, fun, empty_fallback \\ fn -> raise Enum.EmptyError end)
      when is_function(fun, 1) and is_function(empty_fallback, 0) do
    first_fun = &{&1, fun.(&1)}

    reduce_fun = fn entry, {_, fun_max} = old ->
      fun_entry = fun.(entry)
      if(fun_entry > fun_max, do: {entry, fun_entry}, else: old)
    end

    case reduce_by(enumerable, first_fun, reduce_fun) do
      :empty -> empty_fallback.()
      {entry, _} -> entry
    end
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

  @doc """
  Returns the minimal element in the `enumerable` according
  to Erlang's term ordering.

  If multiple elements are considered minimal, the first one that was found
  is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min([1, 2, 3])
      1

      iex> Enum.min([], fn -> 0 end)
      0

  The fact this function uses Erlang's term ordering means that the comparison
  is structural and not semantic. For example:

      iex> Enum.min([~D[2017-03-31], ~D[2017-04-01]])
      ~D[2017-04-01]

  In the example above, `min/1` returned April 1st instead of March 31st
  because the structural comparison compares the day before the year. This
  can be addressed by using `min_by/3` and by relying on structures where
  the most significant digits come first. In this particular case, we can
  use `Date.to_erl/1` to get a tuple representation with year, month and day
  fields:

      iex> Enum.min_by([~D[2017-03-31], ~D[2017-04-01]], &Date.to_erl/1)
      ~D[2017-03-31]

  For selecting a minimal value out of two consider using `Kernel.min/2`.

  """
  @spec min(t, (() -> empty_result)) :: element | empty_result when empty_result: any
  def min(enumerable, empty_fallback \\ fn -> raise Enum.EmptyError end)
      when is_function(empty_fallback, 0) do
    aggregate(enumerable, &Kernel.min/2, empty_fallback)
  end

  @doc """
  Returns the minimal element in the `enumerable` as calculated
  by the given function.

  If multiple elements are considered minimal, the first one that was found
  is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min_by(["a", "aa", "aaa"], fn x -> String.length(x) end)
      "a"

      iex> Enum.min_by(["a", "aa", "aaa", "b", "bbb"], &String.length/1)
      "a"

      iex> Enum.min_by([], &String.length/1, fn -> nil end)
      nil

  """
  @spec min_by(t, (element -> any), (() -> empty_result)) :: element | empty_result
        when empty_result: any
  def min_by(enumerable, fun, empty_fallback \\ fn -> raise Enum.EmptyError end)
      when is_function(fun, 1) and is_function(empty_fallback, 0) do
    first_fun = &{&1, fun.(&1)}

    reduce_fun = fn entry, {_, fun_min} = old ->
      fun_entry = fun.(entry)
      if(fun_entry < fun_min, do: {entry, fun_entry}, else: old)
    end

    case reduce_by(enumerable, first_fun, reduce_fun) do
      :empty -> empty_fallback.()
      {entry, _} -> entry
    end
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
  @spec min_max(t, (() -> empty_result)) :: {element, element} | empty_result
        when empty_result: any
  def min_max(enumerable, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def min_max(first..last, empty_fallback) when is_function(empty_fallback, 0) do
    {Kernel.min(first, last), Kernel.max(first, last)}
  end

  def min_max(enumerable, empty_fallback) when is_function(empty_fallback, 0) do
    first_fun = &{&1, &1}

    reduce_fun = fn entry, {min, max} ->
      {Kernel.min(min, entry), Kernel.max(max, entry)}
    end

    case reduce_by(enumerable, first_fun, reduce_fun) do
      :empty -> empty_fallback.()
      entry -> entry
    end
  end

  @doc """
  Returns a tuple with the minimal and the maximal elements in the
  enumerable as calculated by the given function.

  If multiple elements are considered maximal or minimal, the first one
  that was found is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min_max_by(["aaa", "bb", "c"], fn x -> String.length(x) end)
      {"c", "aaa"}

      iex> Enum.min_max_by(["aaa", "a", "bb", "c", "ccc"], &String.length/1)
      {"a", "aaa"}

      iex> Enum.min_max_by([], &String.length/1, fn -> {nil, nil} end)
      {nil, nil}

  """
  @spec min_max_by(t, (element -> any), (() -> empty_result)) :: {element, element} | empty_result
        when empty_result: any
  def min_max_by(enumerable, fun, empty_fallback \\ fn -> raise Enum.EmptyError end)
      when is_function(fun, 1) and is_function(empty_fallback, 0) do
    first_fun = fn entry ->
      fun_entry = fun.(entry)
      {{entry, entry}, {fun_entry, fun_entry}}
    end

    reduce_fun = fn entry, {{prev_min, prev_max}, {fun_min, fun_max}} = acc ->
      fun_entry = fun.(entry)

      cond do
        fun_entry < fun_min ->
          {{entry, prev_max}, {fun_entry, fun_max}}

        fun_entry > fun_max ->
          {{prev_min, entry}, {fun_min, fun_entry}}

        true ->
          acc
      end
    end

    case reduce_by(enumerable, first_fun, reduce_fun) do
      :empty -> empty_fallback.()
      {entry, _} -> entry
    end
  end

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

      iex> Enum.split_with(%{a: 1, b: -2, c: 1, d: -3}, fn {_k, v} -> v < 0 end)
      {[b: -2, d: -3], [a: 1, c: 1]}

      iex> Enum.split_with(%{a: 1, b: -2, c: 1, d: -3}, fn {_k, v} -> v > 50 end)
      {[], [a: 1, b: -2, c: 1, d: -3]}

      iex> Enum.split_with(%{}, fn {_k, v} -> v > 50 end)
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

  This function uses Erlang's [`:rand` module](http://www.erlang.org/doc/man/rand.html) to calculate
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

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsplus, {101, 102, 103})
      iex> Enum.random([1, 2, 3])
      1
      iex> Enum.random([1, 2, 3])
      3
      iex> Enum.random(1..1_000)
      556

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

        {:ok, count, fun} when is_function(fun) ->
          fun.(random_integer(0, count - 1), 1)

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
  of the accumulator. Then the function is invoked with the next
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
  @spec reduce(t, any, (element, any -> any)) :: any
  def reduce(enumerable, acc, fun) when is_list(enumerable) do
    :lists.foldl(fun, acc, enumerable)
  end

  def reduce(first..last, acc, fun) do
    if first <= last do
      reduce_range_inc(first, last, acc, fun)
    else
      reduce_range_dec(first, last, acc, fun)
    end
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
      ...>   if x < 5, do: {:cont, acc + x}, else: {:halt, acc}
      ...> end)
      10
      iex> Enum.reduce_while(1..100, 0, fn x, acc ->
      ...>   if x > 0, do: {:cont, acc + x}, else: {:halt, acc}
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
  Applies the given function to each element in the `enumerable`,
  storing the result in a list and passing it as the accumulator
  for the next computation. Uses the first element in the `enumerable`
  as the starting value.

  ## Examples

      iex> Enum.scan(1..5, &(&1 + &2))
      [1, 3, 6, 10, 15]

  """
  @spec scan(t, (element, any -> any)) :: list
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
  def scan(enumerable, acc, fun) do
    {res, _} = reduce(enumerable, {[], acc}, R.scan3(fun))
    :lists.reverse(res)
  end

  @doc """
  Returns a list with the elements of `enumerable` shuffled.

  This function uses Erlang's [`:rand` module](http://www.erlang.org/doc/man/rand.html) to calculate
  the random value. Check its documentation for setting a
  different random algorithm or a different seed.

  ## Examples

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsplus, {1, 2, 3})
      iex> Enum.shuffle([1, 2, 3])
      [2, 1, 3]
      iex> Enum.shuffle([1, 2, 3])
      [2, 3, 1]

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
  elements before `index_range.first` (zero-base), then takes elements
  until element `index_range.last` (inclusively).

  Indexes are normalized, meaning that negative indexes will be counted
  from the end (for example, `-1` means the last element of the `enumerable`).

  If `index_range.last` is out of bounds, then it is assigned as the index
  of the last element.

  If the normalized `index_range.first` is out of bounds of the given
  `enumerable`, or this one is greater than the normalized `index_range.last`,
  then `[]` is returned.

  ## Examples

      iex> Enum.slice(1..100, 5..10)
      [6, 7, 8, 9, 10, 11]

      iex> Enum.slice(1..10, 5..20)
      [6, 7, 8, 9, 10]

      # last five elements (negative indexes)
      iex> Enum.slice(1..30, -5..-1)
      [26, 27, 28, 29, 30]

      # last five elements (mixed positive and negative indexes)
      iex> Enum.slice(1..30, 25..-1)
      [26, 27, 28, 29, 30]

      # out of bounds
      iex> Enum.slice(1..10, 11..20)
      []

      # index_range.first is greater than index_range.last
      iex> Enum.slice(1..10, 6..5)
      []

  """
  @doc since: "1.6.0"
  @spec slice(t, Range.t()) :: list
  def slice(enumerable, index_range)

  def slice(enumerable, first..last) do
    {count, fun} = slice_count_and_fun(enumerable)
    corr_first = if first >= 0, do: first, else: first + count
    corr_last = if last >= 0, do: last, else: last + count
    amount = corr_last - corr_first + 1

    if corr_first >= 0 and corr_first < count and amount > 0 do
      fun.(corr_first, Kernel.min(amount, count - corr_first))
    else
      []
    end
  end

  @doc """
  Returns a subset list of the given `enumerable`, from `start_index` (zero-based)
  with `amount` number of elements if available.

  Given an `enumerable`, it drops elements right before element `start_index`,
  then takes `amount` of elements, returning as many elements as possible if
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

      # out of bound start index (positive)
      iex> Enum.slice(1..10, 10, 5)
      []

      # out of bound start index (negative)
      iex> Enum.slice(1..10, -11, 5)
      []

  """
  @spec slice(t, index, non_neg_integer) :: list
  def slice(_enumerable, start_index, 0) when is_integer(start_index), do: []

  def slice(enumerable, start_index, amount)
      when is_integer(start_index) and is_integer(amount) and amount >= 0 do
    slice_any(enumerable, start_index, amount)
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
  two arguments, and return `true` if the first argument precedes the second one.

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

  ## Sorting structs

  Do not use `</2`, `<=/2`, `>/2`, `>=/2` and friends when sorting structs.
  That's because the built-in operators above perform structural comparison
  and not a semantic one. Imagine we sort the following list of dates:

      iex> dates = [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]
      iex> Enum.sort(dates)
      [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]

  Notice the returned result is incorrect, because `sort/1` by default uses
  `<=/2`, which will compare their structure. When comparing structures, the
  fields are compared in alphabetical order, which means the dates above will
  be compared by `day`, `month` and then `year`, which is the opposite of what
  we want.

  For this reason, most structs provide a "compare" function, such as
  `Date.compare/2`, which receives two structs and returns `:lt` (less than),
  `:eq` (equal), and `:gt` (greather than). For example, to sort dates
  increasingly, one would do:

      iex> dates = [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]
      iex> Enum.sort(dates, &(Date.compare(&1, &2) != :gt))
      [~D[2019-01-01], ~D[2019-06-06], ~D[2020-03-02]]

  Or in decreasing order:

      iex> dates = [~D[2019-01-01], ~D[2020-03-02], ~D[2019-06-06]]
      iex> Enum.sort(dates, &(Date.compare(&1, &2) != :lt))
      [~D[2020-03-02], ~D[2019-06-06], ~D[2019-01-01]]

  """
  @spec sort(t, (element, element -> boolean)) :: list
  def sort(enumerable, fun) when is_list(enumerable) do
    :lists.sort(fun, enumerable)
  end

  def sort(enumerable, fun) do
    reduce(enumerable, [], &sort_reducer(&1, &2, fun))
    |> sort_terminator(fun)
  end

  @doc """
  Sorts the mapped results of the `enumerable` according to the provided `sorter`
  function.

  This function maps each element of the `enumerable` using the provided `mapper`
  function. The enumerable is then sorted by the mapped elements
  using the `sorter` function, which defaults to `Kernel.<=/2`.

  `sort_by/3` differs from `sort/2` in that it only calculates the
  comparison value for each element in the enumerable once instead of
  once for each element in each comparison.
  If the same function is being called on both elements, it's also more
  compact to use `sort_by/3`.

  ## Examples

  Using the default `sorter` of `<=/2`:

      iex> Enum.sort_by(["some", "kind", "of", "monster"], &byte_size/1)
      ["of", "some", "kind", "monster"]

  Using a custom `sorter` to override the order:

      iex> Enum.sort_by(["some", "kind", "of", "monster"], &byte_size/1, &>=/2)
      ["monster", "some", "kind", "of"]

  Sorting by multiple properties - first by size, then by first letter
  (this takes advantage of the fact that tuples are compared element-by-element):

      iex> Enum.sort_by(["some", "kind", "of", "monster"], &{byte_size(&1), String.first(&1)})
      ["of", "kind", "some", "monster"]

  """
  @spec sort_by(t, (element -> mapped_element), (mapped_element, mapped_element -> boolean)) ::
          list
        when mapped_element: element

  def sort_by(enumerable, mapper, sorter \\ &<=/2) do
    enumerable
    |> map(&{&1, mapper.(&1)})
    |> sort(&sorter.(elem(&1, 1), elem(&2, 1)))
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
  def split(enumerable, count) when is_list(enumerable) and count >= 0 do
    split_list(enumerable, count, [])
  end

  def split(enumerable, count) when count >= 0 do
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

  def split(enumerable, count) when count < 0 do
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

  """
  @spec sum(t) :: number
  def sum(enumerable)

  def sum(first..last) do
    div((last + first) * (abs(last - first) + 1), 2)
  end

  def sum(enumerable) do
    reduce(enumerable, 0, &+/2)
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
    {count, fun} = slice_count_and_fun(enumerable)
    first = Kernel.max(amount + count, 0)
    fun.(first, count - first)
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

  def take_every(enumerable, 1), do: to_list(enumerable)
  def take_every(_enumerable, 0), do: []
  def take_every([], nth) when is_integer(nth) and nth > 1, do: []

  def take_every(enumerable, nth) when is_integer(nth) and nth > 1 do
    {res, _} = reduce(enumerable, {[], :first}, R.take_every(nth))
    :lists.reverse(res)
  end

  @doc """
  Takes `count` random elements from `enumerable`.

  Notice this function will traverse the whole `enumerable` to
  get the random sublist.

  See `random/1` for notes on implementation and random seed.

  ## Examples

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsplus, {1, 2, 3})
      iex> Enum.take_random(1..10, 2)
      [5, 4]
      iex> Enum.take_random(?a..?z, 5)
      'ipybz'

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
  def unzip(enumerable) do
    {list1, list2} =
      reduce(enumerable, {[], []}, fn {el1, el2}, {list1, list2} ->
        {[el1 | list1], [el2 | list2]}
      end)

    {:lists.reverse(list1), :lists.reverse(list2)}
  end

  @doc """
  Returns the `enumerable` with each element wrapped in a tuple
  alongside its index.

  If an `offset` is given, we will index from the given offset instead of from zero.

  ## Examples

      iex> Enum.with_index([:a, :b, :c])
      [a: 0, b: 1, c: 2]

      iex> Enum.with_index([:a, :b, :c], 3)
      [a: 3, b: 4, c: 5]

  """
  @spec with_index(t, integer) :: [{element, index}]
  def with_index(enumerable, offset \\ 0) do
    map_reduce(enumerable, offset, fn x, acc ->
      {{x, acc}, acc + 1}
    end)
    |> elem(0)
  end

  @doc """
  Zips corresponding elements from two enumerables into one list
  of tuples.

  The zipping finishes as soon as any enumerable completes.

  ## Examples

      iex> Enum.zip([1, 2, 3], [:a, :b, :c])
      [{1, :a}, {2, :b}, {3, :c}]

      iex> Enum.zip([1, 2, 3, 4, 5], [:a, :b, :c])
      [{1, :a}, {2, :b}, {3, :c}]

  """
  @spec zip(t, t) :: [{any, any}]
  def zip(enumerable1, enumerable2)
      when is_list(enumerable1) and is_list(enumerable2) do
    zip_list(enumerable1, enumerable2)
  end

  def zip(enumerable1, enumerable2) do
    zip([enumerable1, enumerable2])
  end

  @doc """
  Zips corresponding elements from a finite collection of enumerables
  into one list of tuples.

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
    Stream.zip(enumerables).({:cont, []}, &{:cont, [&1 | &2]})
    |> elem(1)
    |> :lists.reverse()
  end

  ## Helpers

  @compile {:inline,
            aggregate: 3, entry_to_string: 1, reduce: 3, reduce_by: 3, reduce_enumerable: 3}

  defp entry_to_string(entry) when is_binary(entry), do: entry
  defp entry_to_string(entry), do: String.Chars.to_string(entry)

  defp aggregate([head | tail], fun, _empty) do
    aggregate_list(tail, head, fun)
  end

  defp aggregate([], _fun, empty) do
    empty.()
  end

  defp aggregate(first..last, fun, _empty) do
    fun.(first, last)
  end

  defp aggregate(enumerable, fun, empty) do
    ref = make_ref()

    enumerable
    |> reduce(ref, fn
      element, ^ref -> element
      element, acc -> fun.(acc, element)
    end)
    |> case do
      ^ref -> empty.()
      result -> result
    end
  end

  defp aggregate_list([head | tail], acc, fun), do: aggregate_list(tail, fun.(acc, head), fun)
  defp aggregate_list([], acc, _fun), do: acc

  defp reduce_by([head | tail], first, fun) do
    :lists.foldl(fun, first.(head), tail)
  end

  defp reduce_by([], _first, _fun) do
    :empty
  end

  defp reduce_by(enumerable, first, fun) do
    reduce(enumerable, :empty, fn
      element, {_, _} = acc -> fun.(element, acc)
      element, :empty -> first.(element)
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

  ## reduce

  defp reduce_range_inc(first, first, acc, fun) do
    fun.(first, acc)
  end

  defp reduce_range_inc(first, last, acc, fun) do
    reduce_range_inc(first + 1, last, fun.(first, acc), fun)
  end

  defp reduce_range_dec(first, first, acc, fun) do
    fun.(first, acc)
  end

  defp reduce_range_dec(first, last, acc, fun) do
    reduce_range_dec(first - 1, last, fun.(first, acc), fun)
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

  ## shuffle

  defp shuffle_unwrap([{_, h} | enumerable], t) do
    shuffle_unwrap(enumerable, [h | t])
  end

  defp shuffle_unwrap([], t), do: t

  ## slice

  defp slice_any(enumerable, start, amount) when start < 0 do
    {count, fun} = slice_count_and_fun(enumerable)
    start = count + start

    if start >= 0 do
      fun.(start, Kernel.min(amount, count - start))
    else
      []
    end
  end

  defp slice_any(list, start, amount) when is_list(list) do
    list |> drop_list(start) |> take_list(amount)
  end

  defp slice_any(enumerable, start, amount) do
    case Enumerable.slice(enumerable) do
      {:ok, count, _} when start >= count ->
        []

      {:ok, count, fun} when is_function(fun) ->
        fun.(start, Kernel.min(amount, count - start))

      {:error, module} ->
        slice_enum(enumerable, module, start, amount)
    end
  end

  defp slice_enum(enumerable, module, start, amount) do
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

  defp slice_count_and_fun(enumerable) when is_list(enumerable) do
    length = length(enumerable)
    {length, &Enumerable.List.slice(enumerable, &1, &2, length)}
  end

  defp slice_count_and_fun(enumerable) do
    case Enumerable.slice(enumerable) do
      {:ok, count, fun} when is_function(fun) ->
        {count, fun}

      {:error, module} ->
        {_, {list, count}} =
          module.reduce(enumerable, {:cont, {[], 0}}, fn elem, {acc, count} ->
            {:cont, {[elem | acc], count + 1}}
          end)

        {count, &Enumerable.List.slice(:lists.reverse(list), &1, &2, count)}
    end
  end

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

  defp take_list([head | _], 1), do: [head]
  defp take_list([head | tail], counter), do: [head | take_list(tail, counter - 1)]
  defp take_list([], _counter), do: []

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

  ## zip

  defp zip_list([h1 | next1], [h2 | next2]) do
    [{h1, h2} | zip_list(next1, next2)]
  end

  defp zip_list(_, []), do: []
  defp zip_list([], _), do: []
end

defimpl Enumerable, for: List do
  def count(_list), do: {:error, __MODULE__}
  def member?(_list, _value), do: {:error, __MODULE__}
  def slice(_list), do: {:error, __MODULE__}

  def reduce(_list, {:halt, acc}, _fun), do: {:halted, acc}
  def reduce(list, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
  def reduce([], {:cont, acc}, _fun), do: {:done, acc}
  def reduce([head | tail], {:cont, acc}, fun), do: reduce(tail, fun.(head, acc), fun)

  @doc false
  def slice(_list, _start, 0, _size), do: []
  def slice(list, start, count, size) when start + count == size, do: list |> drop(start)
  def slice(list, start, count, _size), do: list |> drop(start) |> take(count)

  defp drop(list, 0), do: list
  defp drop([_ | tail], count), do: drop(tail, count - 1)

  defp take(_list, 0), do: []
  defp take([head | tail], count), do: [head | take(tail, count - 1)]
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
    {:ok, size, &Enumerable.List.slice(:maps.to_list(map), &1, &2, size)}
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
