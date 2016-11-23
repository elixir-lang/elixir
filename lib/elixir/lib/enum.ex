defprotocol Enumerable do
  @moduledoc """
  Enumerable protocol used by `Enum` and `Stream` modules.

  When you invoke a function in the `Enum` module, the first argument
  is usually a collection that must implement this protocol.
  For example, the expression:

      Enum.map([1, 2, 3], &(&1 * 2))

  invokes `Enumerable.reduce/3` to perform the reducing
  operation that builds a mapped list by calling the mapping function
  `&(&1 * 2)` on every element in the collection and consuming the
  element with an accumulated list.

  Internally, `Enum.map/2` is implemented as follows:

      def map(enum, fun) do
        reducer = fn x, acc -> {:cont, [fun.(x) | acc]} end
        Enumerable.reduce(enum, {:cont, []}, reducer) |> elem(1) |> :lists.reverse()
      end

  Notice the user-supplied function is wrapped into a `t:reducer/0` function.
  The `t:reducer/0` function must return a tagged tuple after each step,
  as described in the `t:acc/0` type.

  The reason the accumulator requires a tagged tuple is to allow the
  `t:reducer/0` function to communicate the end of enumeration to the underlying
  enumerable, allowing any open resources to be properly closed.
  It also allows suspension of the enumeration, which is useful when
  interleaving between many enumerables is required (as in zip).

  Finally, `Enumerable.reduce/3` will return another tagged tuple,
  as represented by the `t:result/0` type.
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

  Should be called with the enumerable element and the
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
  @type result :: {:done, term} |
                  {:halted, term} |
                  {:suspended, term, continuation}

  @typedoc """
  A partially applied reduce function.

  The continuation is the closure returned as a result when
  the enumeration is suspended. When invoked, it expects
  a new accumulator and it returns the result.

  A continuation is easily implemented as long as the reduce
  function is defined in a tail recursive fashion. If the function
  is tail recursive, all the state is passed as arguments, so
  the continuation would simply be the reducing function partially
  applied.
  """
  @type continuation :: (acc -> result)

  @doc """
  Reduces the enumerable into an element.

  Most of the operations in `Enum` are implemented in terms of reduce.
  This function should apply the given `t:reducer/0` function to each
  item in the enumerable and proceed as expected by the returned
  accumulator.

  As an example, here is the implementation of `reduce` for lists:

      def reduce(_,       {:halt, acc}, _fun),   do: {:halted, acc}
      def reduce(list,    {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
      def reduce([],      {:cont, acc}, _fun),   do: {:done, acc}
      def reduce([h | t], {:cont, acc}, fun),    do: reduce(t, fun.(h, acc), fun)

  """
  @spec reduce(t, acc, reducer) :: result
  def reduce(enumerable, acc, fun)

  @doc """
  Checks if an element exists within the enumerable.

  It should return `{:ok, boolean}`.

  If `{:error, __MODULE__}` is returned a default algorithm using
  `reduce` and the match (`===`) operator is used. This algorithm runs
  in linear time.

  _Please force use of the default algorithm unless you can implement an
  algorithm that is significantly faster._
  """
  @spec member?(t, term) :: {:ok, boolean} | {:error, module}
  def member?(enumerable, element)

  @doc """
  Retrieves the enumerable's size.

  It should return `{:ok, size}`.

  If `{:error, __MODULE__}` is returned a default algorithm using
  `reduce` and the match (`===`) operator is used. This algorithm runs
  in linear time.

  _Please force use of the default algorithm unless you can implement an
  algorithm that is significantly faster._
  """
  @spec count(t) :: {:ok, non_neg_integer} | {:error, module}
  def count(enumerable)
end

defmodule Enum do
  import Kernel, except: [max: 2, min: 2]

  @moduledoc """
  Provides a set of algorithms that enumerate over enumerables according
  to the `Enumerable` protocol.

      iex> Enum.map([1, 2, 3], fn(x) -> x * 2 end)
      [2, 4, 6]

  Some particular types, like maps, yield a specific format on enumeration.
  For example, the argument is always a `{key, value}` tuple for maps:

      iex> map = %{a: 1, b: 2}
      iex> Enum.map(map, fn {k, v} -> {k, v * 2} end)
      [a: 2, b: 4]

  Note that the functions in the `Enum` module are eager: they always
  start the enumeration of the given enumerable. The `Stream` module
  allows lazy enumeration of enumerables and provides infinite streams.

  Since the majority of the functions in `Enum` enumerate the whole
  enumerable and return a list as result, infinite streams need to
  be carefully used with such functions, as they can potentially run
  forever. For example:

      Enum.each Stream.cycle([1, 2, 3]), &IO.puts(&1)

  """

  @compile :inline_list_funcs

  @type t :: Enumerable.t
  @type element :: any
  @type index :: integer
  @type default :: any

  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop skip(acc) do
    acc
  end

  defmacrop next(_, entry, acc) do
    quote do: [unquote(entry) | unquote(acc)]
  end

  defmacrop acc(head, state, _) do
    quote do: {unquote(head), unquote(state)}
  end

  defmacrop next_with_acc(_, entry, head, state, _) do
    quote do
      {[unquote(entry) | unquote(head)], unquote(state)}
    end
  end

  @doc """
  Returns true if the given `fun` evaluates to true on all of the items in the enumerable.

  It stops the iteration at the first invocation that returns `false` or `nil`.

  ## Examples

      iex> Enum.all?([2, 4, 6], fn(x) -> rem(x, 2) == 0 end)
      true

      iex> Enum.all?([2, 3, 4], fn(x) -> rem(x, 2) == 0 end)
      false

  If no function is given, it defaults to checking if
  all items in the enumerable are truthy values.

      iex> Enum.all?([1, 2, 3])
      true

      iex> Enum.all?([1, nil, 3])
      false

  """
  @spec all?(t) :: boolean
  @spec all?(t, (element -> as_boolean(term))) :: boolean

  def all?(enumerable, fun \\ fn(x) -> x end)

  def all?(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_all?(enumerable, fun)
  end

  def all?(enumerable, fun) when is_function(fun, 1) do
    Enumerable.reduce(enumerable, {:cont, true}, fn(entry, _) ->
      if fun.(entry), do: {:cont, true}, else: {:halt, false}
    end) |> elem(1)
  end

  @doc """
  Returns true if the given `fun` evaluates to true on any of the items in the enumerable.

  It stops the iteration at the first invocation that returns a truthy value (not `false` or `nil`).

  ## Examples

      iex> Enum.any?([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
      false

      iex> Enum.any?([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
      true

  If no function is given, it defaults to checking if at least one item
  in the enumerable is a truthy value.

      iex> Enum.any?([false, false, false])
      false

      iex> Enum.any?([false, true, false])
      true

  """
  @spec any?(t) :: boolean
  @spec any?(t, (element -> as_boolean(term))) :: boolean

  def any?(enumerable, fun \\ fn(x) -> x end)

  def any?(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_any?(enumerable, fun)
  end

  def any?(enumerable, fun) when is_function(fun, 1) do
    Enumerable.reduce(enumerable, {:cont, false}, fn(entry, _) ->
      if fun.(entry), do: {:halt, true}, else: {:cont, false}
    end) |> elem(1)
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Returns `default` if `index` is out of bounds.

  A negative `index` can be passed, which means the `enumerable` is
  enumerated once and the `index` is counted from the end (e.g.
  `-1` finds the last element).

  Note this operation takes linear time. In order to access
  the element at index `index`, it will need to traverse `index`
  previous elements.

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
    case fetch(enumerable, index) do
      {:ok, h} -> h
      :error   -> default
    end
  end

  @doc """
  Shortcut to `chunk(enumerable, count, count)`.
  """
  @spec chunk(t, pos_integer) :: [list]
  def chunk(enumerable, count), do: chunk(enumerable, count, count, nil)

  @doc """
  Returns list of lists containing `count` items each, where
  each new chunk starts `step` elements into the enumerable.

  `step` is optional and, if not passed, defaults to `count`, i.e.
  chunks do not overlap.

  If the final chunk does not have `count` elements to fill the chunk,
  elements are taken as necessary from `leftover` if it was passed.

  If `leftover` is passed and does not have enough elements to fill the
  chunk, then a partial chunk is returned with less than `count`
  elements. If `leftover` is not passed at all or is `nil`, then the
  partial chunk is discarded from the result.

  If `count` is greater than the number of elements in the enumerable
  and `leftover` is not passed, empty list will be returned.

  ## Examples

      iex> Enum.chunk([1, 2, 3, 4, 5, 6], 2)
      [[1, 2], [3, 4], [5, 6]]

      iex> Enum.chunk([1, 2, 3, 4, 5, 6], 3, 2)
      [[1, 2, 3], [3, 4, 5]]

      iex> Enum.chunk([1, 2, 3, 4, 5, 6], 3, 2, [7])
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

      iex> Enum.chunk([1, 2, 3, 4], 3, 3, [])
      [[1, 2, 3], [4]]

      iex> Enum.chunk([1, 2, 3, 4], 10)
      []

      iex> Enum.chunk([1, 2, 3, 4], 10, 10, [])
      [[1, 2, 3, 4]]

  """
  @spec chunk(t, pos_integer, pos_integer, t | nil) :: [list]
  def chunk(enumerable, count, step, leftover \\ nil)
      when is_integer(count) and count > 0 and is_integer(step) and step > 0 do
    limit = :erlang.max(count, step)

    {acc, {buffer, i}} =
      reduce(enumerable, {[], {[], 0}}, R.chunk(count, step, limit))

    if is_nil(leftover) || i == 0 do
      :lists.reverse(acc)
    else
      buffer = :lists.reverse(buffer, take(leftover, count - i))
      :lists.reverse([buffer | acc])
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
  def chunk_by(enumerable, fun) when is_function(fun, 1) do
    {acc, res} = reduce(enumerable, {[], nil}, R.chunk_by(fun))

    case res do
      {buffer, _} ->
        :lists.reverse([:lists.reverse(buffer) | acc])
      nil ->
        []
    end
  end

  @doc """
  Given an enumerable of enumerables, concatenates the enumerables into
  a single list.

  ## Examples

      iex> Enum.concat([1..3, 4..6, 7..9])
      [1, 2, 3, 4, 5, 6, 7, 8, 9]

      iex> Enum.concat([[1, [2], 3], [4], [5, 6]])
      [1, [2], 3, 4, 5, 6]

  """
  @spec concat(t) :: t
  def concat(enumerables) do
    do_concat(enumerables)
  end

  @doc """
  Concatenates the enumerable on the right with the enumerable on the
  left.

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
    do_concat([left, right])
  end

  defp do_concat(enumerable) do
    fun = &[&1 | &2]
    reduce(enumerable, [], &reduce(&1, &2, fun)) |> :lists.reverse
  end

  @doc """
  Returns the size of the enumerable.

  ## Examples

      iex> Enum.count([1, 2, 3])
      3

  """
  @spec count(t) :: non_neg_integer
  def count(enumerable) when is_list(enumerable) do
    :erlang.length(enumerable)
  end

  def count(enumerable) do
    case Enumerable.count(enumerable) do
      {:ok, value} when is_integer(value) ->
        value
      {:error, module} ->
        module.reduce(enumerable, {:cont, 0}, fn
          _, acc -> {:cont, acc + 1}
        end) |> elem(1)
    end
  end

  @doc """
  Returns the count of items in the enumerable for which `fun` returns
  a truthy value.

  ## Examples

      iex> Enum.count([1, 2, 3, 4, 5], fn(x) -> rem(x, 2) == 0 end)
      2

  """
  @spec count(t, (element -> as_boolean(term))) :: non_neg_integer
  def count(enumerable, fun) when is_function(fun, 1) do
    Enumerable.reduce(enumerable, {:cont, 0}, fn(entry, acc) ->
      {:cont, if(fun.(entry), do: acc + 1, else: acc)}
    end) |> elem(1)
  end

  @doc """
  Enumerates the `enumerable`, returning a list where all consecutive
  duplicated elements are collapsed to a single element.

  Elements are compared using `===`.

  If you want to remove all duplicated elements, regardless of order,
  see `uniq/1`.

  ## Examples

      iex> Enum.dedup([1, 2, 3, 3, 2, 1])
      [1, 2, 3, 2, 1]

      iex> Enum.dedup([1, 1, 2, 2.0, :three, :"three"])
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
  def dedup_by(enumerable, fun) when is_function(fun, 1) do
    {list, _} = reduce(enumerable, {[], []}, R.dedup(fun))
    :lists.reverse(list)
  end

  @doc """
  Drops the `amount` of items from the enumerable.

  If a negative `amount` is given, the `amount` of last values will be dropped.

  The `enumerable` is enumerated once to retrieve the proper index and
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
    drop_list(reverse(enumerable), -amount) |> :lists.reverse
  end

  @doc """
  Returns a list of every `nth` item in the enumerable dropped,
  starting with the first element.

  The first item is always dropped, unless `nth` is 0.

  The second argument specifying every `nth` item must be a non-negative
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
  Drops items at the beginning of the enumerable while `fun` returns a
  truthy value.

  ## Examples

      iex> Enum.drop_while([1, 2, 3, 2, 1], fn(x) -> x < 3 end)
      [3, 2, 1]

  """
  @spec drop_while(t, (element -> as_boolean(term))) :: list
  def drop_while(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_drop_while(enumerable, fun)
  end

  def drop_while(enumerable, fun) do
    {res, _} = reduce(enumerable, {[], true}, R.drop_while(fun))
    :lists.reverse(res)
  end

  @doc """
  Invokes the given `fun` for each item in the enumerable.

  Returns `:ok`.

  ## Examples

      Enum.each(["some", "example"], fn(x) -> IO.puts x end)
      "some"
      "example"
      #=> :ok

  """
  @spec each(t, (element -> any)) :: :ok
  def each(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    :lists.foreach(fun, enumerable)
    :ok
  end

  def each(enumerable, fun) when is_function(fun, 1) do
    reduce(enumerable, nil, fn(entry, _) ->
      fun.(entry)
      nil
    end)
    :ok
  end

  @doc """
  Determines if the enumerable is empty.

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
    case Enumerable.count(enumerable) do
      {:ok, value} when is_integer(value) ->
        value == 0
      {:error, module} ->
        module.reduce(enumerable, {:cont, true},
          fn(_, _) -> {:halt, false} end)
        |> elem(1)
    end
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Returns `{:ok, element}` if found, otherwise `:error`.

  A negative `index` can be passed, which means the `enumerable` is
  enumerated once and the `index` is counted from the end (e.g.
  `-1` fetches the last element).

  Note this operation takes linear time. In order to access
  the element at index `index`, it will need to traverse `index`
  previous elements.

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
  def fetch(enumerable, index)

  def fetch(enumerable, index) when is_list(enumerable) and is_integer(index) do
    if index < 0 do
      enumerable |> :lists.reverse |> fetch_list((-index) - 1)
    else
      fetch_list(enumerable, index)
    end
  end

  def fetch(first..last, index) when is_integer(index) do
    fetch_range(first, last, index)
  end

  def fetch(enumerable, index) when is_integer(index) and index < 0 do
    case Enumerable.count(enumerable) do
      {:error, _module} ->
        enumerable |> reverse |> fetch_list((-index) - 1)

      {:ok, count} when (count + index) < 0 ->
        :error

      {:ok, count} ->
        fetch_enumerable(enumerable, count + index, Enumerable)
    end
  end

  def fetch(enumerable, index) when is_integer(index) do
    case Enumerable.count(enumerable) do
      {:error, module} ->
        fetch_enumerable(enumerable, index, module)

      {:ok, count} when count <= index ->
        :error

      {:ok, _count} ->
        fetch_enumerable(enumerable, index, Enumerable)
    end
  end

  defp fetch_enumerable(enumerable, index, module) do
    reduce_result =
      module.reduce(enumerable, {:cont, {:not_found, 0}}, fn
        entry, {_, ^index} ->
          {:halt, {:found, entry}}
        _entry, {_, index} ->
         {:cont, {:not_found, index + 1}}
      end)

    case elem(reduce_result, 1) do
      {:found, entry} -> {:ok, entry}
      {:not_found, _} -> :error
    end
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Raises `OutOfBoundsError` if the given `index` is outside the range of
  the enumerable.

  Note this operation takes linear time. In order to access the element
  at index `index`, it will need to traverse `index` previous elements.

  ## Examples

      iex> Enum.fetch!([2, 4, 6], 0)
      2

      iex> Enum.fetch!([2, 4, 6], 2)
      6

      iex> Enum.fetch!([2, 4, 6], 4)
      ** (Enum.OutOfBoundsError) out of bounds error

  """
  @spec fetch!(t, index) :: element | no_return
  def fetch!(enumerable, index) do
    case fetch(enumerable, index) do
      {:ok, h} -> h
      :error -> raise Enum.OutOfBoundsError
    end
  end

  @doc """
  Filters the enumerable, i.e. returns only those elements
  for which `fun` returns a truthy value.

  See also `reject/2`.

  ## Examples

      iex> Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [2]

  """
  @spec filter(t, (element -> as_boolean(term))) :: list
  def filter(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    for item <- enumerable, fun.(item), do: item
  end

  def filter(enumerable, fun) when is_function(fun, 1) do
    reduce(enumerable, [], R.filter(fun)) |> :lists.reverse
  end

  @doc """
  Filters the enumerable and maps its elements in one pass.

  ## Examples

      iex> Enum.filter_map([1, 2, 3], fn(x) -> rem(x, 2) == 0 end, &(&1 * 2))
      [4]

  """
  @spec filter_map(t, (element -> as_boolean(term)),
    (element -> element)) :: list

  def filter_map(enumerable, filter, mapper)
      when is_list(enumerable) and is_function(filter, 1) and is_function(mapper, 1) do
    for item <- enumerable, filter.(item), do: mapper.(item)
  end

  def filter_map(enumerable, filter, mapper)
      when is_function(filter, 1) and is_function(mapper, 1) do
    reduce(enumerable, [], R.filter_map(filter, mapper))
    |> :lists.reverse
  end

  @doc """
  Returns the first item for which `fun` returns a truthy value.
  If no such item is found, returns `default`.

  ## Examples

      iex> Enum.find([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find([2, 4, 6], 0, fn(x) -> rem(x, 2) == 1 end)
      0

      iex> Enum.find([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
      3

  """
  @spec find(t, default, (element -> any)) :: element | default
  def find(enumerable, default \\ nil, fun)

  def find(enumerable, default, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_find(enumerable, default, fun)
  end

  def find(enumerable, default, fun) when is_function(fun, 1) do
    Enumerable.reduce(enumerable, {:cont, default}, fn(entry, default) ->
      if fun.(entry), do: {:halt, entry}, else: {:cont, default}
    end) |> elem(1)
  end

  @doc """
  Similar to `find/3`, but returns the index (zero-based)
  of the element instead of the element itself.

  ## Examples

      iex> Enum.find_index([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find_index([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
      1

  """
  @spec find_index(t, (element -> any)) :: non_neg_integer | nil
  def find_index(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_find_index(enumerable, 0, fun)
  end

  def find_index(enumerable, fun) when is_function(fun, 1) do
    result =
      Enumerable.reduce(enumerable, {:cont, {:not_found, 0}}, fn(entry, {_, index}) ->
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

      iex> Enum.find_value([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find_value([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
      true

      iex> Enum.find_value([1, 2, 3], "no bools!", &is_boolean/1)
      "no bools!"

  """
  @spec find_value(t, any, (element -> any)) :: any | nil
  def find_value(enumerable, default \\ nil, fun)

  def find_value(enumerable, default, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_find_value(enumerable, default, fun)
  end

  def find_value(enumerable, default, fun) when is_function(fun, 1) do
    Enumerable.reduce(enumerable, {:cont, default}, fn(entry, default) ->
      fun_entry = fun.(entry)
      if fun_entry, do: {:halt, fun_entry}, else: {:cont, default}
    end) |> elem(1)
  end

  @doc """
  Returns a new enumerable appending the result of invoking `fun` on
  each corresponding item of `enumerable`.

  The given function must return an enumerable.

  ## Examples

      iex> Enum.flat_map([:a, :b, :c], fn(x) -> [x, x] end)
      [:a, :a, :b, :b, :c, :c]

      iex> Enum.flat_map([{1, 3}, {4, 6}], fn({x, y}) -> x..y end)
      [1, 2, 3, 4, 5, 6]

      iex> Enum.flat_map([:a, :b, :c], fn(x) -> [[x]] end)
      [[:a], [:b], [:c]]

  """
  @spec flat_map(t, (element -> t)) :: list
  def flat_map(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    flat_map_list(enumerable, fun)
  end

  def flat_map(enumerable, fun) when is_function(fun, 1) do
    reduce(enumerable, [], fn(entry, acc) ->
      case fun.(entry) do
        list when is_list(list) -> :lists.reverse(list, acc)
        other -> reduce(other, acc, &[&1 | &2])
      end
    end) |> :lists.reverse
  end

  defp flat_map_list([head | tail], fun) do
    case fun.(head) do
      list when is_list(list) -> list ++ flat_map_list(tail, fun)
      other -> to_list(other) ++ flat_map_list(tail, fun)
    end
  end
  defp flat_map_list([], _fun) do
    []
  end

  @doc """
  Maps and reduces an enumerable, flattening the given results (only one level deep).

  It expects an accumulator and a function that receives each enumerable
  item, and must return a tuple containing a new enumerable (often a list)
  with the new accumulator or a tuple with `:halt` as first element and
  the accumulator as second.

  ## Examples

      iex> enum = 1..100
      iex> n = 3
      iex> Enum.flat_map_reduce(enum, 0, fn i, acc ->
      ...>   if acc < n, do: {[i], acc + 1}, else: {:halt, acc}
      ...> end)
      {[1, 2, 3], 3}

      iex> Enum.flat_map_reduce(1..5, 0, fn(i, acc) -> {[[i]], acc + i} end)
      {[[1], [2], [3], [4], [5]], 15}

  """
  @spec flat_map_reduce(t, acc, fun) :: {[any], any}
        when fun: (element, acc -> {t, acc} | {:halt, acc}),
             acc: any
  def flat_map_reduce(enumerable, acc, fun) when is_function(fun, 2) do
    {_, {list, acc}} =
      Enumerable.reduce(enumerable, {:cont, {[], acc}},
        fn(entry, {list, acc}) ->
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
  Splits the enumerable into groups based on `key_fun`.

  The result is a map where each key is given by `key_fun` and each
  value is a list of elements given by `value_fun`. Ordering is preserved.

  ## Examples

      iex> Enum.group_by(~w{ant buffalo cat dingo}, &String.length/1)
      %{3 => ["ant", "cat"], 7 => ["buffalo"], 5 => ["dingo"]}

      iex> Enum.group_by(~w{ant buffalo cat dingo}, &String.length/1, &String.first/1)
      %{3 => ["a", "c"], 7 => ["b"], 5 => ["d"]}

  """
  @spec group_by(t, (element -> any), (element -> any)) :: map
  def group_by(enumerable, key_fun, value_fun \\ fn x -> x end)

  def group_by(enumerable, key_fun, value_fun)
      when is_function(key_fun, 1) and is_function(value_fun, 1) do
    reduce(reverse(enumerable), %{}, fn entry, categories ->
      value = value_fun.(entry)
      Map.update(categories, key_fun.(entry), [value], &[value | &1])
    end)
  end

  # TODO: Remove on 2.0
  def group_by(enumerable, dict, fun) when is_function(fun, 1) do
    IO.warn "Enum.group_by/3 with a map/dictionary as second element is deprecated. " <>
      "A map is used by default and it is no longer required to pass one to this function"
    reduce(reverse(enumerable), dict, fn(entry, categories) ->
      Dict.update(categories, fun.(entry), [entry], &[entry | &1])
    end)
  end

  @doc """
  Intersperses `element` between each element of the enumeration.

  Complexity: O(n).

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
      reduce(enumerable, [], fn(x, acc) ->
        [x, element | acc]
      end) |> :lists.reverse()

    case list do
      []      -> []
      [_ | t] -> t  # Head is a superfluous intersperser element
    end
  end

  @doc """
  Inserts the given `enumerable` into a `collectable`.

  ## Examples

      iex> Enum.into([1, 2], [0])
      [0, 1, 2]

      iex> Enum.into([a: 1, b: 2], %{})
      %{a: 1, b: 2}

      iex> Enum.into(%{a: 1}, %{b: 2})
      %{a: 1, b: 2}

      iex> Enum.into([a: 1, a: 2], %{})
      %{a: 2}

  """
  @spec into(Enumerable.t, Collectable.t) :: Collectable.t
  def into(enumerable, collectable) when is_list(collectable) do
    collectable ++ to_list(enumerable)
  end

  def into(%_{} = enumerable, collectable) do
    do_into(enumerable, collectable)
  end

  def into(enumerable, %_{} = collectable) do
    do_into(enumerable, collectable)
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
    do_into(enumerable, collectable)
  end

  defp do_into(enumerable, collectable) do
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

  """
  @spec into(Enumerable.t, Collectable.t, (term -> term)) :: Collectable.t

  def into(enumerable, collectable, transform)
      when is_list(collectable) and is_function(transform, 1) do
    collectable ++ map(enumerable, transform)
  end

  def into(enumerable, collectable, transform)
      when is_function(transform, 1) do
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
        stacktrace = System.stacktrace
        fun.(initial, :halt)
        :erlang.raise(kind, reason, stacktrace)
    else
      acc -> fun.(acc, :done)
    end
  end

  @doc """
  Joins the given enumerable into a binary using `joiner` as a
  separator.

  If `joiner` is not passed at all, it defaults to the empty binary.

  All items in the enumerable must be convertible to a binary,
  otherwise an error is raised.

  ## Examples

      iex> Enum.join([1, 2, 3])
      "123"

      iex> Enum.join([1, 2, 3], " = ")
      "1 = 2 = 3"

  """
  @spec join(t, String.t) :: String.t
  def join(enumerable, joiner \\ "")

  def join(enumerable, joiner) when is_binary(joiner) do
    reduced = reduce(enumerable, :first, fn
      entry, :first -> enum_to_string(entry)
      entry, acc -> [acc, joiner | enum_to_string(entry)]
    end)
    if reduced == :first do
      ""
    else
      IO.iodata_to_binary reduced
    end
  end

  @doc """
  Returns a list where each item is the result of invoking
  `fun` on each corresponding item of `enumerable`.

  For maps, the function expects a key-value tuple.

  ## Examples

      iex> Enum.map([1, 2, 3], fn(x) -> x * 2 end)
      [2, 4, 6]

      iex> Enum.map([a: 1, b: 2], fn({k, v}) -> {k, -v} end)
      [a: -1, b: -2]

  """
  @spec map(t, (element -> any)) :: list
  def map(enumerable, fun)

  def map(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    :lists.map(fun, enumerable)
  end

  def map(enumerable, fun) when is_function(fun, 1) do
    reduce(enumerable, [], R.map(fun)) |> :lists.reverse
  end

  @doc """
  Returns a list of results of invoking `fun` on every `nth`
  item of `enumerable`, starting with the first element.

  The first item is always passed to the given function.

  The second argument specifying every `nth` item must be a non-negative
  integer.

  ## Examples

      iex> Enum.map_every(1..10, 2, fn(x) -> x * 2 end)
      [2, 2, 6, 4, 10, 6, 14, 8, 18, 10]

      iex> Enum.map_every(1..5, 0, fn(x) -> x * 2 end)
      [1, 2, 3, 4, 5]

      iex> Enum.map_every([1, 2, 3], 1, fn(x) -> x * 2 end)
      [2, 4, 6]

  """
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
  Maps and joins the given enumerable in one pass.

  `joiner` can be either a binary or a list and the result will be of
  the same type as `joiner`.
  If `joiner` is not passed at all, it defaults to an empty binary.

  All items in the enumerable must be convertible to a binary,
  otherwise an error is raised.

  ## Examples

      iex> Enum.map_join([1, 2, 3], &(&1 * 2))
      "246"

      iex> Enum.map_join([1, 2, 3], " = ", &(&1 * 2))
      "2 = 4 = 6"

  """
  @spec map_join(t, String.t, (element -> any)) :: String.t
  def map_join(enumerable, joiner \\ "", mapper)

  def map_join(enumerable, joiner, mapper) when is_binary(joiner) and is_function(mapper, 1) do
    reduced = reduce(enumerable, :first, fn
      entry, :first -> enum_to_string(mapper.(entry))
      entry, acc    -> [acc, joiner | enum_to_string(mapper.(entry))]
    end)

    if reduced == :first do
      ""
    else
      IO.iodata_to_binary reduced
    end
  end

  @doc """
  Invokes the given function to each item in the enumerable to reduce
  it to a single element, while keeping an accumulator.

  Returns a tuple where the first element is the mapped enumerable and
  the second one is the final accumulator.

  The function, `fun`, receives two arguments: the first one is the
  element, and the second one is the accumulator. `fun` must return
  a tuple with two elements in the form of `{result, accumulator}`.

  For maps, the first tuple element must be a `{key, value}` tuple.

  ## Examples

      iex> Enum.map_reduce([1, 2, 3], 0, fn(x, acc) -> {x * 2, x + acc} end)
      {[2, 4, 6], 6}

  """
  @spec map_reduce(t, any, (element, any -> {any, any})) :: {any, any}
  def map_reduce(enumerable, acc, fun) when is_list(enumerable) and is_function(fun) do
    :lists.mapfoldl(fun, acc, enumerable)
  end

  def map_reduce(enumerable, acc, fun) when is_function(fun, 2) do
    {list, acc} = reduce(enumerable, {[], acc},
      fn(entry, {list, acc}) ->
        {new_entry, acc} = fun.(entry, acc)
        {[new_entry | list], acc}
    end)
    {:lists.reverse(list), acc}
  end

  @doc """
  Returns the maximal element in the enumerable according
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

  """
  @spec max(t, (() -> empty_result)) :: element | empty_result | no_return when empty_result: any
  def max(enumerable, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def max(enumerable, empty_fallback) when is_function(empty_fallback, 0) do
    aggregate(enumerable, &(&1), &Kernel.max/2, empty_fallback)
  end

  @doc """
  Returns the maximal element in the enumerable as calculated
  by the given function.

  If multiple elements are considered maximal, the first one that was found
  is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.max_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "aaa"

      iex> Enum.max_by(["a", "aa", "aaa", "b", "bbb"], &String.length/1)
      "aaa"

      iex> Enum.max_by([], &String.length/1, fn -> nil end)
      nil

  """
  @spec max_by(t, (element -> any), (() -> empty_result)) :: element | empty_result | no_return when empty_result: any
  def max_by(enumerable, fun, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def max_by(enumerable, fun, empty_fallback) when is_function(fun, 1) and is_function(empty_fallback, 0) do
    aggregate_by(enumerable, &{&1, fun.(&1)}, fn entry, {_, fun_max} = old ->
      fun_entry = fun.(entry)
      if(fun_entry > fun_max, do: {entry, fun_entry}, else: old)
    end, empty_fallback)
  end

  @doc """
  Checks if `element` exists within the enumerable.

  Membership is tested with the match (`===`) operator.

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
          _, _                    -> {:cont, false}
        end) |> elem(1)
    end
  end

  @doc """
  Returns the minimal element in the enumerable according
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

  """
  @spec min(t, (() -> empty_result)) :: element | empty_result | no_return when empty_result: any
  def min(enumerable, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def min(enumerable, empty_fallback) when is_function(empty_fallback, 0) do
    aggregate(enumerable, &(&1), &Kernel.min/2, empty_fallback)
  end

  @doc """
  Returns the minimal element in the enumerable as calculated
  by the given function.

  If multiple elements are considered minimal, the first one that was found
  is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "a"

      iex> Enum.min_by(["a", "aa", "aaa", "b", "bbb"], &String.length/1)
      "a"

      iex> Enum.min_by([], &String.length/1, fn -> nil end)
      nil

  """
  @spec min_by(t, (element -> any), (() -> empty_result)) :: element | empty_result | no_return when empty_result: any
  def min_by(enumerable, fun, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def min_by(enumerable, fun, empty_fallback) when is_function(fun, 1) and is_function(empty_fallback, 0) do
    aggregate_by(enumerable, &{&1, fun.(&1)}, fn entry, {_, fun_min} = old ->
      fun_entry = fun.(entry)
      if(fun_entry < fun_min, do: {entry, fun_entry}, else: old)
    end, empty_fallback)
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
  @spec min_max(t, (() -> empty_result)) :: {element, element} | empty_result | no_return when empty_result: any
  def min_max(enumerable, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def min_max(enumerable, empty_fallback) when is_function(empty_fallback, 0) do
    aggregate(enumerable, &{&1, &1}, fn entry, {min_value, max_value} ->
      {Kernel.min(entry, min_value), Kernel.max(entry, max_value)}
    end, empty_fallback)
  end

  @doc """
  Returns a tuple with the minimal and the maximal elements in the
  enumerable as calculated by the given function.

  If multiple elements are considered maximal or minimal, the first one
  that was found is returned.

  Calls the provided `empty_fallback` function and returns its value if
  `enumerable` is empty. The default `empty_fallback` raises `Enum.EmptyError`.

  ## Examples

      iex> Enum.min_max_by(["aaa", "bb", "c"], fn(x) -> String.length(x) end)
      {"c", "aaa"}

      iex> Enum.min_max_by(["aaa", "a", "bb", "c", "ccc"], &String.length/1)
      {"a", "aaa"}

      iex> Enum.min_max_by([], &String.lenth/1, fn -> {nil, nil} end)
      {nil, nil}

  """
  @spec min_max_by(t, (element -> any), (() -> empty_result)) :: {element, element} | empty_result | no_return when empty_result: any
  def min_max_by(enumerable, fun, empty_fallback \\ fn -> raise Enum.EmptyError end)

  def min_max_by(enumerable, fun, empty_fallback) when is_function(fun, 1) and is_function(empty_fallback, 0) do
    aggregate_by(enumerable,
      fn entry ->
        fun_entry = fun.(entry)
        {{entry, entry}, {fun_entry, fun_entry}}
      end,
      fn entry, {{prev_min, prev_max}, {fun_min, fun_max}} = acc ->
        fun_entry = fun.(entry)
        cond do
          fun_entry < fun_min ->
            {{entry, prev_max}, {fun_entry, fun_max}}
          fun_entry > fun_max ->
            {{prev_min, entry}, {fun_min, fun_entry}}
          true ->
            acc
        end
      end,
      empty_fallback)
  end

  defp aggregate([head | tail], first, fun, _empty) do
    :lists.foldl(fun, first.(head), tail)
  end
  defp aggregate(enumerable, first, fun, empty) do
    ref = make_ref()
    reduce(enumerable, ref, fn
      element, ^ref -> first.(element)
      element, acc -> fun.(element, acc)
    end) |> apply_if_ref_or_return(ref, empty)
  end

  defp apply_if_ref_or_return(ref, ref, fun), do: fun.()
  defp apply_if_ref_or_return(val, _, _fun), do: val

  defp aggregate_by([head | tail], first, fun, _empty) do
    :lists.foldl(fun, first.(head), tail) |> elem(0)
  end
  defp aggregate_by(enumerable, first, fun, empty) do
    reduce(enumerable, :empty, fn
      element, :empty -> first.(element)
      element, acc -> fun.(element, acc)
    end) |> apply_if_empty_or_zeroth(empty)
  end

  defp apply_if_empty_or_zeroth(:empty, fun), do: fun.()
  defp apply_if_empty_or_zeroth(tuple, _fun) when is_tuple(tuple), do: elem(tuple, 0)

  @doc """
  Splits the `enumerable` in two lists according to the given function `fun`.

  Splits the given `enumerable` in two lists by calling `fun` with each element
  in the `enumerable` as its only argument. Returns a tuple with the first list
  containing all the elements in `enumerable` for which applying `fun` returned
  a truthy value, and a second list with all the elements for which applying
  `fun` returned a falsey value (`false` or `nil`).

  The elements in both the returned lists are in the same relative order as they
  were in the original enumerable (if such enumerable was ordered, e.g., a
  list); see the examples below.

  ## Examples

      iex> Enum.split_with([5, 4, 3, 2, 1, 0], fn(x) -> rem(x, 2) == 0 end)
      {[4, 2, 0], [5, 3, 1]}

      iex> Enum.split_with(%{a: 1, b: -2, c: 1, d: -3}, fn({_k, v}) -> v < 0 end)
      {[b: -2, d: -3], [a: 1, c: 1]}

      iex> Enum.split_with(%{a: 1, b: -2, c: 1, d: -3}, fn({_k, v}) -> v > 50 end)
      {[], [a: 1, b: -2, c: 1, d: -3]}

      iex> Enum.split_with(%{}, fn({_k, v}) -> v > 50 end)
      {[], []}

  """
  @spec split_with(t, (element -> any)) :: {list, list}
  def split_with(enumerable, fun) when is_function(fun, 1) do
    {acc1, acc2} =
      reduce(enumerable, {[], []}, fn(entry, {acc1, acc2}) ->
        if fun.(entry) do
          {[entry | acc1], acc2}
        else
          {acc1, [entry | acc2]}
        end
      end)
    {:lists.reverse(acc1), :lists.reverse(acc2)}
  end

  @doc false
  # TODO: Deprecate by v1.5
  @spec partition(t, (element -> any)) :: {list, list}
  def partition(enumerable, fun) when is_function(fun, 1) do
    split_with(enumerable, fun)
  end

  @doc """
  Returns a random element of an enumerable.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  This function uses Erlang's `:rand` module to calculate
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
      2
      iex> Enum.random([1, 2, 3])
      1
      iex> Enum.random(1..1_000)
      776

  """
  @spec random(t) :: element | no_return
  def random(enumerable)

  def random(first..last),
    do: random_integer(first, last)

  def random(enumerable) do
    case Enumerable.count(enumerable) do
      {:ok, 0} ->
        raise Enum.EmptyError
      {:ok, count} ->
        at(enumerable, random_integer(0, count - 1))
      {:error, _} ->
        case take_random(enumerable, 1) do
          []     -> raise Enum.EmptyError
          [elem] -> elem
        end
    end
  end

  @doc """
  Invokes `fun` for each element in the `enumerable`, passing that
  element and the accumulator as arguments. `fun`'s return value
  is stored in the accumulator.

  The first element of the enumerable is used as the initial value of
  the accumulator.
  If you wish to use another value for the accumulator, use
  `Enumerable.reduce/3`.
  This function won't call the specified function for enumerables that
  are one-element long.

  Returns the accumulator.

  Note that since the first element of the enumerable is used as the
  initial value of the accumulator, `fun` will only be executed `n - 1`
  times where `n` is the length of the enumerable.

  ## Examples

      iex> Enum.reduce([1, 2, 3, 4], fn(x, acc) -> x * acc end)
      24

  """
  @spec reduce(t, (element, any -> any)) :: any
  def reduce(enumerable, fun)

  def reduce([h | t], fun) when is_function(fun, 2) do
    reduce(t, h, fun)
  end

  def reduce([], _fun) do
    raise Enum.EmptyError
  end

  def reduce(enumerable, fun) when is_function(fun, 2) do
    result =
      Enumerable.reduce(enumerable, {:cont, :first}, fn
        x, :first ->
          {:cont, {:acc, x}}
        x, {:acc, acc} ->
          {:cont, {:acc, fun.(x, acc)}}
      end) |> elem(1)

    case result do
      :first      -> raise Enum.EmptyError
      {:acc, acc} -> acc
    end
  end

  @doc """
  Invokes `fun` for each element in the `enumerable`, passing that
  element and the accumulator `acc` as arguments. `fun`'s return value
  is stored in `acc`.

  Returns the accumulator.

  ## Examples

      iex> Enum.reduce([1, 2, 3], 0, fn(x, acc) -> x + acc end)
      6

  """
  @spec reduce(t, any, (element, any -> any)) :: any
  def reduce(enumerable, acc, fun) when is_list(enumerable) and is_function(fun, 2) do
    :lists.foldl(fun, acc, enumerable)
  end

  def reduce(first..last, acc, fun) when is_function(fun, 2) do
    if first <= last do
      reduce_range_inc(first, last, acc, fun)
    else
      reduce_range_dec(first, last, acc, fun)
    end
  end

  def reduce(%{__struct__: _} = enumerable, acc, fun) when is_function(fun, 2) do
    Enumerable.reduce(enumerable, {:cont, acc},
                      fn x, acc -> {:cont, fun.(x, acc)} end) |> elem(1)
  end

  def reduce(%{} = enumerable, acc, fun) when is_function(fun, 2) do
    :maps.fold(fn k, v, acc -> fun.({k, v}, acc) end, acc, enumerable)
  end

  def reduce(enumerable, acc, fun) when is_function(fun, 2) do
    Enumerable.reduce(enumerable, {:cont, acc},
                      fn x, acc -> {:cont, fun.(x, acc)} end) |> elem(1)
  end

  defp reduce_range_inc(first, first, acc, fun) do
    fun.(first, acc)
  end

  defp reduce_range_inc(first, last, acc, fun) do
    reduce_range_inc(first + 1, last, fun.(first, acc), fun)
  end

  defp reduce_range_dec(first, first, acc, fun) do
    fun.(first, acc)
  end

  defp reduce_range_dec(first, last, acc, fun)  do
    reduce_range_dec(first - 1, last, fun.(first, acc), fun)
  end

  @doc """
  Reduces the enumerable until `fun` returns `{:halt, term}`.

  The return value for `fun` is expected to be

    * `{:cont, acc}` to continue the reduction with `acc` as the new
      accumulator or
    * `{:halt, acc}` to halt the reduction and return `acc` as the return
      value of this function

  ## Examples

      iex> Enum.reduce_while(1..100, 0, fn i, acc ->
      ...>   if i < 3, do: {:cont, acc + i}, else: {:halt, acc}
      ...> end)
      3

  """
  @spec reduce_while(t, any, (element, any -> {:cont, any} | {:halt, any})) :: any
  def reduce_while(enumerable, acc, fun) when is_function(fun, 2) do
    Enumerable.reduce(enumerable, {:cont, acc}, fun) |> elem(1)
  end

  @doc """
  Returns elements of `enumerable` for which the function `fun` returns
  `false` or `nil`.

  See also `filter/2`.

  ## Examples

      iex> Enum.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [1, 3]

  """
  @spec reject(t, (element -> as_boolean(term))) :: list
  def reject(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    for item <- enumerable, !fun.(item), do: item
  end

  def reject(enumerable, fun) when is_function(fun, 1) do
    reduce(enumerable, [], R.reject(fun)) |> :lists.reverse
  end

  @doc """
  Returns a list of elements in `enumerable` in reverse order.

  ## Examples

      iex> Enum.reverse([1, 2, 3])
      [3, 2, 1]

  """
  @spec reverse(t) :: list
  def reverse(enumerable) do
    reverse(enumerable, [])
  end

  @doc """
  Reverses the elements in `enumerable`, appends the tail, and returns
  it as a list.

  This is an optimization for
  `Enum.concat(Enum.reverse(enumerable), tail)`.

  ## Examples

      iex> Enum.reverse([1, 2, 3], [4, 5, 6])
      [3, 2, 1, 4, 5, 6]

  """
  @spec reverse(t, t) :: list
  def reverse(enumerable, tail) when is_list(enumerable) do
    :lists.reverse(enumerable, to_list(tail))
  end

  def reverse(enumerable, tail) do
    reduce(enumerable, to_list(tail), fn(entry, acc) ->
      [entry | acc]
    end)
  end

  @doc """
  Reverses the enumerable in the range from initial position `start`
  through `count` elements.

  If `count` is greater than the size of the rest of the enumerable,
  then this function will reverse the rest of the enumerable.

  ## Examples

      iex> Enum.reverse_slice([1, 2, 3, 4, 5, 6], 2, 4)
      [1, 2, 6, 5, 4, 3]

  """
  @spec reverse_slice(t, non_neg_integer, non_neg_integer) :: list
  def reverse_slice(enumerable, start, count)
      when is_integer(start) and start >= 0 and is_integer(count) and count >= 0 do
    list = reverse(enumerable)
    length = length(list)
    count = Kernel.min(count, length - start)

    if count > 0 do
      reverse_slice(list, length, start + count, count, [])
    else
      :lists.reverse(list)
    end
  end

  @doc """
  Applies the given function to each element in the enumerable,
  storing the result in a list and passing it as the accumulator
  for the next computation.

  ## Examples

      iex> Enum.scan(1..5, &(&1 + &2))
      [1, 3, 6, 10, 15]

  """
  @spec scan(t, (element, any -> any)) :: list
  def scan(enumerable, fun) when is_function(fun, 2) do
    {res, _} = reduce(enumerable, {[], :first}, R.scan2(fun))
    :lists.reverse(res)
  end

  @doc """
  Applies the given function to each element in the enumerable,
  storing the result in a list and passing it as the accumulator
  for the next computation. Uses the given `acc` as the starting value.

  ## Examples

      iex> Enum.scan(1..5, 0, &(&1 + &2))
      [1, 3, 6, 10, 15]

  """
  @spec scan(t, any, (element, any -> any)) :: list
  def scan(enumerable, acc, fun) when is_function(fun, 2) do
    {res, _} = reduce(enumerable, {[], acc}, R.scan3(fun))
    :lists.reverse(res)
  end

  @doc """
  Returns a list with the elements of `enumerable` shuffled.

  This function uses Erlang's `:rand` module to calculate
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
    randomized = reduce(enumerable, [], fn x, acc ->
      [{:rand.uniform, x} | acc]
    end)
    unwrap(:lists.keysort(1, randomized), [])
  end

  @doc """
  Returns a subset list of the given enumerable, from `range.first` to `range.last` positions.

  Given `enumerable`, it drops elements until element position `range.first`,
  then takes elements until element position `range.last` (inclusive).

  Positions are normalized, meaning that negative positions will be counted from the end
  (e.g. `-1` means the last element of the enumerable).
  If `range.last` is out of bounds, then it is assigned as the position of the last element.

  If the normalized `range.first` position is out of bounds of the given enumerable,
  or this one is greater than the normalized `range.last` position, then `[]` is returned.

  ## Examples

      iex> Enum.slice(1..100, 5..10)
      [6, 7, 8, 9, 10, 11]

      iex> Enum.slice(1..10, 5..20)
      [6, 7, 8, 9, 10]

      # last five elements (negative positions)
      iex> Enum.slice(1..30, -5..-1)
      [26, 27, 28, 29, 30]

      # last five elements (mixed positive and negative positions)
      iex> Enum.slice(1..30, 25..-1)
      [26, 27, 28, 29, 30]

      # out of bounds
      iex> Enum.slice(1..10, 11..20)
      []

      # range.first is greater than range.last
      iex> Enum.slice(1..10, 6..5)
      []

  """
  @spec slice(t, Range.t) :: list
  def slice(enumerable, range)

  def slice(enumerable, first..last) do
    {enumerable, count} = enumerable_and_count(enumerable, 0)
    corr_first = if first >= 0, do: first, else: first + count
    corr_last = if last >= 0, do: last, else: last + count
    amount = corr_last - corr_first + 1
    if corr_first >= 0 and amount > 0 do
      slice(enumerable, corr_first, amount)
    else
      []
    end
  end

  @doc """
  Returns a subset list of the given enumerable, from `start` position with `amount` of elements if available.

  Given `enumerable`, it drops elements until element position `start`,
  then takes `amount` of elements until the end of the enumerable.

  If `start` is out of bounds, it returns `[]`.

  If `amount` is greater than `enumerable` length, it returns as many elements as possible.
  If `amount` is zero, then `[]` is returned.

  ## Examples

      iex> Enum.slice(1..100, 5, 10)
      [6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

      # amount to take is greater than the number of elements
      iex> Enum.slice(1..10, 5, 100)
      [6, 7, 8, 9, 10]

      iex> Enum.slice(1..10, 5, 0)
      []

      # out of bound start position
      iex> Enum.slice(1..10, 10, 5)
      []

      # out of bound start position (negative)
      iex> Enum.slice(1..10, -11, 5)
      []

  """
  @spec slice(t, index, non_neg_integer) :: list
  def slice(_enumerable, start, 0) when is_integer(start), do: []

  def slice(enumerable, start, amount)
      when is_integer(start) and start < 0 and is_integer(amount) and amount >= 0 do
    {enumerable, new_start} = enumerable_and_count(enumerable, start)
    if new_start >= 0 do
      slice(enumerable, new_start, amount)
    else
      []
    end
  end

  def slice(first..last, start, amount)
      when is_integer(start) and start >= 0 and is_integer(amount) and amount > 0 do
    case fetch_range(first, last, start) do
      {:ok, sliced_first} ->
        finish = start + amount - 1
        case fetch_range(first, last, finish) do
          {:ok, sliced_last} ->
            reverse(sliced_last..sliced_first)
          :error ->
            reverse(last..sliced_first)
        end
      :error ->
        []
    end
  end

  def slice(enumerable, start, amount)
      when is_list(enumerable) and
           is_integer(start) and start >= 0 and is_integer(amount) and amount > 0 do
    slice_list(enumerable, start, amount)
  end

  def slice(enumerable, start, amount)
      when is_integer(start) and start >= 0 and is_integer(amount) and amount > 0 do
    Enumerable.reduce(enumerable, {:cont, {start, amount, []}}, fn
      _entry, {start, amount, _list} when start > 0 ->
        {:cont, {start - 1, amount, []}}
      entry, {start, amount, list} when amount > 1 ->
        {:cont, {start, amount - 1, [entry | list]}}
      entry, {start, amount, list} ->
        {:halt, {start, amount, [entry | list]}}
    end)
    |> elem(1)
    |> elem(2)
    |> :lists.reverse()
  end

  @doc """
  Sorts the enumerable according to Erlang's term ordering.

  Uses the merge sort algorithm.

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
  Sorts the enumerable by the given function.

  This function uses the merge sort algorithm. The given function should compare
  two arguments, and return `true` if the first argument precedes the second one.

  ## Examples

      iex> Enum.sort([1, 2, 3], &(&1 >= &2))
      [3, 2, 1]

  The sorting algorithm will be stable as long as the given function
  returns `true` for values considered equal:

      iex> Enum.sort ["some", "kind", "of", "monster"], &(byte_size(&1) <= byte_size(&2))
      ["of", "some", "kind", "monster"]

  If the function does not return `true` for equal values, the sorting
  is not stable and the order of equal terms may be shuffled.
  For example:

      iex> Enum.sort ["some", "kind", "of", "monster"], &(byte_size(&1) < byte_size(&2))
      ["of", "kind", "some", "monster"]

  """
  @spec sort(t, (element, element -> boolean)) :: list
  def sort(enumerable, fun) when is_list(enumerable) and is_function(fun, 2) do
    :lists.sort(fun, enumerable)
  end

  def sort(enumerable, fun) when is_function(fun, 2) do
    reduce(enumerable, [], &sort_reducer(&1, &2, fun))
    |> sort_terminator(fun)
  end

  @doc """
  Sorts the mapped results of the enumerable according to the provided `sorter`
  function.

  This function maps each element of the enumerable using the provided `mapper`
  function.  The enumerable is then sorted by the mapped elements
  using the `sorter` function, which defaults to `Kernel.<=/2`

  `sort_by/3` differs from `sort/2` in that it only calculates the
  comparison value for each element in the enumerable once instead of
  once for each element in each comparison.
  If the same function is being called on both element, it's also more
  compact to use `sort_by/3`.

  This technique is also known as a
  _[Schwartzian Transform](https://en.wikipedia.org/wiki/Schwartzian_transform)_,
  or the _Lisp decorate-sort-undecorate idiom_ as the `mapper`
  is decorating the original `enumerable`; then `sorter` is sorting the
  decorations; and finally the enumerable is being undecorated so only
  the original elements remain, but now in sorted order.

  ## Examples

  Using the default `sorter` of `<=/2`:

      iex> Enum.sort_by ["some", "kind", "of", "monster"], &byte_size/1
      ["of", "some", "kind", "monster"]

  Using a custom `sorter` to override the order:

      iex> Enum.sort_by ["some", "kind", "of", "monster"], &byte_size/1, &>=/2
      ["monster", "some", "kind", "of"]

  """
  @spec sort_by(t, (element -> mapped_element),
                (mapped_element, mapped_element -> boolean))
                :: list when mapped_element: element

  def sort_by(enumerable, mapper, sorter \\ &<=/2)
      when is_function(mapper, 1) and is_function(sorter, 2) do
    enumerable
    |> map(&{&1, mapper.(&1)})
    |> sort(&sorter.(elem(&1, 1), elem(&2, 1)))
    |> map(&elem(&1, 0))
  end

  @doc """
  Splits the `enumerable` into two enumerables, leaving `count`
  elements in the first one.

  If `count` is a negative number, it starts counting from the
  back to the beginning of the enumerable.

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
    do_split(enumerable, count, [])
  end

  def split(enumerable, count) when count >= 0 do
    {_, list1, list2} =
      reduce(enumerable, {count, [], []},
        fn(entry, {counter, acc1, acc2}) ->
          if counter > 0 do
            {counter - 1, [entry | acc1], acc2}
          else
            {counter, acc1, [entry | acc2]}
          end
      end)

    {:lists.reverse(list1), :lists.reverse(list2)}
  end

  def split(enumerable, count) when count < 0 do
    do_split_reverse(reverse(enumerable), abs(count), [])
  end

  @doc """
  Splits enumerable in two at the position of the element for which
  `fun` returns `false` for the first time.

  ## Examples

      iex> Enum.split_while([1, 2, 3, 4], fn(x) -> x < 3 end)
      {[1, 2], [3, 4]}

  """
  @spec split_while(t, (element -> as_boolean(term))) :: {list, list}
  def split_while(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_split_while(enumerable, fun, [])
  end

  def split_while(enumerable, fun) when is_function(fun, 1) do
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

  def sum(first..first),
    do: first

  def sum(first..last) when last < first,
    do: sum(last..first)

  def sum(first..last) when last > first do
    div((last + first) * (last - first + 1), 2)
  end

  def sum(enumerable) do
    reduce(enumerable, 0, &+/2)
  end

  @doc """
  Takes the first `count` items from the enumerable.

  `count` must be an integer. If a negative `count` is given, the last
  `count` values will be taken.
  For such, the enumerable is fully enumerated keeping up
  to `2 * count` elements in memory. Once the end of the enumerable is
  reached, the last `count` elements are returned.

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
  def take(enumerable, count)

  def take(_enumerable, 0), do: []
  def take([], _count), do: []

  def take(enumerable, count)
      when is_list(enumerable) and is_integer(count) and count > 0 do
    do_take(enumerable, count, [])
  end

  def take(enumerable, count) when is_integer(count) and count > 0 do
    {_, {res, _}} =
      Enumerable.reduce(enumerable, {:cont, {[], count}},
        fn(entry, {list, n}) ->
          case n do
            0 -> {:halt, {list, n}}
            1 -> {:halt, {[entry | list], n - 1}}
            _ -> {:cont, {[entry | list], n - 1}}
          end
      end)
    :lists.reverse(res)
  end

  def take(enumerable, count) when is_integer(count) and count < 0 do
    count = abs(count)

    {_count, buf1, buf2} =
      reduce(enumerable, {0, [], []}, fn entry, {n, buf1, buf2} ->
        buf1  = [entry | buf1]
        n = n + 1
        if n == count do
          {0, [], buf1}
        else
          {n, buf1, buf2}
        end
      end)

    do_take_last(buf1, buf2, count, [])
  end

  defp do_take_last(_buf1, _buf2, 0, acc),
    do: acc
  defp do_take_last([], [], _, acc),
    do: acc
  defp do_take_last([], [h | t], count, acc),
    do: do_take_last([], t, count-1, [h | acc])
  defp do_take_last([h | t], buf2, count, acc),
    do: do_take_last(t, buf2, count-1, [h | acc])

  @doc """
  Returns a list of every `nth` item in the enumerable,
  starting with the first element.

  The first item is always included, unless `nth` is 0.

  The second argument specifying every `nth` item must be a non-negative
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
  Takes `count` random items from `enumerable`.

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

  def take_random(_enumerable, 0),
    do: []
  def take_random(first..first, count) when is_integer(count) and count >= 1,
    do: [first]

  def take_random(enumerable, count) when is_integer(count) and count > 128 do
    reducer = fn(elem, {idx, sample}) ->
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

  def take_random(enumerable, count) when is_integer(count) and count > 0 do
    sample = Tuple.duplicate(nil, count)

    reducer = fn(elem, {idx, sample}) ->
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
    sample |> Tuple.to_list |> take(Kernel.min(count, size))
  end

  defp take_random(_sample, 0, acc), do: acc

  defp take_random(sample, position, acc) do
    position = position - 1
    take_random(sample, position, [Map.get(sample, position) | acc])
  end

  @doc """
  Takes the items from the beginning of the enumerable while `fun` returns
  a truthy value.

  ## Examples

      iex> Enum.take_while([1, 2, 3], fn(x) -> x < 3 end)
      [1, 2]

  """
  @spec take_while(t, (element -> as_boolean(term))) :: list
  def take_while(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_take_while(enumerable, fun, [])
  end

  def take_while(enumerable, fun) when is_function(fun, 1) do
    {_, res} =
      Enumerable.reduce(enumerable, {:cont, []}, fn(entry, acc) ->
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
  def to_list(enumerable) when is_list(enumerable) do
    enumerable
  end

  def to_list(enumerable) do
    reverse(enumerable) |> :lists.reverse
  end

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
  def uniq(enumerable, fun) do
    IO.warn "Enum.uniq/2 is deprecated, use Enum.uniq_by/2 instead"
    uniq_by(enumerable, fun)
  end

  @doc """
  Enumerates the `enumerable`, by removing the elements for which
  function `fun` returned duplicate items.

  The function `fun` maps every element to a term which is used to
  determine if two elements are duplicates.

  The first occurrence of each element is kept.

  ## Example

      iex> Enum.uniq_by([{1, :x}, {2, :y}, {1, :z}], fn {x, _} -> x end)
      [{1, :x}, {2, :y}]

      iex> Enum.uniq_by([a: {:tea, 2}, b: {:tea, 2}, c: {:coffee, 1}], fn {_, y} -> y end)
      [a: {:tea, 2}, c: {:coffee, 1}]

  """
  @spec uniq_by(t, (element -> term)) :: list

  def uniq_by(enumerable, fun) when is_list(enumerable) and is_function(fun, 1) do
    do_uniq(enumerable, %{}, fun, [])
  end

  def uniq_by(enumerable, fun) when is_function(fun, 1) do
    {list, _} = reduce(enumerable, {[], %{}}, R.uniq_by(fun))
    :lists.reverse(list)
  end

  @doc """
  Opposite of `Enum.zip/2`; extracts a two-element tuples from the
  enumerable and groups them together.

  It takes an enumerable with items being two-element tuples and returns
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
    {list1, list2} = reduce(enumerable, {[], []},
      fn({el1, el2}, {list1, list2}) ->
        {[el1 | list1], [el2 | list2]}
    end)

    {:lists.reverse(list1), :lists.reverse(list2)}
  end

  @doc """
  Returns the enumerable with each element wrapped in a tuple
  alongside its index.

  If an `offset` is given, we will index from the given offset instead of from zero.

  ## Examples

      iex> Enum.with_index([:a, :b, :c])
      [a: 0, b: 1, c: 2]

      iex> Enum.with_index([:a, :b, :c], 3)
      [a: 3, b: 4, c: 5]

  """
  @spec with_index(t) :: [{element, index}]
  @spec with_index(t, integer) :: [{element, index}]
  def with_index(enumerable, offset \\ 0) do
    map_reduce(enumerable, offset, fn x, acc ->
      {{x, acc}, acc + 1}
    end) |> elem(0)
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
    do_zip(enumerable1, enumerable2, [])
  end

  def zip(enumerable1, enumerable2) do
    zip([enumerable1, enumerable2])
  end

  @doc """
  Zips corresponding elements from a collection of enumerables
  into one list of tuples.

  The zipping finishes as soon as any enumerable completes.

  ## Examples

      iex> Enum.zip([[1, 2, 3], [:a, :b, :c], ["foo", "bar", "baz"]])
      [{1, :a, "foo"}, {2, :b, "bar"}, {3, :c, "baz"}]

      iex> Enum.zip([[1, 2, 3, 4, 5], [:a, :b, :c]])
      [{1, :a}, {2, :b}, {3, :c}]

  """
  @spec zip([t]) :: t

  def zip([]), do: []

  def zip(enumerables) do
    Stream.zip(enumerables).({:cont, []}, &{:cont, [&1 | &2]})
    |> elem(1)
    |> :lists.reverse
  end

  ## Helpers

  @compile {:inline, enum_to_string: 1, reduce: 3}

  defp enum_to_string(entry) when is_binary(entry), do: entry
  defp enum_to_string(entry), do: String.Chars.to_string(entry)

  defp enumerable_and_count(enumerable, count) when is_list(enumerable) do
    {enumerable, length(enumerable) - abs(count)}
  end

  defp enumerable_and_count(enumerable, count) do
    case Enumerable.count(enumerable) do
      {:ok, result} ->
        {enumerable, result - abs(count)}
      {:error, _module} ->
        map_reduce(enumerable, -abs(count), fn(elem, acc) -> {elem, acc + 1} end)
    end
  end

  defp random_integer(limit, limit) when is_integer(limit),
    do: limit

  defp random_integer(lower_limit, upper_limit) when upper_limit < lower_limit,
    do: random_integer(upper_limit, lower_limit)

  defp random_integer(lower_limit, upper_limit) do
    lower_limit + :rand.uniform(upper_limit - lower_limit + 1) - 1
  end

  ## Implementations

  ## all?

  defp do_all?([h | t], fun) do
    if fun.(h) do
      do_all?(t, fun)
    else
      false
    end
  end

  defp do_all?([], _) do
    true
  end

  ## any?

  defp do_any?([h | t], fun) do
    if fun.(h) do
      true
    else
      do_any?(t, fun)
    end
  end

  defp do_any?([], _) do
    false
  end

  ## drop

  defp drop_list([_ | t], counter) when counter > 0 do
    drop_list(t, counter - 1)
  end

  defp drop_list(list, 0) do
    list
  end

  defp drop_list([], _) do
    []
  end

  ## drop_while

  defp do_drop_while([h | t], fun) do
    if fun.(h) do
      do_drop_while(t, fun)
    else
      [h | t]
    end
  end

  defp do_drop_while([], _) do
    []
  end

  ## fetch

  defp fetch_list([], _index),
    do: :error
  defp fetch_list([head | _], 0),
    do: {:ok, head}
  defp fetch_list([_ | tail], index),
    do: fetch_list(tail, index - 1)

  defp fetch_range(first, last, index) when first <= last and index >= 0 do
    item = first + index
    if item > last, do: :error, else: {:ok, item}
  end

  defp fetch_range(first, last, index) when first <= last  do
    item = last + index + 1
    if item < first, do: :error, else: {:ok, item}
  end

  defp fetch_range(first, last, index) when index >= 0 do
    item = first - index
    if item < last, do: :error, else: {:ok, item}
  end

  defp fetch_range(first, last, index) do
    item = last - index - 1
    if item > first, do: :error, else: {:ok, item}
  end

  ## find

  defp do_find([h | t], default, fun) do
    if fun.(h) do
      h
    else
      do_find(t, default, fun)
    end
  end

  defp do_find([], default, _) do
    default
  end

  ## find_index

  defp do_find_index([h | t], counter, fun) do
    if fun.(h) do
      counter
    else
      do_find_index(t, counter + 1, fun)
    end
  end

  defp do_find_index([], _, _) do
    nil
  end

  ## find_value

  defp do_find_value([h | t], default, fun) do
    fun.(h) || do_find_value(t, default, fun)
  end

  defp do_find_value([], default, _) do
    default
  end

  ## shuffle

  defp unwrap([{_, h} | enumerable], t) do
    unwrap(enumerable, [h | t])
  end

  defp unwrap([], t), do: t

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

  ## slice

  defp slice_list([], _start, _count),
    do: []
  defp slice_list(_list, _start, 0),
    do: []
  defp slice_list([head | tail], 0, count),
    do: [head | slice_list(tail, 0, count - 1)]
  defp slice_list([_ | tail], start, count),
    do: slice_list(tail, start - 1, count)

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

  defp sort_merge(list, fun, true), do:
    reverse_sort_merge(list, [], fun, true)

  defp sort_merge(list, fun, false), do:
    sort_merge(list, [], fun, false)

  defp sort_merge([t1, [h2 | t2] | l], acc, fun, true), do:
    sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, false) | acc], fun, true)

  defp sort_merge([[h2 | t2], t1 | l], acc, fun, false), do:
    sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, false) | acc], fun, false)

  defp sort_merge([l], [], _fun, _bool), do: l

  defp sort_merge([l], acc, fun, bool), do:
    reverse_sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp sort_merge([], acc, fun, bool), do:
    reverse_sort_merge(acc, [], fun, bool)

  defp reverse_sort_merge([[h2 | t2], t1 | l], acc, fun, true), do:
    reverse_sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, true) | acc], fun, true)

  defp reverse_sort_merge([t1, [h2 | t2] | l], acc, fun, false), do:
    reverse_sort_merge(l, [sort_merge1(t1, h2, t2, [], fun, true) | acc], fun, false)

  defp reverse_sort_merge([l], acc, fun, bool), do:
    sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp reverse_sort_merge([], acc, fun, bool), do:
    sort_merge(acc, [], fun, bool)

  defp sort_merge1([h1 | t1], h2, t2, m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge1([], h2, t2, m, _fun, _bool), do:
    :lists.reverse(t2, [h2 | m])

  defp sort_merge2(h1, t1, [h2 | t2], m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge2(h1, t1, [], m, _fun, _bool), do:
    :lists.reverse(t1, [h1 | m])

  ## split

  defp do_split([h | t], counter, acc) when counter > 0 do
    do_split(t, counter - 1, [h | acc])
  end

  defp do_split(list, 0, acc) do
    {:lists.reverse(acc), list}
  end

  defp do_split([], _, acc) do
    {:lists.reverse(acc), []}
  end

  defp do_split_reverse([h | t], counter, acc) when counter > 0 do
    do_split_reverse(t, counter - 1, [h | acc])
  end

  defp do_split_reverse(list, 0, acc) do
    {:lists.reverse(list), acc}
  end

  defp do_split_reverse([], _, acc) do
    {[], acc}
  end

  ## split_while

  defp do_split_while([h | t], fun, acc) do
    if fun.(h) do
      do_split_while(t, fun, [h | acc])
    else
      {:lists.reverse(acc), [h | t]}
    end
  end

  defp do_split_while([], _, acc) do
    {:lists.reverse(acc), []}
  end


  ## take

  defp do_take([h | t], counter, acc) when counter > 0 do
    do_take(t, counter - 1, [h | acc])
  end

  defp do_take(_list, 0, acc) do
    :lists.reverse(acc)
  end

  defp do_take([], _, acc) do
    :lists.reverse(acc)
  end

  ## take_while

  defp do_take_while([h | t], fun, acc) do
    if fun.(h) do
      do_take_while(t, fun, [h | acc])
    else
      :lists.reverse(acc)
    end
  end

  defp do_take_while([], _, acc) do
    :lists.reverse(acc)
  end

  ## uniq

  defp do_uniq([h | t], set, fun, acc) do
    value = fun.(h)
    case set do
      %{^value => true} -> do_uniq(t, set, fun, acc)
      %{} -> do_uniq(t, Map.put(set, value, true), fun, [h | acc])
    end
  end

  defp do_uniq([], _set, _fun, acc) do
    :lists.reverse(acc)
  end

  ## zip

  defp do_zip([h1 | next1], [h2 | next2], acc) do
    do_zip(next1, next2, [{h1, h2} | acc])
  end

  defp do_zip(_, [], acc), do: :lists.reverse(acc)
  defp do_zip([], _, acc), do: :lists.reverse(acc)
end

defimpl Enumerable, for: List do
  def count(_list),
    do: {:error, __MODULE__}

  def member?(_list, _value),
    do: {:error, __MODULE__}

  def reduce(_,       {:halt, acc}, _fun),   do: {:halted, acc}
  def reduce(list,    {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
  def reduce([],      {:cont, acc}, _fun),   do: {:done, acc}
  def reduce([h | t], {:cont, acc}, fun),    do: reduce(t, fun.(h, acc), fun)
end

defimpl Enumerable, for: Map do
  def count(map) do
    {:ok, map_size(map)}
  end

  def member?(map, {key, value}) do
    {:ok, match?({:ok, ^value}, :maps.find(key, map))}
  end

  def member?(_map, _other) do
    {:ok, false}
  end

  def reduce(map, acc, fun) do
    do_reduce(:maps.to_list(map), acc, fun)
  end

  defp do_reduce(_,       {:halt, acc}, _fun),   do: {:halted, acc}
  defp do_reduce(list,    {:suspend, acc}, fun), do: {:suspended, acc, &do_reduce(list, &1, fun)}
  defp do_reduce([],      {:cont, acc}, _fun),   do: {:done, acc}
  defp do_reduce([h | t], {:cont, acc}, fun),    do: do_reduce(t, fun.(h, acc), fun)
end

defimpl Enumerable, for: Function do
  def count(_function),
    do: {:error, __MODULE__}

  def member?(_function, _value),
    do: {:error, __MODULE__}

  def reduce(function, acc, fun) when is_function(function, 2),
    do: function.(acc, fun)
end
