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
        reducer = fn x, acc -> {:cont, [fun.(x)|acc]} end
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

      def reduce(_,     {:halt, acc}, _fun),   do: {:halted, acc}
      def reduce(list,  {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
      def reduce([],    {:cont, acc}, _fun),   do: {:done, acc}
      def reduce([h|t], {:cont, acc}, fun),    do: reduce(t, fun.(h, acc), fun)

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
  @type index :: non_neg_integer
  @type default :: any

  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop skip(acc) do
    acc
  end

  defmacrop next(_, entry, acc) do
    quote do: [unquote(entry)|unquote(acc)]
  end

  defmacrop acc(h, n, _) do
    quote do: {unquote(h), unquote(n)}
  end

  defmacrop next_with_acc(f, entry, h, n, _) do
    quote do
      {[unquote(entry)|unquote(h)], unquote(n)}
    end
  end

  @doc """
  Invokes the given `fun` for each item in the enumerable.
  It stops the iteration at the first invocation that returns `false` or `nil`.
  It returns `false` if at least one invocation returns `false` or `nil`.
  Otherwise returns `true`.

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

  def all?(enumerable, fun) when is_list(enumerable) do
    do_all?(enumerable, fun)
  end

  def all?(enumerable, fun) do
    Enumerable.reduce(enumerable, {:cont, true}, fn(entry, _) ->
      if fun.(entry), do: {:cont, true}, else: {:halt, false}
    end) |> elem(1)
  end

  @doc """
  Invokes the given `fun` for each item in the enumerable.
  It stops the iteration at the first invocation that returns a truthy value.
  Returns `true` if at least one invocation returns a truthy value.
  Otherwise returns `false`.

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

  def any?(enumerable, fun) when is_list(enumerable) do
    do_any?(enumerable, fun)
  end

  def any?(enumerable, fun) do
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
  @spec at(t, integer, default) :: element | default
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
  elements are taken as necessary from `pad` if it was passed.

  If `pad` is passed and does not have enough elements to fill the
  chunk, then the chunk is returned anyway with less than `count`
  elements.
  If `pad` is not passed at all or is `nil`, then the partial chunk is
  discarded from the result.

  ## Examples

      iex> Enum.chunk([1, 2, 3, 4, 5, 6], 2)
      [[1, 2], [3, 4], [5, 6]]

      iex> Enum.chunk([1, 2, 3, 4, 5, 6], 3, 2)
      [[1, 2, 3], [3, 4, 5]]

      iex> Enum.chunk([1, 2, 3, 4, 5, 6], 3, 2, [7])
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

      iex> Enum.chunk([1, 2, 3, 4, 5, 6], 3, 3, [])
      [[1, 2, 3], [4, 5, 6]]

  """
  @spec chunk(t, pos_integer, pos_integer, t | nil) :: [list]
  def chunk(enumerable, count, step, pad \\ nil) when count > 0
  and step > 0 do
    limit = :erlang.max(count, step)

    {acc, {buffer, i}} =
      reduce(enumerable, {[], {[], 0}}, R.chunk(count, step, limit))

    if is_nil(pad) || i == 0 do
      :lists.reverse(acc)
    else
      buffer = :lists.reverse(buffer, take(pad, count - i))
      :lists.reverse([buffer|acc])
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
    fun = &[&1|&2]
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
  def count(enumerable, fun) do
    Enumerable.reduce(enumerable, {:cont, 0}, fn(entry, acc) ->
      {:cont, if(fun.(entry), do: acc + 1, else: acc)}
    end) |> elem(1)
  end


  @doc """
  Enumerates the `enumerable`, returning a list where all consecutive
  duplicated elements are collapsed to a single element.

  Elements are compared using `===`.

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
  Drops the first `n` items from then enumerable.

  If a negative value `n` is given, the last `n` values will be dropped.

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
  def drop(enumerable, n) when is_list(enumerable) and n >= 0 do
    do_drop(enumerable, n)
  end

  def drop(enumerable, n) when n >= 0 do
    res =
      reduce(enumerable, n, fn
        x, acc when is_list(acc) -> [x|acc]
        x, 0                     -> [x]
        _, acc when acc > 0      -> acc - 1
      end)
    if is_list(res), do: :lists.reverse(res), else: []
  end

  def drop(enumerable, n) when n < 0 do
    do_drop(reverse(enumerable), abs(n)) |> :lists.reverse
  end

  @doc """
  Drops items at the beginning of the enumerable while `fun` returns a
  truthy value.

  ## Examples

      iex> Enum.drop_while([1, 2, 3, 4, 5], fn(x) -> x < 3 end)
      [3, 4, 5]

  """
  @spec drop_while(t, (element -> as_boolean(term))) :: list
  def drop_while(enumerable, fun) when is_list(enumerable) do
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
  def each(enumerable, fun) when is_list(enumerable) do
    :lists.foreach(fun, enumerable)
    :ok
  end

  def each(enumerable, fun) do
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

      iex> Enum.fetch([2, 4, 6], 2)
      {:ok, 6}

      iex> Enum.fetch([2, 4, 6], 4)
      :error

  """
  @spec fetch(t, integer) :: {:ok, element} | :error
  def fetch(enumerable, index) when is_list(enumerable)
  and is_integer(index) and index >= 0 do
    do_fetch(enumerable, index)
  end

  def fetch(enumerable, index) when is_integer(index) and index >= 0 do
    res =
      Enumerable.reduce(enumerable, {:cont, 0}, fn(entry, acc) ->
        if acc == index do
          {:halt, entry}
        else
          {:cont, acc + 1}
        end
      end)

    case res do
      {:halted, entry} -> {:ok, entry}
      {:done, _} -> :error
    end
  end

  def fetch(enumerable, index) when is_integer(index) and index < 0 do
    do_fetch(reverse(enumerable), abs(index + 1))
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
  @spec fetch!(t, integer) :: element | no_return
  def fetch!(enumerable, index) do
    case fetch(enumerable, index) do
      {:ok, h} -> h
      :error     -> raise Enum.OutOfBoundsError
    end
  end

  @doc """
  Filters the enumerable, i.e. returns only those elements
  for which `fun` returns a truthy value.

  ## Examples

      iex> Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [2]

  """
  @spec filter(t, (element -> as_boolean(term))) :: list
  def filter(enumerable, fun) when is_list(enumerable) do
    for item <- enumerable, fun.(item), do: item
  end

  def filter(enumerable, fun) do
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

  def filter_map(enumerable, filter, mapper) when is_list(enumerable) do
    for item <- enumerable, filter.(item), do: mapper.(item)
  end

  def filter_map(enumerable, filter, mapper) do
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

  def find(enumerable, default, fun) when is_list(enumerable) do
    do_find(enumerable, default, fun)
  end

  def find(enumerable, default, fun) do
    Enumerable.reduce(enumerable, {:cont, default}, fn(entry, default) ->
      if fun.(entry), do: {:halt, entry}, else: {:cont, default}
    end) |> elem(1)
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
  @spec find_value(t, any, (element -> any)) :: any | :nil
  def find_value(enumerable, default \\ nil, fun)

  def find_value(enumerable, default, fun) when is_list(enumerable) do
    do_find_value(enumerable, default, fun)
  end

  def find_value(enumerable, default, fun) do
    Enumerable.reduce(enumerable, {:cont, default}, fn(entry, default) ->
      fun_entry = fun.(entry)
      if fun_entry, do: {:halt, fun_entry}, else: {:cont, default}
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
  @spec find_index(t, (element -> any)) :: index | :nil
  def find_index(enumerable, fun) when is_list(enumerable) do
    do_find_index(enumerable, 0, fun)
  end

  def find_index(enumerable, fun) do
    res =
      Enumerable.reduce(enumerable, {:cont, 0}, fn(entry, acc) ->
        if fun.(entry), do: {:halt, acc}, else: {:cont, acc + 1}
      end)

    case res do
      {:halted, entry} -> entry
      {:done, _} -> nil
    end
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

  """
  @spec flat_map(t, (element -> t)) :: list
  def flat_map(enumerable, fun) do
    reduce(enumerable, [], fn(entry, acc) ->
      reduce(fun.(entry), acc, &[&1|&2])
    end) |> :lists.reverse
  end

  @doc """
  Maps and reduces an enumerable, flattening the given results.

  It expects an accumulator and a function that receives each stream
  item, and must return a tuple containing a new stream (often a list)
  with the new accumulator or a tuple with `:halt` as first element and
  the accumulator as second.

  ## Examples

      iex> enum = 1..100
      iex> n = 3
      iex> Enum.flat_map_reduce(enum, 0, fn i, acc ->
      ...>   if acc < n, do: {[i], acc + 1}, else: {:halt, acc}
      ...> end)
      {[1, 2, 3], 3}

  """
  @spec flat_map_reduce(t, acc, fun) :: {[any], any} when
        fun: (element, acc -> {t, acc} | {:halt, acc}),
        acc: any
  def flat_map_reduce(enumerable, acc, fun) do
    {_, {list, acc}} =
      Enumerable.reduce(enumerable, {:cont, {[], acc}},
        fn(entry, {list, acc}) ->
          case fun.(entry, acc) do
            {:halt, acc} ->
              {:halt, {list, acc}}
            {[], acc} ->
              {:cont, {list, acc}}
            {[entry], acc} ->
              {:cont, {[entry|list], acc}}
            {entries, acc} ->
              {:cont, {reduce(entries, list, &[&1|&2]), acc}}
          end
      end)

    {:lists.reverse(list), acc}
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
      []    -> []
      [_|t] -> t  # Head is a superfluous intersperser element
    end
  end

  @doc """
  Inserts the given `enumerable` into a `collectable`.

  ## Examples

      iex> Enum.into([1, 2], [0])
      [0, 1, 2]

      iex> Enum.into([a: 1, b: 2], %{})
      %{a: 1, b: 2}

  """
  @spec into(Enumerable.t, Collectable.t) :: Collectable.t
  def into(enumerable, collectable) when is_list(collectable) do
    collectable ++ to_list(enumerable)
  end

  def into(%{__struct__: _} = enumerable, collectable) do
    do_into(enumerable, collectable)
  end

  def into(enumerable, %{__struct__: _} = collectable) do
    do_into(enumerable, collectable)
  end

  def into(%{} = enumerable, %{} = collectable) do
    Map.merge(collectable, enumerable)
  end

  def into(enumerable, %{} = collectable) when is_list(enumerable) do
    Map.merge(collectable, :maps.from_list(enumerable))
  end

  def into(enumerable, %{} = collectable) do
    reduce(enumerable, collectable, fn {k, v}, acc ->
      Map.put(acc, k, v)
    end)
  end

  def into(enumerable, collectable) do
    do_into(enumerable, collectable)
  end

  defp do_into(enumerable, collectable) do
    {initial, fun} = Collectable.into(collectable)
    into(enumerable, initial, fun, fn x, acc ->
      fun.(acc, {:cont, x})
    end)
  end

  @doc """
  Inserts the given `enumerable` into a `collectable` according to the
  transformation function.

  ## Examples

      iex> Enum.into([2, 3], [3], fn x -> x * 3 end)
      [3, 6, 9]

  """
  @spec into(Enumerable.t, Collectable.t, (term -> term))
    :: Collectable.t

  def into(enumerable, collectable, transform) when is_list(collectable)
    and is_function(transform, 1) do
    collectable ++ map(enumerable, transform)
  end

  def into(enumerable, collectable, transform)
  when is_function(transform, 1) do
    {initial, fun} = Collectable.into(collectable)
    into(enumerable, initial, fun, fn x, acc ->
      fun.(acc, {:cont, transform.(x)})
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
      entry, acc -> [acc, joiner|enum_to_string(entry)]
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

  def map(enumerable, fun) when is_list(enumerable) do
    :lists.map(fun, enumerable)
  end

  def map(enumerable, fun) do
    reduce(enumerable, [], R.map(fun)) |> :lists.reverse
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

  def map_join(enumerable, joiner, mapper) when is_binary(joiner) do
    reduced = reduce(enumerable, :first, fn
      entry, :first -> enum_to_string(mapper.(entry))
      entry, acc    -> [acc, joiner|enum_to_string(mapper.(entry))]
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
  element, and the second one is the accumulator. `fun` must return a
  a tuple with two elements in the form of `{result, accumulator}`.

  For maps, the first tuple element must be a `{key, value}` tuple.

  ## Examples

      iex> Enum.map_reduce([1, 2, 3], 0, fn(x, acc) -> {x * 2, x + acc} end)
      {[2, 4, 6], 6}

  """
  @spec map_reduce(t, any, (element, any -> {any, any})) :: {any, any}
  def map_reduce(enumerable, acc, fun) when is_list(enumerable) do
    :lists.mapfoldl(fun, acc, enumerable)
  end

  def map_reduce(enumerable, acc, fun) do
    {list, acc} = reduce(enumerable, {[], acc},
      fn(entry, {list, acc}) ->
        {new_entry, acc} = fun.(entry, acc)
        {[new_entry|list], acc}
    end)
    {:lists.reverse(list), acc}
  end

  @doc """
  Returns the biggest of the elements in the enumerable according
  to Erlang's term ordering.

  If more than one elements compare equal, the first one that was found
  is returned.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  ## Examples

      iex> Enum.max([1, 2, 3])
      3

  """
  @spec max(t) :: element | no_return
  def max(enumerable) do
    reduce(enumerable, &Kernel.max(&1, &2))
  end

  @doc """
  Returns the biggest of the elements in the enumerable as calculated
  by the given function.

  If more than one elements compare equal, the first one that was found
  is returned.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  ## Examples

      iex> Enum.max_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "aaa"

  """
  @spec max_by(t, (element -> any)) :: element | no_return
  def max_by([h|t], fun) do
    reduce(t, {h, fun.(h)}, fn(entry, {_, fun_max} = old) ->
      fun_entry = fun.(entry)
      if(fun_entry > fun_max, do: {entry, fun_entry}, else: old)
    end) |> elem(0)
  end

  def max_by([], _fun) do
    raise Enum.EmptyError
  end

  def max_by(enumerable, fun) do
    result =
      reduce(enumerable, :first, fn
        entry, {_, fun_max} = old ->
          fun_entry = fun.(entry)
          if(fun_entry > fun_max, do: {entry, fun_entry}, else: old)
        entry, :first ->
          {entry, fun.(entry)}
      end)

    case result do
      :first       -> raise Enum.EmptyError
      {entry, _} -> entry
    end
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
  Returns the smallest of the elements in the enumerable according
  to Erlang's term ordering.

  If more than one elements compare equal, the first one that was found
  is returned.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  ## Examples

      iex> Enum.min([1, 2, 3])
      1

  """
  @spec min(t) :: element | no_return
  def min(enumerable) do
    reduce(enumerable, &Kernel.min(&1, &2))
  end

  @doc """
  Returns the smallest of the elements in the enumerable as calculated
  by the given function.

  If more than one elements compare equal, the first one that was found
  is returned.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  ## Examples

      iex> Enum.min_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "a"

  """
  @spec min_by(t, (element -> any)) :: element | no_return
  def min_by([h|t], fun) do
    reduce(t, {h, fun.(h)}, fn(entry, {_, fun_min} = old) ->
      fun_entry = fun.(entry)
      if(fun_entry < fun_min, do: {entry, fun_entry}, else: old)
    end) |> elem(0)
  end

  def min_by([], _fun) do
    raise Enum.EmptyError
  end

  def min_by(enumerable, fun) do
    result =
      reduce(enumerable, :first, fn
        entry, {_, fun_min} = old ->
          fun_entry = fun.(entry)
          if(fun_entry < fun_min, do: {entry, fun_entry}, else: old)
        entry, :first ->
          {entry, fun.(entry)}
      end)

    case result do
      :first       -> raise Enum.EmptyError
      {entry, _} -> entry
    end
  end

  @doc """
  Returns a tuple with the smallest and the biggest elements in the
  enumerable according to Erlang's term ordering.

  If more than one elements compare equal, the first one that was found
  is picked.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  ## Examples

      iex> Enum.min_max([2, 3, 1])
      {1, 3}

  """
  @spec min_max(t) :: {element, element} | no_return
  def min_max(enumerable) do
    result =
      Enum.reduce(enumerable, :first, fn
        entry, {min_value, max_value} ->
          {Kernel.min(entry, min_value), Kernel.max(entry, max_value)}
        entry, :first ->
          {entry, entry}
      end)

    case result do
      :first -> raise Enum.EmptyError
      result -> result
    end
  end

  @doc """
  Returns a tuple with the smallest and the biggest elements in the
  enumerable as calculated by the given function.

  If more than one elements compare equal, the first one that was found
  is picked.

  Raises `Enum.EmptyError` if `enumerable` is empty.

  ## Examples

      iex> Enum.min_max_by(["aaa", "bb", "c"], fn(x) -> String.length(x) end)
      {"c", "aaa"}

  """
  @spec min_max_by(t, (element -> any)) :: {element, element} | no_return
  def min_max_by(enumerable, fun) do
    result =
      Enum.reduce(enumerable, :first, fn
        entry, {{_, fun_min} = acc_min, {_, fun_max} = acc_max} ->
          fun_entry = fun.(entry)
          acc_min = if fun_entry < fun_min, do: {entry, fun_entry}, else: acc_min
          acc_max = if fun_entry > fun_max, do: {entry, fun_entry}, else: acc_max
          {acc_min, acc_max}
        entry, :first ->
          fun_entry = fun.(entry)
          {{entry, fun_entry}, {entry, fun_entry}}
      end)

    case result do
      :first ->
        raise Enum.EmptyError
      {{min_entry, _}, {max_entry, _}} ->
        {min_entry, max_entry}
    end
  end

  @doc """
  Returns the sum of all elements.

  Raises `ArithmeticError` if `enumerable` contains a non-numeric value.

  ## Examples

      iex> Enum.sum([1, 2, 3])
      6

  """
  @spec sum(t) :: number
  def sum(enumerable) do
    reduce(enumerable, 0, &+/2)
  end

  @doc """
  Partitions `enumerable` into two enumerables, where the first one
  contains elements for which `fun` returns a truthy value, and the
  second one – for which `fun` returns `false` or `nil`.

  ## Examples

      iex> Enum.partition([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      {[2], [1, 3]}

  """
  @spec partition(t, (element -> any)) :: {list, list}
  def partition(enumerable, fun) do
    {acc1, acc2} =
      reduce(enumerable, {[], []}, fn(entry, {acc1, acc2}) ->
        if fun.(entry) do
          {[entry|acc1], acc2}
        else
          {acc1, [entry|acc2]}
        end
      end)

    {:lists.reverse(acc1), :lists.reverse(acc2)}
  end

  @doc """
  Splits the enumerable into groups based on `fun`.

  The result is a map where each key is a group and each value is
  a list of elements from enumerable for which `fun` returned that
  group. Ordering is preserved.

  ## Examples

      iex> Enum.group_by(~w{ant buffalo cat dingo}, &String.length/1)
      %{3 => ["ant", "cat"], 7 => ["buffalo"], 5 => ["dingo"]}

  """
  @spec group_by(t, (element -> any)) :: map
  def group_by(enumerable, map \\ %{}, fun)

  def group_by(enumerable, %{__struct__: _} = dict, fun) do
    group_by_dict(enumerable, dict, fun)
  end

  def group_by(enumerable, map, fun) when is_map(map) do
    reduce(reverse(enumerable), map, fn entry, categories ->
      Map.update(categories, fun.(entry), [entry], &[entry|&1])
    end)
  end

  def group_by(enumerable, dict, fun) do
    group_by_dict(enumerable, dict, fun)
  end

  defp group_by_dict(enumerable, dict, fun) do
    IO.write :stderr, "warning: Enum.group_by/3 with a dictionary is deprecated, please use a map instead\n" <>
                      Exception.format_stacktrace
    reduce(reverse(enumerable), dict, fn(entry, categories) ->
      Dict.update(categories, fun.(entry), [entry], &[entry|&1])
    end)
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
  def reduce(enumerable, acc, fun) when is_list(enumerable) do
    :lists.foldl(fun, acc, enumerable)
  end

  def reduce(%{__struct__: _} = enumerable, acc, fun) do
    Enumerable.reduce(enumerable, {:cont, acc},
                      fn x, acc -> {:cont, fun.(x, acc)} end) |> elem(1)
  end

  def reduce(%{} = enumerable, acc, fun) do
    :maps.fold(fn k, v, acc -> fun.({k, v}, acc) end, acc, enumerable)
  end

  def reduce(enumerable, acc, fun) do
    Enumerable.reduce(enumerable, {:cont, acc},
                      fn x, acc -> {:cont, fun.(x, acc)} end) |> elem(1)
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

  def reduce([h|t], fun) do
    reduce(t, h, fun)
  end

  def reduce([], _fun) do
    raise Enum.EmptyError
  end

  def reduce(enumerable, fun) do
    result =
      Enumerable.reduce(enumerable, {:cont, :first}, fn
        x, :first ->
          {:cont, {:acc, x}}
        x, {:acc, acc} ->
          {:cont, {:acc, fun.(x, acc)}}
      end) |> elem(1)

    case result do
      :first        -> raise Enum.EmptyError
      {:acc, acc} -> acc
    end
  end

  @doc """
  Reduces the enumerable until `halt` is emitted.

  The return value for `fun` is expected to be `{:cont, acc}`, return
  `{:halt, acc}` to end the reduction early.

  Returns the accumulator.

  ## Examples

      iex> Enum.reduce_while(1..100, 0, fn i, acc ->
      ...>   if i < 3, do: {:cont, acc + i}, else: {:halt, acc}
      ...> end)
      3

  """
  def reduce_while(enumerable, acc, fun) do
    Enumerable.reduce(enumerable, {:cont, acc}, fun) |> elem(1)
  end

  @doc """
  Returns elements of `enumerable` for which the function `fun` returns
  `false` or `nil`.

  ## Examples

      iex> Enum.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [1, 3]

  """
  @spec reject(t, (element -> as_boolean(term))) :: list
  def reject(enumerable, fun) when is_list(enumerable) do
    for item <- enumerable, !fun.(item), do: item
  end

  def reject(enumerable, fun) do
    reduce(enumerable, [], R.reject(fun)) |> :lists.reverse
  end

  @doc """
  Returns a list of elements in `enumerable` in reverse order.

  ## Examples

      iex> Enum.reverse([1, 2, 3])
      [3, 2, 1]

  """
  @spec reverse(t) :: list
  def reverse(enumerable) when is_list(enumerable) do
    :lists.reverse(enumerable)
  end

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
  def reverse(enumerable, tail) when is_list(enumerable)
  and is_list(tail) do
    :lists.reverse(enumerable, tail)
  end

  def reverse(enumerable, tail) do
    reduce(enumerable, to_list(tail), fn(entry, acc) ->
      [entry|acc]
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
  def reverse_slice(enumerable, start, count) when start >= 0
  and count >= 0 do
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

  ## Examples

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsplus, {1, 2, 3})
      iex> Enum.random([1, 2, 3])
      2
      iex> Enum.random([1, 2, 3])
      1

  """
  @spec random(t) :: element | no_return
  def random(enumerable) do
    case take_random(enumerable, 1) do
      [] -> raise Enum.EmptyError
      [e] -> e
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
  def scan(enumerable, fun) do
    {res, _} = reduce(enumerable, {[], :first}, R.scan_2(fun))
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
  def scan(enumerable, acc, fun) do
    {res, _} = reduce(enumerable, {[], acc}, R.scan_3(fun))
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
      [{:rand.uniform, x}|acc]
    end)
    unwrap(:lists.keysort(1, randomized), [])
  end

  @doc """
  Returns a subset list of the given enumerable. Drops elements
  until element position `start`, then takes `count` elements.

  If the count is greater than `enumerable` length, it returns as
  many as possible. If zero, then it returns `[]`.

  ## Examples

      iex> Enum.slice(1..100, 5, 10)
      [6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

      iex> Enum.slice(1..10, 5, 100)
      [6, 7, 8, 9, 10]

      iex> Enum.slice(1..10, 5, 0)
      []

  """
  @spec slice(t, integer, non_neg_integer) :: list

  def slice(_enumerable, start, 0) when is_integer(start), do: []

  def slice(enumerable, start, count) when is_integer(start)
  and start < 0 and is_integer(count) and count >= 0 do
    {list, new_start} = enumerate_and_count(enumerable, start)
    if new_start >= 0 do
      slice(list, new_start, count)
    else
      []
    end
  end

  def slice(enumerable, start, count) when is_list(enumerable)
  and is_integer(start) and start >= 0 and is_integer(count)
  and count > 0 do
    do_slice(enumerable, start, count)
  end

  def slice(enumerable, start, count) when is_integer(start)
  and start >= 0 and is_integer(count) and count > 0 do
    {_, _, list} = Enumerable.reduce(enumerable,
      {:cont, {start, count, []}}, fn
        _entry, {start, count, _list} when start > 0 ->
          {:cont, {start-1, count, []}}
        entry, {start, count, list} when count > 1 ->
          {:cont, {start, count-1, [entry|list]}}
        entry, {start, count, list} ->
          {:halt, {start, count, [entry|list]}}
    end) |> elem(1)

    :lists.reverse(list)
  end

  @doc """
  Returns a subset list of the given enumerable. Drops elements
  until element position `range.first`, then takes elements until
  element position `range.last` (inclusive).

  Positions are calculated by adding the number of items in the
  enumerable to negative positions (e.g. position -3 in an
  enumerable with count 5 becomes position 2).

  The first position (after adding count to negative positions) must be
  smaller or equal to the last position.

  If the start of the range is not a valid offset for the given
  enumerable or if the range is in reverse order, returns `[]`.

  ## Examples

      iex> Enum.slice(1..100, 5..10)
      [6, 7, 8, 9, 10, 11]

      iex> Enum.slice(1..10, 5..20)
      [6, 7, 8, 9, 10]

      iex> Enum.slice(1..10, 11..20)
      []

      iex> Enum.slice(1..10, 6..5)
      []

  """
  @spec slice(t, Range.t) :: list
  def slice(enumerable, range)

  def slice(enumerable, first..last) when is_integer(first)
  and first >= 0 and is_integer(last) and last >= 0 do
    # Simple case, which works on infinite enumerables
    if last - first >= 0 do
      slice(enumerable, first, last - first + 1)
    else
      []
    end
  end

  def slice(enumerable, first..last) when is_integer(first)
  and is_integer(last) do
    {list, count} = enumerate_and_count(enumerable, 0)
    corr_first = if first >= 0, do: first, else: first + count
    corr_last = if last >= 0, do: last, else: last + count
    length = corr_last - corr_first + 1
    if corr_first >= 0 and length > 0 do
      slice(list, corr_first, length)
    else
      []
    end
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
  two arguments, and return `false` if the first argument follows the second one.

  ## Examples

      iex> Enum.sort([1, 2, 3], &(&1 > &2))
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
  def sort(enumerable, fun) when is_list(enumerable) do
    :lists.sort(fun, enumerable)
  end

  def sort(enumerable, fun) do
    reduce(enumerable, [], &sort_reducer(&1, &2, fun))
    |> sort_terminator(fun)
  end

  @doc """
  Sorts the mapped results of the enumerable according to the `sorter`
  function.

  This function maps each element of the enumerable using the `mapper`
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

  def sort_by(enumerable, mapper, sorter \\ &<=/2) do
    enumerable
    |> map(&{&1, mapper.(&1)})
    |> sort(&sorter.(elem(&1, 1), elem(&2, 1)))
    |> map(&elem(&1, 0))
  end

  @doc """
  Splits the `enumerable` into two enumerables, leaving `count`
  elements in the first one. If `count` is a negative number,
  it starts counting from the back to the beginning of the
  enumerable.

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
            {counter - 1, [entry|acc1], acc2}
          else
            {counter, acc1, [entry|acc2]}
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
  def split_while(enumerable, fun) when is_list(enumerable) do
    do_split_while(enumerable, fun, [])
  end

  def split_while(enumerable, fun) do
    {list1, list2} =
      reduce(enumerable, {[], []}, fn
        entry, {acc1, []} ->
          if(fun.(entry), do: {[entry|acc1], []}, else: {acc1, [entry]})
        entry, {acc1, acc2} ->
          {acc1, [entry|acc2]}
      end)

    {:lists.reverse(list1), :lists.reverse(list2)}
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

  def take(enumerable, count) when is_list(enumerable)
  and is_integer(count) and count > 0 do
    do_take(enumerable, count)
  end

  def take(enumerable, count) when is_integer(count) and count > 0 do
    {_, {res, _}} =
      Enumerable.reduce(enumerable, {:cont, {[], count}},
        fn(entry, {list, n}) ->
          case n do
            0 -> {:halt, {list, n}}
            1 -> {:halt, {[entry|list], n - 1}}
            _ -> {:cont, {[entry|list], n - 1}}
          end
      end)
    :lists.reverse(res)
  end

  def take(enumerable, count) when is_integer(count) and count < 0 do
    count = abs(count)

    {_count, buf1, buf2} =
      reduce(enumerable, {0, [], []}, fn entry, {n, buf1, buf2} ->
        buf1  = [entry|buf1]
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
  defp do_take_last([], [h|t], count, acc),
    do: do_take_last([], t, count-1, [h|acc])
  defp do_take_last([h|t], buf2, count, acc),
    do: do_take_last(t, buf2, count-1, [h|acc])

  @doc """
  Returns a list of every `nth` item in the enumerable,
  starting with the first element.

  The first item is always included, unless `nth` is 0.

  The second argument specifying every `nth` item must be a non-negative
  integer, otherwise `FunctionClauseError` will be raised.

  ## Examples

      iex> Enum.take_every(1..10, 2)
      [1, 3, 5, 7, 9]

      iex> Enum.take_every(1..10, 0)
      []

      iex> Enum.take_every([1, 2, 3], 1)
      [1, 2, 3]

  """
  @spec take_every(t, non_neg_integer) :: list | no_return
  def take_every(enumerable, nth)

  def take_every(enumerable, 1), do: to_list(enumerable)
  def take_every(_enumerable, 0), do: []
  def take_every([], _nth), do: []

  def take_every(enumerable, nth) when is_integer(nth) and nth > 0 do
    {res, _} = reduce(enumerable, {[], :first}, R.take_every(nth))
    :lists.reverse(res)
  end

  @doc """
  Takes random items from the enumerable.

  Notice this function will traverse the whole enumerable to
  get the random sublist of `enumerable`.

  See `random/1` for notes on implementation and random seed.

  ## Examples

      # Although not necessary, let's seed the random algorithm
      iex> :rand.seed(:exsplus, {1, 2, 3})
      iex> Enum.take_random(1..10, 2)
      [5, 8]
      iex> Enum.take_random(?a..?z, 5)
      'fhjni'

  """
  @spec take_random(t, integer) :: list
  def take_random(_enumerable, 0), do: []

  def take_random(first..last, 1) when first > last do
    take_random(last..first, 1)
  end

  def take_random(first..last, 1) do
    [random_index(last - first) + first]
  end

  def take_random(enumerable, count) when count > 128 do
    reducer = fn(elem, {idx, sample}) ->
      jdx = random_index(idx)
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

  def take_random(enumerable, count) when count > 0 do
    sample = Tuple.duplicate(nil, count)

    reducer = fn(elem, {idx, sample}) ->
      jdx = random_index(idx)
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
    acc = [Map.get(sample, position) | acc]
    take_random(sample, position, acc)
  end

  @doc """
  Takes the items from the beginning of the enumerable while `fun` returns
  a truthy value.

  ## Examples

      iex> Enum.take_while([1, 2, 3], fn(x) -> x < 3 end)
      [1, 2]

  """
  @spec take_while(t, (element -> as_boolean(term))) :: list
  def take_while(enumerable, fun) when is_list(enumerable) do
    do_take_while(enumerable, fun)
  end

  def take_while(enumerable, fun) do
    {_, res} =
      Enumerable.reduce(enumerable, {:cont, []}, fn(entry, acc) ->
        if fun.(entry) do
          {:cont, [entry|acc]}
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
    # TODO: Deprecate on 1.3 or 1.4 depending on warnings on projects
    # IO.write :stderr, "warning: Enum.uniq/2 is deprecated, please use Enum.uniq_by/2 instead\n" <>
    #                   Exception.format_stacktrace
    uniq_by(enumerable, fun)
  end

  @doc """
  Enumerates the `enumerable`, by removing the elements for which
  function `fun` returned duplicate items.

  The function `fun` maps every element to a term which is used to
  determine if two elements are duplicates.
  ## Example

      iex> Enum.uniq_by([{1, :x}, {2, :y}, {1, :z}], fn {x, _} -> x end)
      [{1, :x}, {2, :y}]

      iex> Enum.uniq_by([a: {:tea, 2}, b: {:tea, 2}, c: {:coffee, 1}],  fn {_, y} -> y end)
      [a: {:tea, 2}, c: {:coffee, 1}]

  """
  @spec uniq_by(t, (element -> term)) :: list

  def uniq_by(enumerable, fun) when is_list(enumerable) do
    do_uniq(enumerable, %{}, fun)
  end

  def uniq_by(enumerable, fun) do
    {list, _} = reduce(enumerable, {[], %{}}, R.uniq(fun))
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
        {[el1|list1], [el2|list2]}
    end)

    {:lists.reverse(list1), :lists.reverse(list2)}
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
  def zip(enumerable1, enumerable2) when is_list(enumerable1)
  and is_list(enumerable2) do
    do_zip(enumerable1, enumerable2)
  end

  def zip(enumerable1, enumerable2) do
    Stream.zip(enumerable1, enumerable2).({:cont, []}, &{:cont, [&1|&2]})
    |> elem(1)
    |> :lists.reverse
  end

  @doc """
  Returns the enumerable with each element wrapped in a tuple
  alongside its index.

  ## Examples

      iex> Enum.with_index([:a, :b, :c])
      [a: 0, b: 1, c: 2]

      iex> Enum.with_index([:a, :b, :c], 3)
      [a: 3, b: 4, c: 5]

  """
  @spec with_index(t) :: [{element, integer}]
  @spec with_index(t, integer) :: [{element, integer}]
  def with_index(enumerable, offset \\ 0) do
    map_reduce(enumerable, offset, fn x, acc ->
      {{x, acc}, acc + 1}
    end) |> elem(0)
  end

  ## Helpers

  @compile {:inline, enum_to_string: 1}

  defp enumerate_and_count(enumerable, count) when is_list(enumerable) do
    {enumerable, length(enumerable) - abs(count)}
  end

  defp enumerate_and_count(enumerable, count) do
    map_reduce(enumerable, -abs(count), fn(x, acc) -> {x, acc + 1} end)
  end

  defp enum_to_string(entry) when is_binary(entry), do: entry
  defp enum_to_string(entry), do: String.Chars.to_string(entry)

  defp random_index(n) do
    :rand.uniform(n + 1) - 1
  end

  ## Implementations

  ## all?

  defp do_all?([h|t], fun) do
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

  defp do_any?([h|t], fun) do
    if fun.(h) do
      true
    else
      do_any?(t, fun)
    end
  end

  defp do_any?([], _) do
    false
  end

  ## fetch

  defp do_fetch([h|_], 0), do: {:ok, h}
  defp do_fetch([_|t], n), do: do_fetch(t, n - 1)
  defp do_fetch([], _),    do: :error

  ## drop

  defp do_drop([_|t], counter) when counter > 0 do
    do_drop(t, counter - 1)
  end

  defp do_drop(list, 0) do
    list
  end

  defp do_drop([], _) do
    []
  end

  ## drop_while

  defp do_drop_while([h|t], fun) do
    if fun.(h) do
      do_drop_while(t, fun)
    else
      [h|t]
    end
  end

  defp do_drop_while([], _) do
    []
  end

  ## find

  defp do_find([h|t], default, fun) do
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

  defp do_find_index([h|t], counter, fun) do
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

  defp do_find_value([h|t], default, fun) do
    fun.(h) || do_find_value(t, default, fun)
  end

  defp do_find_value([], default, _) do
    default
  end

  ## shuffle

  defp unwrap([{_, h} | enumerable], t) do
    unwrap(enumerable, [h|t])
  end

  defp unwrap([], t), do: t

  ## sort

  defp sort_reducer(entry, {:split, y, x, r, rs, bool}, fun) do
    cond do
      fun.(y, entry) == bool ->
        {:split, entry, y, [x|r], rs, bool}
      fun.(x, entry) == bool ->
        {:split, y, entry, [x|r], rs, bool}
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
    [entry|acc]
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
    sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, false) | acc], fun, true)

  defp sort_merge([[h2 | t2], t1 | l], acc, fun, false), do:
    sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, false) | acc], fun, false)

  defp sort_merge([l], [], _fun, _bool), do: l

  defp sort_merge([l], acc, fun, bool), do:
    reverse_sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp sort_merge([], acc, fun, bool), do:
    reverse_sort_merge(acc, [], fun, bool)


  defp reverse_sort_merge([[h2 | t2], t1 | l], acc, fun, true), do:
    reverse_sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, true) | acc], fun, true)

  defp reverse_sort_merge([t1, [h2 | t2] | l], acc, fun, false), do:
    reverse_sort_merge(l, [sort_merge_1(t1, h2, t2, [], fun, true) | acc], fun, false)

  defp reverse_sort_merge([l], acc, fun, bool), do:
    sort_merge([:lists.reverse(l, []) | acc], [], fun, bool)

  defp reverse_sort_merge([], acc, fun, bool), do:
    sort_merge(acc, [], fun, bool)


  defp sort_merge_1([h1 | t1], h2, t2, m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge_2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge_1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge_1([], h2, t2, m, _fun, _bool), do:
    :lists.reverse(t2, [h2 | m])


  defp sort_merge_2(h1, t1, [h2 | t2], m, fun, bool) do
    if fun.(h1, h2) == bool do
      sort_merge_2(h1, t1, t2, [h2 | m], fun, bool)
    else
      sort_merge_1(t1, h2, t2, [h1 | m], fun, bool)
    end
  end

  defp sort_merge_2(h1, t1, [], m, _fun, _bool), do:
    :lists.reverse(t1, [h1 | m])

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

  ## split

  defp do_split([h|t], counter, acc) when counter > 0 do
    do_split(t, counter - 1, [h|acc])
  end

  defp do_split(list, 0, acc) do
    {:lists.reverse(acc), list}
  end

  defp do_split([], _, acc) do
    {:lists.reverse(acc), []}
  end

  defp do_split_reverse([h|t], counter, acc) when counter > 0 do
    do_split_reverse(t, counter - 1, [h|acc])
  end

  defp do_split_reverse(list, 0, acc) do
    {:lists.reverse(list), acc}
  end

  defp do_split_reverse([], _, acc) do
    {[], acc}
  end

  ## split_while

  defp do_split_while([h|t], fun, acc) do
    if fun.(h) do
      do_split_while(t, fun, [h|acc])
    else
      {:lists.reverse(acc), [h|t]}
    end
  end

  defp do_split_while([], _, acc) do
    {:lists.reverse(acc), []}
  end

  ## take

  defp do_take([h|t], counter) when counter > 0 do
    [h|do_take(t, counter - 1)]
  end

  defp do_take(_list, 0) do
    []
  end

  defp do_take([], _) do
    []
  end

  ## take_while

  defp do_take_while([h|t], fun) do
    if fun.(h) do
      [h|do_take_while(t, fun)]
    else
      []
    end
  end

  defp do_take_while([], _) do
    []
  end

  ## uniq

  defp do_uniq([h|t], acc, fun) do
    fun_h = fun.(h)
    if Map.has_key?(acc, fun_h) do
      do_uniq(t, acc, fun)
    else
      [h|do_uniq(t, Map.put(acc, fun_h, true), fun)]
    end
  end

  defp do_uniq([], _acc, _fun) do
    []
  end

  ## zip

  defp do_zip([h1|next1], [h2|next2]) do
    [{h1, h2}|do_zip(next1, next2)]
  end

  defp do_zip(_, []), do: []
  defp do_zip([], _), do: []

  ## slice

  defp do_slice([], _start, _count) do
    []
  end

  defp do_slice(_list, _start, 0) do
    []
  end

  defp do_slice([h|t], 0, count) do
    [h|do_slice(t, 0, count-1)]
  end

  defp do_slice([_|t], start, count) do
    do_slice(t, start-1, count)
  end
end

defimpl Enumerable, for: List do
  def reduce(_,     {:halt, acc}, _fun),   do: {:halted, acc}
  def reduce(list,  {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
  def reduce([],    {:cont, acc}, _fun),   do: {:done, acc}
  def reduce([h|t], {:cont, acc}, fun),    do: reduce(t, fun.(h, acc), fun)

  def member?(_list, _value),
    do: {:error, __MODULE__}
  def count(_list),
    do: {:error, __MODULE__}
end

defimpl Enumerable, for: Map do
  def reduce(map, acc, fun) do
    do_reduce(:maps.to_list(map), acc, fun)
  end

  defp do_reduce(_,     {:halt, acc}, _fun),   do: {:halted, acc}
  defp do_reduce(list,  {:suspend, acc}, fun), do: {:suspended, acc, &do_reduce(list, &1, fun)}
  defp do_reduce([],    {:cont, acc}, _fun),   do: {:done, acc}
  defp do_reduce([h|t], {:cont, acc}, fun),    do: do_reduce(t, fun.(h, acc), fun)

  def member?(map, {key, value}) do
    {:ok, match?({:ok, ^value}, :maps.find(key, map))}
  end

  def member?(_map, _other) do
    {:ok, false}
  end

  def count(map) do
    {:ok, map_size(map)}
  end
end

defimpl Enumerable, for: Function do
  def reduce(function, acc, fun) when is_function(function, 2),
    do: function.(acc, fun)
  def member?(_function, _value),
    do: {:error, __MODULE__}
  def count(_function),
    do: {:error, __MODULE__}
end
