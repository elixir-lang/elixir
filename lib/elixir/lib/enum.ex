defprotocol Enumerable do
  @moduledoc """
  Enumerable protocol used by `Enum` and `Stream` modules.

  When you invoke a function in the `Enum` module, the first argument
  is usually a collection that must implement this protocol. For example,
  the expression

      Enum.map([1, 2, 3], &(&1 * 2))

  invokes underneath `Enumerable.reduce/3` to perform the reducing
  operation that builds a mapped list by calling the mapping function
  `&(&1 * 2)` on every element in the collection and cons'ing the
  element with an accumulated list.

  Internally, `Enum.map/2` is implemented as follows:

      def map(enum, fun) do
        reducer = fn x, acc -> { :cont, [fun.(x)|acc] } end
        Enumerable.reduce(enum, { :cont, [] }, reducer) |> elem(1) |> :lists.reverse()
      end

  Notice the user given function is wrapped into a `reducer` function.
  The `reducer` function must return a tagged tuple after each step,
  as described in the `acc/0` type.

  The reason the accumulator requires a tagged tuple is to allow the
  reducer function to communicate to the underlying enumerable the end
  of enumeration, allowing any open resource to be properly closed. It
  also allows suspension of the enumeration, which is useful when
  interleaving between many enumerables is required (as in zip).

  Finally, `Enumerable.reduce/3` will return another tagged tuple,
  as represented by the `result/0` type.
  """

  @typedoc """
  The accumulator value for each step.

  It must be a tagged tuple with one of the following "tags":

  * `:cont`    - the enumeration should continue
  * `:halt`    - the enumeration should halt immediately
  * `:suspend` - the enumeration should be suspended immediately

  Depending on the accumulator value, the result returned by
  `Enumerable.reduce/3` will change. Please check the `result`
  type docs for more information.

  In case a reducer function returns a `:suspend` accumulator,
  it must be explicitly handled by the caller and never leak.
  """
  @type acc :: { :cont, term } | { :halt, term } | { :suspend, term }

  @typedoc """
  The reducer function.

  Should be called with the collection element and the
  accumulator contents. Returns the accumulator for
  the next enumeration step.
  """
  @type reducer :: (term, term -> acc)

  @typedoc """
  The result of the reduce operation.

  It may be *done* when the enumeration is finished by reaching
  its end, or *halted*/*suspended* when the enumeration was halted
  or suspended by the reducer function.

  In case a reducer function returns the `:suspend` accumulator, the
  `:suspended` tuple must be explicitly handled by the caller and
  never leak. In practice, this means regular enumeration functions
  just need to be concerned about `:done` and `:halted` results.

  Furthermore, a `:suspend` call must always be followed by another call,
  eventually halting or continuing until the end.
  """
  @type result :: { :done, term } | { :halted, term } | { :suspended, term, continuation }

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
  Reduces the collection into a value.

  Most of the operations in `Enum` are implemented in terms of reduce.
  This function should apply the given `reducer` function to each
  item in the collection and proceed as expected by the returned accumulator.

  As an example, here is the implementation of `reduce` for lists:

      def reduce(_,     { :halt, acc }, _fun),   do: { :halted, acc }
      def reduce(list,  { :suspend, acc }, fun), do: { :suspended, acc, &reduce(list, &1, fun) }
      def reduce([],    { :cont, acc }, _fun),   do: { :done, acc }
      def reduce([h|t], { :cont, acc }, fun),    do: reduce(t, fun.(h, acc), fun)

  """
  @spec reduce(t, acc, reducer) :: result
  def reduce(collection, acc, fun)

  @doc """
  Checks if a value exists within the collection.

  It should return `{ :ok, boolean }` if membership can be tested
  faster than linear time with the match (`===`) operator, otherwise
  should return `{ :error, __MODULE__ }`.
  """
  @spec member?(t, term) :: { :ok, boolean } | { :error, module }
  def member?(collection, value)

  @doc """
  Retrieves the collection's size.

  Should return `{ :ok, size }` if the size is pre-calculated,
  `{ :error, __MODULE__ }` otherwise.
  """
  @spec count(t) :: { :ok, non_neg_integer } | { :error, module }
  def count(collection)
end

defmodule Enum do
  import Kernel, except: [max: 2, min: 2]

  @moduledoc """
  Provides a set of algorithms that enumerate over collections according to the
  `Enumerable` protocol:

      iex> Enum.map([1, 2, 3], fn(x) -> x * 2 end)
      [2,4,6]

  Some particular types, like dictionaries, yield a specific format on
  enumeration. For dicts, the argument is always a `{ key, value }` tuple:

      iex> dict = %{a: 1, b: 2}
      iex> Enum.map(dict, fn { k, v } -> { k, v * 2 } end)
      [a: 2, b: 4]

  Note that the functions in the `Enum` module are eager: they always start
  the enumeration of the given collection. The `Stream` module allows
  lazy enumeration of collections and provides infinite streams.

  Since the majority of the functions in `Enum` enumerate the whole
  collection and return a list as result, infinite streams need to
  be carefully used with such functions, as they can potentially run
  forever. For example:

      Enum.each Stream.cycle([1,2,3]), &IO.puts(&1)

  """

  @compile :inline_list_funcs

  @type t :: Enumerable.t
  @type element :: any
  @type index :: non_neg_integer
  @type default :: any

  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop cont(_, entry, acc) do
    quote do: { :cont, [unquote(entry)|unquote(acc)] }
  end

  defmacrop acc(h, n, _) do
    quote do: { unquote(h), unquote(n) }
  end

  defmacrop cont_with_acc(f, entry, h, n, _) do
    quote do
      { :cont, { [unquote(entry)|unquote(h)], unquote(n) } }
    end
  end

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns `false`
  if at least one invocation returns `false`. Otherwise returns `true`.

  ## Examples

      iex> Enum.all?([2, 4, 6], fn(x) -> rem(x, 2) == 0 end)
      true

      iex> Enum.all?([2, 3, 4], fn(x) -> rem(x, 2) == 0 end)
      false

  If no function is given, it defaults to checking if
  all items in the collection evaluate to `true`.

      iex> Enum.all?([1, 2, 3])
      true

      iex> Enum.all?([1, nil, 3])
      false

  """
  @spec all?(t) :: boolean
  @spec all?(t, (element -> as_boolean(term))) :: boolean

  def all?(collection, fun \\ fn(x) -> x end)

  def all?(collection, fun) when is_list(collection) do
    do_all?(collection, fun)
  end

  def all?(collection, fun) do
    Enumerable.reduce(collection, { :cont, true }, fn(entry, _) ->
      if fun.(entry), do: { :cont, true }, else: { :halt, false }
    end) |> elem(1)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection` and returns `true` if
  at least one invocation returns `true`. Returns `false` otherwise.

  ## Examples

      iex> Enum.any?([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
      false

      iex> Enum.any?([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
      true

  If no function is given, it defaults to checking if
  at least one item in the collection evaluates to `true`.

      iex> Enum.any?([false, false, false])
      false

      iex> Enum.any?([false, true, false])
      true

  """
  @spec any?(t) :: boolean
  @spec any?(t, (element -> as_boolean(term))) :: boolean

  def any?(collection, fun \\ fn(x) -> x end)

  def any?(collection, fun) when is_list(collection) do
    do_any?(collection, fun)
  end

  def any?(collection, fun) do
    Enumerable.reduce(collection, { :cont, false }, fn(entry, _) ->
      if fun.(entry), do: { :halt, true }, else: { :cont, false }
    end) |> elem(1)
  end

  @doc """
  Finds the element at the given index (zero-based).
  Returns `default` if index is out of bounds.

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
  @spec at(t, integer) :: element | nil
  @spec at(t, integer, default) :: element | default
  def at(collection, n, default \\ nil) do
    case fetch(collection, n) do
      { :ok, h } -> h
      :error     -> default
    end
  end

  @doc """
  Shortcut to `chunk(coll, n, n)`.
  """
  @spec chunk(t, non_neg_integer) :: [list]
  def chunk(coll, n), do: chunk(coll, n, n, nil)

  @doc """
  Returns a collection of lists containing `n` items each, where
  each new chunk starts `step` elements into the collection.

  `step` is optional and, if not passed, defaults to `n`, i.e.
  chunks do not overlap. If the final chunk does not have `n`
  elements to fill the chunk, elements are taken as necessary
  from `pad` if it was passed. If `pad` is passed and does not
  have enough elements to fill the chunk, then the chunk is
  returned anyway with less than `n` elements. If `pad` is not
  passed at all or is `nil`, then the partial chunk is discarded
  from the result.

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
  @spec chunk(t, non_neg_integer, non_neg_integer) :: [list]
  @spec chunk(t, non_neg_integer, non_neg_integer, t | nil) :: [list]
  def chunk(coll, n, step, pad \\ nil) when n > 0 and step > 0 do
    limit = :erlang.max(n, step)

    { _, { acc, { buffer, i } } } =
      Enumerable.reduce(coll, { :cont, { [], { [], 0 } } }, R.chunk(n, step, limit))

    if nil?(pad) || i == 0 do
      :lists.reverse(acc)
    else
      buffer = :lists.reverse(buffer) ++ take(pad, n - i)
      :lists.reverse([buffer|acc])
    end
  end

  @doc """
  Splits `coll` on every element for which `fun` returns a new value.

  ## Examples

      iex> Enum.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

  """
  @spec chunk_by(t, (element -> any)) :: [list]
  def chunk_by(coll, fun) do
    { _, { acc, res } } =
      Enumerable.reduce(coll, { :cont, { [], nil } }, R.chunk_by(fun))

    case res do
      { buffer, _ } ->
        :lists.reverse([:lists.reverse(buffer) | acc])
      nil ->
        []
    end
  end

  @doc """
  Given an enumerable of enumerables, concatenate the enumerables into a single list.

  ## Examples

      iex> Enum.concat([1..3, 4..6, 7..9])
      [1,2,3,4,5,6,7,8,9]

      iex> Enum.concat([[1, [2], 3], [4], [5, 6]])
      [1,[2],3,4,5,6]

  """
  @spec concat(t) :: t
  def concat(enumerables) do
    do_concat(enumerables)
  end

  @doc """
  Concatenates the enumerable on the right with the enumerable on the left.

  This function produces the same result as the `Kernel.++/2` operator for lists.

  ## Examples

      iex> Enum.concat(1..3, 4..6)
      [1,2,3,4,5,6]

      iex> Enum.concat([1, 2, 3], [4, 5, 6])
      [1,2,3,4,5,6]

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
  Returns the collection's size.

  ## Examples

      iex> Enum.count([1, 2, 3])
      3

  """
  @spec count(t) :: non_neg_integer
  def count(collection) when is_list(collection) do
    :erlang.length(collection)
  end

  def count(collection) do
    case Enumerable.count(collection) do
      { :ok, value } when is_integer(value) ->
        value
      { :error, module } ->
        module.reduce(collection, { :cont, 0 }, fn
          _, acc -> { :cont, acc + 1 }
        end) |> elem(1)
    end
  end

  @doc """
  Returns the count of items in the collection for which
  `fun` returns `true`.

  ## Examples

      iex> Enum.count([1, 2, 3, 4, 5], fn(x) -> rem(x, 2) == 0 end)
      2

  """
  @spec count(t, (element -> as_boolean(term))) :: non_neg_integer
  def count(collection, fun) do
    Enumerable.reduce(collection, { :cont, 0 }, fn(entry, acc) ->
      { :cont, if(fun.(entry), do: acc + 1, else: acc) }
    end) |> elem(1)
  end

  @doc """
  Drops the first `count` items from `collection`.

  If a negative value `count` is given, the last `count`
  values will be dropped. The collection is enumerated
  once to retrieve the proper index and the remaining
  calculation is performed from the end.

  ## Examples

      iex> Enum.drop([1, 2, 3], 2)
      [3]

      iex> Enum.drop([1, 2, 3], 10)
      []

      iex> Enum.drop([1, 2, 3], 0)
      [1,2,3]

      iex> Enum.drop([1, 2, 3], -1)
      [1,2]

  """
  @spec drop(t, integer) :: list
  def drop(collection, count) when is_list(collection) and count >= 0 do
    do_drop(collection, count)
  end

  def drop(collection, count) when count >= 0 do
    res =
      reduce(collection, count, fn
        x, acc when is_list(acc) -> [x|acc]
        x, 0                     -> [x]
        _, acc when acc > 0      -> acc - 1
      end)
    if is_list(res), do: :lists.reverse(res), else: []
  end

  def drop(collection, count) when count < 0 do
    do_drop(reverse(collection), abs(count)) |> :lists.reverse
  end

  @doc """
  Drops items at the beginning of `collection` while `fun` returns `true`.

  ## Examples

      iex> Enum.drop_while([1, 2, 3, 4, 5], fn(x) -> x < 3 end)
      [3,4,5]

  """
  @spec drop_while(t, (element -> as_boolean(term))) :: list
  def drop_while(collection, fun) when is_list(collection) do
    do_drop_while(collection, fun)
  end

  def drop_while(collection, fun) do
    { _, { res, _ } } =
      Enumerable.reduce(collection, { :cont, { [], true } }, R.drop_while(fun))
    :lists.reverse(res)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`.
  Returns `:ok`.

  ## Examples

      Enum.each(["some", "example"], fn(x) -> IO.puts x end)
      "some"
      "example"
      #=> :ok

  """
  @spec each(t, (element -> any)) :: :ok
  def each(collection, fun) when is_list(collection) do
    :lists.foreach(fun, collection)
    :ok
  end

  def each(collection, fun) do
    reduce(collection, nil, fn(entry, _) ->
      fun.(entry)
      nil
    end)
    :ok
  end

  @doc """
  Returns `true` if the collection is empty, otherwise `false`.

  ## Examples

      iex> Enum.empty?([])
      true

      iex> Enum.empty?([1, 2, 3])
      false

  """
  @spec empty?(t) :: boolean
  def empty?(collection) when is_list(collection) do
    collection == []
  end

  def empty?(collection) do
    Enumerable.reduce(collection, { :cont, true }, fn(_, _) -> { :halt, false } end) |> elem(1)
  end

  @doc """
  Finds the element at the given index (zero-based).
  Returns `{ :ok, element }` if found, otherwise `:error`.

  A negative index can be passed, which means the collection is
  enumerated once and the index is counted from the end (i.e.
  `-1` fetches the last element).

  ## Examples

      iex> Enum.fetch([2, 4, 6], 0)
      { :ok, 2 }

      iex> Enum.fetch([2, 4, 6], 2)
      { :ok, 6 }

      iex> Enum.fetch([2, 4, 6], 4)
      :error

  """
  @spec fetch(t, integer) :: { :ok, element } | :error
  def fetch(collection, n) when is_list(collection) and n >= 0 do
    do_fetch(collection, n)
  end

  def fetch(collection, n) when n >= 0 do
    res =
      Enumerable.reduce(collection, { :cont, 0 }, fn(entry, acc) ->
        if acc == n do
          { :halt, entry }
        else
          { :cont, acc + 1 }
        end
      end)

    case res do
      { :halted, entry } -> { :ok, entry }
      { :done, _ } -> :error
    end
  end

  def fetch(collection, n) when n < 0 do
    do_fetch(reverse(collection), abs(n + 1))
  end

  @doc """
  Finds the element at the given index (zero-based).
  Raises `OutOfBoundsError` if the given position
  is outside the range of the collection.

  ## Examples

      iex> Enum.fetch!([2, 4, 6], 0)
      2

      iex> Enum.fetch!([2, 4, 6], 2)
      6

      iex> Enum.fetch!([2, 4, 6], 4)
      ** (Enum.OutOfBoundsError) out of bounds error

  """
  @spec fetch!(t, integer) :: element | no_return
  def fetch!(collection, n) do
    case fetch(collection, n) do
      { :ok, h } -> h
      :error     -> raise Enum.OutOfBoundsError
    end
  end

  @doc """
  Filters the collection, i.e. returns only those elements
  for which `fun` returns `true`.

  ## Examples

      iex> Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [2]

  """
  @spec filter(t, (element -> as_boolean(term))) :: list
  def filter(collection, fun) when is_list(collection) do
    for item <- collection, fun.(item), do: item
  end

  def filter(collection, fun) do
    Enumerable.reduce(collection, { :cont, [] }, R.filter(fun))
    |> elem(1) |> :lists.reverse
  end

  @doc """
  Filters the collection and maps its values in one pass.

  ## Examples

      iex> Enum.filter_map([1, 2, 3], fn(x) -> rem(x, 2) == 0 end, &(&1 * 2))
      [4]

  """
  @spec filter_map(t, (element -> as_boolean(term)), (element -> element)) :: list
  def filter_map(collection, filter, mapper) when is_list(collection) do
    for item <- collection, filter.(item), do: mapper.(item)
  end

  def filter_map(collection, filter, mapper) do
    Enumerable.reduce(collection, { :cont, [] }, R.filter_map(filter, mapper))
    |> elem(1) |> :lists.reverse
  end

  @doc """
  Returns the first item for which `fun` returns a truthy value. If no such
  item is found, returns `ifnone`.

  ## Examples

      iex> Enum.find([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
      nil

      iex> Enum.find([2, 4, 6], 0, fn(x) -> rem(x, 2) == 1 end)
      0

      iex> Enum.find([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
      3

  """
  @spec find(t, (element -> any)) :: element | nil
  @spec find(t, default, (element -> any)) :: element | default
  def find(collection, ifnone \\ nil, fun)

  def find(collection, ifnone, fun) when is_list(collection) do
    do_find(collection, ifnone, fun)
  end

  def find(collection, ifnone, fun) do
    Enumerable.reduce(collection, { :cont, ifnone }, fn(entry, ifnone) ->
      if fun.(entry), do: { :halt, entry }, else: { :cont, ifnone }
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

  """
  @spec find_value(t, (element -> any)) :: any | :nil
  @spec find_value(t, any, (element -> any)) :: any | :nil
  def find_value(collection, ifnone \\ nil, fun)

  def find_value(collection, ifnone, fun) when is_list(collection) do
    do_find_value(collection, ifnone, fun)
  end

  def find_value(collection, ifnone, fun) do
    Enumerable.reduce(collection, { :cont, ifnone }, fn(entry, ifnone) ->
      fun_entry = fun.(entry)
      if fun_entry, do: { :halt, fun_entry }, else: { :cont, ifnone }
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
  def find_index(collection, fun) when is_list(collection) do
    do_find_index(collection, 0, fun)
  end

  def find_index(collection, fun) do
    res =
      Enumerable.reduce(collection, { :cont, 0 }, fn(entry, acc) ->
        if fun.(entry), do: { :halt, acc }, else: { :cont, acc + 1 }
      end)

    case res do
      { :halted, entry } -> entry
      { :done, _ } -> nil
    end
  end

  @doc """
  Returns a new collection appending the result of invoking `fun`
  on each corresponding item of `collection`.

  The given function should return an enumerable.

  ## Examples

      iex> Enum.flat_map([:a, :b, :c], fn(x) -> [x, x] end)
      [:a, :a, :b, :b, :c, :c]

      iex> Enum.flat_map([{1,3}, {4,6}], fn({x,y}) -> x..y end)
      [1, 2, 3, 4, 5, 6]

  """
  @spec flat_map(t, (element -> t)) :: list
  def flat_map(collection, fun) do
    reduce(collection, [], fn(entry, acc) ->
      reduce(fun.(entry), acc, &[&1|&2])
    end) |> :lists.reverse
  end

  @doc """
  Maps and reduces a collection, flattening the given results.

  It expects an accumulator and a function that receives each stream item
  and an accumulator, and must return a tuple containing a new stream
  (often a list) with the new accumulator or a tuple with `:halt` as first
  element and the accumulator as second.

  ## Examples

      iex> enum = 1..100
      iex> n = 3
      iex> Enum.flat_map_reduce(enum, 0, fn i, acc ->
      ...>   if acc < n, do: { [i], acc + 1 }, else: { :halt, acc }
      ...> end)
      { [1,2,3], 3 }

  """
  @spec flat_map_reduce(t, acc, fun) :: { [any], any } when
        fun: (element, acc -> { t, acc } | { :halt, acc }),
        acc: any
  def flat_map_reduce(collection, acc, fun) do
    { _, { list, acc } } =
      Enumerable.reduce(collection, { :cont, { [], acc } }, fn(entry, { list, acc }) ->
        case fun.(entry, acc) do
          { :halt, acc } ->
            { :halt, { list, acc } }
          { entries, acc } ->
            { :cont, { reduce(entries, list, &[&1|&2]), acc } }
        end
      end)

    { :lists.reverse(list), acc }
  end

  @doc """
  Intersperses `element` between each element of the enumeration.

  Complexity: O(n)

  ## Examples

      iex> Enum.intersperse([1, 2, 3], 0)
      [1, 0, 2, 0, 3]

      iex> Enum.intersperse([1], 0)
      [1]

      iex> Enum.intersperse([], 0)
      []

  """
  @spec intersperse(t, element) :: list
  def intersperse(collection, element) do
    list =
      reduce(collection, [], fn(x, acc) ->
        [x, element | acc]
      end) |> :lists.reverse()

    case list do
      []    -> []
      [_|t] -> t  # Head is a superfluous intersperser element
    end
  end

  @doc """
  Inserts the given enumerable into a collectable.

  ## Examples

      iex> Enum.into([1, 2], [0])
      [0, 1, 2]

      iex> Enum.into([a: 1, b: 2], %{})
      %{a: 1, b: 2}

  """
  @spec into(Enumerable.t, Collectable.t) :: Collectable.t
  def into(collection, list) when is_list(list) do
    list ++ to_list(collection)
  end

  def into(collection, %{} = map) when is_list(collection) and (map_size(map) == 0) do
    :maps.from_list(collection)
  end

  def into(collection, collectable) do
    { initial, fun } = Collectable.into(collectable)
    into(collection, initial, fun, fn x, acc ->
      fun.(acc, { :cont, x })
    end)
  end

  @doc """
  Inserts the given enumerable into a collectable
  according to the transformation function.

  ## Examples

      iex> Enum.into([2, 3], [3], fn x -> x * 3 end)
      [3, 6, 9]

  """
  @spec into(Enumerable.t, Collectable.t, (term -> term)) :: Collectable.t

  def into(collection, list, transform) when is_list(list) and is_function(transform, 1) do
    list ++ map(collection, transform)
  end

  def into(collection, collectable, transform) when is_function(transform, 1) do
    { initial, fun } = Collectable.into(collectable)
    into(collection, initial, fun, fn x, acc ->
      fun.(acc, { :cont, transform.(x) })
    end)
  end

  defp into(collection, initial, fun, callback) do
    try do
      reduce(collection, initial, callback)
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
  Joins the given `collection` according to `joiner`.
  `joiner` can be either a binary or a list and the
  result will be of the same type as `joiner`. If
  `joiner` is not passed at all, it defaults to an
  empty binary.

  All items in the collection must be convertible
  to a binary, otherwise an error is raised.

  ## Examples

      iex> Enum.join([1, 2, 3])
      "123"

      iex> Enum.join([1, 2, 3], " = ")
      "1 = 2 = 3"

  """
  @spec join(t) :: String.t
  @spec join(t, String.t) :: String.t
  def join(collection, joiner \\ "")

  def join(collection, joiner) when is_binary(joiner) do
    reduce(collection, "", fn
      entry, ""  -> to_string(entry)
      entry, acc -> acc <> joiner <> to_string(entry)
    end)
  end

  @doc """
  Returns a new collection, where each item is the result
  of invoking `fun` on each corresponding item of `collection`.

  For dicts, the function expects a key-value tuple.

  ## Examples

      iex> Enum.map([1, 2, 3], fn(x) -> x * 2 end)
      [2, 4, 6]

      iex> Enum.map([a: 1, b: 2], fn({k, v}) -> { k, -v } end)
      [a: -1, b: -2]

  """
  @spec map(t, (element -> any)) :: list
  def map(collection, fun) when is_list(collection) do
    for item <- collection, do: fun.(item)
  end

  def map(collection, fun) do
    Enumerable.reduce(collection, { :cont, [] }, R.map(fun)) |> elem(1) |> :lists.reverse
  end

  @doc """
  Maps and joins the given `collection` in one pass.
  `joiner` can be either a binary or a list and the
  result will be of the same type as `joiner`. If
  `joiner` is not passed at all, it defaults to an
  empty binary.

  All items in the collection must be convertible
  to a binary, otherwise an error is raised.

  ## Examples

      iex> Enum.map_join([1, 2, 3], &(&1 * 2))
      "246"

      iex> Enum.map_join([1, 2, 3], " = ", &(&1 * 2))
      "2 = 4 = 6"

  """
  @spec map_join(t, (element -> any)) :: String.t
  @spec map_join(t, String.t, (element -> any)) :: String.t
  def map_join(collection, joiner \\ "", mapper)

  def map_join(collection, joiner, mapper) when is_binary(joiner) do
    reduce(collection, "", fn
      entry, ""  -> to_string(mapper, entry)
      entry, acc -> acc <> joiner <> to_string(mapper, entry)
    end)
  end

  @doc """
  Invokes the given `fun` for each item in the `collection`
  while also keeping an accumulator. Returns a tuple where
  the first element is the mapped collection and the second
  one is the final accumulator.

  For dicts, the first tuple element must be a `{ key, value }`
  tuple.

  ## Examples

      iex> Enum.map_reduce([1, 2, 3], 0, fn(x, acc) -> { x * 2, x + acc } end)
      { [2, 4, 6], 6 }

  """
  @spec map_reduce(t, any, (element, any -> any)) :: any
  def map_reduce(collection, acc, fun) when is_list(collection) do
    :lists.mapfoldl(fun, acc, collection)
  end

  def map_reduce(collection, acc, fun) do
    { list, acc } = reduce(collection, { [], acc }, fn(entry, { list, acc }) ->
      { new_entry, acc } = fun.(entry, acc)
      { [new_entry|list], acc }
    end)
    { :lists.reverse(list), acc }
  end

  @doc """
  Returns the maximum value.
  Raises `EmptyError` if the collection is empty.

  ## Examples

      iex> Enum.max([1, 2, 3])
      3

  """
  @spec max(t) :: element | no_return
  def max(collection) do
    reduce(collection, &Kernel.max(&1, &2))
  end

  @doc """
  Returns the maximum value as calculated by the given function.
  Raises `EmptyError` if the collection is empty.

  ## Examples

      iex> Enum.max_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "aaa"

  """
  @spec max_by(t, (element -> any)) :: element | no_return
  def max_by([h|t], fun) do
    reduce(t, { h, fun.(h) }, fn(entry, { _, fun_max } = old) ->
      fun_entry = fun.(entry)
      if(fun_entry > fun_max, do: { entry, fun_entry }, else: old)
    end) |> elem(0)
  end

  def max_by([], _fun) do
    raise Enum.EmptyError
  end

  def max_by(collection, fun) do
    result =
      reduce(collection, :first, fn
        entry, { _, fun_max } = old ->
          fun_entry = fun.(entry)
          if(fun_entry > fun_max, do: { entry, fun_entry }, else: old)
        entry, :first ->
          { entry, fun.(entry) }
      end)

    case result do
      :first       -> raise Enum.EmptyError
      { entry, _ } -> entry
    end
  end

  @doc """
  Checks if `value` exists within the `collection`.

  Membership is tested with the match (`===`) operator, although
  enumerables like ranges may include floats inside the given
  range.

  ## Examples

      iex> Enum.member?(1..10, 5)
      true

      iex> Enum.member?([:a, :b, :c], :d)
      false

  """
  @spec member?(t, element) :: boolean
  def member?(collection, value) when is_list(collection) do
    :lists.member(value, collection)
  end

  def member?(collection, value) do
    case Enumerable.member?(collection, value) do
      { :ok, value } when is_boolean(value) ->
        value
      { :error, module } ->
        module.reduce(collection, { :cont, false }, fn
          v, _ when v === value -> { :halt, true }
          _, _                  -> { :cont, false }
        end) |> elem(1)
    end
  end

  @doc """
  Returns the minimum value.
  Raises `EmptyError` if the collection is empty.

  ## Examples

      iex> Enum.min([1, 2, 3])
      1

  """
  @spec min(t) :: element | no_return
  def min(collection) do
    reduce(collection, &Kernel.min(&1, &2))
  end

  @doc """
  Returns the minimum value as calculated by the given function.
  Raises `EmptyError` if the collection is empty.

  ## Examples

      iex> Enum.min_by(["a", "aa", "aaa"], fn(x) -> String.length(x) end)
      "a"

  """
  @spec min_by(t, (element -> any)) :: element | no_return
  def min_by([h|t], fun) do
    reduce(t, { h, fun.(h) }, fn(entry, { _, fun_min } = old) ->
      fun_entry = fun.(entry)
      if(fun_entry < fun_min, do: { entry, fun_entry }, else: old)
    end) |> elem(0)
  end

  def min_by([], _fun) do
    raise Enum.EmptyError
  end

  def min_by(collection, fun) do
    result =
      reduce(collection, :first, fn
        entry, { _, fun_min } = old ->
          fun_entry = fun.(entry)
          if(fun_entry < fun_min, do: { entry, fun_entry }, else: old)
        entry, :first ->
          { entry, fun.(entry) }
      end)

    case result do
      :first       -> raise Enum.EmptyError
      { entry, _ } -> entry
    end
  end

  @doc """
  Returns the sum of all values.

  Raises `ArithmeticError` if collection contains a non-numeric value.

  ## Examples

      iex> Enum.sum([1, 2, 3])
      6

  """
  @spec sum(t) :: number
  def sum(collection) do
    reduce(collection, 0, &+/2)
  end

  @doc """
  Partitions `collection` into two collections, where the first one contains elements
  for which `fun` returns a truthy value, and the second one -- for which `fun`
  returns `false` or `nil`.

  ## Examples

      iex> Enum.partition([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      { [2], [1,3] }

  """
  @spec partition(t, (element -> any)) :: {list, list}
  def partition(collection, fun) do
    { acc1, acc2 } =
      reduce(collection, { [], [] }, fn(entry, { acc1, acc2 }) ->
        if fun.(entry) do
          { [entry|acc1], acc2 }
        else
          { acc1, [entry|acc2] }
        end
      end)

    { :lists.reverse(acc1), :lists.reverse(acc2) }
  end

  @doc """
  Splits `collection` into groups based on `fun`.

  The result is a dict (by default a map) where each key is
  a group and each value is a list of elements from `collection`
  for which `fun` returned that group. Ordering is not necessarily
  preserved.

  ## Examples

      iex> Enum.group_by(~w{ant buffalo cat dingo}, &String.length/1)
      %{ 3 => ["cat", "ant"], 7 => ["buffalo"], 5 => ["dingo"] }

  """
  @spec group_by(t, (element -> any)) :: HashDict
  def group_by(collection, dict \\ %{}, fun) do
    reduce(collection, dict, fn(entry, categories) ->
      Dict.update(categories, fun.(entry), [entry], &[entry|&1])
    end)
  end

  @doc """
  Invokes `fun` for each element in the collection passing that element and the
  accumulator `acc` as arguments. `fun`'s return value is stored in `acc`.
  Returns the accumulator.

  ## Examples

      iex> Enum.reduce([1, 2, 3], 0, fn(x, acc) -> x + acc end)
      6

  """
  @spec reduce(t, any, (element, any -> any)) :: any
  def reduce(collection, acc, fun) when is_list(collection) do
    :lists.foldl(fun, acc, collection)
  end

  def reduce(collection, acc, fun) do
    Enumerable.reduce(collection, { :cont, acc },
                      fn x, acc -> { :cont, fun.(x, acc) } end) |> elem(1)
  end

  @doc """
  Invokes `fun` for each element in the collection passing that element and the
  accumulator `acc` as arguments. `fun`'s return value is stored in `acc`.
  The first element of the collection is used as the initial value of `acc`.
  Returns the accumulator.

  ## Examples

      iex> Enum.reduce([1, 2, 3, 4], fn(x, acc) -> x * acc end)
      24

  """
  @spec reduce(t, (element, any -> any)) :: any
  def reduce([h|t], fun) do
    reduce(t, h, fun)
  end

  def reduce([], _fun) do
    raise Enum.EmptyError
  end

  def reduce(collection, fun) do
    result =
      Enumerable.reduce(collection, { :cont, :first }, fn
        x, :first ->
          { :cont, { :acc, x } }
        x, { :acc, acc } ->
          { :cont, { :acc, fun.(x, acc) } }
      end) |> elem(1)

    case result do
      :first        -> raise Enum.EmptyError
      { :acc, acc } -> acc
    end
  end

  @doc """
  Returns elements of collection for which `fun` returns `false`.

  ## Examples

      iex> Enum.reject([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [1, 3]

  """
  @spec reject(t, (element -> as_boolean(term))) :: list
  def reject(collection, fun) when is_list(collection) do
    for item <- collection, !fun.(item), do: item
  end

  def reject(collection, fun) do
    Enumerable.reduce(collection, { :cont, [] }, R.reject(fun)) |> elem(1) |> :lists.reverse
  end

  @doc """
  Reverses the collection.

  ## Examples

      iex> Enum.reverse([1, 2, 3])
      [3, 2, 1]

  """
  @spec reverse(t) :: list
  def reverse(collection) when is_list(collection) do
    :lists.reverse(collection)
  end

  def reverse(collection) do
    reverse(collection, [])
  end

  @doc """
  Reverses the collection and appends the tail.
  This is an optimization for
  `Enum.concat(Enum.reverse(collection), tail)`.

  ## Examples

      iex> Enum.reverse([1, 2, 3], [4, 5, 6])
      [3, 2, 1, 4, 5, 6]

  """
  @spec reverse(t, t) :: list
  def reverse(collection, tail) when is_list(collection) and is_list(tail) do
    :lists.reverse(collection, tail)
  end

  def reverse(collection, tail) do
    reduce(collection, to_list(tail), fn(entry, acc) ->
      [entry|acc]
    end)
  end

  @doc """
  Applies the given function to each element in the collection,
  storing the result in a list and passing it as the accumulator
  for the next computation.

  ## Examples

      iex> Enum.scan(1..5, &(&1 + &2))
      [1,3,6,10,15]

  """
  @spec scan(t, (element, any -> any)) :: list
  def scan(enum, fun) do
    { _, { res, _ } } =
      Enumerable.reduce(enum, { :cont, { [], :first } }, R.scan_2(fun))
    :lists.reverse(res)
  end

  @doc """
  Applies the given function to each element in the collection,
  storing the result in a list and passing it as the accumulator
  for the next computation. Uses the given `acc` as the starting value.

  ## Examples

      iex> Enum.scan(1..5, 0, &(&1 + &2))
      [1,3,6,10,15]

  """
  @spec scan(t, any, (element, any -> any)) :: list
  def scan(enum, acc, fun) do
    { _, { res, _ } } =
      Enumerable.reduce(enum, { :cont, { [], acc } }, R.scan_3(fun))
    :lists.reverse(res)
  end

  @doc """
  Returns a list of collection elements shuffled.

  Notice that you need to explicitly call `:random.seed/1` and
  set a seed value for the random algorithm. Otherwise, the
  default seed will be set which will always return the same
  result. For example, one could do the following to set a seed
  dynamically:

      :random.seed(:erlang.now)

  ## Examples

      iex> Enum.shuffle([1, 2, 3])
      [3, 2, 1]
      iex> Enum.shuffle([1, 2, 3])
      [3, 1, 2]

  """
  @spec shuffle(t) :: list
  def shuffle(collection) do
    randomized = reduce(collection, [], fn x, acc ->
      [{ :random.uniform, x }|acc]
    end)
    unwrap(:lists.keysort(1, randomized), [])
  end

  @doc """
  Returns a subset list of the given collection. Drops elements
  until element position `start`, then takes `count` elements.

  ## Examples

      iex> Enum.slice(1..100, 5, 10)
      [6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

  """
  @spec slice(t, integer, non_neg_integer) :: list

  def slice(coll, start, count) when start < 0 do
    { list, new_start } = enumerate_and_count(coll, start)
    if new_start >= 0, do: slice(list, new_start, count)
  end

  def slice(coll, start, count) when is_list(coll) and start >= 0 and count > 0 do
    do_slice(coll, start, count)
  end

  def slice(coll, start, count) when start >= 0 and count > 0 do
    { start, _, list } = Enumerable.reduce(coll, { :cont, { start, count, [] } }, fn
      _entry, { start, count, _list } when start > 0 ->
        { :cont, { start-1, count, [] } }
      entry, { start, count, list } when count > 1 ->
        { :cont, { start, count-1, [entry|list] } }
      entry, { start, count, list } ->
        { :halt, { start, count, [entry|list] } }
    end) |> elem(1)

    if start <= 0, do: :lists.reverse(list)
  end

  def slice(coll, start, 0) do
    res =
      Enumerable.reduce(coll, { :cont, start }, fn _, start ->
        if start > 0, do: { :cont, start-1 }, else: { :halt, [] }
      end) |> elem(1)
    if is_list(res), do: res
  end

  @doc """
  Returns a subset list of the given collection. Drops elements
  until element position `range.first`, then takes elements until element
  position `range.last` (inclusive).

  Positions are calculated by adding the number of items in the collection to
  negative positions (so position -3 in a collection with count 5 becomes
  position 2).

  The first position (after adding count to negative positions) must be smaller
  or equal to the last position.

  ## Examples

      iex> Enum.slice(1..100, 5..10)
      [6, 7, 8, 9, 10, 11]

  """
  @spec slice(t, Range.t) :: list
  def slice(coll, first..last) when first >= 0 and last >= 0 do
    # Simple case, which works on infinite collections
    if last - first >= 0 do
      slice(coll, first, last - first + 1)
    end
  end

  def slice(coll, first..last) do
    { list, count } = enumerate_and_count(coll, 0)
    corr_first = if first >= 0, do: first, else: first + count
    corr_last = if last >= 0, do: last, else: last + count
    length = corr_last - corr_first + 1
    if corr_first >= 0 and length > 0 do
      slice(list, corr_first, length)
    end
  end

  @doc """
  Sorts the collection according to Elixir's term ordering.

  Uses the merge sort algorithm.

  ## Examples

      iex> Enum.sort([3, 2, 1])
      [1, 2, 3]

  """
  @spec sort(t) :: list
  def sort(collection) when is_list(collection) do
    :lists.sort(collection)
  end

  def sort(collection) do
    sort(collection, &(&1 <= &2))
  end

  @doc """
  Sorts the collection by the given function.

  This function uses the merge sort algorithm. The given function
  must return false if the first argument is less than right one.

  ## Examples

      iex> Enum.sort([1, 2, 3], &(&1 > &2))
      [3, 2, 1]

  The sorting algorithm will be stable as long as the given function
  returns true for values considered equal:

      iex> Enum.sort ["some", "kind", "of", "monster"], &(byte_size(&1) <= byte_size(&2))
      ["of", "some", "kind", "monster"]

  If the function does not return true, the sorting is not stable and
  the order of equal terms may be shuffled:

      iex> Enum.sort ["some", "kind", "of", "monster"], &(byte_size(&1) < byte_size(&2))
      ["of", "kind", "some", "monster"]

  """
  @spec sort(t, (element, element -> boolean)) :: list
  def sort(collection, fun) when is_list(collection) do
    :lists.sort(fun, collection)
  end

  def sort(collection, fun) do
    reduce(collection, [], &sort_reducer(&1, &2, fun)) |> sort_terminator(fun)
  end

  @doc """
  Splits the enumerable into two collections, leaving `count`
  elements in the first one. If `count` is a negative number,
  it starts counting from the back to the beginning of the
  collection.

  Be aware that a negative `count` implies the collection
  will be enumerated twice: once to calculate the position, and
  a second time to do the actual splitting.

  ## Examples

      iex> Enum.split([1, 2, 3], 2)
      { [1,2], [3] }

      iex> Enum.split([1, 2, 3], 10)
      { [1,2,3], [] }

      iex> Enum.split([1, 2, 3], 0)
      { [], [1,2,3] }

      iex> Enum.split([1, 2, 3], -1)
      { [1,2], [3] }

      iex> Enum.split([1, 2, 3], -5)
      { [], [1,2,3] }

  """
  @spec split(t, integer) :: {list, list}
  def split(collection, count) when is_list(collection) and count >= 0 do
    do_split(collection, count, [])
  end

  def split(collection, count) when count >= 0 do
    { _, list1, list2 } =
      reduce(collection, { count, [], [] }, fn(entry, { counter, acc1, acc2 }) ->
        if counter > 0 do
          { counter - 1, [entry|acc1], acc2 }
        else
          { counter, acc1, [entry|acc2] }
        end
      end)

    { :lists.reverse(list1), :lists.reverse(list2) }
  end

  def split(collection, count) when count < 0 do
    do_split_reverse(reverse(collection), abs(count), [])
  end

  @doc """
  Splits `collection` in two while `fun` returns `true`.

  ## Examples

      iex> Enum.split_while([1, 2, 3, 4], fn(x) -> x < 3 end)
      { [1, 2], [3, 4] }

  """
  @spec split_while(t, (element -> as_boolean(term))) :: {list, list}
  def split_while(collection, fun) when is_list(collection) do
    do_split_while(collection, fun, [])
  end

  def split_while(collection, fun) do
    { list1, list2 } =
      reduce(collection, { [], [] }, fn
        entry, { acc1, [] } ->
          if(fun.(entry), do: { [entry|acc1], [] }, else: { acc1, [entry] })
        entry, { acc1, acc2 } ->
          { acc1, [entry|acc2] }
      end)

    { :lists.reverse(list1), :lists.reverse(list2) }
  end

  @doc """
  Takes the first `count` items from the collection.

  If a negative `count` is given, the last `count` values will
  be taken. For such, the collection is fully enumerated keeping up
  to `2 * count` elements in memory. Once the end of the collection is
  reached, the last `count` elements are returned.

  ## Examples

      iex> Enum.take([1, 2, 3], 2)
      [1,2]

      iex> Enum.take([1, 2, 3], 10)
      [1,2,3]

      iex> Enum.take([1, 2, 3], 0)
      []

      iex> Enum.take([1, 2, 3], -1)
      [3]

  """
  @spec take(t, integer) :: list

  def take(_collection, 0) do
    []
  end

  def take(collection, count) when is_list(collection) and count > 0 do
    do_take(collection, count)
  end

  def take(collection, count) when count > 0 do
    { _, { res, _ } } =
      Enumerable.reduce(collection, { :cont, { [], count } }, fn(entry, { list, count }) ->
        if count > 1 do
          { :cont, { [entry|list], count - 1 } }
        else
          { :halt, { [entry|list], count } }
        end
      end)
    :lists.reverse(res)
  end

  def take(collection, count) when count < 0 do
    Stream.take(collection, count).({ :cont, [] }, &{ :cont, [&1|&2] })
    |> elem(1) |> :lists.reverse
  end

  @doc """
  Returns a collection of every `nth` item in the collection,
  starting with the first element.

  ## Examples

      iex> Enum.take_every(1..10, 2)
      [1, 3, 5, 7, 9]

  """
  @spec take_every(t, integer) :: list
  def take_every(_collection, 0), do: []
  def take_every(collection, nth) do
    { _, { res, _ } } =
      Enumerable.reduce(collection, { :cont, { [], :first } }, R.take_every(nth))
    :lists.reverse(res)
  end

  @doc """
  Takes the items at the beginning of `collection` while `fun` returns `true`.

  ## Examples

      iex> Enum.take_while([1, 2, 3], fn(x) -> x < 3 end)
      [1, 2]

  """
  @spec take_while(t, (element -> as_boolean(term))) :: list
  def take_while(collection, fun) when is_list(collection) do
    do_take_while(collection, fun)
  end

  def take_while(collection, fun) do
    Enumerable.reduce(collection, { :cont, [] }, R.take_while(fun))
    |> elem(1) |> :lists.reverse
  end

  @doc """
  Convert `collection` to a list.

  ## Examples

      iex> Enum.to_list(1 .. 3)
      [1, 2, 3]

  """
  @spec to_list(t) :: [term]
  def to_list(collection) when is_list(collection) do
    collection
  end

  def to_list(collection) do
    reverse(collection) |> :lists.reverse
  end


  @doc """
  Traverses the given enumerable keeping its shape.

  It also expects the enumerable to implement the `Collectable` protocol.

  ## Examples

      iex> Enum.traverse(%{a: 1, b: 2}, fn { k, v } -> { k, v * 2 } end)
      %{a: 2, b: 4}

  """
  @spec traverse(Enumerable.t, (term -> term)) :: Collectable.t
  def traverse(collection, transform) when is_list(collection) do
    :lists.map(transform, collection)
  end

  def traverse(collection, transform) do
    into(collection, Collectable.empty(collection), transform)
  end

  @doc """
  Enumerates the collection, removing all duplicated items.

  ## Examples

      iex> Enum.uniq([1, 2, 3, 2, 1])
      [1, 2, 3]

      iex> Enum.uniq([{1, :x}, {2, :y}, {1, :z}], fn {x, _} -> x end)
      [{1,:x}, {2,:y}]

  """
  @spec uniq(t) :: list
  @spec uniq(t, (element -> term)) :: list
  def uniq(collection, fun \\ fn x -> x end)

  def uniq(collection, fun) when is_list(collection) do
    do_uniq(collection, [], fun)
  end

  def uniq(collection, fun) do
    { _, { list, _ } } =
      Enumerable.reduce(collection, { :cont, { [], [] } }, R.uniq(fun))
    :lists.reverse(list)
  end

  @doc """
  Zips corresponding elements from two collections into one list
  of tuples.

  The zipping finishes as soon as any enumerable completes.

  ## Examples

      iex> Enum.zip([1, 2, 3], [:a, :b, :c])
      [{1,:a},{2,:b},{3,:c}]

      iex> Enum.zip([1,2,3,4,5], [:a, :b, :c])
      [{1,:a},{2,:b},{3,:c}]

  """
  @spec zip(t, t) :: [{any, any}]
  def zip(coll1, coll2) when is_list(coll1) and is_list(coll2) do
    do_zip(coll1, coll2)
  end

  def zip(coll1, coll2) do
    Stream.zip(coll1, coll2).({ :cont, [] }, &{ :cont, [&1|&2] }) |> elem(1) |> :lists.reverse
  end

  @doc """
  Returns the collection with each element wrapped in a tuple
  alongside its index.

  ## Examples

      iex> Enum.with_index [1,2,3]
      [{1,0},{2,1},{3,2}]

  """
  @spec with_index(t) :: list({ element, non_neg_integer })
  def with_index(collection) do
    map_reduce(collection, 0, fn x, acc ->
      { { x, acc }, acc + 1 }
    end) |> elem(0)
  end

  ## Helpers

  @compile { :inline, to_string: 2 }

  defp enumerate_and_count(collection, count) when is_list(collection) do
    { collection, length(collection) - abs(count) }
  end

  defp enumerate_and_count(collection, count) do
    map_reduce(collection, -abs(count), fn(x, acc) -> { x, acc + 1 } end)
  end

  defp to_string(mapper, entry) do
    case mapper.(entry) do
      x when is_binary(x) -> x
      o -> String.Chars.to_string(o)
    end
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

  defp do_fetch([h|_], 0), do: { :ok, h }
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

  defp do_find([h|t], ifnone, fun) do
    if fun.(h) do
      h
    else
      do_find(t, ifnone, fun)
    end
  end

  defp do_find([], ifnone, _) do
    ifnone
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

  defp do_find_value([h|t], ifnone, fun) do
    fun.(h) || do_find_value(t, ifnone, fun)
  end

  defp do_find_value([], ifnone, _) do
    ifnone
  end

  ## shuffle

  defp unwrap([{_, h} | collection], t) do
    unwrap(collection, [h|t])
  end

  defp unwrap([], t), do: t

  ## sort

  defp sort_reducer(entry, { :split, y, x, r, rs, bool }, fun) do
    cond do
      fun.(y, entry) == bool ->
        { :split, entry, y, [x|r], rs, bool }
      fun.(x, entry) == bool ->
        { :split, y, entry, [x|r], rs, bool }
      r == [] ->
        { :split, y, x, [entry], rs, bool }
      true ->
        { :pivot, y, x, r, rs, entry, bool }
    end
  end

  defp sort_reducer(entry, { :pivot, y, x, r, rs, s, bool }, fun) do
    cond do
      fun.(y, entry) == bool ->
        { :pivot, entry, y, [x | r], rs, s, bool }
      fun.(x, entry) == bool ->
        { :pivot, y, entry, [x | r], rs, s, bool }
      fun.(s, entry) == bool ->
        { :split, entry, s, [], [[y, x | r] | rs], bool }
      true ->
        { :split, s, entry, [], [[y, x | r] | rs], bool }
    end
  end

  defp sort_reducer(entry, [x], fun) do
    { :split, entry, x, [], [], fun.(x, entry) }
  end

  defp sort_reducer(entry, acc, _fun) do
    [entry|acc]
  end

  defp sort_terminator({ :split, y, x, r, rs, bool }, fun) do
    sort_merge([[y, x | r] | rs], fun, bool)
  end

  defp sort_terminator({ :pivot, y, x, r, rs, s, bool }, fun) do
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

  ## split

  defp do_split([h|t], counter, acc) when counter > 0 do
    do_split(t, counter - 1, [h|acc])
  end

  defp do_split(list, 0, acc) do
    { :lists.reverse(acc), list }
  end

  defp do_split([], _, acc) do
    { :lists.reverse(acc), [] }
  end

  defp do_split_reverse([h|t], counter, acc) when counter > 0 do
    do_split_reverse(t, counter - 1, [h|acc])
  end

  defp do_split_reverse(list, 0, acc) do
    { :lists.reverse(list), acc }
  end

  defp do_split_reverse([], _, acc) do
    { [], acc }
  end

  ## split_while

  defp do_split_while([h|t], fun, acc) do
    if fun.(h) do
      do_split_while(t, fun, [h|acc])
    else
      { :lists.reverse(acc), [h|t] }
    end
  end

  defp do_split_while([], _, acc) do
    { :lists.reverse(acc), [] }
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
    case :lists.member(fun_h, acc) do
      true  -> do_uniq(t, acc, fun)
      false -> [h|do_uniq(t, [fun_h|acc], fun)]
    end
  end

  defp do_uniq([], _acc, _fun) do
    []
  end

  ## zip

  defp do_zip([h1|next1], [h2|next2]) do
    [{ h1, h2 }|do_zip(next1, next2)]
  end

  defp do_zip(_, []), do: []
  defp do_zip([], _), do: []

  ## slice

  defp do_slice([], start, _count) do
    if start == 0, do: []
  end

  defp do_slice(list, start, 0) do
    if start < length(list), do: []
  end

  defp do_slice([h|t], 0, count) do
    [h|do_slice(t, 0, count-1)]
  end

  defp do_slice([_|t], start, count) do
    do_slice(t, start-1, count)
  end
end

defimpl Enumerable, for: List do
  def reduce(_,     { :halt, acc }, _fun),   do: { :halted, acc }
  def reduce(list,  { :suspend, acc }, fun), do: { :suspended, acc, &reduce(list, &1, fun) }
  def reduce([],    { :cont, acc }, _fun),   do: { :done, acc }
  def reduce([h|t], { :cont, acc }, fun),    do: reduce(t, fun.(h, acc), fun)

  def member?(_list, _value),
    do: { :error, __MODULE__ }
  def count(_list),
    do: { :error, __MODULE__ }
end

defimpl Enumerable, for: Map do
  def reduce(map, acc, fun) do
    do_reduce(:maps.to_list(map), acc, fun)
  end

  defp do_reduce(_,     { :halt, acc }, _fun),   do: { :halted, acc }
  defp do_reduce(list,  { :suspend, acc }, fun), do: { :suspended, acc, &do_reduce(list, &1, fun) }
  defp do_reduce([],    { :cont, acc }, _fun),   do: { :done, acc }
  defp do_reduce([h|t], { :cont, acc }, fun),    do: do_reduce(t, fun.(h, acc), fun)

  def member?(map, { key, value }) do
    { :ok, match?({ :ok, ^value }, :maps.find(key, map)) }
  end

  def member?(_map, _other) do
    { :ok, false }
  end

  def count(map) do
    { :ok, map_size(map) }
  end
end

defimpl Enumerable, for: Function do
  def reduce(function, acc, fun),
    do: function.(acc, fun)
  def member?(_function, _value),
    do: { :error, __MODULE__ }
  def count(_function),
    do: { :error, __MODULE__ }
end
