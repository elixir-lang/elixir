defmodule Range do
  @moduledoc """
  Ranges represent a sequence of zero, one or many, ascending
  or descending integers with a common difference called step.

  The most common form of creating and matching on ranges is
  via the [`first..last`](`../2`) and [`first..last//step`](`..///3`)
  notations, auto-imported from `Kernel`:

      iex> 1 in 1..10
      true
      iex> 5 in 1..10
      true
      iex> 10 in 1..10
      true

  Ranges are always inclusive in Elixir. When a step is defined,
  integers will only belong to the range if they match the step:

      iex> 5 in 1..10//2
      true
      iex> 4 in 1..10//2
      false

  When defining a range without a step, the step will be
  defined based on the first and last position of the
  range, If `first >= last`, it will be an increasing range
  with a step of 1. Otherwise, it is a decreasing range.
  Note however implicit decreasing ranges are deprecated.
  Therefore, if you need a decreasing range from `3` to `1`,
  prefer to write `3..1//-1` instead.

  `../0` can also be used as a shortcut to create the range `0..-1//1`,
  also known as the full-slice range:

      iex> ..
      0..-1//1

  ## Use cases

  Ranges typically have two uses in Elixir: as a collection or
  to represent a slice of another data structure.

  ### Ranges as collections

  Ranges in Elixir are enumerables and therefore can be used
  with the `Enum` module:

      iex> Enum.to_list(1..3)
      [1, 2, 3]
      iex> Enum.to_list(3..1//-1)
      [3, 2, 1]
      iex> Enum.to_list(1..5//2)
      [1, 3, 5]

  Ranges may also have a single element:

      iex> Enum.to_list(1..1)
      [1]
      iex> Enum.to_list(1..1//2)
      [1]

  Or even no elements at all:

      iex> Enum.to_list(10..0//1)
      []
      iex> Enum.to_list(0..10//-1)
      []

  The full-slice range, returned by `../0`, is an empty collection:

      iex> Enum.to_list(..)
      []

  ### Ranges as slices

  Ranges are also frequently used to slice collections.
  You can slice strings or any enumerable:

      iex> String.slice("elixir", 1..4)
      "lixi"
      iex> Enum.slice([0, 1, 2, 3, 4, 5], 1..4)
      [1, 2, 3, 4]

  In those cases, the first and last values of the range
  are mapped to positions in the collections.

  If a negative number is given, it maps to a position
  from the back:

      iex> String.slice("elixir", 1..-2//1)
      "lixi"
      iex> Enum.slice([0, 1, 2, 3, 4, 5], 1..-2//1)
      [1, 2, 3, 4]

  The range `0..-1//1`, returned by `../0`, returns the
  collection as is, which is why it is called the full-slice
  range:

      iex> String.slice("elixir", ..)
      "elixir"
      iex> Enum.slice([0, 1, 2, 3, 4, 5], ..)
      [0, 1, 2, 3, 4, 5]

  ## Definition

  An increasing range `first..last//step` is a range from `first`
  to `last` increasing by `step` where  `step` must be a positive
  integer and all values `v` must be `first <= v and v <= last`.
  Therefore, a range `10..0//1` is an empty range because there
  is no value `v` that is `10 <= v and v <= 0`.

  Similarly, a decreasing range `first..last//step` is a range
  from `first` to `last` decreasing by `step` where `step` must
  be a negative integer and  values `v` must be `first >= v and v >= last`.
  Therefore, a range `0..10//-1` is an empty range because there
  is no value `v` that is `0 >= v and v >= 10`.

  ## Representation

  Internally, ranges are represented as structs:

      iex> range = 1..9//2
      1..9//2
      iex> first..last//step = range
      iex> first
      1
      iex> last
      9
      iex> step
      2
      iex> range.step
      2

  You can access the range fields (`first`, `last`, and `step`)
  directly but you should not modify nor create ranges by hand.
  Instead use the proper operators or `new/2` and `new/3`.

  Ranges implement the `Enumerable` protocol with memory
  efficient versions of all `Enumerable` callbacks:

      iex> range = 1..10
      1..10
      iex> Enum.reduce(range, 0, fn i, acc -> i * i + acc end)
      385
      iex> Enum.count(range)
      10
      iex> Enum.member?(range, 11)
      false
      iex> Enum.member?(range, 8)
      true

  Such function calls are efficient memory-wise no matter the
  size of the range. The implementation of the `Enumerable`
  protocol uses logic based solely on the endpoints and does
  not materialize the whole list of integers.
  """

  @enforce_keys [:first, :last, :step]
  defstruct first: nil, last: nil, step: nil

  @type limit :: integer
  @type step :: pos_integer | neg_integer
  @type t :: %__MODULE__{first: limit, last: limit, step: step}
  @type t(first, last) :: %__MODULE__{first: first, last: last, step: step}

  @doc """
  Creates a new range.

  If `first` is less than `last`, the range will be increasing from
  `first` to `last`. If `first` is equal to `last`, the range will contain
  one element, which is the number itself.

  If `first` is greater than `last`, the range will be decreasing from `first`
  to `last`, albeit this behaviour is deprecated. Therefore, it is advised to
  explicitly list the step with `new/3`.

  ## Examples

      iex> Range.new(-100, 100)
      -100..100

  """

  @spec new(limit, limit) :: t
  def new(first, last) when is_integer(first) and is_integer(last) do
    # TODO: Deprecate inferring a range with a step of -1 on Elixir v1.17
    step = if first <= last, do: 1, else: -1
    %Range{first: first, last: last, step: step}
  end

  def new(first, last) do
    raise ArgumentError,
          "ranges (first..last) expect both sides to be integers, " <>
            "got: #{inspect(first)}..#{inspect(last)}"
  end

  @doc """
  Creates a new range with `step`.

  ## Examples

      iex> Range.new(-100, 100, 2)
      -100..100//2

  """
  @doc since: "1.12.0"
  @spec new(limit, limit, step) :: t
  def new(first, last, step)
      when is_integer(first) and is_integer(last) and is_integer(step) and step != 0 do
    %Range{first: first, last: last, step: step}
  end

  def new(first, last, step) do
    raise ArgumentError,
          "ranges (first..last//step) expect both sides to be integers and the step to be a " <>
            "non-zero integer, got: #{inspect(first)}..#{inspect(last)}//#{inspect(step)}"
  end

  @doc """
  Returns the size of `range`.

  ## Examples

      iex> Range.size(1..10)
      10
      iex> Range.size(1..10//2)
      5
      iex> Range.size(1..10//3)
      4
      iex> Range.size(1..10//-1)
      0

      iex> Range.size(10..1)
      10
      iex> Range.size(10..1//-1)
      10
      iex> Range.size(10..1//-2)
      5
      iex> Range.size(10..1//-3)
      4
      iex> Range.size(10..1//1)
      0

  """
  @doc since: "1.12.0"
  def size(range)
  def size(first..last//step) when step > 0 and first > last, do: 0
  def size(first..last//step) when step < 0 and first < last, do: 0
  def size(first..last//step), do: abs(div(last - first, step)) + 1

  # TODO: Remove me on v2.0
  def size(%{__struct__: Range, first: first, last: last} = range) do
    step = if first <= last, do: 1, else: -1
    size(Map.put(range, :step, step))
  end

  @doc """
  Shifts a range by the given number of steps.

  ## Examples

      iex> Range.shift(0..10, 1)
      1..11
      iex> Range.shift(0..10, 2)
      2..12

      iex> Range.shift(0..10//2, 2)
      4..14//2
  """
  @doc since: "1.14.0"
  def shift(first..last//step, steps_to_shift)
      when is_integer(first) and is_integer(last) and is_integer(step) and
             is_integer(steps_to_shift) do
    new(first + steps_to_shift * step, last + steps_to_shift * step, step)
  end

  @doc """
  Checks if two ranges are disjoint.

  ## Examples

      iex> Range.disjoint?(1..5, 6..9)
      true
      iex> Range.disjoint?(5..1, 6..9)
      true
      iex> Range.disjoint?(1..5, 5..9)
      false
      iex> Range.disjoint?(1..5, 2..7)
      false

  Steps are also considered when computing the ranges to be disjoint:

      iex> Range.disjoint?(1..10//2, 2..10//2)
      true

      # First element in common in all below is 29
      iex> Range.disjoint?(2..100//3, 9..100//5)
      false
      iex> Range.disjoint?(101..2//-3, 99..9//-5)
      false
      iex> Range.disjoint?(1..100//14, 8..100//21)
      false
      iex> Range.disjoint?(57..-1//-14, 8..100//21)
      false
      iex> Range.disjoint?(1..100//14, 51..8//-21)
      false

      # If 29 is out of range
      iex> Range.disjoint?(1..28//14, 8..28//21)
      true
      iex> Range.disjoint?(2..28//3, 9..28//5)
      true

  """
  @doc since: "1.8.0"
  @spec disjoint?(t, t) :: boolean
  def disjoint?(first1..last1//step1 = range1, first2..last2//step2 = range2) do
    if size(range1) == 0 or size(range2) == 0 do
      true
    else
      {first1, last1, step1} = normalize(first1, last1, step1)
      {first2, last2, step2} = normalize(first2, last2, step2)

      cond do
        last2 < first1 or last1 < first2 ->
          true

        abs(step1) == 1 and abs(step2) == 1 ->
          false

        true ->
          # We need to find the first intersection of two arithmetic
          # progressions and see if they belong within the ranges
          # https://math.stackexchange.com/questions/1656120/formula-to-find-the-first-intersection-of-two-arithmetic-progressions
          {gcd, u, v} = Integer.extended_gcd(-step1, step2)
          c = first1 - first2 + step2 - step1
          t1 = -c / step1 * u
          t2 = -c / step2 * v
          t = max(floor(t1) + 1, floor(t2) + 1)
          x = div(c * u + t * step2, gcd) - 1
          y = div(c * v + t * step1, gcd) - 1

          x < 0 or first1 + x * step1 > last1 or
            y < 0 or first2 + y * step2 > last2
      end
    end
  end

  @compile inline: [normalize: 3]
  defp normalize(first, last, step) when first > last, do: {last, first, -step}
  defp normalize(first, last, step), do: {first, last, step}

  @doc false
  @deprecated "Pattern match on first..last//step instead"
  def range?(term)
  def range?(first..last) when is_integer(first) and is_integer(last), do: true
  def range?(_), do: false
end

defimpl Enumerable, for: Range do
  def reduce(first..last//step, acc, fun) do
    reduce(first, last, acc, fun, step)
  end

  # TODO: Remove me on v2.0
  def reduce(%{__struct__: Range, first: first, last: last} = range, acc, fun) do
    step = if first <= last, do: 1, else: -1
    reduce(Map.put(range, :step, step), acc, fun)
  end

  defp reduce(_first, _last, {:halt, acc}, _fun, _step) do
    {:halted, acc}
  end

  defp reduce(first, last, {:suspend, acc}, fun, step) do
    {:suspended, acc, &reduce(first, last, &1, fun, step)}
  end

  defp reduce(first, last, {:cont, acc}, fun, step)
       when step > 0 and first <= last
       when step < 0 and first >= last do
    reduce(first + step, last, fun.(first, acc), fun, step)
  end

  defp reduce(_, _, {:cont, acc}, _fun, _up) do
    {:done, acc}
  end

  def member?(first..last//step = range, value) when is_integer(value) do
    cond do
      Range.size(range) == 0 ->
        {:ok, false}

      first <= last ->
        {:ok, first <= value and value <= last and rem(value - first, step) == 0}

      true ->
        {:ok, last <= value and value <= first and rem(value - first, step) == 0}
    end
  end

  # TODO: Remove me on v2.0
  def member?(%{__struct__: Range, first: first, last: last} = range, value)
      when is_integer(value) do
    step = if first <= last, do: 1, else: -1
    member?(Map.put(range, :step, step), value)
  end

  def member?(_, _value) do
    {:ok, false}
  end

  def count(range) do
    {:ok, Range.size(range)}
  end

  def slice(first.._//step = range) do
    {:ok, Range.size(range), &slice(first + &1 * step, step + &3 - 1, &2)}
  end

  # TODO: Remove me on v2.0
  def slice(%{__struct__: Range, first: first, last: last} = range) do
    step = if first <= last, do: 1, else: -1
    slice(Map.put(range, :step, step))
  end

  defp slice(_current, _step, 0), do: []
  defp slice(current, step, remaining), do: [current | slice(current + step, step, remaining - 1)]
end

defimpl Inspect, for: Range do
  import Inspect.Algebra
  import Kernel, except: [inspect: 2]

  def inspect(first..last//1, opts) when last >= first do
    concat([to_doc(first, opts), "..", to_doc(last, opts)])
  end

  def inspect(first..last//step, opts) do
    concat([to_doc(first, opts), "..", to_doc(last, opts), "//", to_doc(step, opts)])
  end

  # TODO: Remove me on v2.0
  def inspect(%{__struct__: Range, first: first, last: last} = range, opts) do
    step = if first <= last, do: 1, else: -1
    inspect(Map.put(range, :step, step), opts)
  end
end
