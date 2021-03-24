defmodule Range do
  @moduledoc """
  Ranges represent a sequence of one or many, ascending
  or descending, consecutive integers.

  Ranges are always inclusive and they may have custom
  steps. The most common form of creating and matching
  on ranges is via the `../2` and `..///3` macros,
  auto-imported from `Kernel`:

      iex> Enum.to_list(1..3)
      [1, 2, 3]
      iex> Enum.to_list(1..3//2)
      [1, 3]
      iex> Enum.to_list(3..1//-1)
      [3, 2, 1]

  Intenrally, ranges are represented as structs:

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

  A range implements the `Enumerable` protocol, which means
  functions in the `Enum` module can be used to work with
  ranges:

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

  @type first :: integer
  @type last :: integer
  @type step :: pos_integer | neg_integer
  @type t :: %__MODULE__{first: first, last: last, step: step}
  @type t(first, last) :: %__MODULE__{first: first, last: last}

  @doc """
  Creates a new range.

  ## Examples

      iex> Range.new(-100, 100)
      -100..100

  """
  @spec new(integer, integer) :: t
  def new(first, last) when is_integer(first) and is_integer(last) do
    # TODO: Deprecate inferring a range with step of -1 on Elixir v1.16
    step = if first <= last, do: 1, else: -1
    %Range{first: first, last: last, step: step}
  end

  def new(first, last) do
    raise ArgumentError,
          "ranges (first..last) expect both sides to be integers, " <>
            "got: #{inspect(first)}..#{inspect(last)}"
  end

  @doc """
  Creates a new range with step.

  ## Examples

      iex> Range.new(-100, 100, 2)
      -100..100//2

  """
  @spec new(integer, integer, integer) :: t
  def new(first, last, step)
      when is_integer(first) and is_integer(last) and is_integer(step) and step != 0 do
    %Range{first: first, last: last, step: step}
  end

  def new(first, last, step) do
    raise ArgumentError,
          "ranges (first..last//step) expect both sides to be integers and the step to be an integer " <>
            "different than zero, got: #{inspect(first)}..#{inspect(last)}//#{inspect(step)}"
  end

  @doc """
  Returns the size of the range.

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
  def size(first..last//step) when step > 0 and first > last, do: 0
  def size(first..last//step) when step < 0 and first < last, do: 0
  def size(first..last//step), do: abs(div(last - first, step)) + 1

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

  """
  @doc since: "1.8.0"
  @spec disjoint?(t, t) :: boolean
  def disjoint?(first1..last1 = _range1, first2..last2 = _range2) do
    {first1, last1} = normalize(first1, last1)
    {first2, last2} = normalize(first2, last2)
    last2 < first1 or last1 < first2
  end

  @compile inline: [normalize: 2]
  defp normalize(first, last) when first > last, do: {last, first}
  defp normalize(first, last), do: {first, last}

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

  def member?(first..last//step, value) when is_integer(value) do
    if first <= last do
      {:ok, first <= value and value <= last and rem(value - first, step) == 0}
    else
      {:ok, last <= value and value <= first and rem(value - first, step) == 0}
    end
  end

  def member?(_, _value) do
    {:ok, false}
  end

  def count(range) do
    {:ok, Range.size(range)}
  end

  def slice(first.._//step = range) do
    {:ok, Range.size(range), &slice(first + &1 * step, step, &2)}
  end

  defp slice(current, _step, 1), do: [current]
  defp slice(current, step, remaining), do: [current | slice(current + step, step, remaining - 1)]
end

defimpl Inspect, for: Range do
  import Inspect.Algebra

  def inspect(first..last//1, opts) do
    concat([to_doc(first, opts), "..", to_doc(last, opts)])
  end

  def inspect(first..last//step, opts) do
    concat([to_doc(first, opts), "..", to_doc(last, opts), "//", to_doc(step, opts)])
  end
end
