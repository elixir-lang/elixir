defmodule Range do
  @moduledoc """
  Defines a Range.

  A Range represents a discrete number of values where
  the first and last values are integers.

  Ranges can be either increasing (first <= last) or
  decresing (first > last). Ranges are also always
  inclusive.

  A Range is represented internally as a struct. However,
  the most common form of creating and matching on ranges
  is via the `../2` macro, auto-imported from Kernel:

      iex> range = 1..3
      1..3
      iex> first .. last = range
      iex> first
      1
      iex> last
      3

  """

  defstruct first: nil, last: nil

  @type t :: %Range{}
  @type t(first, last) :: %Range{first: first, last: last}

  @doc """
  Creates a new range.
  """
  def new(first, last) do
    %Range{first: first, last: last}
  end

  @doc """
  Returns `true` if the given argument is a range.

  ## Examples

      iex> Range.range?(1..3)
      true

      iex> Range.range?(0)
      false

  """
  def range?(%Range{}), do: true
  def range?(_), do: false
end

defimpl Enumerable, for: Range do
  def reduce(first .. last, acc, fun) do
    validate_range!(first, last)
    reduce(first, last, acc, fun, last >= first)
  end

  defp reduce(_x, _y, {:halt, acc}, _fun, _up) do
    {:halted, acc}
  end

  defp reduce(x, y, {:suspend, acc}, fun, up) do
    {:suspended, acc, &reduce(x, y, &1, fun, up)}
  end

  defp reduce(x, y, {:cont, acc}, fun, true) when x <= y do
    reduce(x + 1, y, fun.(x, acc), fun, true)
  end

  defp reduce(x, y, {:cont, acc}, fun, false) when x >= y do
    reduce(x - 1, y, fun.(x, acc), fun, false)
  end

  defp reduce(_, _, {:cont, acc}, _fun, _up) do
    {:done, acc}
  end

  def member?(first .. last, value) do
    validate_range!(first, last)
    if first <= last do
      {:ok, first <= value and value <= last}
    else
      {:ok, last <= value and value <= first}
    end
  end

  def count(first .. last) do
    validate_range!(first, last)
    if first <= last do
      {:ok, last - first + 1}
    else
      {:ok, first - last + 1}
    end
  end

  defp validate_range!(first, last) when is_integer(first) and is_integer(last), do: :ok
  defp validate_range!(first, last) do
    raise ArgumentError,
          "ranges (left .. right) expect both sides to be integers, got: #{inspect first..last}"
  end
end

defimpl Inspect, for: Range do
  import Inspect.Algebra

  def inspect(first .. last, opts) do
    concat [to_doc(first, opts), "..", to_doc(last, opts)]
  end
end
