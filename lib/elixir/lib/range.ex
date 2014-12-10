defmodule Range do
  @moduledoc """
  Defines a Range.

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
  Returns true if the given argument is a range.

  ## Examples

      iex> Range.range?(1..3)
      true

      iex> Range.range?(0)
      false

  """
  def range?(%Range{}), do: true
  def range?(_), do: false
end

defprotocol Range.Iterator do
  @moduledoc """
  A protocol used for iterating range elements.
  """

  @doc """
  Returns the function that calculates the next item.
  """
  def next(first, range)

  @doc """
  Counts how many items are in the range.
  """
  def count(first, range)
end

defimpl Enumerable, for: Range do
  def reduce(first .. last = range, acc, fun) do
    reduce(first, last, acc, fun, Range.Iterator.next(first, range), last >= first)
  end

  defp reduce(_x, _y, {:halt, acc}, _fun, _next, _up) do
    {:halted, acc}
  end

  defp reduce(x, y, {:suspend, acc}, fun, next, up) do
    {:suspended, acc, &reduce(x, y, &1, fun, next, up)}
  end

  defp reduce(x, y, {:cont, acc}, fun, next, true) when x <= y do
    reduce(next.(x), y, fun.(x, acc), fun, next, true)
  end

  defp reduce(x, y, {:cont, acc}, fun, next, false) when x >= y do
    reduce(next.(x), y, fun.(x, acc), fun, next, false)
  end

  defp reduce(_, _, {:cont, acc}, _fun, _next, _up) do
    {:done, acc}
  end

  def member?(first .. last, value) do
    if first <= last do
      {:ok, first <= value and value <= last}
    else
      {:ok, last <= value and value <= first}
    end
  end

  def count(first .. _ = range) do
    {:ok, Range.Iterator.count(first, range)}
  end
end

defimpl Range.Iterator, for: Integer do
  def next(first, _ .. last) when is_integer(last) do
    if last >= first do
      &(&1 + 1)
    else
      &(&1 - 1)
    end
  end

  def count(first, _ .. last) when is_integer(last) do
    if last >= first do
      last - first + 1
    else
      first - last + 1
    end
  end
end

defimpl Inspect, for: Range do
  import Inspect.Algebra

  def inspect(first .. last, opts) do
    concat [to_doc(first, opts), "..", to_doc(last, opts)]
  end
end
