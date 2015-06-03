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
    reduce(first, last, acc, fun, Range.Iterator.next(first, range), false)
  end

  defp reduce(_current, _last, {:halt, acc}, _fun, _next, _done) do
    {:halted, acc}
  end

  defp reduce(current, last, {:suspend, acc}, fun, next, done) do
    {:suspended, acc, &reduce(current, last, &1, fun, next, done)}
  end

  defp reduce(last, last, {:cont, acc}, fun, next, false) do
    reduce(last, last, fun.(last, acc), fun, next, true)
  end

  defp reduce(current, last, {:cont, acc}, fun, next, false) do
    reduce(next.(current), last, fun.(current, acc), fun, next, false)
  end

  defp reduce(_, _, {:cont, acc}, _fun, _next, true) do
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
