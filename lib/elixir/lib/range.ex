defrecord Range, [:first, :last] do
  @moduledoc """
  Defines a Range.
  """
end

defprotocol Range.Iterator do
  @doc """
  Returns the function that calculates the next item.
  """
  def next(first, range)

  @doc """
  Count how many items are in the range.
  """
  def count(first, range)
end

defimpl Enumerable, for: Range do
  def reduce(first .. last = range, acc, fun) do
    reduce(first, last, acc, fun, Range.Iterator.next(first, range), last >= first)
  end

  defp reduce(_x, _y, { :halt, acc }, _fun, _next, _up) do
    { :halted, acc }
  end

  defp reduce(x, y, { :suspend, acc }, _fun, next, up) do
    { :suspended, acc, &reduce(x, y, &1, &2, next, up) }
  end

  defp reduce(x, y, { :cont, acc }, fun, next, true) when x <= y do
    reduce(next.(x), y, fun.(x, acc), fun, next, true)
  end

  defp reduce(x, y, { :cont, acc }, fun, next, false) when x >= y do
    reduce(next.(x), y, fun.(x, acc), fun, next, false)
  end

  defp reduce(_, _, { :cont, acc }, _fun, _next, _up) do
    { :done, acc }
  end

  def member?(first .. last, value) do
    if first <= last do
      first <= value and value <= last
    else
      last <= value and value <= first
    end
  end

  def count(first .. _ = range) do
    Range.Iterator.count(first, range)
  end
end

defimpl Range.Iterator, for: Integer do
  def next(first, Range[last: last]) when is_integer(last) do
    if last >= first do
      &(&1 + 1)
    else
      &(&1 - 1)
    end
  end

  def count(first, Range[last: last]) when is_integer(last) do
    if last >= first do
      last - first + 1
    else
      first - last + 1
    end
  end
end

defimpl Inspect, for: Range do
  def inspect(Range[first: first, last: last], opts) do
    Inspect.Algebra.concat [Kernel.inspect(first, opts), "..", Kernel.inspect(last, opts)]
  end
end
