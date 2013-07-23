defrecord Range, [:first, :last] do
  @moduledoc """
  Defines a Range.
  """
end

defprotocol Range.Iterator do
  @doc """
  Reduces the range based on the type of the first argument.
  """
  def reduce(first, range, acc, fun)

  @doc """
  Count how many items are in the range.
  """
  def count(first, range)
end

defimpl Enumerable, for: Range do
  def reduce(Range[first: first] = range, acc, fun) do
    Range.Iterator.reduce(first, range, acc, fun)
  end

  def member?(Range[first: first, last: last], value) do
    value in first..last
  end

  def count(Range[first: first] = range) do
    Range.Iterator.count(first, range)
  end
end

defimpl Range.Iterator, for: Number do
  def reduce(first, Range[last: last], acc, fun) when is_integer(first) and is_integer(last) do
    reducer = if last >= first do
      fn(acc, fun) -> do_reducer_up(first, last, acc, fun) end
    else
      fn(acc, fun) -> do_reducer_down(first, last, acc, fun) end
    end
    Enumerable.Function.reduce(reducer, acc, fun)
  end

  defp do_reducer_up(counter, last, acc, _fun) when counter > last do
    acc
  end

  defp do_reducer_up(counter, last, acc, fun) do
    do_reducer_up(counter + 1, last, fun.(counter, acc), fun)
  end

  defp do_reducer_down(counter, last, acc, _fun) when counter < last do
    acc
  end

  defp do_reducer_down(counter, last, acc, fun) do
    do_reducer_down(counter - 1, last, fun.(counter, acc), fun)
  end

  def count(first, Range[last: last]) when is_integer(first) and is_integer(last) and last >= first do
    last - first + 1
  end

  def count(first, Range[last: last]) when is_integer(first) and is_integer(last) do
    first - last + 1
  end
end

defimpl Inspect, for: Range do
  def inspect(Range[first: first, last: last], opts) do
    Inspect.Algebra.concat [Kernel.inspect(first, opts), "..", Kernel.inspect(last, opts)]
  end
end
