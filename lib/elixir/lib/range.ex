defrecord Range, [:first, :last] do
  @moduledoc """
  Defines a Range.
  """
end

defprotocol Range.Iterator do
  @doc """
  How to iterate the range, receives the first
  and range as arguments. It needs to return a
  function that receives an item and returns
  a tuple with two elements: the given item
  and the next item in the iteration.
  """
  def iterator(first, range)

  @doc """
  Count how many items are in the range.
  """
  def count(first, range)
end

defimpl Enum.Iterator, for: Range do
  def iterator(Range[first: first] = range) do
    iterator = Range.Iterator.iterator(first, range)
    { iterator, iterator.(first) }
  end

  def count(Range[first: first] = range) do
    Range.Iterator.count(first, range)
  end
end

defimpl Range.Iterator, for: Number do
  def iterator(first, Range[first: f, last: last]) when is_integer(first) and is_integer(last) and last < f do
    fn(current) ->
      if current < last, do: :stop, else: { current, current - 1 }
    end
  end

  def iterator(first, Range[last: last]) when is_integer(first) and is_integer(last) do
    fn(current) ->
      if current > last, do: :stop, else: { current, current + 1 }
    end
  end

  def count(first, Range[last: last]) when is_integer(first) and is_integer(last) do
    last - first + 1
  end
end