defmodule Range do
  def __record__(kind, _), do: __record__(kind)
  def __record__(:name),   do: __MODULE__
  def __record__(:fields), do: [{:first,nil},{:last,nil}]

  @doc """
  Returns a new range based on the given options.

  ## Examples

      Range.new first: 1, last: 10

  """
  def new(options) do
    new Keyword.fetch(options, :first), Keyword.fetch(options, :last)
  end

  @doc """
  Returns a new range based on the given first and last.

  ## Examples

      Range.new 1, 10

  """
  def new(first, last) do
    { __MODULE__, first, last }
  end

  @doc """
  Returns the first item of the range.
  """
  def first(range) do
    elem(range, 2)
  end

  @doc """
  Returns the last item of the range.
  """
  def last(range) do
    elem(range, 3)
  end
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

defimpl Enum.OrdIterator, for: Range do
  def iterator(range) do
    Enum.Iterator.Range.iterator(range)
  end

  def to_list({ h, next }, iterator), do: [h|to_list(iterator.(next), iterator)]
  def to_list(:stop, _),              do: []
end

defimpl Range.Iterator, for: Number do
  def iterator(first, Range[last: last]) when is_integer(first) and is_integer(last) do
    fn(current) ->
      if current > last, do: :stop, else: { current, current + 1 }
    end
  end

  def count(first, Range[last: last]) when is_integer(first) and is_integer(last) do
    last - first + 1
  end
end