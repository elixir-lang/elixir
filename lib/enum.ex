defmodule Enum do
  defprotocol Iterator, [iterator(collection)], only: [List]
  require Enum::Iterator, as: I

  # Invokes the given `fun` for each item in the `collection`.
  # Returns the `collection` itself.
  def each(collection, fun) do
    _each(I.iterator(collection).(), fun)
    collection
  end

  # Invokes the given `fun` for each item in the `collection`.
  # Returns the result of all function calls.
  #
  # ## Examples
  #
  #     Enum.map [1, 2, 3], fn(x) { x * 2 }
  #     #=> [2, 4, 6]
  #
  def map(collection, fun) do
    _map(I.iterator(collection).(), fun)
  end

  ## Implementations

  defp _map({ h, next }, fun) do
    [fun.(h)|_map(next.(), fun)]
  end

  defp _map(__STOP_ITERATOR__, _fun) do
    []
  end

  defp _each({ h, next }, fun) do
    fun.(h)
    _each(next.(), fun)
  end

  defp _each(__STOP_ITERATOR__, _fun) do
    []
  end
end

defimpl Enum::Iterator, for: List do
  def iterator(list) do
    fn { iterate(list) }
  end

  defp iterate([h|t]) do
    { h, fn { iterate(t) } }
  end

  defp iterate([]) do
    __STOP_ITERATOR__
  end
end