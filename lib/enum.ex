defmodule Enum do
  defprotocol Iterator, [iterator(collection)], only: [List]
  require Enum::Iterator, as: I

  def map(thing, fun) do
    _map(I.iterator(thing).(), fun)
  end

  ## Implementations

  defp _map({ h, next }, fun) do
    [fun.(h)|_map(next.(), fun)]
  end

  defp _map(__STOP_ITERATOR__, _fun) do
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