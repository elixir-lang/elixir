defmodule Enum do
  defprotocol Iterator, [iterator(collection)], only: [List]
  require Enum::Iterator, as: I

  # Invokes the given `fun` for each item in the `collection`.
  # Returns the `collection` itself.
  def each(collection, fun) do
    _each(I.iterator(collection).(), fun)
    collection
  end

  # Join the given `collection` according to `joiner`.
  # Joiner can be either a binary or a list and the
  # result will be of the same type of joiner.
  #
  # == Examples
  #
  #     List.join([1,2,3], " = ") #=> "1 = 2 = 3"
  #     List.join([1,2,3], ' = ') #=> '1 = 2 = 3'
  #
  def join(collection, joiner) when is_list(joiner) do
    binary_to_list join(collection, list_to_binary(joiner))
  end

  def join(collection, joiner) do
    _join(I.iterator(collection).(), joiner, nil)
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

  ## Each

  defp _each({ h, next }, fun) do
    fun.(h)
    _each(next.(), fun)
  end

  defp _each(__STOP_ITERATOR__, _fun) do
    []
  end

  ## Join

  # The first item is simply stringified unless ...
  def _join({ h, next }, joiner, nil) do
    _join(next.(), joiner, stringify(h))
  end

  # The first item is __STOP_ITERATOR__, then we return an empty string;
  def _join(__STOP_ITERATOR__, _joiner, nil) do
    ""
  end

  # All other items are concatenated to acc, by first adding the joiner;
  def _join({ h, next }, joiner, acc) do
    acc = << acc | :binary, joiner | :binary, stringify(h) | :binary >>
    _join(next.(), joiner, acc)
  end

  # Until we have to stop iteration, then we return acc.
  def _join(__STOP_ITERATOR__, _joiner, acc) do
    acc
  end

  ## Map

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