defmodule Enum do
  defprotocol Iterator, [iterator(collection)], only: [List]
  require Enum::Iterator, as: I

  # Invokes the given `fun` for each item in the `collection`
  # checking if all results evalutes to true. If any does not,
  # abort and return false. Otherwise, true.
  #
  # ## Examples
  #
  #     Enum.all? [2,4,6], fn(x) { rem(x, 2) == 0 }
  #     #=> true
  #
  #     Enum.all? [2,3,4], fn(x) { rem(x, 2) == 0 }
  #     #=> false
  #
  # If no function is given, it defaults to checking if
  # all items in the collection evalutes to true.
  #
  #     Enum.all? [1,2,3]   #=> true
  #     Enum.all? [1,nil,3] #=> false
  #
  def all?(collection, fun // fn(x) { x }) do
    _all?(I.iterator(collection).(), fun)
  end

  # Invokes the given `fun` for each item in the `collection`
  # checking if any of the results returns true. If one does,
  # aborts and returns true. If not, returns false.
  #
  # ## Examples
  #
  #     Enum.any? [2,4,6], fn(x) { rem(x, 2) == 1 }
  #     #=> false
  #
  #     Enum.any? [2,3,4], fn(x) { rem(x, 2) == 1 }
  #     #=> true
  #
  # If no function is given, it defaults to checking if
  # any item in the collection evalutes to true.
  #
  #     Enum.any? [false,false,false] #=> false
  #     Enum.any? [false,true,false]  #=> true
  #
  def any?(collection, fun // fn(x) { x }) do
    _any?(I.iterator(collection).(), fun)
  end

  # Invokes the `fun` for each item in collection
  # and returns the first the function returns a truthy
  # value. If no item is found, returns `ifnone`.
  #
  # ## Examples
  #
  #     Enum.detect [2,4,6], fn(x) { rem(x, 2) == 1 }
  #     # => nil
  #
  #     Enum.detect [2,4,6], 0, fn(x) { rem(x, 2) == 1 }
  #     # => 0
  #
  #     Enum.detect [2,3,4], fn(x) { rem(x, 2) == 1 }
  #     # => 3
  #
  def detect(collection, ifnone // nil, fun) do
    _detect(I.iterator(collection).(), ifnone, fun)
  end

  # Similar to detect, but returns the value of the function
  # invocation instead of the element iterated.
  #
  # ## Examples
  #
  #     Enum.detect_value [2,4,6], fn(x) { rem(x, 2) == 1 }
  #     # => nil
  #
  #     Enum.detect_value [2,4,6], 0, fn(x) { rem(x, 2) == 1 }
  #     # => 0
  #
  #     Enum.detect_value [2,3,4], fn(x) { rem(x, 2) == 1 }
  #     # => true
  #
  def detect_value(collection, ifnone // nil, fun) do
    _detect_value(I.iterator(collection).(), ifnone, fun)
  end

  # Invokes the given `fun` for each item in the `collection`.
  # Returns the `collection` itself.
  #
  # ## Examples
  #
  #     Enum.each ['some', 'example'], fn(x) { IO.puts x }
  #
  def each(collection, fun) do
    _each(I.iterator(collection).(), fun)
    collection
  end

  # Returns if the collection is empty or not.
  #
  # ## Examples
  #
  #     Enum.empty? [] #=> true
  #     Enum.empty? [1,2,3] #=> false
  #
  def empty?(collection) do
    I.iterator(collection).() == __STOP_ITERATOR__
  end

  # Iterates the collection from left to right passing an
  # accumulator as parameter. Returns the accumulator.
  #
  # ## Examples
  #
  #     Enum.foldl [1, 2, 3], 0, fn(x, acc) { x + acc }
  #     #=> 6
  #
  def foldl(collection, acc, f) do
    _foldl(I.iterator(collection).(), acc, f)
  end

  # Join the given `collection` according to `joiner`.
  # Joiner can be either a binary or a list and the
  # result will be of the same type of joiner.
  #
  # ## Examples
  #
  #     Enum.join([1,2,3], " = ") #=> "1 = 2 = 3"
  #     Enum.join([1,2,3], ' = ') #=> '1 = 2 = 3'
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

  # Invokes the given `fun` for each item in the `collection`
  # while also keeping an accumulator. Returns a tuple where
  # the first element is the iterated collection and the second
  # one is the final accumulator.
  #
  # ## Examples
  #
  #     Enum.mapfoldl [1, 2, 3], 0, fn(x, acc) { { x * 2, x + acc } }
  #     #=> { [2, 4, 6], 6 }
  #
  def mapfoldl(collection, acc, fun) do
    _mapfoldl(I.iterator(collection).(), acc, fun)
  end

  ## Implementations

  ## all?

  defp _all?({ h, next }, fun) do
    case fun.(h) do
    match: false
      false
    match: nil
      false
    else:
      _all?(next.(), fun)
    end
  end

  defp _all?(__STOP_ITERATOR__, _) do
    true
  end

  ## any?

  defp _any?({ h, next }, fun) do
    case fun.(h) do
    match: false
      _any?(next.(), fun)
    match: nil
      _any?(next.(), fun)
    else:
      true
    end
  end

  defp _any?(__STOP_ITERATOR__, _) do
    false
  end

  ## detect

  defp _detect({ h, next }, ifnone, fun) do
    case fun.(h) do
    match: false
      _detect(next.(), ifnone, fun)
    match: nil
      _detect(next.(), ifnone, fun)
    else:
      h
    end
  end

  defp _detect(__STOP_ITERATOR__, ifnone, _) do
    ifnone
  end

  ## detect_value

  defp _detect_value({ h, next }, ifnone, fun) do
    case fun.(h) do
    match: false
      _detect_value(next.(), ifnone, fun)
    match: nil
      _detect_value(next.(), ifnone, fun)
    match: other
      other
    end
  end

  defp _detect_value(__STOP_ITERATOR__, ifnone, _) do
    ifnone
  end

  ## each

  defp _each({ h, next }, fun) do
    fun.(h)
    _each(next.(), fun)
  end

  defp _each(__STOP_ITERATOR__, _fun) do
    []
  end

  ## foldl

  defp _foldl({ h, next }, acc, f) do
    _foldl(next.(), f.(h, acc), f)
  end

  defp _foldl(__STOP_ITERATOR__, acc, _f) do
    acc
  end

  ## join

  # The first item is simply stringified unless ...
  defp _join({ h, next }, joiner, nil) do
    _join(next.(), joiner, stringify(h))
  end

  # The first item is __STOP_ITERATOR__, then we return an empty string;
  defp _join(__STOP_ITERATOR__, _joiner, nil) do
    ""
  end

  # All other items are concatenated to acc, by first adding the joiner;
  defp _join({ h, next }, joiner, acc) do
    acc = << acc | :binary, joiner | :binary, stringify(h) | :binary >>
    _join(next.(), joiner, acc)
  end

  # Until we have to stop iteration, then we return acc.
  defp _join(__STOP_ITERATOR__, _joiner, acc) do
    acc
  end

  ## map

  defp _map({ h, next }, fun) do
    [fun.(h)|_map(next.(), fun)]
  end

  defp _map(__STOP_ITERATOR__, _fun) do
    []
  end

  ## mapfoldl

  defp _mapfoldl({ h, next }, acc, f) do
    { result, acc } = f.(h, acc)
    { rest, acc }   = _mapfoldl(next.(), acc, f)
    { [result|rest], acc }
  end

  defp _mapfoldl(__STOP_ITERATOR__, acc, _f) do
    { [], acc }
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