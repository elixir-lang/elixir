defprotocol Enum::Iterator, [iterator(collection)], only: [List, Tuple], as: I

# Evalutes the items in the given collection according to the
# Enum::Iterator protocol. Most functions in this module
# will automatically retrieve the protocol given the collection
# and iterator, for example:
#
#     Enum.map [1,2,3], fun(x) { x * 2 }
#
# However, one can use their own iteration function for any
# collection by passing the iterator function as the first
# argument:
#
#     Enum.map my_iteration_function, [1,2,3], fun(x) { x * 2 }
#
# ## The protocol
#
# When `Enum.map` is invoked without the iterator function,
# it invokes `Enum::Iterator.iterator(collection)` with the
# given collection in order to retrieve the default iterator
# for that collection. You can implement the protocol for any
# data type you wish. Elixir ships with a default iterator
# for lists, implemented as follow:
#
#     defimpl Enum::Iterator, for: List do
#       def iterator(_), do: iterate(_)
#
#       defp iterate([h|t]) do
#         { h, t }
#       end
#
#       defp iterate([]) do
#         __STOP_ITERATOR__
#       end
#     end
#
# The __STOP_ITERATOR__ is a special Elixir token that
# marks when iteration should finish.
defmodule Enum do
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
    all?(I.iterator(collection), collection, fun)
  end

  def all?(iterator, collection, fun) do
    do_all?(iterator.(collection), iterator, fun)
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
    any?(I.iterator(collection), collection, fun)
  end

  def any?(iterator, collection, fun) do
    do_any?(iterator.(collection), iterator, fun)
  end

  # Receives a list of enums and a function and recursively
  # invokes the function with an accumulutaor plus each item
  # in each enum as argument in reverse order. Therefore, the
  # arity of the fun must be 1 + number of enums.
  #
  # This is used internally by `for` and should not be used directly.
  def for(lists, fun) do
    iterators = lc list in lists, do: { list, Enum::Iterator.iterator(list) }
    [{h,iterator}|t] = iterators
    first_comprehension_each iterator, iterator.(h), t, [], fun
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
    detect(I.iterator(collection), collection, ifnone, fun)
  end

  def detect(iterator, collection, ifnone, fun) do
    do_detect(iterator.(collection), iterator, ifnone, fun)
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
    detect_value(I.iterator(collection), collection, ifnone, fun)
  end

  def detect_value(iterator, collection, ifnone, fun) do
    do_detect_value(iterator.(collection), iterator, ifnone, fun)
  end

  # Invokes the given `fun` for each item in the `collection`.
  # Returns the `collection` itself.
  #
  # ## Examples
  #
  #     Enum.each ['some', 'example'], fn(x) { IO.puts x }
  #
  def each(collection, fun) do
    each(I.iterator(collection), collection, fun)
  end

  def each(iterator, collection, fun) do
    do_each(iterator.(collection), iterator, fun)
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
    empty?(I.iterator(collection), collection)
  end

  def empty?(iterator, collection) do
    iterator.(collection) == __STOP_ITERATOR__
  end

  # Invokes the given `fun` for each item in the `collection`.
  # Returns only the items the function evalutes to true.
  #
  # ## Examples
  #
  #     Enum.filter [1, 2, 3], fn(x) { rem(x, 2) == 0 }
  #     #=> [2]
  #
  def filter(collection, fun) do
    filter(I.iterator(collection), collection, fun)
  end

  def filter(iterator, collection, fun) do
    do_filter(iterator.(collection), iterator, fun)
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
    foldl(I.iterator(collection), collection, acc, f)
  end

  def foldl(iterator, collection, acc, f) do
    do_foldl(iterator.(collection), iterator, acc, f)
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
  def join(collection, joiner) do
    join(I.iterator(collection), collection, joiner)
  end

  def join(iterator, collection, joiner) when is_list(joiner) do
    binary_to_list join(iterator, collection, list_to_binary(joiner))
  end

  def join(iterator, collection, joiner) do
    do_join(iterator.(collection), iterator, joiner, nil)
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
    map(I.iterator(collection), collection, fun)
  end

  def map(iterator, collection, fun) do
    do_map(iterator.(collection), iterator, fun)
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
    mapfoldl(I.iterator(collection), collection, acc, fun)
  end

  def mapfoldl(iterator, collection, acc, fun) do
    do_mapfoldl(iterator.(collection), iterator, acc, fun)
  end

  ## Implementations

  ## all?

  defp do_all?({ h, next }, iterator, fun) do
    case fun.(h) do
    match: false
      false
    match: nil
      false
    else:
      do_all?(iterator.(next), iterator, fun)
    end
  end

  defp do_all?(__STOP_ITERATOR__, _, _) do
    true
  end

  ## any?

  defp do_any?({ h, next }, iterator, fun) do
    case fun.(h) do
    match: false
      do_any?(iterator.(next), iterator, fun)
    match: nil
      do_any?(iterator.(next), iterator, fun)
    else:
      true
    end
  end

  defp do_any?(__STOP_ITERATOR__, _, _) do
    false
  end

  ## detect

  defp do_detect({ h, next }, iterator, ifnone, fun) do
    case fun.(h) do
    match: false
      do_detect(iterator.(next), iterator, ifnone, fun)
    match: nil
      do_detect(iterator.(next), iterator, ifnone, fun)
    else:
      h
    end
  end

  defp do_detect(__STOP_ITERATOR__, _, ifnone, _) do
    ifnone
  end

  ## detect_value

  defp do_detect_value({ h, next }, iterator, ifnone, fun) do
    case fun.(h) do
    match: false
      do_detect_value(iterator.(next), iterator, ifnone, fun)
    match: nil
      do_detect_value(iterator.(next), iterator, ifnone, fun)
    match: other
      other
    end
  end

  defp do_detect_value(__STOP_ITERATOR__, _, ifnone, _) do
    ifnone
  end

  ## each

  defp do_each({ h, next }, iterator, fun) do
    fun.(h)
    do_each(iterator.(next), iterator, fun)
  end

  defp do_each(__STOP_ITERATOR__, _, _) do
    []
  end

  ## filter

  defp do_filter({ h, next }, iterator, fun) do
    case fun.(h) do
    match: false
      do_filter(iterator.(next), iterator, fun)
    match: nil
      do_filter(iterator.(next), iterator, fun)
    else:
      [h|do_filter(iterator.(next), iterator, fun)]
    end
  end

  defp do_filter(__STOP_ITERATOR__, _, _) do
    []
  end

  ## foldl

  defp do_foldl({ h, next }, iterator, acc, fun) do
    do_foldl(iterator.(next), iterator, fun.(h, acc), fun)
  end

  defp do_foldl(__STOP_ITERATOR__, _, acc, _) do
    acc
  end

  ## join

  # The first item is simply stringified unless ...
  defp do_join({ h, next }, iterator, joiner, nil) do
    do_join(iterator.(next), iterator, joiner, to_binary(h))
  end

  # The first item is __STOP_ITERATOR__, then we return an empty string;
  defp do_join(__STOP_ITERATOR__, _, _joiner, nil) do
    ""
  end

  # All other items are concatenated to acc, by first adding the joiner;
  defp do_join({ h, next }, iterator, joiner, acc) do
    acc = << acc | :binary, joiner | :binary, to_binary(h) | :binary >>
    do_join(iterator.(next), iterator, joiner, acc)
  end

  # Until we have to stop iteration, then we return acc.
  defp do_join(__STOP_ITERATOR__, _, _joiner, acc) do
    acc
  end

  ## map

  defp do_map({ h, next }, iterator, fun) do
    [fun.(h)|do_map(iterator.(next), iterator, fun)]
  end

  defp do_map(__STOP_ITERATOR__, _, _) do
    []
  end

  ## mapfoldl

  defp do_mapfoldl({ h, next }, iterator, acc, f) do
    { result, acc } = f.(h, acc)
    { rest, acc }   = do_mapfoldl(iterator.(next), iterator, acc, f)
    { [result|rest], acc }
  end

  defp do_mapfoldl(__STOP_ITERATOR__, _, acc, _f) do
    { [], acc }
  end

  ## Comprehensions

  defp first_comprehension_each(iterator, { h, next }, t, acc, fun) do
    first_comprehension_each iterator, iterator.(next), t, next_comprehension(t, acc, fun, [h]), fun
  end

  defp first_comprehension_each(_iterator, __STOP_ITERATOR__, _t, acc, _fun) do
    List.reverse(acc)
  end

  defp next_comprehension([{h,iterator}|t], acc, fun, args) do
    next_comprehension_each iterator, iterator.(h), t, acc, fun, args
  end

  defp next_comprehension([], acc, fun, args) do
    apply fun, [acc|args]
  end

  defp next_comprehension_each(iterator, { h, next }, t, acc, fun, args) do
    next_comprehension_each iterator, iterator.(next), t, next_comprehension(t, acc, fun, [h|args]), fun, args
  end

  defp next_comprehension_each(_iterator, __STOP_ITERATOR__, _t, acc, _fun, _args) do
    acc
  end
end

defimpl Enum::Iterator, for: List do
  def iterator(_), do: iterate(_)

  defp iterate([h|t]) do
    { h, t }
  end

  defp iterate([]) do
    __STOP_ITERATOR__
  end
end