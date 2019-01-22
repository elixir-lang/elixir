defmodule List do
  @moduledoc """
  Functions that work on (linked) lists.

  Many of the functions provided for lists, which implement
  the `Enumerable` protocol, are found in the `Enum` module.

  Additionally, the following functions and operators for lists are
  found in `Kernel`:

    * `++/2`
    * `--/2`
    * `hd/1`
    * `tl/1`
    * `in/2`
    * `length/1`

  Lists in Elixir are specified between square brackets:

      iex> [1, "two", 3, :four]
      [1, "two", 3, :four]

  Two lists can be concatenated and subtracted using the
  `Kernel.++/2` and `Kernel.--/2` operators:

      iex> [1, 2, 3] ++ [4, 5, 6]
      [1, 2, 3, 4, 5, 6]
      iex> [1, true, 2, false, 3, true] -- [true, false]
      [1, 2, 3, true]

  Lists in Elixir are effectively linked lists, which means
  they are internally represented in pairs containing the
  head and the tail of a list:

      iex> [head | tail] = [1, 2, 3]
      iex> head
      1
      iex> tail
      [2, 3]

  Similarly, we could write the list `[1, 2, 3]` using only
  such pairs (called cons cells):

      iex> [1 | [2 | [3 | []]]]
      [1, 2, 3]

  Some lists, called improper lists, do not have an empty list as
  the second element in the last cons cell:

      iex> [1 | [2 | [3 | 4]]]
      [1, 2, 3 | 4]

  Although improper lists are generally avoided, they are used in some
  special circumstances like iodata and chardata entities (see the `IO` module).

  Due to their cons cell based representation, prepending an element
  to a list is always fast (constant time), while appending becomes
  slower as the list grows in size (linear time):

      iex> list = [1, 2, 3]
      iex> [0 | list] # fast
      [0, 1, 2, 3]
      iex> list ++ [4] # slow
      [1, 2, 3, 4]

  Additionally, getting a list's length and accessing it by index are
  linear time operations. Negative indexes are also supported but
  they imply the list will be iterated twice, once to calculate the
  proper index and another time to perform the operation.

  ## Charlists

  If a list is made of non-negative integers, it can also be called
  a charlist. Elixir uses single quotes to define charlists:

      iex> 'héllo'
      [104, 233, 108, 108, 111]

  In particular, charlists may be printed back in single
  quotes if they contain only ASCII-printable codepoints:

      iex> 'abc'
      'abc'

  The rationale behind this behaviour is to better support
  Erlang libraries which may return text as charlists
  instead of Elixir strings. One example of such functions
  is `Application.loaded_applications/0`:

      Application.loaded_applications()
      #=>  [
      #=>    {:stdlib, 'ERTS  CXC 138 10', '2.6'},
      #=>    {:compiler, 'ERTS  CXC 138 10', '6.0.1'},
      #=>    {:elixir, 'elixir', '1.0.0'},
      #=>    {:kernel, 'ERTS  CXC 138 10', '4.1'},
      #=>    {:logger, 'logger', '1.0.0'}
      #=>  ]

  A list can be checked if it is made of printable ASCII
  codepoints with `ascii_printable?/2`.
  """

  @compile :inline_list_funcs

  @doc """
  Deletes the given `element` from the `list`. Returns a new list without
  the element.

  If the `element` occurs more than once in the `list`, just
  the first occurrence is removed.

  ## Examples

      iex> List.delete([:a, :b, :c], :a)
      [:b, :c]

      iex> List.delete([:a, :b, :b, :c], :b)
      [:a, :b, :c]

  """
  @spec delete(list, any) :: list
  def delete(list, element)
  def delete([element | list], element), do: list
  def delete([other | list], element), do: [other | delete(list, element)]
  def delete([], _element), do: []

  @doc """
  Duplicates the given element `n` times in a list.

  ## Examples

      iex> List.duplicate("hello", 3)
      ["hello", "hello", "hello"]

      iex> List.duplicate([1, 2], 2)
      [[1, 2], [1, 2]]

  """
  @spec duplicate(elem, non_neg_integer) :: [elem] when elem: var
  def duplicate(elem, n) do
    :lists.duplicate(n, elem)
  end

  @doc """
  Flattens the given `list` of nested lists.

  ## Examples

      iex> List.flatten([1, [[2], 3]])
      [1, 2, 3]

  """
  @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
  def flatten(list) do
    :lists.flatten(list)
  end

  @doc """
  Flattens the given `list` of nested lists.
  The list `tail` will be added at the end of
  the flattened list.

  ## Examples

      iex> List.flatten([1, [[2], 3]], [4, 5])
      [1, 2, 3, 4, 5]

  """
  @spec flatten(deep_list, [elem]) :: [elem] when elem: var, deep_list: [elem | deep_list]
  def flatten(list, tail) do
    :lists.flatten(list, tail)
  end

  @doc """
  Folds (reduces) the given list from the left with
  a function. Requires an accumulator.

  ## Examples

      iex> List.foldl([5, 5], 10, fn x, acc -> x + acc end)
      20

      iex> List.foldl([1, 2, 3, 4], 0, fn x, acc -> x - acc end)
      2

  """
  @spec foldl([elem], acc, (elem, acc -> acc)) :: acc when elem: var, acc: var
  def foldl(list, acc, fun) when is_list(list) and is_function(fun) do
    :lists.foldl(fun, acc, list)
  end

  @doc """
  Folds (reduces) the given list from the right with
  a function. Requires an accumulator.

  ## Examples

      iex> List.foldr([1, 2, 3, 4], 0, fn x, acc -> x - acc end)
      -2

  """
  @spec foldr([elem], acc, (elem, acc -> acc)) :: acc when elem: var, acc: var
  def foldr(list, acc, fun) when is_list(list) and is_function(fun) do
    :lists.foldr(fun, acc, list)
  end

  @doc """
  Returns the first element in `list` or `nil` if `list` is empty.

  ## Examples

      iex> List.first([])
      nil

      iex> List.first([1])
      1

      iex> List.first([1, 2, 3])
      1

  """
  @spec first([elem]) :: nil | elem when elem: var
  def first([]), do: nil
  def first([head | _]), do: head

  @doc """
  Returns the last element in `list` or `nil` if `list` is empty.

  ## Examples

      iex> List.last([])
      nil

      iex> List.last([1])
      1

      iex> List.last([1, 2, 3])
      3

  """
  @spec last([elem]) :: nil | elem when elem: var
  def last([]), do: nil
  def last([head]), do: head
  def last([_ | tail]), do: last(tail)

  @doc """
  Receives a list of tuples and returns the first tuple
  where the element at `position` in the tuple matches the
  given `key`.

  ## Examples

      iex> List.keyfind([a: 1, b: 2], :a, 0)
      {:a, 1}

      iex> List.keyfind([a: 1, b: 2], 2, 1)
      {:b, 2}

      iex> List.keyfind([a: 1, b: 2], :c, 0)
      nil

  """
  @spec keyfind([tuple], any, non_neg_integer, any) :: any
  def keyfind(list, key, position, default \\ nil) do
    :lists.keyfind(key, position + 1, list) || default
  end

  @doc """
  Receives a list of tuples and returns `true` if there is
  a tuple where the element at `position` in the tuple matches
  the given `key`.

  ## Examples

      iex> List.keymember?([a: 1, b: 2], :a, 0)
      true

      iex> List.keymember?([a: 1, b: 2], 2, 1)
      true

      iex> List.keymember?([a: 1, b: 2], :c, 0)
      false

  """
  @spec keymember?([tuple], any, non_neg_integer) :: boolean
  def keymember?(list, key, position) do
    :lists.keymember(key, position + 1, list)
  end

  @doc """
  Receives a list of tuples and if the identified element by `key` at `position`
  exists, it is replaced with `new_tuple`.

  ## Examples

      iex> List.keyreplace([a: 1, b: 2], :a, 0, {:a, 3})
      [a: 3, b: 2]

      iex> List.keyreplace([a: 1, b: 2], :a, 1, {:a, 3})
      [a: 1, b: 2]

  """
  @spec keyreplace([tuple], any, non_neg_integer, tuple) :: [tuple]
  def keyreplace(list, key, position, new_tuple) do
    :lists.keyreplace(key, position + 1, list, new_tuple)
  end

  @doc """
  Receives a list of tuples and sorts the elements
  at `position` of the tuples. The sort is stable.

  ## Examples

      iex> List.keysort([a: 5, b: 1, c: 3], 1)
      [b: 1, c: 3, a: 5]

      iex> List.keysort([a: 5, c: 1, b: 3], 0)
      [a: 5, b: 3, c: 1]

  """
  @spec keysort([tuple], non_neg_integer) :: [tuple]
  def keysort(list, position) do
    :lists.keysort(position + 1, list)
  end

  @doc """
  Receives a `list` of tuples and replaces the element
  identified by `key` at `position` with `new_tuple`.

  If the element does not exist, it is added to the end of the `list`.

  ## Examples

      iex> List.keystore([a: 1, b: 2], :a, 0, {:a, 3})
      [a: 3, b: 2]

      iex> List.keystore([a: 1, b: 2], :c, 0, {:c, 3})
      [a: 1, b: 2, c: 3]

  """
  @spec keystore([tuple], any, non_neg_integer, tuple) :: [tuple, ...]
  def keystore(list, key, position, new_tuple) do
    :lists.keystore(key, position + 1, list, new_tuple)
  end

  @doc """
  Receives a `list` of tuples and deletes the first tuple
  where the element at `position` matches the
  given `key`. Returns the new list.

  ## Examples

      iex> List.keydelete([a: 1, b: 2], :a, 0)
      [b: 2]

      iex> List.keydelete([a: 1, b: 2], 2, 1)
      [a: 1]

      iex> List.keydelete([a: 1, b: 2], :c, 0)
      [a: 1, b: 2]

  """
  @spec keydelete([tuple], any, non_neg_integer) :: [tuple]
  def keydelete(list, key, position) do
    :lists.keydelete(key, position + 1, list)
  end

  @doc """
  Receives a `list` of tuples and returns the first tuple
  where the element at `position` in the tuple matches the
  given `key`, as well as the `list` without found tuple.

  If such a tuple is not found, `nil` will be returned.

  ## Examples

      iex> List.keytake([a: 1, b: 2], :a, 0)
      {{:a, 1}, [b: 2]}

      iex> List.keytake([a: 1, b: 2], 2, 1)
      {{:b, 2}, [a: 1]}

      iex> List.keytake([a: 1, b: 2], :c, 0)
      nil

  """
  @spec keytake([tuple], any, non_neg_integer) :: {tuple, [tuple]} | nil
  def keytake(list, key, position) do
    case :lists.keytake(key, position + 1, list) do
      {:value, element, list} -> {element, list}
      false -> nil
    end
  end

  @doc """
  Wraps `term` in a list if this is not list.

  If `term` is already a list, it returns the list.
  If `term` is `nil`, it returns an empty list.

  ## Examples

      iex> List.wrap("hello")
      ["hello"]

      iex> List.wrap([1, 2, 3])
      [1, 2, 3]

      iex> List.wrap(nil)
      []

  """
  @spec wrap(nil) :: []
  @spec wrap(list) :: list when list: maybe_improper_list()
  @spec wrap(term) :: nonempty_list(term) when term: any()
  def wrap(term)

  def wrap(list) when is_list(list) do
    list
  end

  def wrap(nil) do
    []
  end

  def wrap(other) do
    [other]
  end

  @doc """
  Zips corresponding elements from each list in `list_of_lists`.

  The zipping finishes as soon as any list terminates.

  ## Examples

      iex> List.zip([[1, 2], [3, 4], [5, 6]])
      [{1, 3, 5}, {2, 4, 6}]

      iex> List.zip([[1, 2], [3], [5, 6]])
      [{1, 3, 5}]

  """
  @spec zip([list]) :: [tuple]
  def zip([]), do: []

  def zip(list_of_lists) when is_list(list_of_lists) do
    do_zip(list_of_lists, [])
  end

  @doc ~S"""
  Checks if `list` is a charlist made only of printable ASCII characters.

  Takes an optional `limit` as a second argument. `ascii_printable?/2` only
  checks the printability of the list up to the `limit`.

  A printable charlist in Elixir contains only the printable characters in the
  standard seven-bit ASCII character encoding, which are characters ranging from
  32 to 126 in decimal notation, plus the following control characters:

    * `?\a` - Bell
    * `?\b` - Backspace
    * `?\t` - Horizontal tab
    * `?\n` - Line feed
    * `?\v` - Vertical tab
    * `?\f` - Form feed
    * `?\r` - Carriage return
    * `?\e` - Escape

  For more information read the [Character groups](https://en.wikipedia.org/wiki/ASCII#Character_groups)
  section in the Wikipedia article of the [ASCII](https://en.wikipedia.org/wiki/ASCII) standard.

  ## Examples

      iex> List.ascii_printable?('abc')
      true

      iex> List.ascii_printable?('abc' ++ [0])
      false

      iex> List.ascii_printable?('abc' ++ [0], 2)
      true

  Improper lists are not printable, even if made only of ASCII characters:

      iex> List.ascii_printable?('abc' ++ ?d)
      false

  """
  @doc since: "1.6.0"
  @spec ascii_printable?(list, limit) :: boolean
        when limit: :infinity | non_neg_integer
  def ascii_printable?(list, limit \\ :infinity)
      when is_list(list) and (limit == :infinity or (is_integer(limit) and limit >= 0)) do
    ascii_printable_guarded?(list, limit)
  end

  defp ascii_printable_guarded?(_, 0) do
    true
  end

  defp ascii_printable_guarded?([char | rest], counter)
       # 7..13 is the range '\a\b\t\n\v\f\r'. 32..126 are ASCII printables.
       when is_integer(char) and
              ((char >= 7 and char <= 13) or char == ?\e or (char >= 32 and char <= 126)) do
    ascii_printable_guarded?(rest, decrement(counter))
  end

  defp ascii_printable_guarded?([], _counter), do: true
  defp ascii_printable_guarded?(_, _counter), do: false

  @compile {:inline, decrement: 1}
  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1

  @doc """
  Returns `true` if `list` is an improper list. Otherwise returns `false`.

  ## Examples

     iex> List.improper?([1, 2 | 3])
     true

     iex> List.improper?([1, 2, 3])
     false

  """
  @doc since: "1.8.0"
  @spec improper?(maybe_improper_list) :: boolean
  def improper?(list) when is_list(list) and length(list) >= 0, do: false
  def improper?(list) when is_list(list), do: true

  @doc """
  Returns a list with `value` inserted at the specified `index`.

  Note that `index` is capped at the list length. Negative indices
  indicate an offset from the end of the `list`.

  ## Examples

      iex> List.insert_at([1, 2, 3, 4], 2, 0)
      [1, 2, 0, 3, 4]

      iex> List.insert_at([1, 2, 3], 10, 0)
      [1, 2, 3, 0]

      iex> List.insert_at([1, 2, 3], -1, 0)
      [1, 2, 3, 0]

      iex> List.insert_at([1, 2, 3], -10, 0)
      [0, 1, 2, 3]

  """
  @spec insert_at(list, integer, any) :: list
  def insert_at(list, index, value) when is_list(list) and is_integer(index) do
    case index do
      -1 ->
        list ++ [value]

      _ when index < 0 ->
        case length(list) + index + 1 do
          index when index < 0 -> [value | list]
          index -> do_insert_at(list, index, value)
        end

      _ ->
        do_insert_at(list, index, value)
    end
  end

  @doc """
  Returns a list with a replaced value at the specified `index`.

  Negative indices indicate an offset from the end of the `list`.
  If `index` is out of bounds, the original `list` is returned.

  ## Examples

      iex> List.replace_at([1, 2, 3], 0, 0)
      [0, 2, 3]

      iex> List.replace_at([1, 2, 3], 10, 0)
      [1, 2, 3]

      iex> List.replace_at([1, 2, 3], -1, 0)
      [1, 2, 0]

      iex> List.replace_at([1, 2, 3], -10, 0)
      [1, 2, 3]

  """
  @spec replace_at(list, integer, any) :: list
  def replace_at(list, index, value) when is_list(list) and is_integer(index) do
    if index < 0 do
      case length(list) + index do
        index when index < 0 -> list
        index -> do_replace_at(list, index, value)
      end
    else
      do_replace_at(list, index, value)
    end
  end

  @doc """
  Returns a list with an updated value at the specified `index`.

  Negative indices indicate an offset from the end of the `list`.
  If `index` is out of bounds, the original `list` is returned.

  ## Examples

      iex> List.update_at([1, 2, 3], 0, &(&1 + 10))
      [11, 2, 3]

      iex> List.update_at([1, 2, 3], 10, &(&1 + 10))
      [1, 2, 3]

      iex> List.update_at([1, 2, 3], -1, &(&1 + 10))
      [1, 2, 13]

      iex> List.update_at([1, 2, 3], -10, &(&1 + 10))
      [1, 2, 3]

  """
  @spec update_at([elem], integer, (elem -> any)) :: list when elem: var
  def update_at(list, index, fun) when is_list(list) and is_function(fun) and is_integer(index) do
    if index < 0 do
      case length(list) + index do
        index when index < 0 -> list
        index -> do_update_at(list, index, fun)
      end
    else
      do_update_at(list, index, fun)
    end
  end

  @doc """
  Produces a new list by removing the value at the specified `index`.

  Negative indices indicate an offset from the end of the `list`.
  If `index` is out of bounds, the original `list` is returned.

  ## Examples

      iex> List.delete_at([1, 2, 3], 0)
      [2, 3]

      iex> List.delete_at([1, 2, 3], 10)
      [1, 2, 3]

      iex> List.delete_at([1, 2, 3], -1)
      [1, 2]

  """
  @spec delete_at(list, integer) :: list
  def delete_at(list, index) when is_integer(index) do
    elem(pop_at(list, index), 1)
  end

  @doc """
  Returns and removes the value at the specified `index` in the `list`.

  Negative indices indicate an offset from the end of the `list`.
  If `index` is out of bounds, the original `list` is returned.

  ## Examples

      iex> List.pop_at([1, 2, 3], 0)
      {1, [2, 3]}
      iex> List.pop_at([1, 2, 3], 5)
      {nil, [1, 2, 3]}
      iex> List.pop_at([1, 2, 3], 5, 10)
      {10, [1, 2, 3]}
      iex> List.pop_at([1, 2, 3], -1)
      {3, [1, 2]}

  """
  @doc since: "1.4.0"
  @spec pop_at(list, integer, any) :: {any, list}
  def pop_at(list, index, default \\ nil) when is_integer(index) do
    if index < 0 do
      do_pop_at(list, length(list) + index, default, [])
    else
      do_pop_at(list, index, default, [])
    end
  end

  @doc """
  Returns `true` if `list` starts with the given `prefix` list; otherwise returns `false`.

  If `prefix` is an empty list, it returns `true`.

  ### Examples

      iex> List.starts_with?([1, 2, 3], [1, 2])
      true

      iex> List.starts_with?([1, 2], [1, 2, 3])
      false

      iex> List.starts_with?([:alpha], [])
      true

      iex> List.starts_with?([], [:alpha])
      false

  """
  @doc since: "1.5.0"
  @spec starts_with?(list, list) :: boolean
  @spec starts_with?(list, []) :: true
  @spec starts_with?([], nonempty_list) :: false
  def starts_with?(list, prefix)

  def starts_with?([head | tail], [head | prefix_tail]), do: starts_with?(tail, prefix_tail)
  def starts_with?(list, []) when is_list(list), do: true
  def starts_with?(list, [_ | _]) when is_list(list), do: false

  @doc """
  Converts a charlist to an atom.

  Elixir supports conversions from charlists which contains any Unicode
  codepoint.

  Inlined by the compiler.

  ## Examples

      iex> List.to_atom('Elixir')
      :Elixir

      iex> List.to_atom('🌢 Elixir')
      :"🌢 Elixir"

  """
  @spec to_atom(charlist) :: atom
  def to_atom(charlist) do
    :erlang.list_to_atom(charlist)
  end

  @doc """
  Converts a charlist to an existing atom. Raises an `ArgumentError`
  if the atom does not exist.

  Elixir supports conversions from charlists which contains any Unicode
  codepoint.

  Inlined by the compiler.

  ## Examples

      iex> _ = :my_atom
      iex> List.to_existing_atom('my_atom')
      :my_atom

      iex> _ = :"🌢 Elixir"
      iex> List.to_existing_atom('🌢 Elixir')
      :"🌢 Elixir"

      iex> List.to_existing_atom('this_atom_will_never_exist')
      ** (ArgumentError) argument error

  """
  @spec to_existing_atom(charlist) :: atom
  def to_existing_atom(charlist) do
    :erlang.list_to_existing_atom(charlist)
  end

  @doc """
  Returns the float whose text representation is `charlist`.

  Inlined by the compiler.

  ## Examples

      iex> List.to_float('2.2017764e+0')
      2.2017764

  """
  @spec to_float(charlist) :: float
  def to_float(charlist) do
    :erlang.list_to_float(charlist)
  end

  @doc """
  Returns an integer whose text representation is `charlist`.

  Inlined by the compiler.

  ## Examples

      iex> List.to_integer('123')
      123

  """
  @spec to_integer(charlist) :: integer
  def to_integer(charlist) do
    :erlang.list_to_integer(charlist)
  end

  @doc """
  Returns an integer whose text representation is `charlist` in base `base`.

  Inlined by the compiler.

  ## Examples

      iex> List.to_integer('3FF', 16)
      1023

  """
  @spec to_integer(charlist, 2..36) :: integer
  def to_integer(charlist, base) do
    :erlang.list_to_integer(charlist, base)
  end

  @doc """
  Converts a list to a tuple.

  Inlined by the compiler.

  ## Examples

      iex> List.to_tuple([:share, [:elixir, 163]])
      {:share, [:elixir, 163]}

  """
  @spec to_tuple(list) :: tuple
  def to_tuple(list) do
    :erlang.list_to_tuple(list)
  end

  @doc """
  Converts a list of integers representing codepoints, lists or
  strings into a string.

  Notice that this function expects a list of integers representing
  UTF-8 codepoints. If you have a list of bytes, you must instead use
  the [`:binary` module](http://www.erlang.org/doc/man/binary.html).

  ## Examples

      iex> List.to_string([0x00E6, 0x00DF])
      "æß"

      iex> List.to_string([0x0061, "bc"])
      "abc"

      iex> List.to_string([0x0064, "ee", ['p']])
      "deep"

  """
  @spec to_string(:unicode.charlist()) :: String.t()
  def to_string(list) when is_list(list) do
    try do
      :unicode.characters_to_binary(list)
    rescue
      ArgumentError ->
        raise ArgumentError, """
        cannot convert the given list to a string.

        To be converted to a string, a list must contain only:

          * strings
          * integers representing Unicode codepoints
          * or a list containing one of these three elements

        Please check the given list or call inspect/1 to get the list representation, got:

        #{inspect(list)}
        """
    else
      result when is_binary(result) ->
        result

      {:error, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :invalid

      {:incomplete, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :incomplete
    end
  end

  @doc """
  Converts a list of integers representing codepoints, lists or
  strings into a charlist.

  Notice that this function expects a list of integers representing
  UTF-8 codepoints. If you have a list of bytes, you must instead use
  the [`:binary` module](http://www.erlang.org/doc/man/binary.html).

  ## Examples

      iex> List.to_charlist([0x00E6, 0x00DF])
      'æß'

      iex> List.to_charlist([0x0061, "bc"])
      'abc'

      iex> List.to_charlist([0x0064, "ee", ['p']])
      'deep'

  """
  @doc since: "1.8.0"
  @spec to_charlist(:unicode.charlist()) :: charlist()
  def to_charlist(list) when is_list(list) do
    try do
      :unicode.characters_to_list(list)
    rescue
      ArgumentError ->
        raise ArgumentError, """
        cannot convert the given list to a charlist.

        To be converted to a charlist, a list must contain only:

          * strings
          * integers representing Unicode codepoints
          * or a list containing one of these three elements

        Please check the given list or call inspect/1 to get the list representation, got:

        #{inspect(list)}
        """
    else
      result when is_list(result) ->
        result

      {:error, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :invalid

      {:incomplete, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :incomplete
    end
  end

  @doc """
  Returns a keyword list that represents an *edit script*.

  The algorithm is outlined in the
  "An O(ND) Difference Algorithm and Its Variations" paper by E. Myers.

  An *edit script* is a keyword list. Each key describes the "editing action" to
  take in order to bring `list1` closer to being equal to `list2`; a key can be
  `:eq`, `:ins`, or `:del`. Each value is a sublist of either `list1` or `list2`
  that should be inserted (if the corresponding key `:ins`), deleted (if the
  corresponding key is `:del`), or left alone (if the corresponding key is
  `:eq`) in `list1` in order to be closer to `list2`.

  See `myers_difference/3` if you want to handle nesting in the diff scripts.

  ## Examples

      iex> List.myers_difference([1, 4, 2, 3], [1, 2, 3, 4])
      [eq: [1], del: [4], eq: [2, 3], ins: [4]]

  """
  @doc since: "1.4.0"
  @spec myers_difference(list, list) :: [{:eq | :ins | :del, list}]
  def myers_difference(list1, list2) when is_list(list1) and is_list(list2) do
    myers_difference_with_diff_script(list1, list2, nil)
  end

  @doc """
  Returns a keyword list that represents an *edit script* with nested diffs.

  This is an extension of `myers_difference/2` where a `diff_script` function
  can be given in case it is desired to compute nested differences. The function
  may return a list with the inner edit script or `nil` in case there is no
  such script. The returned inner edit script will be under the `:diff` key.

  ## Examples

      iex> List.myers_difference(["a", "db", "c"], ["a", "bc"], &String.myers_difference/2)
      [eq: ["a"], diff: [del: "d", eq: "b", ins: "c"], del: ["c"]]

  """
  @doc since: "1.8.0"
  @spec myers_difference(list, list, (term, term -> script | nil)) :: script
        when script: [{:eq | :ins | :del | :diff, list}]
  def myers_difference(list1, list2, diff_script)
      when is_list(list1) and is_list(list2) and is_function(diff_script) do
    myers_difference_with_diff_script(list1, list2, diff_script)
  end

  defp myers_difference_with_diff_script(list1, list2, diff_script) do
    path = {0, list1, list2, []}
    find_script(0, length(list1) + length(list2), [path], diff_script)
  end

  defp find_script(envelope, max, paths, diff_script) do
    case each_diagonal(-envelope, envelope, paths, [], diff_script) do
      {:done, edits} -> compact_reverse(edits, [])
      {:next, paths} -> find_script(envelope + 1, max, paths, diff_script)
    end
  end

  defp compact_reverse([], acc), do: acc

  defp compact_reverse([{:diff, _} = fragment | rest], acc) do
    compact_reverse(rest, [fragment | acc])
  end

  defp compact_reverse([{kind, elem} | rest], [{kind, result} | acc]) do
    compact_reverse(rest, [{kind, [elem | result]} | acc])
  end

  defp compact_reverse(rest, [{:eq, elem}, {:ins, elem}, {:eq, other} | acc]) do
    compact_reverse(rest, [{:ins, elem}, {:eq, elem ++ other} | acc])
  end

  defp compact_reverse([{kind, elem} | rest], acc) do
    compact_reverse(rest, [{kind, [elem]} | acc])
  end

  defp each_diagonal(diag, limit, _paths, next_paths, _diff_script) when diag > limit do
    {:next, :lists.reverse(next_paths)}
  end

  defp each_diagonal(diag, limit, paths, next_paths, diff_script) do
    {path, rest} = proceed_path(diag, limit, paths, diff_script)

    case follow_snake(path) do
      {:cont, path} -> each_diagonal(diag + 2, limit, rest, [path | next_paths], diff_script)
      {:done, edits} -> {:done, edits}
    end
  end

  defp proceed_path(0, 0, [path], _diff_script), do: {path, []}

  defp proceed_path(diag, limit, [path | _] = paths, diff_script) when diag == -limit do
    {move_down(path, diff_script), paths}
  end

  defp proceed_path(diag, limit, [path], diff_script) when diag == limit do
    {move_right(path, diff_script), []}
  end

  defp proceed_path(_diag, _limit, [path1, path2 | rest], diff_script) do
    if elem(path1, 0) > elem(path2, 0) do
      {move_right(path1, diff_script), [path2 | rest]}
    else
      {move_down(path2, diff_script), [path2 | rest]}
    end
  end

  defp move_right({y, [elem1 | rest1] = list1, [elem2 | rest2], edits}, diff_script)
       when diff_script != nil do
    if diff = diff_script.(elem1, elem2) do
      {y + 1, rest1, rest2, [{:diff, diff} | edits]}
    else
      {y, list1, rest2, [{:ins, elem2} | edits]}
    end
  end

  defp move_right({y, list1, [elem | rest], edits}, _diff_script) do
    {y, list1, rest, [{:ins, elem} | edits]}
  end

  defp move_right({y, list1, [], edits}, _diff_script) do
    {y, list1, [], edits}
  end

  defp move_down({y, [elem1 | rest1], [elem2 | rest2] = list2, edits}, diff_script)
       when diff_script != nil do
    if diff = diff_script.(elem1, elem2) do
      {y + 1, rest1, rest2, [{:diff, diff} | edits]}
    else
      {y + 1, rest1, list2, [{:del, elem1} | edits]}
    end
  end

  defp move_down({y, [elem | rest], list2, edits}, _diff_script) do
    {y + 1, rest, list2, [{:del, elem} | edits]}
  end

  defp move_down({y, [], list2, edits}, _diff_script) do
    {y + 1, [], list2, edits}
  end

  defp follow_snake({y, [elem | rest1], [elem | rest2], edits}) do
    follow_snake({y + 1, rest1, rest2, [{:eq, elem} | edits]})
  end

  defp follow_snake({_y, [], [], edits}) do
    {:done, edits}
  end

  defp follow_snake(path) do
    {:cont, path}
  end

  ## Helpers

  # replace_at

  defp do_replace_at([], _index, _value) do
    []
  end

  defp do_replace_at([_old | rest], 0, value) do
    [value | rest]
  end

  defp do_replace_at([head | tail], index, value) do
    [head | do_replace_at(tail, index - 1, value)]
  end

  # insert_at

  defp do_insert_at([], _index, value) do
    [value]
  end

  defp do_insert_at(list, 0, value) do
    [value | list]
  end

  defp do_insert_at([head | tail], index, value) do
    [head | do_insert_at(tail, index - 1, value)]
  end

  # update_at

  defp do_update_at([value | list], 0, fun) do
    [fun.(value) | list]
  end

  defp do_update_at([head | tail], index, fun) do
    [head | do_update_at(tail, index - 1, fun)]
  end

  defp do_update_at([], _index, _fun) do
    []
  end

  # pop_at

  defp do_pop_at([], _index, default, acc) do
    {default, :lists.reverse(acc)}
  end

  defp do_pop_at([head | tail], 0, _default, acc) do
    {head, :lists.reverse(acc, tail)}
  end

  defp do_pop_at([head | tail], index, default, acc) do
    do_pop_at(tail, index - 1, default, [head | acc])
  end

  # zip

  defp do_zip(list, acc) do
    converter = fn x, acc -> do_zip_each(to_list(x), acc) end

    case :lists.mapfoldl(converter, [], list) do
      {_, nil} ->
        :lists.reverse(acc)

      {mlist, heads} ->
        do_zip(mlist, [to_tuple(:lists.reverse(heads)) | acc])
    end
  end

  defp do_zip_each(_, nil) do
    {nil, nil}
  end

  defp do_zip_each([head | tail], acc) do
    {tail, [head | acc]}
  end

  defp do_zip_each([], _) do
    {nil, nil}
  end

  defp to_list(tuple) when is_tuple(tuple), do: Tuple.to_list(tuple)
  defp to_list(list) when is_list(list), do: list
end
