defmodule Tuple do
  @moduledoc """
  Functions for working with tuples.

  Tuples are composite data types with a fixed number of elements. Tuples
  can contain elements of any type, and a tuple can contain elements of
  different types. Curly braces can be used to create tuples:

      iex> {}
      {}
      iex> {1, :two, "three"}
      {1, :two, "three"}

  Tuples store elements contiguously in memory. This means accessing a
  tuple element by index doesn't depend on the number of elements in the
  tuple. We say the operation is done in constant-time, via the
  `Kernel.elem/1` function:

      iex> tuple = {1, :two, "three"}
      iex> elem(tuple, 0)
      1
      iex> elem(tuple, 2)
      "three"

  Same goes for getting the tuple size with `Kernel.tuple_size/1`:

      iex> tuple_size({})
      0
      iex> tuple_size({1, 2, 3})
      3

  Tuples being stored contiguously in memory also means that updating a tuple
  (for example replacing an element with `Kernel.put_elem/3`) will make a
  shallow copy of the whole tuple. The tuple elements are still shared thanks
  to immutability.

  Tuples are not meant to be used as a "collection" type but rather as a
  fixed-size container for multiple elements. That's why it is not possible
  to traverse a tuple dynamically using the functions in the `Enum` module.

  For example, tuples are often used to have functions return "enriched"
  values: a common pattern is for functions to return `{:ok, value}` for
  successful cases and `{:error, reason}` for unsuccessful cases. This is
  exactly what `File.read/1` does: it returns `{:ok, contents}` if reading
  the given file is successful, or `{:error, reason}` otherwise, such as
  when the file does not exist.

  The most common operations performed on tuples are available in `Kernel`
  (`Kernel.tuple_size/1`, `Kernel.elem/2`, `Kernel.put_elem/3`, and others)
  and are automatically imported into your code. The functions in this module
  cover other cases, such as dynamic creation of tuples (`Tuple.duplicate/2`)
  and conversion to list (`Tuple.to_list/1`). The functions that add and remove
  elements from tuples, changing their size, are rarely used in practice, as
  they typically imply tuples are being used as collections. Even if you have
  a tuple `{:ok, atom}` and you want to append another element to it, such as
  an empty map, it is preferrable to rely on pattern matching and create a new
  tuple than manipulating it dynamically:

      tuple = {:ok, :example}

      # Avoid
      Tuple.insert_at(tuple, 2, %{}}

      # Prefer
      {:ok, atom} = tuple
      {:ok, atom, %{}}

  """

  @doc """
  Creates a new tuple.

  Creates a tuple of `size` containing the
  given `data` at every position.

  Inlined by the compiler.

  ## Examples

      iex> Tuple.duplicate(:hello, 3)
      {:hello, :hello, :hello}

  """
  @spec duplicate(term, non_neg_integer) :: tuple
  def duplicate(data, size) do
    :erlang.make_tuple(size, data)
  end

  @doc """
  Inserts an element into a tuple.

  Inserts `value` into `tuple` at the given `index`.
  Raises an `ArgumentError` if `index` is negative or greater than the
  length of `tuple`. Index is zero-based.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:bar, :baz}
      iex> Tuple.insert_at(tuple, 0, :foo)
      {:foo, :bar, :baz}
      iex> Tuple.insert_at(tuple, 2, :bong)
      {:bar, :baz, :bong}

  """
  @spec insert_at(tuple, non_neg_integer, term) :: tuple
  def insert_at(tuple, index, value) do
    :erlang.insert_element(index + 1, tuple, value)
  end

  @doc """
  Inserts an element at the end of a tuple.

  Returns a new tuple with the element appended at the end, and contains
  the elements in `tuple` followed by `value` as the last element.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar}
      iex> Tuple.append(tuple, :baz)
      {:foo, :bar, :baz}

  """
  @spec append(tuple, term) :: tuple
  def append(tuple, value) do
    :erlang.append_element(tuple, value)
  end

  @doc """
  Removes an element from a tuple.

  Deletes the element at the given `index` from `tuple`.
  Raises an `ArgumentError` if `index` is negative or greater than
  or equal to the length of `tuple`. Index is zero-based.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, :baz}
      iex> Tuple.delete_at(tuple, 0)
      {:bar, :baz}

  """
  @spec delete_at(tuple, non_neg_integer) :: tuple
  def delete_at(tuple, index) do
    :erlang.delete_element(index + 1, tuple)
  end

  @doc """
  Converts a tuple to a list.

  Returns a new list with all the tuple elements.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, :baz}
      iex> Tuple.to_list(tuple)
      [:foo, :bar, :baz]

  """
  @spec to_list(tuple) :: list
  def to_list(tuple) do
    :erlang.tuple_to_list(tuple)
  end
end
