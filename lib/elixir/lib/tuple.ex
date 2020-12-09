defmodule Tuple do
  @moduledoc """
  Functions for working with tuples.

  Please note the following functions for tuples are found in `Kernel`:

    * `elem/2` - accesses a tuple by index
    * `put_elem/3` - inserts a value into a tuple by index
    * `tuple_size/1` - gets the number of elements in a tuple

  Tuples are intended as fixed-size containers for multiple elements.
  To manipulate a collection of elements, use a list instead. `Enum`
  functions do not work on tuples.

  Tuples are denoted with curly braces:

      iex> {}
      {}
      iex> {1, :two, "three"}
      {1, :two, "three"}

  A tuple may contain elements of different types, which are stored
  contiguously in memory. Accessing any element takes constant time,
  but modifying a tuple, which produces a shallow copy, takes linear time.
  Tuples are good for reading data while lists are better for traversals.

  Tuples are typically used either when a function has multiple return values
  or for error handling. `File.read/1` returns `{:ok, contents}` if reading
  the given file is successful, or else `{:error, reason}` such as when
  the file does not exist.

  The functions in this module that add and remove elements from tuples are
  rarely used in practice, as they typically imply tuples are being used as
  collections. To append to a tuple, it is preferable to extract the elements
  from the old tuple with pattern matching, and then create a new tuple:

      tuple = {:ok, :example}

      # Avoid
      result = Tuple.insert_at(tuple, 2, %{})

      # Prefer
      {:ok, atom} = tuple
      result = {:ok, atom, %{}}

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
  def duplicate(data, size) when is_integer(size) and size >= 0 do
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
  def insert_at(tuple, index, value) when is_integer(index) and index >= 0 do
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
  def delete_at(tuple, index) when is_integer(index) and index >= 0 do
    :erlang.delete_element(index + 1, tuple)
  end

  @doc """
  Computes a sum of tuple elements.

  ## Examples

      iex> Tuple.sum({255, 255})
      510
      iex> Tuple.sum({255, 0.0})
      255.0
      iex> Tuple.sum({})
      0
  """
  @doc since: "1.12.0"
  @spec sum(tuple) :: number()
  def sum(tuple), do: sum(tuple, tuple_size(tuple))

  defp sum(_tuple, 0), do: 0
  defp sum(tuple, index), do: :erlang.element(index, tuple) + sum(tuple, index - 1)

  @doc """
  Computes a product of tuple elements.

  ## Examples

      iex> Tuple.product({255, 255})
      65025
      iex> Tuple.product({255, 1.0})
      255.0
      iex> Tuple.product({})
      1
  """
  @doc since: "1.12.0"
  @spec product(tuple) :: number()
  def product(tuple), do: product(tuple, tuple_size(tuple))

  defp product(_tuple, 0), do: 1
  defp product(tuple, index), do: :erlang.element(index, tuple) * product(tuple, index - 1)

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
