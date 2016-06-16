defmodule Tuple do
  @moduledoc """
  Functions for working with tuples.

  Tuples are ordered collection of elements; tuples can contain elements of any
  type, and a tuple can contain elements of different types. Curly braces can be
  used to create tuples:

      iex> {}
      {}
      iex> {1, :two, "three"}
      {1, :two, "three"}

  Tuples store elements contiguously in memory; this means that accessing a
  tuple element by index (which can be done through the `Kernel.elem/2`
  function) is a constant-time operation:

      iex> tuple = {1, :two, "three"}
      iex> elem(tuple, 0)
      1
      iex> elem(tuple, 2)
      "three"

  Same goes for getting the tuple size (via `Kernel.tuple_size/1`):

      iex> tuple_size({})
      0
      iex> tuple_size({1, 2, 3})
      3

  Tuples being stored contiguously in memory also means that updating a tuple
  (for example replacing an element with `Kernel.put_elem/3`) will make a copy
  of the whole tuple.

  Tuples are not meant to be used as a "collection" type (which is also
  suggested by the absence of an implementation of the `Enumerable` protocol for
  tuples): they're mostly meant to be used as a fixed-size container for
  multiple elements. For example, tuples are often used to have functions return
  "enriched" values: a common pattern is for functions to return `{:ok, value}`
  for successful cases and `{:error, reason}` for unsuccessful cases. For
  example, this is exactly what `File.read/1` does: it returns `{:ok, contents}`
  if reading the given file is successful, or `{:error, reason}` otherwise
  (e.g., `{:error, :enoent}` if the file doesn't exist).

  This module provides functions to work with tuples; some more functions to
  work with tuples can be found in `Kernel` (`Kernel.tuple_size/1`,
  `Kernel.elem/2`, `Kernel.put_elem/3`, and others).
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
