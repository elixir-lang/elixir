defmodule Tuple do
  @moduledoc """
  Functions for working with tuples.

  See also 'Kernel.elem/2', 'Kernel.is_tuple/1', 'Kernel.put_elem/3', and 
  'Kernel.tuple_size/1'.
  """

  @doc """
  Creates a new tuple.

  Creates a tuple of size `size` containing the
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

  Inserts `value` into `tuple` at the given zero-based `index`.
  Raises an `ArgumentError` if `index` is greater than the
  length of `tuple`.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:bar, :baz}
      iex> Tuple.insert_at(tuple, 0, :foo)
      {:foo, :bar, :baz}

  """
  @spec insert_at(tuple, non_neg_integer, term) :: tuple
  def insert_at(tuple, index, term) do
    :erlang.insert_element(index + 1, tuple, term)
  end

  @doc """
  Removes an element from a tuple.

  Deletes the element at the zero-based `index` from `tuple`.
  Raises an `ArgumentError` if `index` is greater than
  or equal to the length of `tuple`.

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

  Inlined by the compiler.
  """
  @spec to_list(tuple) :: list
  def to_list(tuple) do
    :erlang.tuple_to_list(tuple)
  end
end
