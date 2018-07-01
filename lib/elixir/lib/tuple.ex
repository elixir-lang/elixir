defmodule Tuple do
  @moduledoc """
  Functions for working with tuples.
  
  Please note the following functions for tuples--_all constant-time_
  --are found in `Kernel`:
  
    * `elem/2` - access a tuple by index
    * `put_elem/3` - insert a value into a tuple by index
    * `tuple_size/3` - get the number of elements in a tuple
    
  When deciding whether to use a tuple or a list, please bear in mind
  that tuples are not meant to be used as a "collection" type but rather 
  as a fixed-size container for multiple elements. That's why it is not possible
  to traverse a tuple dynamically using the functions in the `Enum` module.
  
  The functions that add and remove elements from tuples, changing their size, 
  are rarely used in practice, as they typically imply tuples are being used as 
  collections. Even if you have a tuple `{:ok, atom}` and you want to append 
  another element to it, such as an empty map, it is preferrable to rely on 
  pattern matching and create a new tuple than manipulating it dynamically:

      tuple = {:ok, :example}

      # Avoid
      Tuple.insert_at(tuple, 2, %{}}

      # Prefer
      {:ok, atom} = tuple
      {:ok, atom, %{}}
  
  Rather, tuples are typically used when a function has multiple return values.
  A common pattern is for functions to return `{:ok, value}` for successful cases 
  and `{:error, reason}` for unsuccessful cases. For example, `File.read/1` returns 
  `{:ok, contents}` if reading the given file is successful, or else `{:error, reason}` 
  such as when the file does not exist.

  Tuples can contain a fixed number of elements of different types. 
  Curly braces are used to create a tuple:

      iex> {}
      {}
      iex> {1, :two, "three"}
      {1, :two, "three"}

  Tuples store elements contiguously in memory. This means accessing a
  tuple element by index doesn't depend on the number of elements in the
  tuple. Tuples being stored contiguously in memory also means that updating 
  a tuple will make a shallow copy of the whole tuple. The tuple elements 
  are still shared due to immutability.

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
