defmodule Tuple do
  @doc """
  Creates a tuple with size `size` containing the
  given `data` at every position.

  ## Examples

      iex> Tuple.duplicate(:hello, 3)
      { :hello, :hello, :hello }

  """
  @spec duplicate(term, non_neg_integer) :: tuple
  def duplicate(data, size) do
    :erlang.make_tuple(size, data)
  end

  @doc """
  Inserts `value` into `tuple` at the given zero-based `index`.

  ## Examples

      iex> tuple = { :bar, :baz }
      ...> Tuple.insert_at(tuple, 0, :foo)
      { :foo, :bar, :baz }

  """
  @spec insert_at(tuple, non_neg_integer, term) :: tuple
  def insert_at(tuple, index, term) do
    :erlang.insert_element(index + 1, tuple, term)
  end

  @doc """
  Deletes the element at the zero-based `index` from `tuple`.

  ## Examples

      iex> tuple = { :foo, :bar, :baz }
      ...> Tuple.delete_at(tuple, 0)
      { :bar, :baz }

  """
  @spec delete_at(tuple, non_neg_integer) :: tuple
  def delete_at(tuple, index) do
    :erlang.delete_element(index + 1, tuple)
  end
end