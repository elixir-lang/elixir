defmodule Set do
  @moduledoc %B"""
  This module specifies the Set API expected to be
  implemented by different representations.
  """

  use Behaviour

  @type value :: any
  @type values :: [ value ]
  @type t :: tuple

  defcallback delete(t, value) :: t
  defcallback difference(t, t) :: t
  defcallback disjoint?(t, t) :: boolean
  defcallback empty(t) :: t
  defcallback equal?(t, t) :: boolean
  defcallback intersection(t, t) :: t
  defcallback member?(t, value) :: boolean
  defcallback put(t, value) :: t
  defcallback size(t) :: non_neg_integer
  defcallback subset?(t, t) :: boolean
  defcallback to_list(t) :: list()
  defcallback union(t, t) :: t

  defmacrop target(set) do
    quote do
      cond do
        is_tuple(unquote(set)) -> HashSet
      end
    end
  end

  @doc """
  Deletes a value from the set.
  """
  @spec delete(t, value) :: t
  def delete(set, value) do
    target(set).delete(set, value)
  end

  @doc """
  Returns a set that is the first set without members of the second set.
  """
  @spec difference(t, t) :: t
  def difference(set1, set2) do
    target(set1).difference(set1, set2)
  end

  @doc """
  Checks if set1 and set2 have no members in common.
  """
  @spec disjoint?(t, t) :: boolean
  def disjoint?(set1, set2) do
    target(set1).disjoint?(set1, set2)
  end

  @doc """
  Returns an empty set.
  """
  @spec empty(t) :: t
  def empty(set) do
    target(set).empty(set)
  end

  @doc """
  Checks if two sets are equal
  """
  @spec equal?(t, t) :: boolean
  def equal?(set1, set2) do
    target(set1).equal?(set1, set2)
  end

  @doc """
  Returns a set containing only members in common between the two input sets.
  """
  @spec intersection(t, t) :: t
  def intersection(set1, set2) do
    target(set1).intersection(set1, set2)
  end

  @doc """
  Checks if the set has the given value.
  """
  @spec member?(t, value) :: boolean
  def member?(set, value) do
    target(set).member?(set, value)
  end

  @doc """
  Puts the given value into the set if it does not already contain it.
  """
  @spec put(t, value) :: t
  def put(set, value) do
    target(set).put(set, value)
  end

  @doc """
  Returns the set size.
  """
  @spec size(t) :: non_neg_integer
  def size(set) do
    target(set).size(set)
  end

  @doc """
  Checks if set1's members are all contained in set2.
  """
  @spec subset?(t, t) :: boolean
  def subset?(set1, set2) do
    target(set1).subset?(set1, set2)
  end

  @doc """
  Converts a set to a list.
  """
  @spec to_list(t) :: list
  def to_list(set) do
    target(set).to_list(set)
  end

  @doc """
  Returns a set combining the two input sets.
  """
  @spec union(t, t) :: t
  def union(set1, set2) do
    target(set1).union(set1, set2)
  end

end