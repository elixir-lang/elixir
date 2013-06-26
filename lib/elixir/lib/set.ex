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

  @spec delete(t, value) :: t
  def delete(set, value) do
    target(set).delete(set, value)
  end

  @spec difference(t, t) :: t
  def difference(set1, set2) do
    target(set1).difference(set1, set2)
  end

  @spec disjoint?(t, t) :: boolean
  def disjoint?(set1, set2) do
    target(set1).disjoint?(set1, set2)
  end

  @spec empty(t) :: t
  def empty(set) do
    target(set).empty(set)
  end

  @spec equal?(t, t) :: boolean
  def equal?(set1, set2) do
    target(set1).equal?(set1, set2)
  end

  @spec intersection(t, t) :: t
  def intersection(set1, set2) do
    target(set1).intersection(set1, set2)
  end

  @spec member?(t, value) :: boolean
  def member?(set, value) do
    target(set).member?(set, value)
  end

  @spec put(t, value) :: t
  def put(set, value) do
    target(set).put(set, value)
  end

  @spec size(t) :: non_neg_integer
  def size(set) do
    target(set).size(set)
  end

  @spec subset?(t, t) :: boolean
  def subset?(set1, set2) do
    target(set1).subset?(set1, set2)
  end

  @spec to_list(t) :: list
  def to_list(set) do
    target(set).to_list(set)
  end

  @spec union(t, t) :: t
  def union(set1, set2) do
    target(set1).union(set1, set2)
  end

end