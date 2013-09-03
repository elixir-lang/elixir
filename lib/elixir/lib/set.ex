defmodule Set do
  @moduledoc %S"""
  This module specifies the Set API expected to be
  implemented by different representations.

  It also provides functions that redirect to the
  underlying Set, allowing a developer to work with
  different Set implementations using one API.

  To create a new set, use the `new` functions defined
  by each set type:

      HashSet.new  #=> creates an empty HashSet

  For simplicity's sake, in the examples below every time
  `new` is used, it implies one of the module-specific
  calls like above.
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
      if is_tuple(unquote(set)) do
        elem(unquote(set), 0)
      else
        unsupported_set(unquote(set))
      end
    end
  end

  @doc """
  Deletes `value` from `set`.

  ## Examples

      iex> s = HashSet.new([1, 2, 3])
      ...> Set.delete(s, 4) |> HashSet.to_list
      [1, 2, 3]

      iex> s = HashSet.new([1, 2, 3])
      ...> Set.delete(s, 2) |> HashSet.to_list
      [1, 3]
  """
  @spec delete(t, value) :: t
  def delete(set, value) do
    target(set).delete(set, value)
  end

  @doc """
  Returns a set that is `set1` without the members of `set2`.

  ## Examples

      iex> Set.difference(HashSet.new([1,2]), HashSet.new([2,3,4])) |> HashSet.to_list
      [1]

  """
  @spec difference(t, t) :: t
  def difference(set1, set2) do
    target(set1).difference(set1, set2)
  end

  @doc """
  Checks if `set1` and `set2` have no members in common.

  ## Examples

      iex> Set.disjoint?(HashSet.new([1, 2]), HashSet.new([3, 4]))
      true
      iex> Set.disjoint?(HashSet.new([1, 2]), HashSet.new([2, 3]))
      false

  """
  @spec disjoint?(t, t) :: boolean
  def disjoint?(set1, set2) do
    target(set1).disjoint?(set1, set2)
  end

  @doc """
  Returns an empty set of the same type as `set`.
  """
  @spec empty(t) :: t
  def empty(set) do
    target(set).empty(set)
  end

  @doc """
  Checks if `set1` and `set2` are equal.

  ## Examples

      iex> Set.equal?(HashSet.new([1, 2]), HashSet.new([2, 1, 1]))
      true
      
      iex> Set.equal?(HashSet.new([1, 2]), HashSet.new([3, 4]))
      false

  """
  @spec equal?(t, t) :: boolean
  def equal?(set1, set2) do
    target(set1).equal?(set1, set2)
  end

  @doc """
  Returns a set containing only members in common between `set1` and `set2`.

  ## Examples

      iex> Set.intersection(HashSet.new([1,2]), HashSet.new([2,3,4])) |> HashSet.to_list
      [2]

      iex> Set.intersection(HashSet.new([1,2]), HashSet.new([3,4])) |> HashSet.to_list  
      []

  """
  @spec intersection(t, t) :: t
  def intersection(set1, set2) do
    target(set1).intersection(set1, set2)
  end

  @doc """
  Checks if `set` contains `value`.

  ## Examples

      iex> Set.member?(HashSet.new([1, 2, 3]), 2)
      true

      iex> Set.member?(HashSet.new([1, 2, 3]), 4) 
      false

  """
  @spec member?(t, value) :: boolean
  def member?(set, value) do
    target(set).member?(set, value)
  end

  @doc """
  Inserts `value` into `set` if it does not already contain it.

  ## Examples

      iex> Set.put(HashSet.new([1, 2, 3]), 3) |> Set.to_list
      [1, 2, 3]

      iex> Set.put(HashSet.new([1, 2, 3]), 4) |> Set.to_list
      [1, 2, 3, 4]

  """
  @spec put(t, value) :: t
  def put(set, value) do
    target(set).put(set, value)
  end

  @doc """
  Returns the number of elements in `set`.

  ## Examples

      iex> Set.size(HashSet.new([1, 2, 3]))
      3

  """
  @spec size(t) :: non_neg_integer
  def size(set) do
    target(set).size(set)
  end

  @doc """
  Checks if `set1`'s members are all contained in `set2`.

  ## Examples

      iex> Set.subset?(HashSet.new([1, 2]), HashSet.new([1, 2, 3]))
      true
      iex> Set.subset?(HashSet.new([1, 2, 3]), HashSet.new([1, 2]))
      false
  """
  @spec subset?(t, t) :: boolean
  def subset?(set1, set2) do
    target(set1).subset?(set1, set2)
  end

  @doc """
  Converts `set` to a list.

  ## Examples

      iex> HashSet.to_list(HashSet.new([1, 2, 3]))
      [1,2,3]

  """
  @spec to_list(t) :: list
  def to_list(set) do
    target(set).to_list(set)
  end

  @doc """
  Returns a set containing all members of `set1` and `set2`.

  ## Examples

      iex> Set.union(HashSet.new([1,2]), HashSet.new([2,3,4])) |> HashSet.to_list
      [1,2,3,4]

  """
  @spec union(t, t) :: t
  def union(set1, set2) do
    target(set1).union(set1, set2)
  end

  defp unsupported_set(set) do
    raise ArgumentError, message: "unsupported set: #{inspect set}"
  end
end
