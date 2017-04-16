defmodule SortedSet do
  @moduledoc """
    A Set implementation that always remains sorted.

    SortedSet guarantees that no element appears more than once and that
    enumerating over members happens in their sorted order.
  """

  @behaviour Set

  # Define the type as opaque

  @opaque t :: %__MODULE__{members: list, size: non_neg_integer}
  @doc false
  defstruct size: 0, members: []

  @doc ~S"""
  Returns a new `SortedSet`, initialized with the unique, sorted values of
  `members`.

  ## Examples

      iex> inspect SortedSet.new()
      "#SortedSet<[]>"

      iex> inspect SortedSet.new([1,3,5])
      "#SortedSet<[1, 3, 5]>"
  """
  def new(members \\ []) do
    Enum.reduce(members, %SortedSet{}, fn(member, set) ->
      put(set, member)
    end)
  end

  @doc ~S"""
  Returns the number of elements in a `SortedSet`.

  ## Examples

      iex> SortedSet.size SortedSet.new([1,3,5])
      3
  """
  def size(%SortedSet{size: size}) do
    size
  end

  @doc ~S"""
  Returns a `List` with all of the members of `set`.

  ## Examples

      iex> SortedSet.to_list SortedSet.new([1,3,5])
      [1,3,5]
  """
  def to_list(%SortedSet{members: members}) do
    members
  end

  @doc ~S"""
  Returns a `SortedSet` with all of the members of `set` plus `element`.

  ## Examples

      iex> set = SortedSet.new([1,3,5])
      iex> SortedSet.to_list SortedSet.put(set, 1)
      [1,3,5]

      iex> set = SortedSet.new([1,3,5])
      iex> SortedSet.to_list SortedSet.put(set, 2)
      [1,2,3,5]
  """
  def put(%SortedSet{members: members, size: size}, element) do
    {new_members, members_added} = do_put(members, element)
    %SortedSet{members: new_members, size: size + members_added}
  end

  @doc ~S"""
  Returns a `SortedSet` with all of the members of `sortedset` except for `element`.

  ## Examples

      iex> set = SortedSet.new([1,3,5])
      iex> SortedSet.to_list SortedSet.delete(set, 1)
      [3,5]

      iex> set = SortedSet.new([1,3,5])
      iex> SortedSet.to_list SortedSet.delete(set, 2)
      [1,3,5]

      iex> set = SortedSet.new([])
      iex> SortedSet.to_list SortedSet.delete(set, 2)
      []
  """
  def delete(%SortedSet{members: members, size: size}, element) do
    {new_members, members_removed} = do_delete(members, element)
    %SortedSet{members: new_members, size: size - members_removed}
  end

  ## SortedSet predicate methods

  @doc ~S"""
  Returns `true` if `set` contains `element`

  ## Examples

      iex> set = SortedSet.new([1,3,5])
      iex> SortedSet.member?(set, 1)
      true

      iex> set = SortedSet.new([1,3,5])
      iex> SortedSet.member?(set, 0)
      false
  """
  def member?(%SortedSet{}=set, element) do
    do_member?(to_list(set), element)
  end

  # If the sizes are not equal, no need to check members
  def equal?(%SortedSet{size: size1}, %SortedSet{size: size2}) when size1 != size2 do
    false
  end

  @doc ~S"""
  Returns `true` if all elements in `set1` are in `set2` and all elements in
  `set2` are in `set1`

  ## Examples

      iex> set1 = SortedSet.new([1,3,5])
      iex> set2 = SortedSet.new([1,3,5])
      iex> SortedSet.equal?(set1, set2)
      true

      iex> set1 = SortedSet.new([1,3,5])
      iex> set2 = SortedSet.new([1,2,3,4,5])
      iex> SortedSet.equal?(set1, set2)
      false
  """
  def equal?(%SortedSet{}=set1, %SortedSet{}=set2) do
    Enum.all?(to_list(set1), fn(set1_member) ->
      member? set2, set1_member
    end)
  end

  def subset?(%SortedSet{size: size1}, %SortedSet{size: size2}) when size1 > size2 do
    false
  end

  @doc ~S"""
  Returns `true` if all elements in `set1` are in `set2`

  ## Examples

      iex> set1 = SortedSet.new([1,3,5])
      iex> set2 = SortedSet.new([1,2,3,4,5])
      iex> SortedSet.subset?(set1, set2)
      true

      iex> set1 = SortedSet.new([1,2,3,4,5])
      iex> set2 = SortedSet.new([1,3,5])
      iex> SortedSet.subset?(set1, set2)
      false
  """
  # If set1 is larger than set2, it cannot be a subset of it
  def subset?(%SortedSet{}=set1, %SortedSet{}=set2) do
    Enum.all?(to_list(set1), fn(set1_member) ->
      member? set2, set1_member
    end)
  end

  @doc ~S"""
  Returns `true` if no member of `set1` is in `set2`. Otherwise returns
  `false`.

  ## Examples

      iex> set1 = SortedSet.new([1,2,3,4])
      iex> set2 = SortedSet.new([5,6,7,8])
      iex> SortedSet.disjoint?(set1, set2)
      true

      iex> set1 = SortedSet.new([1,2,3,4])
      iex> set2 = SortedSet.new([4,5,6,7])
      iex> SortedSet.disjoint?(set1, set2)
      false
  """
  def disjoint?(%SortedSet{size: size1}=set1, %SortedSet{size: size2}=set2) when size1 <= size2 do
    not Enum.any?(to_list(set1), fn(set1_member) ->
      member?(set2, set1_member)
    end)
  end

  def disjoint?(%SortedSet{}=set1, %SortedSet{}=set2) do
    disjoint?(set2, set1)
  end

  ## SortedSet Operations

  @doc ~S"""
  Returns a `SortedSet` containing the items of both `set1` and `set2`.

  ## Examples

      iex> set1 = SortedSet.new([1,3,5,7])
      iex> set2 = SortedSet.new([0,2,3,4,5])
      iex> SortedSet.to_list SortedSet.union(set1, set2)
      [0,1,2,3,4,5,7]
  """
  def union(%SortedSet{size: size1}=set1, %SortedSet{size: size2}=set2) when size1 <= size2  do
    Enum.reduce(to_list(set1), set2, fn(member, new_set) ->
      put(new_set, member)
    end)
  end

  def union(%SortedSet{}=set1, %SortedSet{}=set2) do
    union(set2, set1)
  end

  # If either set is empty, the intersection is the empty set
  def intersection(%SortedSet{size: 0}=set1, _) do
    set1
  end

  # If either set is empty, the intersection is the empty set
  def intersection(_, %SortedSet{size: 0}=set2) do
    set2
  end

  @doc ~S"""
  Returns a `SortedSet` containing the items contained in both `set1` and
  `set2`.

  ## Examples

      iex> set1 = SortedSet.new([1,3,5,7])
      iex> set2 = SortedSet.new([0,2,3,4,5])
      iex> SortedSet.to_list SortedSet.intersection(set1, set2)
      [3,5]
  """
  def intersection(%SortedSet{size: size1}=set1, %SortedSet{size: size2}=set2) when size1 <= size2 do
    Enum.reduce(to_list(set1), SortedSet.new, fn(set1_member, new_set) ->
      if SortedSet.member?(set2, set1_member) do
        SortedSet.put(new_set, set1_member)
      else
        new_set
      end
    end)
  end

  def intersection(%SortedSet{}=set1, %SortedSet{}=set2) do
    intersection(set2, set1)
  end

  @doc ~S"""
  Returns a `SortedSet` containing the items in `set1` that are not in `set2`.

  ## Examples

      iex> set1 = SortedSet.new([1,2,3,4])
      iex> set2 = SortedSet.new([2,4,6,8])
      iex> SortedSet.to_list SortedSet.difference(set1, set2)
      [1,3]
  """
  def difference(%SortedSet{size: size1}=set1, %SortedSet{size: size2}=set2) when size1 > 0 and size2 > 0 do
    Enum.reduce(to_list(set1), set1, fn(set1_member, new_set) ->
      if SortedSet.member?(set2, set1_member) do
        delete(new_set, set1_member)
      else
        new_set
      end
    end)
  end

  # When the first set is empty, the difference is the empty set
  def difference(%SortedSet{size: 0}=empty_set, _) do
    empty_set
  end

  # When the other set is empty, the difference is the first set
  def difference(%SortedSet{}=set1, %SortedSet{size: 0}) do
    set1
  end

  ## Private helper functions

  # SortedSet put

  defp do_put([head|tail], element) when element > head do
    {tail_members, members_added} = do_put(tail, element)
    {[head | tail_members], members_added}
  end

  defp do_put([head|_tail]=sorted_set, element) when element < head do
    {[element | sorted_set], 1}
  end

  defp do_put([head|_tail]=sorted_set, element) when element == head do
    {sorted_set, 0}
  end

  defp do_put([], element), do: {[element], 1}

  # SortedSet delete

  # If the element is less than the one we are looking at, we can safely
  # know it was never in the set
  defp do_delete([head|_tail]=members, element) when element < head do
    {members, 0}
  end

  # If the element is greater than the current head, we haven't reached where it
  # might exist in the set. Recur again on the tail.
  defp do_delete([head|tail], element) when element > head do
    {tail_members, members_removed} = do_delete(tail, element)
    {[head | tail_members], members_removed}
  end

  # If the element matches the head, drop it
  defp do_delete([head|tail], element) when element == head do
    {tail, 1}
  end

  defp do_delete([], _element) do
    {[], 0}
  end

  # SortedSet member?

  defp do_member?([head|_tail], element) when element < head  do
    false
  end

  defp do_member?([head|tail], element) when element > head  do
    do_member?(tail, element)
  end

  defp do_member?([head|_tail], element) when element == head  do
    true
  end

  defp do_member?([], _element), do: false
end

defimpl Enumerable, for: SortedSet do
  def count(%SortedSet{size: size}), do: {:ok, size}
  def member?(%SortedSet{}=set, element), do: {:ok, SortedSet.member?(set, element)}
  def reduce(%SortedSet{}=set, acc, fun) do
    SortedSet.to_list(set)
    |> Enumerable.List.reduce(acc, fun)
  end
end

defimpl Collectable, for: SortedSet do
  def into(original) do
    {original, fn
      set, {:cont, new_member} -> SortedSet.put(set, new_member)
      set, :done -> set
      _, :halt -> :ok
    end}
  end
end

# We want our own inspect so that it will hide the underlying :members and :size
# fields. Otherwise users may try to play with them directly.
defimpl Inspect, for: SortedSet do
  import Inspect.Algebra

  def inspect(set, opts) do
    concat ["#SortedSet<", Inspect.List.inspect(SortedSet.to_list(set), opts), ">"]
  end
end

