Code.require_file "test_helper.exs", __DIR__

defmodule SetTest.Common do
  defmacro __using__(module) do
    quote location: :keep do
      use ExUnit.Case, async: true

      defp new_set(list // []) do
        unquote(module).new(list)
      end

      defp new_set(list, fun) do
        unquote(module).new(list, fun)
      end

      defp int_set() do
        unquote(module).new([1,2,3])
      end

      test "new/1" do
        assert Set.equal?(new_set([1, 1, 2, 3, 3, 3]), new_set([1, 2, 3]))
      end

      test "new/2" do
        assert Set.equal?(new_set([1, 1, 2, 3, 3, 3], &(&1 * 2)), new_set([2, 4, 6]))
      end

      test "delete/2" do
        result = Set.delete(new_set([1, 2, 3]), 2)
        assert Set.equal?(result, new_set([1, 3]))
      end

      test "delete/2 with match" do
        refute Set.member?(Set.delete(int_set, 1), 1)
        assert Set.member?(Set.delete(int_set, 1.0), 1)
      end

      test "difference/2" do
        result = Set.difference(new_set([1, 2, 3]), new_set([3]))
        assert Set.equal?(result, HashSet.new([1, 2]))
      end

      test "difference/2 with match" do
        refute Set.member?(Set.difference(int_set, new_set([1])), 1)
        assert Set.member?(Set.difference(int_set, new_set([1.0])), 1)
      end

      test "disjoint?/2" do
        assert Set.disjoint?(new_set([1, 2, 3]), new_set([4, 5 ,6])) == true
        assert Set.disjoint?(new_set([1, 2, 3]), new_set([3, 4 ,5])) == false
      end

      test "empty/1" do
        result = Set.empty new_set([1, 2, 3])
        assert Set.equal?(result, new_set)
      end

      test "equal?/2" do
        assert Set.equal?(new_set([1, 2, 3]), new_set([3, 2, 1]))
        refute Set.equal?(new_set([1, 2, 3]), new_set([3.0, 2.0, 1.0]))
      end

      test "intersection/2" do
        result = Set.intersection(new_set([1, 2, 3]), new_set([2, 3, 4]))
        assert Set.equal?(result, new_set([2, 3]))
      end

      test "intersection/2 with match" do
        assert Set.member?(Set.intersection(int_set, new_set([1])), 1)
        refute Set.member?(Set.intersection(int_set, new_set([1.0])), 1)
      end

      test "member?/2" do
        assert Set.member?(new_set([1, 2, 3]), 2)
        refute Set.member?(new_set([1, 2, 3]), 4)
        refute Set.member?(new_set([1, 2, 3]), 1.0)
      end

      test "put/2" do
        result = Set.put(new_set([1, 2]), 3)
        assert Set.equal?(result, new_set([1, 2, 3]))
      end

      test "put/2 with match" do
        assert Set.size(Set.put(int_set, 1)) == 3
        assert Set.size(Set.put(int_set, 1.0)) == 4
      end

      test "size/1" do
        assert Set.size(new_set([1, 2, 3])) == 3
      end

      test "subset?/2" do
        assert Set.subset?(new_set([1, 2]), new_set([1, 2, 3]))
        refute Set.subset?(new_set([1, 2, 3]), new_set([1, 2]))
      end

      test "subset/2 with match?" do
        assert Set.subset?(new_set([1]), int_set)
        refute Set.subset?(new_set([1.0]), int_set)
      end

      test "to_list/1" do
        assert Set.to_list(new_set([1, 2, 3])) |> Enum.sort == [1, 2, 3]
      end

      test "union/2" do
        result = Set.union(new_set([1, 2, 3]), new_set([2, 3, 4]))
        assert Set.equal?(result, new_set([1, 2, 3, 4]))
      end

      test "union/2 with match" do
        assert Set.size(Set.union(int_set, new_set([1]))) == 3
        assert Set.size(Set.union(int_set, new_set([1.0]))) == 4
      end

      test "implements Enum" do
        assert Enum.member?(int_set, 1)
        refute Enum.member?(int_set, 1.0)
        assert Enum.sort(int_set) == [1,2,3]
      end

      test "is zippable" do
        set  = new_set(1..8)
        list = Dict.to_list(set)
        assert Enum.zip(list, list) == Enum.zip(set, set)

        set  = new_set(1..100)
        list = Dict.to_list(set)
        assert Enum.zip(list, list) == Enum.zip(set, set)
      end

      test "unsupported set" do
        assert_raise ArgumentError, "unsupported set: :bad_set", fn ->
          Set.to_list :bad_set
        end
      end
    end
  end
end

defmodule Set.HashSetTest do
  use SetTest.Common, HashSet
end
