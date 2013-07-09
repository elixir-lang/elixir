Code.require_file "test_helper.exs", __DIR__

defmodule SetTest.Common do
  defmacro __using__(module) do
    quote location: :keep do
      use ExUnit.Case, async: true

      test "a set removes duplicates" do
        assert Set.equal?(new_set([1, 1, 2, 3, 3, 3]), new_set([1, 2, 3]))
      end

      test :delete do
        result = Set.delete(new_set([1, 2, 3]), 2)
        assert Set.equal?(result, new_set([1, 3]))
      end

      test :difference do
        result = Set.difference(new_set([1, 2, 3]), new_set([3]))
        assert Set.equal?(result, HashSet.new([1, 2]))
      end

      test :disjoint? do
        assert Set.disjoint?(new_set([1, 2, 3]), new_set([4, 5 ,6])) == true
        assert Set.disjoint?(new_set([1, 2, 3]), new_set([3, 4 ,5])) == false
      end

      test :empty do
        result = Set.empty new_set([1, 2, 3])
        assert Set.equal?(result, new_set)
      end

      test :equal? do
        assert Set.equal?(new_set([1, 2, 3]), new_set([3, 2, 1])) == true
      end

      test :intersection do
        result = Set.intersection(new_set([1, 2, 3]), new_set([2, 3, 4]))
        assert Set.equal?(result, new_set([2, 3]))
      end

      test :member? do
        assert Set.member?(new_set([1, 2, 3]), 2) == true
        assert Set.member?(new_set([1, 2, 3]), 4) == false
      end

      test :put do
        result = Set.put(new_set([1, 2]), 3)
        assert Set.equal?(result, new_set([1, 2, 3]))
      end

      test :size do
        assert Set.size(new_set([1, 2, 3])) == 3
      end

      test :subset? do
        assert Set.subset?(new_set([1, 2]), new_set([1, 2, 3])) == true
        assert Set.subset?(new_set([1, 2, 3]), new_set([1, 2])) == false
      end

      test :to_list do
        assert Set.to_list(new_set([1, 2, 3])) == [1, 2, 3]
      end

      test :union do
        result = Set.union(new_set([1, 2, 3]), new_set([2, 3, 4]))
        assert Set.equal?(result, new_set([1, 2, 3, 4]))
      end

      test "unsupported set" do
        assert_raise ArgumentError, "unsupported set: :bad_set", fn ->
          Set.to_list :bad_set
        end
      end

      defp new_set(list // []) do
        unquote(module).new(list)
      end
    end
  end
end

defmodule Set.HashSetTest do
  use SetTest.Common, HashSet
end
