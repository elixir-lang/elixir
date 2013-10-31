Code.require_file "test_helper.exs", __DIR__

defmodule DictTest.Common do
  defmacro __using__(module) do
    quote location: :keep do
      # Most underlying Dict implementations have no key
      # order guarantees, sort them before we compare:
      defmacrop dicts_equal(actual, expected) do
        quote do
          cmp = fn { k1, _ }, { k2, _ } -> k1 < k2 end
          Enum.sort(unquote(expected), cmp) == Enum.sort(unquote(actual), cmp)
        end
      end

      defp empty_dict, do: unquote(module).new

      defp new_dict(list // [{"first_key", 1}, {"second_key", 2}]) do
        unquote(module).new list
      end

      defp new_dict(list, transform) do
        unquote(module).new list, transform
      end

      defp int_dict do
        unquote(module).new [{1,1}]
      end

      test :access do
        dict = new_dict [{"first_key", 1}, {"second_key", 2}]
        assert dict["first_key"] == 1
        assert dict["third_key"] == nil
      end

      test :access_with_match do
        assert int_dict[1] == 1
        assert int_dict[1.0] == nil
      end

      test :new_pairs do
        dict = new_dict [{"first_key", 1}, {"second_key", 2}]
        assert 2 == Dict.size dict

        assert ["first_key", "second_key"] == Enum.sort Dict.keys dict
        assert [1, 2] == Enum.sort Dict.values dict
      end

      test :new_pairs_with_transform do
        dict = new_dict [{1}, {2}, {3}], fn {x} -> { <<x + 64>>, x } end
        assert 3 == Dict.size dict

        assert ["A", "B", "C"] == Enum.sort Dict.keys dict
        assert [1, 2, 3] == Enum.sort Dict.values dict
      end

      test :get do
        assert 1 == Dict.get(new_dict, "first_key")
        assert 2 == Dict.get(new_dict, "second_key")
        assert nil == Dict.get(new_dict, "other_key")
        assert "default" == Dict.get(empty_dict, "first_key", "default")
      end

      test :get_with_match do
        assert Dict.get(int_dict, 1) == 1
        assert Dict.get(int_dict, 1.0) == nil
      end

      test :fetch do
        assert { :ok, 1 } == Dict.fetch(new_dict, "first_key")
        assert { :ok, 2 } == Dict.fetch(new_dict, "second_key")
        assert :error     == Dict.fetch(new_dict, "other_key")
      end

      test :fetch_with_match do
        assert Dict.fetch(int_dict, 1) == { :ok, 1 }
        assert Dict.fetch(int_dict, 1.0) == :error
      end

      test :fetch! do
        assert 1 == Dict.fetch!(new_dict, "first_key")
        assert 2 == Dict.fetch!(new_dict, "second_key")
        assert_raise KeyError, fn ->
          Dict.fetch!(new_dict, "other_key")
        end
      end

      test :fetch_with_match! do
        assert Dict.fetch!(int_dict, 1) == 1
        assert_raise KeyError, fn ->
          assert Dict.fetch!(int_dict, 1.0)
        end
      end

      test :put do
        dict = Dict.put(new_dict, "first_key", {1})
        assert {1} == Dict.get dict, "first_key"
        assert 2 == Dict.get dict, "second_key"
      end

      test :put_with_match do
        assert Dict.get(Dict.put(int_dict, 1, :other), 1)     == :other
        assert Dict.get(Dict.put(int_dict, 1.0, :other), 1)   == 1
        assert Dict.get(Dict.put(int_dict, 1, :other), 1.0)   == nil
        assert Dict.get(Dict.put(int_dict, 1.0, :other), 1.0) == :other
      end

      test :put_new do
        dict = Dict.put_new(new_dict, "first_key", {1})
        assert 1 == Dict.get dict, "first_key"
      end

      test :put_new_with_match do
        assert Dict.get(Dict.put_new(int_dict, 1, :other), 1)     == 1
        assert Dict.get(Dict.put_new(int_dict, 1.0, :other), 1)   == 1
        assert Dict.get(Dict.put_new(int_dict, 1, :other), 1.0)   == nil
        assert Dict.get(Dict.put_new(int_dict, 1.0, :other), 1.0) == :other
      end

      test :keys do
        assert Enum.sort(Dict.keys new_dict) == ["first_key", "second_key"]
        assert Dict.keys(empty_dict) == []
      end

      test :values do
        assert Enum.sort(Dict.values(new_dict)) == [1, 2]
        assert Dict.values(empty_dict) == []
      end

      test :delete do
        mdict = Dict.delete new_dict, "second_key"
        assert Dict.size(mdict) == 1
        assert Dict.has_key? mdict, "first_key"
        refute Dict.has_key? mdict, "second_key"

        mdict = Dict.delete(new_dict, "other_key")
        assert mdict == new_dict
        assert Dict.size(Dict.delete(empty_dict, "other_key")) == 0
      end

      test :delete_with_match do
        assert Dict.get(Dict.delete(int_dict, 1), 1) == nil
        assert Dict.get(Dict.delete(int_dict, 1.0), 1) == 1
      end

      test :merge do
        dict = new_dict
        assert dict == Dict.merge empty_dict, dict
        assert dict == Dict.merge dict, empty_dict
        assert dict == Dict.merge dict, dict
        assert empty_dict == Dict.merge empty_dict, empty_dict

        dict1 = new_dict Enum.zip ["a", "b", "c"], [1, 2, 3]
        dict2 = new_dict Enum.zip ["a", "c", "d"], [3, :a, 0]
        actual = Dict.merge(dict1, dict2)
        expected = new_dict Enum.zip ["a", "b", "c", "d"], [3, 2, :a, 0]
        assert dicts_equal actual, expected
      end

      test :merge_with_enum do
        dict1 = new_dict Enum.zip ["a", "b", "c"], [1, 2, 3]
        dict2 = Enum.zip ["a", "c", "d"], [3, :a, 0]
        actual = Dict.merge(dict1, dict2)
        expected = new_dict(Enum.zip ["a", "b", "c", "d"], [3, 2, :a, 0])
        assert dicts_equal actual, expected
      end

      test :merge_with_function do
        dict1 = new_dict Enum.zip ["a", "b"], [1, 2]
        dict2 = new_dict Enum.zip ["a", "d"], [3, 4]
        actual = Dict.merge dict1, dict2, fn _k, v1, v2 -> v1 + v2 end
        expected = new_dict(Enum.zip ["a", "b", "d"], [4, 2, 4])
        assert dicts_equal actual, expected
      end

      test :has_key do
        dict = new_dict [{"a", 1}]
        assert Dict.has_key?(dict, "a")
        refute Dict.has_key?(dict, "b")
      end

      test :has_key_with_match do
        assert Dict.has_key?(int_dict, 1)
        refute Dict.has_key?(int_dict, 1.0)
      end

      test :size do
        assert 2 == Dict.size new_dict
        assert 0 == Dict.size empty_dict
      end

      test :update do
        dict = Dict.update!(new_dict, "first_key", fn val -> -val end)
        assert Dict.get(dict, "first_key") == -1

        dict = Dict.update(dict, "non-existent", "...", fn val -> -val end)
        assert Dict.get(dict, "non-existent") == "..."
      end

      test :update_with_match do
        assert Dict.get(Dict.update!(int_dict, 1, &(&1 + 1)), 1) == 2
        assert Dict.get(Dict.update(int_dict, 1.0, 2, &(&1 + 1)), 1) == 1
        assert Dict.get(Dict.update(int_dict, 1.0, 2, &(&1 + 1)), 1.0) == 2
      end

      test :pop do
        {v, actual} = Dict.pop(new_dict, "first_key")
        assert 1 == v
        assert dicts_equal actual, Dict.delete(new_dict, "first_key")

        {v, actual} = Dict.pop(new_dict, "second_key")
        assert 2 == v
        assert dicts_equal actual, Dict.delete(new_dict, "second_key")

        {v, actual} = Dict.pop(new_dict, "other_key")
        assert nil == v
        assert dicts_equal actual, new_dict

        {v, actual} = Dict.pop(empty_dict, "first_key", "default")
        assert "default" == v
        assert dicts_equal actual, empty_dict

        {v, actual} = Dict.pop(new_dict, "other_key", "default")
        assert "default" == v
        assert dicts_equal actual, new_dict
      end

      test :pop_with_match do
        {v, actual} = Dict.pop(int_dict, 1)
        assert v == 1
        assert dicts_equal actual, []

        {v, actual} = Dict.pop(int_dict, 1.0)
        assert v == nil
        assert dicts_equal actual, int_dict
      end

      test :split do
        split_keys = []
        {take, drop} = Dict.split(new_dict, split_keys)
        assert dicts_equal take, Dict.empty(new_dict)
        assert dicts_equal drop, new_dict

        split_keys = ["unknown_key"]
        {take, drop} = Dict.split(new_dict, split_keys)
        assert dicts_equal take, Dict.empty(new_dict)
        assert dicts_equal drop, new_dict

        split_keys   = ["first_key", "second_key", "unknown_key"]
        {take, drop} = Dict.split(new_dict, split_keys)

        first_val  = Dict.get(new_dict, "first_key")
        second_val = Dict.get(new_dict, "second_key")
        take_expected = Dict.empty(new_dict)
        take_expected = Dict.put(take_expected, "first_key", first_val)
        take_expected = Dict.put(take_expected, "second_key", second_val)
        drop_expected = new_dict
        drop_expected = Dict.delete(drop_expected, "first_key")
        drop_expected = Dict.delete(drop_expected, "second_key")

        assert dicts_equal take, take_expected
        assert dicts_equal drop, drop_expected
      end

      test :split_with_match do
        { take, drop } = Dict.split(int_dict, [1])
        assert dicts_equal take, int_dict
        assert dicts_equal drop, []

        { take, drop } = Dict.split(int_dict, [1.0])
        assert dicts_equal take, []
        assert dicts_equal drop, int_dict
      end

      test :take do
        result = Dict.take(new_dict, ["unknown_key"])
        assert dicts_equal result, Dict.empty(new_dict)

        result = Dict.take(new_dict, ["first_key"])
        first_val = Dict.get(new_dict, "first_key")
        expected = Dict.put(Dict.empty(new_dict), "first_key", first_val)
        assert dicts_equal result, expected
      end

      test :take_with_match do
        assert dicts_equal Dict.take(int_dict, [1]), int_dict
        assert dicts_equal Dict.take(int_dict, [1.0]), []
      end

      test :drop do
        result = Dict.drop(new_dict, ["unknown_key"])
        assert dicts_equal result, new_dict

        result = Dict.drop(new_dict, ["first_key"])
        assert dicts_equal result, Dict.delete(new_dict, "first_key")
      end

      test :drop_with_match do
        assert dicts_equal Dict.drop(int_dict, [1]), []
        assert dicts_equal Dict.drop(int_dict, [1.0]), int_dict
      end

      test :empty do
        assert empty_dict == Dict.empty new_dict
      end

      test :equal? do
        dict1 = new_dict(a: 2, b: 3, f: 5, c: 123)
        dict2 = new_dict(a: 2, b: 3, f: 5, c: 123)
        assert dict_impl.equal?(dict1, dict2)
        assert Dict.equal?(dict1, dict2)

        dict2 = Dict.put(dict2, :a, 3)
        refute dict_impl.equal?(dict1, dict2)
        refute Dict.equal?(dict1, dict2)

        dict3 = [a: 2, b: 3, f: 5, c: 123, z: 666]
        refute Dict.equal?(dict1, dict3)
        refute Dict.equal?(dict3, dict1)
      end

      test :equal_with_match? do
        dict1 = new_dict([{1,1}])
        dict2 = new_dict([{1.0,1}])
        assert dict_impl.equal?(dict1, dict1)
        refute dict_impl.equal?(dict1, dict2)
        assert Dict.equal?(dict1, dict1)
        refute Dict.equal?(dict1, dict2)
      end

      test :enum_member do
        assert Enum.member?(new_dict([{1,1}]), { 1, 1 })
        refute Enum.member?(new_dict([{1,1}]), { 1.0, 1 })
        refute Enum.member?(new_dict([{1,1}]), { 1, 1.0 })
        refute Enum.member?(new_dict([{1,1}]), { 1.0, 1.0 })
      end

      test "unsupported dict" do
        assert_raise ArgumentError, "unsupported dict: :bad_dict", fn ->
          Dict.to_list :bad_dict
        end
      end
    end
  end
end

defmodule Dict.HashDictTest do
  use ExUnit.Case, async: true
  use DictTest.Common, HashDict

  doctest Dict
  defp dict_impl, do: HashDict
end

defmodule Dict.ListDictTest do
  use ExUnit.Case, async: true
  use DictTest.Common, ListDict

  doctest Dict
  defp dict_impl, do: ListDict
end
