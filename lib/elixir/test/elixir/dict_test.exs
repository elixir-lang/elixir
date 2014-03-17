Code.require_file "test_helper.exs", __DIR__

# A TestDict implementation used only for testing.
defmodule TestDict do
  def new(list \\ []) when is_list(list) do
    %{__struct__: TestDict, list: list}
  end

  def size(%{__struct__: TestDict, list: list}) do
    length(list)
  end

  def update(%{__struct__: TestDict, list: list} = map, key, initial, fun) do
    %{ map | list: update(list, key, initial, fun) }
  end

  def update([{key, value}|list], key, _initial, fun) do
    [{key, fun.(value)}|list]
  end

  def update([{_, _} = e|list], key, initial, fun) do
    [e|update(list, key, initial, fun)]
  end

  def update([], key, initial, _fun) do
    [{key, initial}]
  end

  defimpl Enumerable do
    def reduce(%{list: list}, acc, fun), do: Enumerable.List.reduce(list, acc, fun)
    def member?(%{list: list}, other), do: Enumerable.List.member(list, other)
    def count(%{list: list}), do: Enumerable.List.count(list)
  end
end

defmodule DictTest.Common do
  defmacro __using__(_) do
    quote location: :keep do
      import Enum, only: [sort: 1]

      defp new_dict(list \\ [{"first_key", 1}, {"second_key", 2}]) do
        Enum.into list, dict_impl.new
      end

      defp new_dict(list, transform) do
        Enum.into list, dict_impl.new, transform
      end

      defp int_dict do
        Enum.into [{1,1}], dict_impl.new
      end

      test "access" do
        dict = new_dict()
        assert dict["first_key"] == 1
        assert dict["other_key"] == nil
      end

      test "access uses match operation" do
        dict = int_dict()
        assert dict[1] == 1
        assert dict[1.0] == nil
      end

      test "get/2 and get/3" do
        dict = new_dict()
        assert Dict.get(dict, "first_key")    == 1
        assert Dict.get(dict, "second_key")   == 2
        assert Dict.get(dict, "other_key")    == nil
        assert Dict.get(dict, "other_key", 3) == 3
      end

      test "get/2 with match" do
        assert Dict.get(int_dict, 1) == 1
        assert Dict.get(int_dict, 1.0) == nil
      end

      test "fetch/2" do
        dict = new_dict()
        assert Dict.fetch(dict, "first_key")  == {:ok, 1}
        assert Dict.fetch(dict, "second_key") == {:ok, 2}
        assert Dict.fetch(dict, "other_key")  == :error
      end

      test "fetch/2 with match" do
        assert Dict.fetch(int_dict, 1)   == {:ok, 1}
        assert Dict.fetch(int_dict, 1.0) == :error
      end

      test "fetch!/2" do
        dict = new_dict()
        assert Dict.fetch!(dict, "first_key") == 1
        assert Dict.fetch!(dict, "second_key") == 2
        assert_raise KeyError, fn ->
          Dict.fetch!(dict, "other_key")
        end
      end

      test "put/3" do
        dict = new_dict() |> Dict.put("first_key", {1})
        assert Dict.get(dict, "first_key")  == {1}
        assert Dict.get(dict, "second_key") == 2
      end

      test "put/3 with_match" do
        dict = int_dict()
        assert Dict.get(Dict.put(dict, 1, :other), 1)     == :other
        assert Dict.get(Dict.put(dict, 1.0, :other), 1)   == 1
        assert Dict.get(Dict.put(dict, 1, :other), 1.0)   == nil
        assert Dict.get(Dict.put(dict, 1.0, :other), 1.0) == :other
      end

      test "put_new/3" do
        dict = Dict.put_new(new_dict(), "first_key", {1})
        assert Dict.get(dict, "first_key") == 1
      end

      test "put_new/3 with_match" do
        assert Dict.get(Dict.put_new(int_dict, 1, :other), 1)     == 1
        assert Dict.get(Dict.put_new(int_dict, 1.0, :other), 1)   == 1
        assert Dict.get(Dict.put_new(int_dict, 1, :other), 1.0)   == nil
        assert Dict.get(Dict.put_new(int_dict, 1.0, :other), 1.0) == :other
      end

      test "keys/1" do
        assert Enum.sort(Dict.keys(new_dict())) == ["first_key", "second_key"]
        assert Dict.keys(new_dict([])) == []
      end

      test "values/1" do
        assert Enum.sort(Dict.values(new_dict())) == [1, 2]
        assert Dict.values(new_dict([])) == []
      end

      test "delete/2" do
        dict = Dict.delete(new_dict(), "second_key")
        assert Dict.size(dict) == 1
        assert Dict.has_key?(dict, "first_key")
        refute Dict.has_key?(dict, "second_key")

        dict = Dict.delete(new_dict(), "other_key")
        assert dict == new_dict()
        assert Dict.size(dict) == 2
      end

      test "delete/2 with match" do
        assert Dict.get(Dict.delete(int_dict, 1), 1) == nil
        assert Dict.get(Dict.delete(int_dict, 1.0), 1) == 1
      end

      test "merge/2" do
        dict = new_dict()
        assert Dict.merge(new_dict([]), dict) == dict
        assert Dict.merge(dict, new_dict([])) == dict
        assert Dict.merge(dict, dict)         == dict
        assert Dict.merge(new_dict([]), new_dict([])) == new_dict([])

        dict1 = new_dict [{"a", 1}, {"b", 2}, {"c", 3}]
        dict2 = new_dict [{"a", 3}, {"c", :a}, {"d", 0}]
        assert Dict.merge(dict1, dict2) |> Enum.sort ==
               [{"a", 3}, {"b", 2}, {"c", :a}, {"d", 0}]
      end

      test "merge/2 with other dict" do
        dict1 = new_dict [{"a", 1}, {"b", 2}, {"c", 3}]
        dict2 = TestDict.new [{"a",3}, {"c",:a}, {"d",0}]
        actual = Dict.merge(dict1, dict2)
        assert Dict.merge(dict1, dict2) |> Enum.sort ==
               [{"a", 3}, {"b", 2}, {"c", :a}, {"d", 0}]
        assert Dict.merge(dict2, dict1) |> Enum.sort ==
               [{"a", 1}, {"b", 2}, {"c", 3}, {"d", 0}]
      end

      test "merge/3" do
        dict1 = new_dict [{"a", 1}, {"b", 2}]
        dict2 = new_dict [{"a", 3}, {"d", 4}]
        actual = Dict.merge dict1, dict2, fn _k, v1, v2 -> v1 + v2 end
        assert Enum.sort(actual) == [{"a", 4}, {"b", 2}, {"d", 4}]
      end

      test "has_key?/2" do
        dict = new_dict()
        assert Dict.has_key?(dict, "first_key")
        refute Dict.has_key?(dict, "other_key")
      end

      test "has_key?/2 with match" do
        assert Dict.has_key?(int_dict, 1)
        refute Dict.has_key?(int_dict, 1.0)
      end

      test "size/1" do
        assert Dict.size(new_dict()) == 2
        assert Dict.size(new_dict([])) == 0
      end

      test "update!/3" do
        dict = Dict.update!(new_dict(), "first_key", fn val -> -val end)
        assert Dict.get(dict, "first_key") == -1

        assert_raise KeyError, fn ->
          Dict.update!(new_dict(), "non-existent", fn val -> -val end)
        end
      end

      test "update!/3 with match" do
        assert Dict.get(Dict.update!(int_dict(), 1, &(&1 + 1)), 1) == 2
      end

      test "update/4" do
        dict = Dict.update(new_dict(), "first_key", 0, fn val -> -val end)
        assert Dict.get(dict, "first_key") == -1

        dict = Dict.update(new_dict(), "non-existent", "...", fn val -> -val end)
        assert Dict.get(dict, "non-existent") == "..."
      end

      test "update/4 with match" do
        dict = int_dict()
        assert Dict.get(Dict.update(dict, 1.0, 2, &(&1 + 1)), 1) == 1
        assert Dict.get(Dict.update(dict, 1.0, 2, &(&1 + 1)), 1.0) == 2
      end

      test "pop/2 and pop/3" do
        dict = new_dict()

        {v, actual} = Dict.pop(dict, "first_key")
        assert v == 1
        assert actual == new_dict([{"second_key", 2}])

        {v, actual} = Dict.pop(dict, "other_key")
        assert v == nil
        assert dict == actual

        {v, actual} = Dict.pop(dict, "other_key", "default")
        assert v == "default"
        assert dict == actual
      end

      test "pop/2 and pop/3 with match" do
        dict = int_dict()

        {v, actual} = Dict.pop(dict, 1)
        assert v == 1
        assert Enum.sort(actual) == []

        {v, actual} = Dict.pop(dict, 1.0)
        assert v == nil
        assert actual == dict
      end

      test "split/2" do
        dict = new_dict()

        {take, drop} = Dict.split(dict, [])
        assert take == new_dict([])
        assert drop == dict

        {take, drop} = Dict.split(dict, ["unknown_key"])
        assert take == new_dict([])
        assert drop == dict

        split_keys   = ["first_key", "second_key", "unknown_key"]
        {take, drop} = Dict.split(dict, split_keys)

        take_expected = new_dict([])
                        |> Dict.put("first_key", 1)
                        |> Dict.put("second_key", 2)

        drop_expected = new_dict([])
                        |> Dict.delete("first_key")
                        |> Dict.delete("second_key")

        assert Enum.sort(take) == Enum.sort(take_expected)
        assert Enum.sort(drop) == Enum.sort(drop_expected)
      end

      test "split/2 with match" do
        dict = int_dict()
        { take, drop } = Dict.split(dict, [1])
        assert take == dict
        assert drop == new_dict([])

        { take, drop } = Dict.split(dict, [1.0])
        assert take == new_dict([])
        assert drop == dict
      end

      test "split/2 with enum" do
        dict = int_dict()
        { take, drop } = Dict.split(dict, 1..3)
        assert take == dict
        assert drop == new_dict([])
      end

      test "take/2" do
        dict = new_dict()
        take = Dict.take(dict, ["unknown_key"])
        assert take == new_dict([])

        take = Dict.take(dict, ["first_key"])
        assert take == new_dict([{"first_key", 1}])
      end

      test "take/2 with match" do
        dict = int_dict()
        assert Dict.take(dict, [1])   == dict
        assert Dict.take(dict, [1.0]) == new_dict([])
      end

      test "take/2 with enum" do
        dict = int_dict()
        assert Dict.take(dict, 1..3) == dict
      end

      test "drop/2" do
        dict = new_dict()
        drop = Dict.drop(dict, ["unknown_key"])
        assert drop == dict

        drop = Dict.drop(dict, ["first_key"])
        assert drop == new_dict([{"second_key", 2}])
      end

      test "drop/2 with match" do
        dict = int_dict()
        assert Dict.drop(dict, [1]) == new_dict([])
        assert Dict.drop(dict, [1.0]) == dict
      end

      test "drop/2 with enum" do
        dict = int_dict()
        assert Dict.drop(dict, 1..3) == new_dict([])
      end

      test "equal?/2" do
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

      test "equal?/2 with match" do
        dict1 = new_dict([{1,1}])
        dict2 = new_dict([{1.0,1}])
        assert Dict.equal?(dict1, dict1)
        refute Dict.equal?(dict1, dict2)
      end

      test "equal?/2 with other dict" do
        dict = new_dict([{1,1}])
        assert Dict.equal?(dict, TestDict.new([{1,1}]))
        refute Dict.equal?(dict, TestDict.new([{1.0,1}]))
      end

      test "is enumerable" do
        dict = new_dict()
        assert Enum.empty?(new_dict([]))
        refute Enum.empty?(dict)
        assert Enum.member?(dict, { "first_key", 1 })
        refute Enum.member?(dict, { "first_key", 2 })
        assert Enum.count(dict) == 2
        assert Enum.reduce(dict, 0, fn({ k, v }, acc) -> v + acc end) == 3
      end

      test "is collectable" do
        dict = new_dict()
        assert Dict.size(dict) == 2
        assert Enum.sort(dict) == [{"first_key", 1}, {"second_key", 2}]

        dict = new_dict([{1}, {2}, {3}], fn {x} -> { <<x + 64>>, x } end)
        assert Dict.size(dict) == 3
        assert Enum.sort(dict) == [{"A", 1}, {"B", 2}, {"C", 3}]

        assert Collectable.empty(new_dict) == new_dict([])
      end

      test "is zippable" do
        dict = new_dict()
        list = Dict.to_list(dict)
        assert Enum.zip(list, list) == Enum.zip(dict, dict)

        dict = new_dict(1..120, fn i -> { i, i } end)
        list = Dict.to_list(dict)
        assert Enum.zip(list, list) == Enum.zip(dict, dict)
      end
    end
  end
end

defmodule Dict.HashDictTest do
  use ExUnit.Case, async: true
  use DictTest.Common

  doctest Dict
  defp dict_impl, do: HashDict
end

defmodule Dict.ListDictTest do
  use ExUnit.Case, async: true
  use DictTest.Common

  doctest Dict
  defp dict_impl, do: ListDict
end

defmodule Dict.MapDictTest do
  use ExUnit.Case, async: true
  use DictTest.Common

  doctest Dict
  defp dict_impl, do: Map
end
