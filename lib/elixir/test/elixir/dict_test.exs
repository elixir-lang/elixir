Code.require_file "../test_helper.exs", __FILE__

defmodule DictTest.Common do
  defmacro __using__(_) do
    quote location: :keep do
      use ExUnit.Case, async: true

      test :access do
        dict = new_dict [{"first_key", 1}, {"second_key", 2}]
        assert dict["first_key"] == 1
        assert dict["third_key"] == nil
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

      test :get! do
        assert 1 == Dict.get!(new_dict, "first_key")
        assert 2 == Dict.get!(new_dict, "second_key")
        assert_raise KeyError, fn ->
          Dict.get!(new_dict, "other_key")
        end
      end

      test :put do
        dict = Dict.put(new_dict, "first_key", {1})
        assert {1} == Dict.get dict, "first_key"
        assert 2 == Dict.get dict, "second_key"
      end

      test :put_new do
        dict = Dict.put_new(new_dict, "first_key", {1})
        assert 1 == Dict.get dict, "first_key"
      end

      test :keys do
        assert ["first_key", "second_key"] == Enum.sort Dict.keys new_dict
        assert [] == Dict.keys empty_dict
      end

      test :values do
        assert [1, 2] ==Enum.sort Dict.values(new_dict)
        assert [] == Dict.values empty_dict
      end

      test :delete do
        mdict = Dict.delete new_dict, "second_key"
        assert 1 == Dict.size mdict
        assert Dict.has_key? mdict, "first_key"
        refute Dict.has_key? mdict, "second_key"

        mdict = Dict.delete(new_dict, "other_key")
        assert mdict == new_dict
        assert 0 == Dict.size Dict.delete(empty_dict, "other_key")
      end

      test :merge do
        dict = new_dict
        assert dict == Dict.merge empty_dict, dict
        assert dict == Dict.merge dict, empty_dict
        assert dict == Dict.merge dict, dict
        assert empty_dict == Dict.merge empty_dict, empty_dict

        dict1 = new_dict Enum.zip ["a", "b", "c"], [1, 2, 3]
        dict2 = new_dict Enum.zip ["a", "c", "d"], [3, :a, 0]
        merged = Dict.merge(dict1, dict2)
        final = new_dict Enum.zip ["a", "b", "c", "d"], [3, 2, :a, 0]

        cmp = fn {k1, _}, {k2, _} -> k1 < k2 end
        actual = Enum.sort(Dict.to_list(merged), cmp)
        expected = Enum.sort(final, cmp)
        assert expected == actual
      end

      test :merge_with_enum do
        dict1 = new_dict Enum.zip ["a", "b", "c"], [1, 2, 3]
        dict2 = Enum.zip ["a", "c", "d"], [3, :a, 0]
        merged = Dict.merge(dict1, dict2)
        final = new_dict(Enum.zip ["a", "b", "c", "d"], [3, 2, :a, 0])

        cmp = fn {k1, _}, {k2, _} -> k1 < k2 end
        actual = Enum.sort(Dict.to_list(merged), cmp)
        expected = Enum.sort(final, cmp)
        assert expected == actual
      end

      test :merge_with_function do
        dict1 = new_dict Enum.zip ["a", "b"], [1, 2]
        dict2 = new_dict Enum.zip ["a", "d"], [3, 4]
        result = Dict.merge dict1, dict2, fn _k, v1, v2 ->
          v1 + v2
        end
        assert new_dict(Enum.zip ["a", "b", "d"], [4, 2, 4]) == result
      end

      test :has_key do
        dict = new_dict [{"a", 1}]
        assert Dict.has_key?(dict, "a")
        refute Dict.has_key?(dict, "b")
      end

      test :size do
        assert 2 == Dict.size new_dict
        assert 0 == Dict.size empty_dict
      end

      test :update do
        dict = Dict.update new_dict, "first_key", fn val -> -val end
        assert -1 == Dict.get dict, "first_key"

        dict = Dict.update dict, "non-existent", "...", fn val -> -val end
        assert "..." == Dict.get dict, "non-existent"
      end

      test :empty do
        assert empty_dict == Dict.empty new_dict
      end
    end
  end
end

defmodule DictTest do
  use DictTest.Common

  test :new do
    assert :dict.new == elem(new_dict([]), 1)
  end

  defp empty_dict, do: HashDict.new

  defp new_dict(list // [{"first_key", 1}, {"second_key", 2}])
  defp new_dict(list), do: HashDict.new list
  defp new_dict(list, transform), do: HashDict.new list, transform
end

defmodule OrdDictTest do
  use DictTest.Common

  test :new do
    assert [] == elem(new_dict([]), 1)
  end

  defp empty_dict, do: OrdDict.new

  defp new_dict(list // [{"first_key", 1}, {"second_key", 2}])
  defp new_dict(list), do: OrdDict.new list
  defp new_dict(list, transform), do: OrdDict.new list, transform
end

defmodule Binary.DictTest do
  use DictTest.Common

  test :new do
    assert [] == elem(new_dict([]), 1)
  end

  test :merge_mixed do
    merged = Dict.merge(new_dict, new_dict([first_key: 13]))
    assert merged[:first_key]  == 13
    assert merged["first_key"] == 13
  end

  defp empty_dict, do: Binary.Dict.new

  defp new_dict(list // [{"first_key", 1}, {"second_key", 2}])
  defp new_dict(list), do: Binary.Dict.new list
  defp new_dict(list, transform), do: Binary.Dict.new list, transform
end

defmodule ListDictTest do
  use DictTest.Common

  defp empty_dict, do: []

  defp new_dict(list // [{"first_key", 1}, {"second_key", 2}])
  defp new_dict(list), do: list
  defp new_dict(list, transform) do
    Enum.map list, transform
  end
end