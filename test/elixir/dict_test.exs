Code.require_file "../test_helper", __FILE__

defmodule DictTest.Common do
  defmacro __using__(module, _opts // []) do
    quote do
      use ExUnit.Case

      test :new_pair do
        dict = new_dict {"a", 0}
        assert_equal 1, Dict.size dict

        assert Dict.has_key? dict, "a"
        assert_equal 0, Dict.get dict, "a"
      end

      test :new_pairs do
        dict = new_dict [{"first key", 1}, {"second key", 2}]
        assert_equal 2, Dict.size dict

        assert_equal ["first key", "second key"], List.sort Dict.keys dict
        assert_equal [1, 2], List.sort Dict.values dict
      end

      test :new_two_lists do
        dict = new_dict ["first key", "second key"], [1, 2]
        assert_equal 2, Dict.size dict
        assert_equal 1, Dict.get dict, "first key"
        assert_equal 2, Dict.get dict, "second key"

        assert_raises ArgumentError, fn() -> new_dict(["first key"], [1, 2]) end
      end

      test :new_pairs_with_transform do
        dict = new_dict [{1}, {2}, {3}], fn({x}) -> { {x}, x } end
        assert_equal 3, Dict.size dict

        assert_equal [{1}, {2}, {3}], List.sort Dict.keys dict
        assert_equal [1, 2, 3], List.sort Dict.values dict
      end

      test :get do
        assert_equal 1, Dict.get(new_dict, "first_key")
        assert_equal 2, Dict.get(new_dict, "second_key")
        assert_equal nil, Dict.get(new_dict, "other_key")
        assert_equal "default", Dict.get(empty_dict, "first_key", "default")
      end

      test :put do
        dict = Dict.put(empty_dict, {"first_key", 1})
        assert_equal 1, Dict.get dict, "first_key"

        dict = Dict.put(new_dict, "first_key", {1})
        assert_equal {1}, Dict.get dict, "first_key"
        assert_equal 2, Dict.get dict, "second_key"
      end

      test :keys do
        assert_equal ["first_key", "second_key"], List.sort Dict.keys new_dict
        assert_equal [], Dict.keys empty_dict
      end

      test :values do
        assert_equal [1, 2], List.sort Dict.values(new_dict)
        assert_equal [], Dict.values empty_dict
      end

      test :delete do
        mdict = Dict.delete new_dict, "second_key"
        assert_equal 1, Dict.size mdict
        assert Dict.has_key? mdict, "first_key"
        refute Dict.has_key? mdict, "second_key"

        mdict = Dict.delete(new_dict, "other_key")
        assert_equal mdict, new_dict
        assert_equal 0, Dict.size Dict.delete(empty_dict, "other_key")
      end

      test :merge do
        dict = new_dict
        assert_equal dict, Dict.merge empty_dict, dict
        assert_equal dict, Dict.merge dict, empty_dict
        assert_equal dict, Dict.merge dict, dict
        assert_equal empty_dict, Dict.merge empty_dict, empty_dict

        dict1 = new_dict ["a", "b", "c"], [1, 2, 3]
        dict2 = new_dict ["a", "c", "d"], [3, :a, 0]
        assert_equal new_dict(["a", "b", "c", "d"], [3, 2, :a, 0]), Dict.merge(dict1, dict2)
      end

      test :merge_with_function do
        dict1 = new_dict ["a", "b"], [1, 2]
        dict2 = new_dict ["a", "d"], [3, 4]
        result = Dict.merge dict1, dict2, fn(_k, v1, v2) ->
          v1 + v2
        end
        assert_equal new_dict(["a", "b", "d"], [4, 2, 4]), result
      end

      test :has_key do
        dict = new_dict [{"a", 1}]
        assert Dict.has_key?(dict, "a")
        refute Dict.has_key?(dict, "b")
      end

      test :size do
        assert_equal 2, Dict.size new_dict
        assert_equal 0, Dict.size empty_dict
      end

      test :update do
        dict = Dict.update new_dict, "first_key", fn(val) -> -val end
        assert_equal -1, Dict.get dict, "first_key"

        dict = Dict.update dict, "non-existent", "...", fn(val) -> -val end
        assert_equal "...", Dict.get dict, "non-existent"
      end

      test :empty do
        assert_equal empty_dict, Dict.empty new_dict
      end

      defp empty_dict, do: unquote(module).new
      defp new_dict({k, v}), do: unquote(module).new {k, v}
      defp new_dict(list // [{"first_key", 1}, {"second_key", 2}]), do: unquote(module).new list
      defp new_dict(list, transform) when is_function(transform), do: unquote(module).new list, transform
      defp new_dict(keys, values), do: unquote(module).new keys, values
    end
  end
end

defmodule DictTest do
  require DictTest.Common
  DictTest.Common.__using__(HashDict)

  test :new do
    assert_equal :dict.new, (HashDict.new).data
  end
end

defmodule OrddictTest do
  require DictTest.Common
  DictTest.Common.__using__(Orddict)

  test :new do
    assert_equal [], Orddict.new
  end
end
