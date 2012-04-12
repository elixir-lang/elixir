Code.require_file "../test_helper", __FILE__

defmodule DictTest do
  use ExUnit.Case

  test :new_pair do
    dict = Dict.new {"a", 0}
    assert is_record(dict, Dict.Record)
    assert_equal 1, GenDict.size dict

    assert GenDict.has_key? dict, "a"
    assert_equal 0, GenDict.get dict, "a"
  end

  test :new_pairs do
    dict = Dict.new [{"first key", 1}, {"second key", 2}]
    assert is_record(dict, Dict.Record)
    assert_equal 2, GenDict.size dict

    assert_equal ["first key", "second key"], List.sort GenDict.keys dict
    assert_equal [1, 2], List.sort GenDict.values dict
  end

  test :new_two_lists do
    dict = Dict.new ["first key", "second key"], [1, 2]
    assert_equal 2, GenDict.size dict
    assert_equal 1, GenDict.get dict, "first key"
    assert_equal 2, GenDict.get dict, "second key"

    assert_raises ArgumentError, Dict.new ["first key"], [1, 2]
  end

  test :new_pairs_with_transform do
    dict = Dict.new [{1}, {2}, {3}], fn({x}) -> { {x}, x } end
    assert is_record(dict, Dict.Record)
    assert_equal 3, GenDict.size dict

    assert_equal [{1}, {2}, {3}], List.sort GenDict.keys dict
    assert_equal [1, 2, 3], List.sort GenDict.values dict
  end

  test :get do
    assert_equal 1, GenDict.get(create_dict, "first_key")
    assert_equal 2, GenDict.get(create_dict, "second_key")
    assert_equal nil, GenDict.get(create_dict, "other_key")
    assert_equal "default", GenDict.get(create_empty_dict, "first_key", "default")
  end

  test :keys do
    assert_equal ["first_key", "second_key"], List.sort GenDict.keys create_dict
    assert_equal [], GenDict.keys create_empty_dict
  end

  test :values do
    assert_equal [1, 2], List.sort GenDict.values(create_dict)
    assert_equal [], GenDict.values create_empty_dict
  end

  test :delete do
    mdict = GenDict.delete create_dict, "second_key"
    assert_equal 1, GenDict.size mdict
    assert GenDict.has_key? mdict, "first_key"
    refute GenDict.has_key? mdict, "second_key"

    mdict = GenDict.delete(create_dict, "other_key")
    assert_equal mdict, create_dict
    assert_equal 0, GenDict.size GenDict.delete(create_empty_dict, "other_key")
  end

  test :put do
    dict = GenDict.put(create_empty_dict, {"first_key", 1})
    assert_equal 1, GenDict.get dict, "first_key"

    dict = GenDict.put(create_dict, {"first_key", {1}})
    assert_equal {1}, GenDict.get "first_key"
    assert_equal 2, GenDict.get "second_key"
  end

  test :merge do
    empty_dict = create_empty_dict
    dict = create_dict
    assert_equal dict, GenDict.merge empty_dict, dict
    assert_equal dict, GenDict.merge dict, empty_dict
    assert_equal dict, GenDict.merge dict, dict
    assert_equal [], GenDict.merge empty_dict, empty_dict

    dict1 = Dict.new ["a", "b", "c"], [1, 2, 3]
    dict2 = Dict.new ["a", "c", "d"], [3, :a, 0]
    assert_equal Dict.new(["a", "b", "c", "d"], [3, 2, :a, 0]), GenDict.merge(dict1, dict2)
  end

  test :merge_with_function do
    dict1 = Dict.new ["a", "b"], [1, 2]
    dict2 = Dict.new ["a", "d"], [3, 4]
    result = GenDict.merge dict1, dict2, fn(_k, v1, v2) ->
      v1 + v2
    end
    assert_equal Dict.new(["a", "b", "d"], [4, 2, 4]), result
  end

  test :has_key do
    dict = Dict.new [{"a", 1}]
    assert GenDict.has_key?(dict, "a")
    refute GenDict.has_key?(dict, "b")
  end

  defp create_empty_dict, do: Dict.new
  defp create_dict(list // [{"first_key", 1}, {"second_key", 2}]), do: Dict.new list
end
