Code.require_file "../test_helper", __FILE__

defmodule OrddictTest do
  use ExUnit::Case

  test :from_enum do
    assert_equal [first_key: 1, second_key: 2], Orddict.from_enum([{:second_key, 2}, {:first_key, 1}])
  end

  test :from_enum_with_function do
    assert_equal [a: :a, b: :b], Orddict.from_enum([:a, :b], fn(x) -> { x, x } end)
  end

  test :fetch do
    assert_equal 1, Orddict.get(create_dict, :first_key)
    assert_equal 2, Orddict.get(create_dict, :second_key)
    assert_equal nil, Orddict.get(create_dict, :other_key)
    assert_equal "default", Orddict.get(create_empty_dict, :first_key, "default")
  end

  test :keys do
    assert_equal [:first_key, :second_key], Orddict.keys(create_dict)
    assert_equal [], Orddict.keys(create_empty_dict)
  end

  test :values do
    assert_equal [1, 2], Orddict.values(create_dict)
    assert_equal [], Orddict.values(create_empty_dict)
  end

  test :delete do
    assert_equal [first_key: 1], Orddict.delete(create_dict, :second_key)
    assert_equal [first_key: 1, second_key: 2], Orddict.delete(create_dict, :other_key)
    assert_equal [], Orddict.delete(create_empty_dict, :other_key)
  end

  test :put do
    assert_equal [first_key: 1], Orddict.put(create_empty_dict, :first_key, 1)
    assert_equal [first_key: 1, second_key: 2], Orddict.put(create_dict, :first_key, 1)
  end

  test :merge do
    assert_equal [first_key: 1, second_key: 2], Orddict.merge(create_empty_dict, create_dict)
    assert_equal [first_key: 1, second_key: 2], Orddict.merge(create_dict, create_empty_dict)
    assert_equal [first_key: 1, second_key: 2], Orddict.merge(create_dict, create_dict)
    assert_equal [], Orddict.merge(create_empty_dict, create_empty_dict)
  end

  test :merge_with_function do
    result = Orddict.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
      v1 + v2
    end
    assert_equal [a:4, b:2, d: 4], result
  end

  test :key do
    assert_equal true, Orddict.key?([a: 1], :a)
    assert_equal false, Orddict.key?([a: 1], :b)
  end

  defp create_empty_dict, do: create_dict([])
  defp create_dict(list // [first_key: 1, second_key: 2]), do: Orddict.from_enum(list)
end
