Code.require_file "../test_helper", __FILE__

defmodule KeywordTest do
  use ExUnit.Case

  test :from_enum do
    assert_equal [first_key: 1, second_key: 2], Keyword.from_enum([{:second_key, 2}, {:first_key, 1}])
  end

  test :from_enum_with_function do
    assert_equal [a: :a, b: :b], Keyword.from_enum([:a, :b], fn(x) -> { x, x } end)
  end

  test :get do
    assert_equal 1, Keyword.get(create_keywords, :first_key)
    assert_equal 2, Keyword.get(create_keywords, :second_key)
    assert_equal nil, Keyword.get(create_keywords, :other_key)
    assert_equal "default", Keyword.get(create_empty_keywords, :first_key, "default")
  end

  test :keys do
    assert_equal [:first_key, :second_key], Keyword.keys(create_keywords)
    assert_equal [], Keyword.keys(create_empty_keywords)
  end

  test :values do
    assert_equal [1, 2], Keyword.values(create_keywords)
    assert_equal [], Keyword.values(create_empty_keywords)
  end

  test :delete do
    assert_equal [first_key: 1], Keyword.delete(create_keywords, :second_key)
    assert_equal [first_key: 1, second_key: 2], Keyword.delete(create_keywords, :other_key)
    assert_equal [], Keyword.delete(create_empty_keywords, :other_key)
  end

  test :put do
    assert_equal [first_key: 1], Keyword.put(create_empty_keywords, :first_key, 1)
    assert_equal [first_key: 1, second_key: 2], Keyword.put(create_keywords, :first_key, 1)
  end

  test :merge do
    assert_equal [first_key: 1, second_key: 2], Keyword.merge(create_empty_keywords, create_keywords)
    assert_equal [first_key: 1, second_key: 2], Keyword.merge(create_keywords, create_empty_keywords)
    assert_equal [first_key: 1, second_key: 2], Keyword.merge(create_keywords, create_keywords)
    assert_equal [], Keyword.merge(create_empty_keywords, create_empty_keywords)
  end

  test :merge_with_function do
    result = Keyword.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
      v1 + v2
    end
    assert_equal [a:4, b:2, d: 4], result
  end

  test :key do
    assert_equal true, Keyword.key?([a: 1], :a)
    assert_equal false, Keyword.key?([a: 1], :b)
  end

  defp create_empty_keywords, do: []
  defp create_keywords, do: [first_key: 1, second_key: 2]
end

defmodule Keyword.DuplicatedTest do
  use ExUnit.Case

  test :duplicated_entries do
    assert_equal [{:first_key,1},{:first_key,2},{:second_key,2}], create_keywords
  end

  test :get do
    assert_equal 1, Keyword.get(create_keywords, :first_key)
    assert_equal 2, Keyword.get(create_keywords, :second_key)
    assert_equal nil, Keyword.get(create_keywords, :other_key)
    assert_equal "default", Keyword.get(create_empty_keywords, :first_key, "default")
  end

  test :get_values do
    assert_equal [1,2], Keyword.get_values(create_keywords, :first_key)
    assert_equal [2], Keyword.get_values(create_keywords, :second_key)
    assert_equal [], Keyword.get_values(create_keywords, :other_key)
  end

  test :keys do
    assert_equal [:first_key, :first_key, :second_key], Keyword.keys(create_keywords)
    assert_equal [], Keyword.keys(create_empty_keywords)
  end

  test :values do
    assert_equal [1, 2, 2], Keyword.values(create_keywords)
    assert_equal [], Keyword.values(create_empty_keywords)
  end

  test :delete do
    assert_equal [second_key: 2], Keyword.delete(create_keywords, :first_key)
    assert_equal create_keywords, Keyword.delete(create_keywords, :other_key)
    assert_equal [], Keyword.delete(create_empty_keywords, :other_key)
  end

  test :put do
    assert_equal [first_key: 1], Keyword.put(create_empty_keywords, :first_key, 1)
    assert_equal [first_key: 1, second_key: 2], Keyword.put(create_keywords, :first_key, 1)
  end

  test :merge do
    assert_equal create_keywords, Keyword.merge(create_empty_keywords, create_keywords)
    assert_equal create_keywords, Keyword.merge(create_keywords, create_empty_keywords)
    assert_equal create_keywords, Keyword.merge(create_keywords, create_keywords)
    assert_equal [], Keyword.merge(create_empty_keywords, create_empty_keywords)
    assert_equal [first_key: 0, first_key: 2, second_key: 2], Keyword.merge(create_keywords, [first_key: 0])
    assert_equal [first_key: 0, first_key: 3, second_key: 2], Keyword.merge(create_keywords, [first_key: 0, first_key: 3])
  end

  test :merge_with_function do
    result = Keyword.merge [a: 1, b: 2], [a: 3, d: 4], fn(_k, v1, v2) ->
      v1 + v2
    end
    assert_equal [a:4, b:2, d: 4], result
  end

  test :key do
    assert_equal true, Keyword.key?([a: 1], :a)
    assert_equal false, Keyword.key?([a: 1], :b)
  end

  defp create_empty_keywords, do: []
  defp create_keywords, do: [first_key: 1, first_key: 2, second_key: 2]
end
