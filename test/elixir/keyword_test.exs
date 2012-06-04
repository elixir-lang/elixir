Code.require_file "../test_helper", __FILE__

defmodule KeywordTest do
  use ExUnit.Case

  test :literal do
    assert [{:foo?, :bar}] == [foo?: :bar]
  end

  test :from_enum do
    list = [{:b,2},{:a,1},{:c,3}]
    dict = Orddict.new list
    assert Keyword.from_enum(list) == [a: 1, b: 2, c: 3]
    assert Keyword.from_enum(dict) == [a: 1, b: 2, c: 3]
  end

  test :new do
    assert Keyword.new == []
  end

  test :new_with_pairs do
    assert Keyword.new([{:second_key, 2}, {:first_key, 1}]) == [first_key: 1, second_key: 2]
  end

  test :new_with_function do
    assert Keyword.new([:a, :b], fn x -> { x, x } end) == [a: :a, b: :b]
  end

  test :get do
    assert Keyword.get(create_keywords, :first_key) == 1
    assert Keyword.get(create_keywords, :second_key) == 2
    assert Keyword.get(create_keywords, :other_key) == nil
    assert Keyword.get(create_empty_keywords, :first_key, "default") == "default"
  end

  test :keys do
    assert Keyword.keys(create_keywords) == [:first_key, :second_key]
    assert Keyword.keys(create_empty_keywords) == []
  end

  test :values do
    assert Keyword.values(create_keywords) == [1, 2]
    assert Keyword.values(create_empty_keywords) == []
  end

  test :delete do
    assert Keyword.delete(create_keywords, :second_key) == [first_key: 1]
    assert Keyword.delete(create_keywords, :other_key) == [first_key: 1, second_key: 2]
    assert Keyword.delete(create_empty_keywords, :other_key) == []
  end

  test :put do
    assert Keyword.put(create_empty_keywords, :first_key, 1) == [first_key: 1]
    assert Keyword.put(create_keywords, :first_key, 1) == [first_key: 1, second_key: 2]
  end

  test :merge do
    assert Keyword.merge(create_empty_keywords, create_keywords) == [first_key: 1, second_key: 2]
    assert Keyword.merge(create_keywords, create_empty_keywords) == [first_key: 1, second_key: 2]
    assert Keyword.merge(create_keywords, create_keywords) == [first_key: 1, second_key: 2]
    assert Keyword.merge(create_empty_keywords, create_empty_keywords) == []
  end

  test :merge_with_function do
    result = Keyword.merge [a: 1, b: 2], [a: 3, d: 4], fn _k, v1, v2 ->
      v1 + v2
    end
    assert result == [a:4, b:2, d: 4]
  end

  test :key do
    assert Keyword.key?([a: 1], :a) == true
    assert Keyword.key?([a: 1], :b) == false
  end

  defp create_empty_keywords, do: []
  defp create_keywords, do: [first_key: 1, second_key: 2]
end

defmodule Keyword.DuplicatedTest do
  use ExUnit.Case

  test :duplicated_entries do
    assert create_keywords == [{:first_key,1},{:first_key,2},{:second_key,2}]
  end

  test :get do
    assert Keyword.get(create_keywords, :first_key) == 1
    assert Keyword.get(create_keywords, :second_key) == 2
    assert Keyword.get(create_keywords, :other_key) == nil
    assert Keyword.get(create_empty_keywords, :first_key, "default") == "default"
  end

  test :get_values do
    assert Keyword.get_values(create_keywords, :first_key) == [1,2]
    assert Keyword.get_values(create_keywords, :second_key) == [2]
    assert Keyword.get_values(create_keywords, :other_key) == []
  end

  test :keys do
    assert Keyword.keys(create_keywords) == [:first_key, :first_key, :second_key]
    assert Keyword.keys(create_empty_keywords) == []
  end

  test :values do
    assert Keyword.values(create_keywords) == [1, 2, 2]
    assert Keyword.values(create_empty_keywords) == []
  end

  test :delete do
    assert Keyword.delete(create_keywords, :first_key) == [second_key: 2]
    assert Keyword.delete(create_keywords, :other_key) == create_keywords
    assert Keyword.delete(create_empty_keywords, :other_key) == []
  end

  test :put do
    assert Keyword.put(create_empty_keywords, :first_key, 1) == [first_key: 1]
    assert Keyword.put(create_keywords, :first_key, 1) == [first_key: 1, second_key: 2]
  end

  test :merge do
    assert Keyword.merge(create_empty_keywords, create_keywords) == create_keywords
    assert Keyword.merge(create_keywords, create_empty_keywords) == create_keywords
    assert Keyword.merge(create_keywords, create_keywords) == create_keywords
    assert Keyword.merge(create_empty_keywords, create_empty_keywords) == []
    assert Keyword.merge(create_keywords, [first_key: 0]) == [first_key: 0, first_key: 2, second_key: 2]
    assert Keyword.merge(create_keywords, [first_key: 0, first_key: 3]) == [first_key: 0, first_key: 3, second_key: 2]
  end

  test :merge_with_function do
    result = Keyword.merge [a: 1, b: 2], [a: 3, d: 4], fn _k, v1, v2 ->
      v1 + v2
    end
    assert result == [a:4, b:2, d: 4]
  end

  test :key do
    assert Keyword.key?([a: 1], :a) == true
    assert Keyword.key?([a: 1], :b) == false
  end

  defp create_empty_keywords, do: []
  defp create_keywords, do: [first_key: 1, first_key: 2, second_key: 2]
end
