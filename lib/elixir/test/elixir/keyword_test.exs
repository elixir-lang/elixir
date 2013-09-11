Code.require_file "test_helper.exs", __DIR__

defmodule KeywordTest do
  use ExUnit.Case, async: true

  test :literal do
    assert [B: 1] == [{ :B, 1 }]
    assert [foo?: :bar] == [{:foo?, :bar}]
    assert [||: 2, +: 1] == [{:||, 2}, {:+, 1}]
  end

  test :ambiguity do
    # This raises a warning, so let's leave it commented
    # assert quote(do: [a:b])  == [a: { :b, 0, :quoted }]
    assert [{ :::, _, [{ :a, _, _ }, { :b, _, _ }] }] = quote(do: [a::b])
  end

  test :optional_comma do
    [a: 1,
     b: 2,
     c: 3, ]
  end

  test :from_enum do
    list = [{:b, 2}, {:a, 1}, {:c, 3}]
    dict = HashDict.new list
    assert Keyword.from_enum(list) == [b: 2, a: 1, c: 3]
    assert Keyword.equal?(Keyword.from_enum(dict), [a: 1, b: 2, c: 3])
  end

  test :keyword? do
    assert Keyword.keyword?([])
    assert Keyword.keyword?([a: 1])
    assert Keyword.keyword?([{Foo, 1}])
    refute Keyword.keyword?([{}])
    refute Keyword.keyword?(<<>>)
  end

  test :new do
    assert Keyword.new == []
  end

  test :new_with_pairs do
    assert Keyword.new([{:second_key, 2}, {:first_key, 1}]) == [first_key: 1, second_key: 2]
  end

  test :new_with_function do
    assert Keyword.new([:a, :b], fn x -> { x, x } end) == [b: :b, a: :a]
  end

  test :get do
    assert Keyword.get(create_keywords, :first_key) == 1
    assert Keyword.get(create_keywords, :second_key) == 2
    assert Keyword.get(create_keywords, :other_key) == nil
    assert Keyword.get(create_empty_keywords, :first_key, "default") == "default"
  end

  test :fetch! do
    assert Keyword.fetch!(create_keywords, :first_key) == 1

    error = assert_raise KeyError, fn ->
      Keyword.fetch!(create_keywords, :unknown)
    end

    assert error.key == :unknown
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
    assert Keyword.put(create_keywords, :first_key, 3) == [first_key: 3, second_key: 2]
  end

  test :put_new do
    assert Keyword.put_new(create_empty_keywords, :first_key, 1) == [first_key: 1]
    assert Keyword.put_new(create_keywords, :first_key, 3) == [first_key: 1, second_key: 2]
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
    assert result == [a: 4, b: 2, d: 4]
  end

  test :key? do
    assert Keyword.has_key?([a: 1], :a) == true
    assert Keyword.has_key?([a: 1], :b) == false
  end

  test :update do
    assert Keyword.update!([a: 1], :a, &(&1 * 2)) == [a: 2]
    assert_raise KeyError, fn ->
      assert Keyword.update!([a: 1], :b, &(&1 * 2))
    end
  end

  test :update_with_initial do
    assert Keyword.update([a: 1], :a, 13, &(&1 * 2)) == [a: 2]
    assert Keyword.update([a: 1], :b, 11, &(&1 * 2)) == [a: 1, b: 11]
  end

  defp create_empty_keywords, do: []
  defp create_keywords, do: [first_key: 1, second_key: 2]
end

defmodule Keyword.DuplicatedTest do
  use ExUnit.Case, async: true

  test :duplicated_entries do
    assert create_keywords == [{:first_key, 1}, {:first_key, 2}, {:second_key, 2}]
  end

  test :get do
    assert Keyword.get(create_keywords, :first_key) == 1
    assert Keyword.get(create_keywords, :second_key) == 2
    assert Keyword.get(create_keywords, :other_key) == nil
    assert Keyword.get(create_empty_keywords, :first_key, "default") == "default"
  end

  test :get_values do
    assert Keyword.get_values(create_keywords, :first_key) == [1, 2]
    assert Keyword.get_values(create_keywords, :second_key) == [2]
    assert Keyword.get_values(create_keywords, :other_key) == []
  end

  test :keys do
    assert Keyword.keys(create_keywords) == [:first_key, :first_key, :second_key]
    assert Keyword.keys(create_empty_keywords) == []
  end

  test :equal? do
    assert Keyword.equal? [a: 1, b: 2], [b: 2, a: 1]
    refute Keyword.equal? [a: 1, b: 2], [b: 2, c: 3]
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

  test :delete_first do
    assert Keyword.delete_first(create_keywords, :first_key) == [first_key: 2, second_key: 2]
    assert Keyword.delete_first(create_keywords, :other_key) == [first_key: 1, first_key: 2, second_key: 2]
    assert Keyword.delete_first(create_empty_keywords, :other_key) == []
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
    assert Keyword.merge(create_keywords, [first_key: 0]) == [first_key: 0, second_key: 2]
    assert Keyword.merge(create_keywords, [first_key: 0, first_key: 3]) == [first_key: 0, first_key: 3, second_key: 2]
  end

  test :merge_with_function do
    result = Keyword.merge [a: 1, b: 2], [a: 3, d: 4], fn _k, v1, v2 ->
      v1 + v2
    end
    assert Keyword.equal?(result, [a: 4, b: 2, d: 4])
  end

  test :key do
    assert Keyword.has_key?([a: 1], :a) == true
    assert Keyword.has_key?([a: 1], :b) == false
  end

  defp create_empty_keywords, do: []
  defp create_keywords, do: [first_key: 1, first_key: 2, second_key: 2]
end
