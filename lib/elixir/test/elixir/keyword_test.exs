Code.require_file "test_helper.exs", __DIR__

defmodule KeywordTest do
  use ExUnit.Case, async: true

  doctest Keyword

  test "has a literal syntax" do
    assert [B: 1] == [{:B, 1}]
    assert [foo?: :bar] == [{:foo?, :bar}]
    assert [||: 2, +: 1] == [{:||, 2}, {:+, 1}]
    assert [1, 2, three: :four] == [1, 2, {:three, :four}]
  end

  test "is a :: operator on ambiguity" do
    assert [{:::, _, [{:a, _, _}, {:b, _, _}]}] = quote(do: [a::b])
  end

  test "supports optional comma" do
    [a: 1,
     b: 2,
     c: 3,]
  end

  test "implements (almost) all functions in Map" do
    assert Map.__info__(:functions) -- Keyword.__info__(:functions) ==
           [from_struct: 1]
  end

  test "get_and_update/3 raises on bad return value from the argument function" do
    assert_raise RuntimeError, "the given function must return a two-element tuple or :pop, got: 1", fn ->
      Keyword.get_and_update([a: 1], :a, fn value -> value end)
    end

    assert_raise RuntimeError, "the given function must return a two-element tuple or :pop, got: nil", fn ->
      Keyword.get_and_update([], :a, fn value -> value end)
    end
  end

  test "get_and_update!/3 raises on bad return value from the argument function" do
    assert_raise RuntimeError, "the given function must return a two-element tuple or :pop, got: 1", fn ->
      Keyword.get_and_update!([a: 1], :a, fn value -> value end)
    end
  end

  test "merge/2" do
    assert Keyword.merge([a: 1, b: 2], [c: 11, d: 12]) == [a: 1, b: 2, c: 11, d: 12]
    assert Keyword.merge([], [c: 11, d: 12]) == [c: 11, d: 12]
    assert Keyword.merge([a: 1, b: 2], []) == [a: 1, b: 2]

    assert_raise ArgumentError, "expected a keyword list as the first argument, got: [1, 2]", fn ->
      Keyword.merge([1, 2], [c: 11, d: 12])
    end

    assert_raise ArgumentError, "expected a keyword list as the first argument, got: [1 | 2]", fn ->
      Keyword.merge([1 | 2], [c: 11, d: 12])
    end

    assert_raise ArgumentError, "expected a keyword list as the second argument, got: [11, 12, 0]", fn ->
      Keyword.merge([a: 1, b: 2], [11, 12, 0])
    end

    assert_raise ArgumentError, "expected a keyword list as the second argument, got: [11 | 12]", fn ->
      Keyword.merge([a: 1, b: 2], [11 | 12])
    end

    # duplicate keys in keywords1 are kept if key is not present in keywords2
    assert Keyword.merge([a: 1, b: 2, a: 3], [c: 11, d: 12]) == [a: 1, b: 2, a: 3, c: 11, d: 12]
    assert Keyword.merge([a: 1, b: 2, a: 3], [a: 11]) == [b: 2, a: 11]

    # duplicate keys in keywords2 are always kept
    assert Keyword.merge([a: 1, b: 2], [c: 11, c: 12, d: 13]) == [a: 1, b: 2, c: 11, c: 12, d: 13]

    # any key in keywords1 is removed if key is present in keyword2
    assert Keyword.merge([a: 1, b: 2, c: 3, c: 4], [c: 11, c: 12, d: 13]) == [a: 1, b: 2, c: 11, c: 12, d: 13]
  end
end
