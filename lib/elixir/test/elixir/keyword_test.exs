Code.require_file("test_helper.exs", __DIR__)

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
    assert [{:::, _, [{:a, _, _}, {:b, _, _}]}] = quote(do: [a :: b])
  end

  test "supports optional comma" do
    assert Code.eval_string("[a: 1, b: 2, c: 3,]") == {[a: 1, b: 2, c: 3], []}
  end

  test "implements (almost) all functions in Map" do
    assert Map.__info__(:functions) -- Keyword.__info__(:functions) == [from_struct: 1]
  end

  test "get_and_update/3 raises on bad return value from the argument function" do
    message = "the given function must return a two-element tuple or :pop, got: 1"

    assert_raise RuntimeError, message, fn ->
      Keyword.get_and_update([a: 1], :a, fn value -> value end)
    end

    message = "the given function must return a two-element tuple or :pop, got: nil"

    assert_raise RuntimeError, message, fn ->
      Keyword.get_and_update([], :a, fn value -> value end)
    end
  end

  test "get_and_update!/3 raises on bad return value from the argument function" do
    message = "the given function must return a two-element tuple or :pop, got: 1"

    assert_raise RuntimeError, message, fn ->
      Keyword.get_and_update!([a: 1], :a, fn value -> value end)
    end
  end

  test "merge/2" do
    assert Keyword.merge([a: 1, b: 2], c: 11, d: 12) == [a: 1, b: 2, c: 11, d: 12]
    assert Keyword.merge([], c: 11, d: 12) == [c: 11, d: 12]
    assert Keyword.merge([a: 1, b: 2], []) == [a: 1, b: 2]

    message = "expected a keyword list as the first argument, got: [1, 2]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([1, 2], c: 11, d: 12)
    end

    message = "expected a keyword list as the first argument, got: [1 | 2]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([1 | 2], c: 11, d: 12)
    end

    message = "expected a keyword list as the second argument, got: [11, 12, 0]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([a: 1, b: 2], [11, 12, 0])
    end

    message = "expected a keyword list as the second argument, got: [11 | 12]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([a: 1, b: 2], [11 | 12])
    end

    # duplicate keys in keywords1 are kept if key is not present in keywords2
    result = [a: 1, b: 2, a: 3, c: 11, d: 12]
    assert Keyword.merge([a: 1, b: 2, a: 3], c: 11, d: 12) == result

    result = [b: 2, a: 11]
    assert Keyword.merge([a: 1, b: 2, a: 3], a: 11) == result

    # duplicate keys in keywords2 are always kept
    result = [a: 1, b: 2, c: 11, c: 12, d: 13]
    assert Keyword.merge([a: 1, b: 2], c: 11, c: 12, d: 13) == result

    # any key in keywords1 is removed if key is present in keyword2
    result = [a: 1, b: 2, c: 11, c: 12, d: 13]
    assert Keyword.merge([a: 1, b: 2, c: 3, c: 4], c: 11, c: 12, d: 13) == result
  end

  test "merge/3" do
    fun = fn _key, value1, value2 -> value1 + value2 end

    assert Keyword.merge([a: 1, b: 2], [c: 11, d: 12], fun) == [a: 1, b: 2, c: 11, d: 12]
    assert Keyword.merge([], [c: 11, d: 12], fun) == [c: 11, d: 12]
    assert Keyword.merge([a: 1, b: 2], [], fun) == [a: 1, b: 2]

    message = "expected a keyword list as the first argument, got: [1, 2]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([1, 2], [c: 11, d: 12], fun)
    end

    message = "expected a keyword list as the first argument, got: [1 | 2]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([1 | 2], [c: 11, d: 12], fun)
    end

    message = "expected a keyword list as the second argument, got: [{:x, 1}, :y, :z]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([a: 1, b: 2], [{:x, 1}, :y, :z], fun)
    end

    message = "expected a keyword list as the second argument, got: [:x | :y]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([a: 1, b: 2], [:x | :y], fun)
    end

    message = "expected a keyword list as the second argument, got: [{:x, 1} | :y]"

    assert_raise ArgumentError, message, fn ->
      Keyword.merge([a: 1, b: 2], [{:x, 1} | :y], fun)
    end

    # duplicate keys in keywords1 are left untouched if key is not present in keywords2
    result = [a: 1, b: 2, a: 3, c: 11, d: 12]
    assert Keyword.merge([a: 1, b: 2, a: 3], [c: 11, d: 12], fun) == result

    result = [b: 2, a: 12]
    assert Keyword.merge([a: 1, b: 2, a: 3], [a: 11], fun) == result

    # duplicate keys in keywords2 are always kept
    result = [a: 1, b: 2, c: 11, c: 12, d: 13]
    assert Keyword.merge([a: 1, b: 2], [c: 11, c: 12, d: 13], fun) == result

    # every key in keywords1 is replaced with fun result if key is present in keyword2
    result = [a: 1, b: 2, c: 14, c: 54, d: 13]
    assert Keyword.merge([a: 1, b: 2, c: 3, c: 4], [c: 11, c: 50, d: 13], fun) == result
  end

  test "merge/2 and merge/3 behave exactly the same way" do
    fun = fn _key, _value1, value2 -> value2 end

    args = [
      {[a: 1, b: 2], [c: 11, d: 12]},
      {[], [c: 11, d: 12]},
      {[a: 1, b: 2], []},
      {[a: 1, b: 2, a: 3], [c: 11, d: 12]},
      {[a: 1, b: 2, a: 3], [a: 11]},
      {[a: 1, b: 2], [c: 11, c: 12, d: 13]},
      {[a: 1, b: 2, c: 3, c: 4], [c: 11, c: 12, d: 13]}
    ]

    args_error = [
      {[1, 2], [c: 11, d: 12]},
      {[1 | 2], [c: 11, d: 12]},
      {[a: 1, b: 2], [11, 12, 0]},
      {[a: 1, b: 2], [11 | 12]},
      {[a: 1, b: 2], [{:x, 1}, :y, :z]},
      {[a: 1, b: 2], [:x | :y]},
      {[a: 1, b: 2], [{:x, 1} | :y]}
    ]

    for {arg1, arg2} <- args do
      assert Keyword.merge(arg1, arg2) == Keyword.merge(arg1, arg2, fun)
    end

    for {arg1, arg2} <- args_error do
      error = assert_raise ArgumentError, fn -> Keyword.merge(arg1, arg2) end
      assert_raise ArgumentError, error.message, fn -> Keyword.merge(arg1, arg2, fun) end
    end
  end

  test "take!/2 behaves as take/2 when keys exist" do
    args = [
      {[], []},
      {[a: 1, b: 2], [:a]},
      {[a: 1, b: 2], [:b]},
      {[a: 1, b: 2], [:a, :b]}
    ]

    for {arg1, arg2} <- args do
      assert Keyword.take!(arg1, arg2) == Keyword.take(arg1, arg2)
    end
  end

  test "take!/2 throws KeyError when key is missing" do
    assert_raise KeyError, "key :b not found in: [a: 1]", fn -> Keyword.take!([a: 1], [:b]) end
  end
end
