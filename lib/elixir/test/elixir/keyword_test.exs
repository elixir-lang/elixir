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
  end

  test "get_and_update/3 raises on bad return value from the argument function (given empty keyword list)" do
    assert_raise RuntimeError, "the given function must return a two-element tuple or :pop, got: nil", fn ->
      Keyword.get_and_update([], :a, fn value -> value end)
    end
  end

  test "get_and_update!/3  raises on bad return value from the argument function" do
    assert_raise RuntimeError, "the given function must return a two-element tuple or :pop, got: 1", fn ->
      Keyword.get_and_update([a: 1], :a, fn value -> value end)
    end
  end
end
