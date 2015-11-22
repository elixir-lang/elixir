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
end
