Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.SeeItMatch do
  use ExUnit.Case

  alias ExUnit.Pattern

  test "single element" do
    assert match?(1, 2)
  end

  test "pinned var" do
    a = 1
    assert match?(^a, 2)
  end

  test "tuple" do
    assert match?({1, 2}, {2, 2})
  end

  test "three element tuple" do
    assert match?({1, 2, 3}, {1, 2, 1})
  end

  test "nested tuple" do
    assert match?({1, {1, 2}, {3, 4}}, {1, {3, 4}, {1, 2}})
  end

  test "nested tuple2" do
    assert match?({:a, {1, 2}, :b}, {:a, {3, 4}, :b})
  end

  test "tuple with vars" do
    assert match?({a, a}, {1, 2})
  end

  test "tuple with 2 vars" do
    assert match?({a, a, b}, {1, 2, 3})
  end

  test "tuple with pinned var" do
    a = 1
    assert match?({^a, 2}, {2, 2})
  end

  test "tuple with pinned var matching" do
    a = 1
    assert match?({^a, 2}, {1, 1})
  end

  test "tuple with nested map and list" do
    assert match?({1, %{a: [1, 2, 3]}, 3}, {1, %{a: [1, 2, 3]}, 4})
  end

  test "two element tuple with nested map and list" do
    assert match?({1, %{a: [1, 1, 3]}}, {2, %{a: [1, 2, 3]}})
  end

  test "mismatched tuple size - right" do
    assert match?({:a, :b, :c}, {:a, :b, :c, :d})
  end

  test "mismatched tuple size - left" do
    assert match?({:a, :b, :c, :d}, {:a, :b, :c})
  end

  test "simple map" do
    assert match?(%{a: 1}, %{a: 2})
  end

  test "simple map with no shared keys" do
    assert match?(%{a: 1}, %{b: 1})
  end

  test "simple two element map, keys match" do
    assert match?(%{a: 1, b: 2}, %{a: 1, b: 1})
  end

  test "simple three element map, keys match" do
    assert match?(%{a: 1, b: 2, c: 1}, %{a: 1, b: 1, c: 1})
  end

  test "nested map, nested matches" do
    assert match?(%{a: 1, b: %{a: 1}}, %{a: 2, b: %{a: 1}})
  end

  test "nested map, nested does not match" do
    assert match?(%{a: 1, b: %{a: 1}}, %{a: 1, b: %{a: 2}})
  end

  test "two-element map, keys don't match base" do
    assert %{a: 1, c: 2} == %{a: 1, b: 1}
  end

  test "simple list" do
    assert match?([1, 2, :a], [1, 2, :b])
  end

  test "nested simple list" do
    assert match?([[1, 2], :a], [[1, 2], :b])
  end

  test "nested simple list 2" do
    assert match?([[1, 1], :a], [[1, 2], :a])
  end

  test "list cons operator" do
    assert match?([1 | _rest], [2, 3, 4])
  end
end
