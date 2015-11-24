Code.require_file "test_helper.exs", __DIR__

defmodule TupleTest do
  use ExUnit.Case, async: true

  doctest Tuple

  test "elem" do
    assert elem({:a, :b, :c}, 1) == :b
  end

  test "put elem" do
    assert put_elem({:a, :b, :c}, 1, :d) == {:a, :d, :c}
  end

  test "keywords" do
    assert {1, 2, three: :four} == {1, 2, [three: :four]}
  end

  test "optional comma" do
    assert {1} == {1,}
    assert {1, 2, 3} == {1, 2, 3,}
  end

  test "partial application" do
    assert (&{&1, 2}).(1) == {1, 2}
    assert (&{&1, &2}).(1, 2) == {1, 2}
    assert (&{&2, &1}).(2, 1) == {1, 2}
  end

  # Tuple module
  # We check two variants due to inlining.

  test "duplicate" do
    assert Tuple.duplicate(:foo, 0) == {}
    assert Tuple.duplicate(:foo, 3) == {:foo, :foo, :foo}

    mod = Tuple
    assert mod.duplicate(:foo, 0) == {}
    assert mod.duplicate(:foo, 3) == {:foo, :foo, :foo}
  end

  test "insert at" do
    assert Tuple.insert_at({:bar, :baz}, 0, :foo) == {:foo, :bar, :baz}

    mod = Tuple
    assert mod.insert_at({:bar, :baz}, 0, :foo) == {:foo, :bar, :baz}
  end

  test "append" do
    assert Tuple.append({:foo, :bar}, :baz) == {:foo, :bar, :baz}

    mod = Tuple
    assert mod.append({:foo, :bar}, :baz) == {:foo, :bar, :baz}
  end

  test "delete at" do
    assert Tuple.delete_at({:foo, :bar, :baz}, 0) == {:bar, :baz}

    mod = Tuple
    assert mod.delete_at({:foo, :bar, :baz}, 0) == {:bar, :baz}
  end
end
