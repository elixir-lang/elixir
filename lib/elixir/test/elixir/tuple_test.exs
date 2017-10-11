Code.require_file("test_helper.exs", __DIR__)

defmodule TupleTest do
  use ExUnit.Case, async: true

  doctest Tuple

  # Tuple-related functions in the Kernel module.

  test "Kernel.elem/2" do
    assert elem({:a, :b, :c}, 1) == :b
  end

  test "Kernel.put_elem/3" do
    assert put_elem({:a, :b, :c}, 1, :d) == {:a, :d, :c}
  end

  test "keyword syntax is supported in tuple literals" do
    assert {1, 2, three: :four} == {1, 2, [three: :four]}
  end

  test "optional comma is supported in tuple literals" do
    assert Code.eval_string("{1,}") == {{1}, []}
    assert Code.eval_string("{1, 2, 3,}") == {{1, 2, 3}, []}
  end

  test "partial application" do
    assert (&{&1, 2}).(1) == {1, 2}
    assert (&{&1, &2}).(1, 2) == {1, 2}
    assert (&{&2, &1}).(2, 1) == {1, 2}
  end

  # Tuple module
  # We check two variants of each function due to inlining.

  test "duplicate/2" do
    assert Tuple.duplicate(:foo, 0) == {}
    assert Tuple.duplicate(:foo, 3) == {:foo, :foo, :foo}

    mod = Tuple
    assert mod.duplicate(:foo, 0) == {}
    assert mod.duplicate(:foo, 3) == {:foo, :foo, :foo}
  end

  test "insert_at/3" do
    assert Tuple.insert_at({:bar, :baz}, 0, :foo) == {:foo, :bar, :baz}

    mod = Tuple
    assert mod.insert_at({:bar, :baz}, 0, :foo) == {:foo, :bar, :baz}
  end

  test "append/2" do
    assert Tuple.append({:foo, :bar}, :baz) == {:foo, :bar, :baz}

    mod = Tuple
    assert mod.append({:foo, :bar}, :baz) == {:foo, :bar, :baz}
  end

  test "delete_at/2" do
    assert Tuple.delete_at({:foo, :bar, :baz}, 0) == {:bar, :baz}

    mod = Tuple
    assert mod.delete_at({:foo, :bar, :baz}, 0) == {:bar, :baz}
  end
end
