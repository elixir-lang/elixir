Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.FnTest do
  use ExUnit.Case, async: true
  import CompileAssertion

  test "arithmetic constants on match" do
    assert (fn 1 + 2 -> :ok end).(3)  == :ok
    assert (fn 1 - 2 -> :ok end).(-1) == :ok
    assert (fn -1 -> :ok end).(-1) == :ok
    assert (fn +1 -> :ok end).(1)  == :ok
  end

  test "capture with access" do
    assert (&(&1[:hello])).([hello: :world]) == :world
  end

  test "capture remote" do
    assert (&:erlang.atom_to_list/1).(:a) == 'a'
    assert (&Kernel.atom_to_list/1).(:a) == 'a'

    assert (&List.flatten/1).([[0]]) == [0]
    assert (&(List.flatten/1)).([[0]]) == [0]
    assert (&List.flatten(&1)).([[0]]) == [0]
    assert &List.flatten(&1) == &List.flatten/1
  end

  test "capture local" do
    assert (&atl/1).(:a) == 'a'
    assert (&(atl/1)).(:a) == 'a'
    assert (&atl(&1)).(:a) == 'a'
  end

  test "capture local with question mark" do
    assert (&is_a?/2).(:atom, :a)
    assert (&(is_a?/2)).(:atom, :a)
    assert (&is_a?(&1, &2)).(:atom, :a)
  end

  test "capture imported" do
    assert (&atom_to_list/1).(:a) == 'a'
    assert (&(atom_to_list/1)).(:a) == 'a'
    assert (&atom_to_list(&1)).(:a) == 'a'
    assert &atom_to_list(&1) == &atom_to_list/1
  end

  test "capture macro" do
    assert (&to_string/1).(:a) == "a"
    assert (&to_string(&1)).(:a) == "a"
    assert (&Kernel.to_string/1).(:a) == "a"
    assert (&Kernel.to_string(&1)).(:a) == "a"
  end

  test "capture operator" do
    assert is_function &+/2
    assert is_function &(&&/2)
  end

  test "local partial application" do
    assert (&atb(&1, :utf8)).(:a) == "a"
    assert (&atb(list_to_atom(&1), :utf8)).('a') == "a"
  end

  test "imported partial application" do
    assert (&is_record(&1, Range)).(1..3)
  end

  test "remote partial application" do
    assert (&:erlang.binary_part(&1, 1, 2)).("foo") == "oo"
    assert (&:erlang.binary_part(atom_to_binary(&1), 1, 2)).(:foo) == "oo"
  end

  test "capture and partially apply tuples" do
    assert (&{ &1, &2 }).(1, 2) == { 1, 2 }
    assert (&{ &1, &2, &3 }).(1, 2, 3) == { 1, 2, 3 }

    assert (&{ 1, &1 }).(2) == { 1, 2 }
    assert (&{ 1, &1, &2 }).(2, 3) == { 1, 2, 3 }
  end

  test "capture and partially apply lists" do
    assert (&[ &1, &2 ]).(1, 2) == [ 1, 2 ]
    assert (&[ &1, &2, &3 ]).(1, 2, 3) == [ 1, 2, 3 ]

    assert (&[ 1, &1 ]).(2) == [ 1, 2 ]
    assert (&[ 1, &1, &2 ]).(2, 3) == [ 1, 2, 3 ]

    assert (&[&1|&2]).(1, 2) == [1|2]
  end

  test "capture and partially apply on call" do
    assert (&(&1.module)).(__ENV__) == __MODULE__
    assert (&(&1.module(&2))).(__ENV__, Hello).module == Hello
  end

  test "capture block like" do
    assert (&(!is_atom(&1))).(:foo) == false
  end

  test "capture other" do
    assert (& &1).(:ok) == :ok

    fun = fn a, b -> a + b end
    assert (&fun.(&1, 2)).(1) == 3
  end

  test "failure on non-continuous" do
    assert_compile_fail CompileError, "nofile:1: capture &2 cannot be defined without &1", "&(&2)"
  end

  test "failure on integers" do
    assert_compile_fail CompileError, "nofile:1: unhandled &1 outside of a capture", "&1"
    assert_compile_fail CompileError, "nofile:1: capture &0 is not allowed", "&foo(&0)"
  end

  test "failure on block" do
    assert_compile_fail CompileError,
      "nofile:1: invalid args for &, block expressions " <>
      "are not allowed, got: (\n  1\n  2\n)",
      "&(1;2)"
  end

  test "failure on other types" do
    assert_compile_fail CompileError,
      "nofile:1: invalid args for &, expected an expression in the format of &Mod.fun/arity, " <>
      "&local/arity or a capture containing at least one argument as &1, got: :foo",
      "&:foo"
  end

  test "failure when no captures" do
    assert_compile_fail CompileError,
      "nofile:1: invalid args for &, expected an expression in the format of &Mod.fun/arity, " <>
      "&local/arity or a capture containing at least one argument as &1, got: foo()",
      "&foo()"
  end

  defp is_a?(:atom, atom) when is_atom(atom), do: true
  defp is_a?(_, _), do: false

  defp atl(arg) do
    :erlang.atom_to_list arg
  end

  defp atb(arg, encoding) do
    :erlang.atom_to_binary(arg, encoding)
  end
end
