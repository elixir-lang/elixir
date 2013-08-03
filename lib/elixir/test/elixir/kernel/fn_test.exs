Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.FnTest do
  use ExUnit.Case, async: true
  import CompileAssertion

  test "clause with ^" do
    x = 1
    assert (fn ^x -> :ok; _ -> :error end).(1) == :ok
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

  test "capture imported" do
    assert (&atom_to_list/1).(:a) == 'a'
    assert (&(atom_to_list/1)).(:a) == 'a'
    assert (&atom_to_list(&1)).(:a) == 'a'
    assert &atom_to_list(&1) == &atom_to_list/1
  end

  test "capture macro" do
    assert (&to_binary/1).(:a) == "a"
    assert (&to_binary(&1)).(:a) == "a"
    assert (&Kernel.to_binary/1).(:a) == "a"
    assert (&Kernel.to_binary(&1)).(:a) == "a"
  end

  test "local partial application" do
    assert (&atb(&1, :utf8)).(:a) == "a"
    assert (&atb(list_to_atom(&1), :utf8)).('a') == "a"
  end

  test "imported partial application" do
    assert (&atom_to_binary(&1, :utf8)).(:a) == "a"
    assert (&atom_to_binary(list_to_atom(&1), :utf8)).('a') == "a"
  end

  test "remote partial application" do
    assert (&:erlang.atom_to_binary(&1, :utf8)).(:a) == "a"
    assert (&:erlang.atom_to_binary(list_to_atom(&1), :utf8)).('a') == "a"
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
    assert (&(&1.file)).(__ENV__) == __FILE__
    assert (&(&1.file(&2))).(__ENV__, "Hello").file == "Hello"
  end

  test "failure on non-continuous" do
    assert_compile_fail CompileError, "nofile:1: capture &2 cannot be defined without &1", "&(&2)"
  end

  test "failure on integers" do
    assert_compile_fail CompileError, "nofile:1: unhandled &1 outside of a capture", "&1"
  end

  test "failure on block" do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid args for &, block expressions " <>
      "are not allowed, got: (\n  1\n  2\n)",
      "&(1;2)"
  end

  test "failure on other types" do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid args for &, expected an expression in the format of &Mod.fun/arity, " <>
      "&local/arity or a capture containing at least one argument as &1, got: :foo",
      "&:foo"
  end

  test "failure when no captures" do
    assert_compile_fail SyntaxError,
      "nofile:1: invalid args for &, expected an expression in the format of &Mod.fun/arity, " <>
      "&local/arity or a capture containing at least one argument as &1, got: foo()",
      "&foo()"
  end

  defp atl(arg) do
    :erlang.atom_to_list arg
  end

  defp atb(arg, encoding) do
    :erlang.atom_to_binary(arg, encoding)
  end
end
