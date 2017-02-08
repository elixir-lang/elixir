Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.FnTest do
  use ExUnit.Case, async: true
  import CompileAssertion

  test "arithmetic constants on match" do
    assert (fn -1 -> true end).(-1)
    assert (fn +1 -> true end).(1)
  end

  test "pin operator on match" do
    x = 1
    refute (fn ^x -> true; _ -> false end).(0)
    assert (fn ^x -> true; _ -> false end).(1)
    refute (fn ^x -> true; _ -> false end).(1.0)
  end

  test "case function hoisting does not affect anonymous fns" do
    result =
      if atom?(0) do
        user = :defined
        user
      else
        (fn() ->
           user = :undefined
           user
         end).()
      end
    assert result == :undefined
  end

  test "capture with access" do
    assert (&(&1[:hello])).([hello: :world]) == :world
  end

  test "capture remote" do
    assert (&:erlang.atom_to_list/1).(:a) == 'a'
    assert (&Atom.to_charlist/1).(:a) == 'a'

    assert (&List.flatten/1).([[0]]) == [0]
    assert (&(List.flatten/1)).([[0]]) == [0]
    assert (&List.flatten(&1)).([[0]]) == [0]
    assert (&List.flatten(&1)) == (&List.flatten/1)
  end

  test "capture local" do
    assert (&atl/1).(:a) == 'a'
    assert (&(atl/1)).(:a) == 'a'
    assert (&atl(&1)).(:a) == 'a'
  end

  test "capture local with question mark" do
    assert (&atom?/1).(:a)
    assert (&(atom?/1)).(:a)
    assert (&atom?(&1)).(:a)
  end

  test "capture imported" do
    assert (&is_atom/1).(:a)
    assert (&(is_atom/1)).(:a)
    assert (&is_atom(&1)).(:a)
    assert (&is_atom(&1)) == &is_atom/1
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
    assert is_function & &1 + &2, 2
    assert is_function &and/2
  end

  test "capture with variable module" do
    mod = List
    assert (&mod.flatten(&1)).([1, [2], 3]) == [1, 2, 3]
    assert (&mod.flatten/1).([1, [2], 3]) == [1, 2, 3]
    assert (&mod.flatten/1) == &List.flatten/1
  end

  test "local partial application" do
    assert (&atb(&1, :utf8)).(:a) == "a"
    assert (&atb(List.to_atom(&1), :utf8)).('a') == "a"
  end

  test "imported partial application" do
    import Record
    assert (&is_record(&1, :sample)).({:sample, 1})
  end

  test "remote partial application" do
    assert (&:erlang.binary_part(&1, 1, 2)).("foo") == "oo"
    assert (&:erlang.binary_part(Atom.to_string(&1), 1, 2)).(:foo) == "oo"
  end

  test "capture and partially apply tuples" do
    assert (&{&1, &2}).(1, 2) == {1, 2}
    assert (&{&1, &2, &3}).(1, 2, 3) == {1, 2, 3}

    assert (&{1, &1}).(2) == {1, 2}
    assert (&{1, &1, &2}).(2, 3) == {1, 2, 3}
  end

  test "capture and partially apply lists" do
    assert (&[ &1, &2 ]).(1, 2) == [ 1, 2 ]
    assert (&[ &1, &2, &3 ]).(1, 2, 3) == [ 1, 2, 3 ]

    assert (&[ 1, &1 ]).(2) == [ 1, 2 ]
    assert (&[ 1, &1, &2 ]).(2, 3) == [ 1, 2, 3 ]

    assert (&[&1 | &2]).(1, 2) == [1 | 2]
  end

  test "capture and partially apply on call" do
    assert (&(&1.module)).(__ENV__) == __MODULE__
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
    assert_compile_fail CompileError, "nofile:1: capture &255 cannot be defined without &1", "&(&255)"
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

  test "failure on invalid arity" do
    assert_compile_fail CompileError,
      "nofile:1: invalid arity for &, expected a number between 0 and 255, got: 256",
      "&Mod.fun/256"
  end

  test "failure when no captures" do
    assert_compile_fail CompileError,
      "nofile:1: invalid args for &, expected an expression in the format of &Mod.fun/arity, " <>
      "&local/arity or a capture containing at least one argument as &1, got: foo()",
      "&foo()"
  end

  test "failure on nested capture" do
    assert_compile_fail CompileError,
      "nofile:1: nested captures via & are not allowed: &(nil)",
      "&(&())"
  end

  defp atom?(atom) when is_atom(atom), do: true
  defp atom?(_), do: false

  defp atl(arg) do
    :erlang.atom_to_list(arg)
  end

  defp atb(arg, encoding) do
    :erlang.atom_to_binary(arg, encoding)
  end
end
