Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.FnTest do
  use ExUnit.Case, async: true

  test "arithmetic constants on match" do
    assert (fn -1 -> true end).(-1)
    assert (fn +1 -> true end).(1)
  end

  defp fun_match(x) do
    fn
      ^x -> true
      _ -> false
    end
  end

  test "pin operator on match" do
    refute fun_match(1).(0)
    assert fun_match(1).(1)
    refute fun_match(1).(1.0)
  end

  test "guards with no args" do
    fun = fn () when node() == :nonode@nohost -> true end
    assert is_function(fun, 0)
  end

  test "case function hoisting does not affect anonymous fns" do
    result =
      if atom?(0) do
        user = :defined
        user
      else
        (fn ->
           user = :undefined
           user
         end).()
      end

    assert result == :undefined
  end

  test "capture with access" do
    assert (& &1[:hello]).(hello: :world) == :world
  end

  test "capture remote" do
    assert (&:erlang.atom_to_list/1).(:a) == ~c"a"
    assert (&Atom.to_charlist/1).(:a) == ~c"a"

    assert (&List.flatten/1).([[0]]) == [0]
    assert (&List.flatten/1).([[0]]) == [0]
    assert (&List.flatten(&1)).([[0]]) == [0]
    assert (&List.flatten(&1)) == (&List.flatten/1)
  end

  test "capture local" do
    assert (&atl/1).(:a) == ~c"a"
    assert (&atl/1).(:a) == ~c"a"
    assert (&atl(&1)).(:a) == ~c"a"
  end

  test "capture local with question mark" do
    assert (&atom?/1).(:a)
    assert (&atom?/1).(:a)
    assert (&atom?(&1)).(:a)
  end

  test "capture imported" do
    assert (&is_atom/1).(:a)
    assert (&is_atom/1).(:a)
    assert (&is_atom(&1)).(:a)
    assert (&is_atom(&1)) == (&is_atom/1)
  end

  test "capture macro" do
    assert (&to_string/1).(:a) == "a"
    assert (&to_string(&1)).(:a) == "a"
    assert (&Kernel.to_string/1).(:a) == "a"
    assert (&Kernel.to_string(&1)).(:a) == "a"
  end

  test "capture operator" do
    assert is_function(&+/2)
    assert is_function(& &&/2)
    assert is_function(&(&1 + &2), 2)
    assert is_function(&and/2)
  end

  test "capture with variable module" do
    mod = List
    assert (&mod.flatten(&1)).([1, [2], 3]) == [1, 2, 3]
    assert (&mod.flatten/1).([1, [2], 3]) == [1, 2, 3]
    assert (&mod.flatten/1) == (&List.flatten/1)
  end

  test "local partial application" do
    assert (&atb(&1, :utf8)).(:a) == "a"
    assert (&atb(List.to_atom(&1), :utf8)).(~c"a") == "a"
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
    assert (&[&1, &2]).(1, 2) == [1, 2]
    assert (&[&1, &2, &3]).(1, 2, 3) == [1, 2, 3]

    assert (&[1, &1]).(2) == [1, 2]
    assert (&[1, &1, &2]).(2, 3) == [1, 2, 3]

    assert (&[&1 | &2]).(1, 2) == [1 | 2]
  end

  test "capture and partially apply on call" do
    assert (& &1.module).(__ENV__) == __MODULE__
  end

  test "capture block like" do
    assert (&(!is_atom(&1))).(:foo) == false
  end

  test "capture with function call" do
    assert (& &1).(:ok) == :ok

    fun = fn a, b -> a + b end
    assert (&fun.(&1, 2)).(1) == 3
  end

  defmacro c(x) do
    quote do
      &(unquote(x) <> &1)
    end
  end

  test "capture within capture through macro" do
    assert (&c(&1).("b")).("a") == "ab"
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
