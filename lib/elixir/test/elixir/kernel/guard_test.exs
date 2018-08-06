Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.GuardTest do
  use ExUnit.Case, async: true

  describe "defguard(p) usage" do
    defmodule GuardsInMacros do
      defguard is_foo(atom) when atom == :foo

      defmacro is_compile_time_foo(atom) when is_foo(atom) do
        quote do: unquote(__MODULE__).is_foo(unquote(atom))
      end
    end

    test "guards can be used in other macros in the same module" do
      require GuardsInMacros
      assert GuardsInMacros.is_foo(:foo)
      refute GuardsInMacros.is_foo(:baz)
      assert GuardsInMacros.is_compile_time_foo(:foo)
    end

    defmodule GuardsInFuns do
      defguard is_foo(atom) when atom == :foo
      defguard is_equal(foo, bar) when foo == bar

      def is_foobar(atom) when is_foo(atom) do
        is_foo(atom)
      end
    end

    test "guards can be used in other funs in the same module" do
      require GuardsInFuns
      assert GuardsInFuns.is_foo(:foo)
      refute GuardsInFuns.is_foo(:bar)
    end

    test "guards do not change code evaluation semantics" do
      require GuardsInFuns
      x = 1
      assert GuardsInFuns.is_equal(x = 2, x) == false
      assert x == 2
    end

    defmodule MacrosInGuards do
      defmacro is_foo(atom) do
        quote do
          unquote(atom) == :foo
        end
      end

      defguard is_foobar(atom) when is_foo(atom) or atom == :bar
    end

    test "macros can be used in other guards in the same module" do
      require MacrosInGuards
      assert MacrosInGuards.is_foobar(:foo)
      assert MacrosInGuards.is_foobar(:bar)
      refute MacrosInGuards.is_foobar(:baz)
    end

    defmodule GuardsInGuards do
      defguard is_foo(atom) when atom == :foo
      defguard is_foobar(atom) when is_foo(atom) or atom == :bar
    end

    test "guards can be used in other guards in the same module" do
      require GuardsInGuards
      assert GuardsInGuards.is_foobar(:foo)
      assert GuardsInGuards.is_foobar(:bar)
      refute GuardsInGuards.is_foobar(:baz)
    end

    defmodule DefaultArgs do
      defguard is_divisible(value, remainder \\ 2)
               when is_integer(value) and rem(value, remainder) == 0
    end

    test "permits default values in args" do
      require DefaultArgs
      assert DefaultArgs.is_divisible(2)
      refute DefaultArgs.is_divisible(1)
      assert DefaultArgs.is_divisible(3, 3)
      refute DefaultArgs.is_divisible(3, 4)
    end

    test "doesn't allow matching in args" do
      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule Integer.Args do
          defguard foo(value, 1) when is_integer(value)
        end
      end

      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule String.Args do
          defguard foo(value, "string") when is_integer(value)
        end
      end

      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule Atom.Args do
          defguard foo(value, :atom) when is_integer(value)
        end
      end

      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule Tuple.Args do
          defguard foo(value, {foo, bar}) when is_integer(value)
        end
      end
    end

    defmodule GuardFromMacro do
      defmacro __using__(_) do
        quote do
          defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        end
      end
    end

    test "defguard defines a guard from inside another macro" do
      defmodule UseGuardFromMacro do
        use GuardFromMacro

        def assert! do
          assert is_even(0)
          refute is_even(1)
        end
      end

      UseGuardFromMacro.assert!()
    end

    defmodule IntegerPrivateGuards do
      defguardp is_even(value) when is_integer(value) and rem(value, 2) == 0

      def is_even_and_large?(value) when is_even(value) and value > 100, do: true
      def is_even_and_large?(_), do: false

      def is_even_and_small?(value) do
        if is_even(value) and value <= 100, do: true, else: false
      end
    end

    test "defguardp defines private guards that work inside and outside guard clauses" do
      assert IntegerPrivateGuards.is_even_and_large?(102)
      refute IntegerPrivateGuards.is_even_and_large?(98)
      refute IntegerPrivateGuards.is_even_and_large?(99)
      refute IntegerPrivateGuards.is_even_and_large?(103)

      assert IntegerPrivateGuards.is_even_and_small?(98)
      refute IntegerPrivateGuards.is_even_and_small?(99)
      refute IntegerPrivateGuards.is_even_and_small?(102)
      refute IntegerPrivateGuards.is_even_and_small?(103)

      assert_raise CompileError, ~r"cannot find or invoke local is_even/1", fn ->
        defmodule IntegerPrivateGuardUtils do
          import IntegerPrivateGuards

          def is_even_and_large?(value) when is_even(value) and value > 100, do: true
          def is_even_and_large?(_), do: false
        end
      end

      assert_raise CompileError, ~r"undefined function is_even/1", fn ->
        defmodule IntegerPrivateFunctionUtils do
          import IntegerPrivateGuards

          def is_even_and_small?(value) do
            if is_even(value) and value <= 100, do: true, else: false
          end
        end
      end
    end

    test "requires a proper macro name" do
      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule(LiteralUsage, do: defguard("literal is bad"))
      end

      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule(RemoteUsage, do: defguard(Remote.call(is_bad)))
      end
    end

    test "handles overriding appropriately" do
      assert_raise CompileError, ~r"defmacro (.*?) already defined as def", fn ->
        defmodule OverriddenFunUsage do
          def foo(bar), do: bar
          defguard foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacro (.*?) already defined as defp", fn ->
        defmodule OverriddenPrivateFunUsage do
          defp foo(bar), do: bar
          defguard foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacro (.*?) already defined as defmacrop", fn ->
        defmodule OverriddenPrivateFunUsage do
          defmacrop foo(bar), do: bar
          defguard foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacrop (.*?) already defined as def", fn ->
        defmodule OverriddenFunUsage do
          def foo(bar), do: bar
          defguardp foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacrop (.*?) already defined as defp", fn ->
        defmodule OverriddenPrivateFunUsage do
          defp foo(bar), do: bar
          defguardp foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacrop (.*?) already defined as defmacro", fn ->
        defmodule OverriddenPrivateFunUsage do
          defmacro foo(bar), do: bar
          defguardp foo(bar) when bar
        end
      end
    end

    test "does not allow multiple guard clauses" do
      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule MultiGuardUsage do
          defguardp foo(bar, baz) when bar == 1 when baz == 2
        end
      end
    end

    test "does not accept a block" do
      assert_raise CompileError, ~r"undefined function defguard/2", fn ->
        defmodule OnelinerBlockUsage do
          defguard(foo(bar), do: one_liner)
        end
      end

      assert_raise CompileError, ~r"undefined function defguard/2", fn ->
        defmodule MultilineBlockUsage do
          defguard foo(bar) do
            multi
            liner
          end
        end
      end

      assert_raise CompileError, ~r"undefined function defguard/2", fn ->
        defmodule ImplAndBlockUsage do
          defguard(foo(bar) when both_given, do: error)
        end
      end
    end
  end

  describe "defguard(p) compilation" do
    test "refuses to compile nonsensical code" do
      assert_raise CompileError, ~r"cannot find or invoke local undefined/1", fn ->
        defmodule UndefinedUsage do
          defguard foo(function) when undefined(function)
        end
      end
    end

    test "fails on expressions not allowed in guards" do
      # Slightly unique errors

      assert_raise ArgumentError, ~r{invalid args for operator "in"}, fn ->
        defmodule RuntimeListUsage do
          defguard foo(bar, baz) when bar in baz
        end
      end

      assert_raise CompileError, ~r"cannot invoke remote function", fn ->
        defmodule BadErlangFunctionUsage do
          defguard foo(bar) when :erlang.binary_to_atom("foo")
        end
      end

      assert_raise CompileError, ~r"cannot invoke remote function", fn ->
        defmodule SendUsage do
          defguard foo(bar) when send(self(), :baz)
        end
      end

      # Consistent errors

      assert_raise ArgumentError, ~r"invalid expression in guard, ! is not allowed", fn ->
        defmodule SoftNegationLogicUsage do
          defguard foo(logic) when !logic
        end
      end

      assert_raise ArgumentError, ~r"invalid expression in guard, && is not allowed", fn ->
        defmodule SoftAndLogicUsage do
          defguard foo(soft, logic) when soft && logic
        end
      end

      assert_raise ArgumentError, ~r"invalid expression in guard, || is not allowed", fn ->
        defmodule SoftOrLogicUsage do
          defguard foo(soft, logic) when soft || logic
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule LocalCallUsage do
          defguard foo(local, call) when local.(call)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule ComprehensionUsage do
          defguard foo(bar) when for(x <- [1, 2, 3], do: x * bar)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule AliasUsage do
          defguard foo(bar) when alias(bar)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule ImportUsage do
          defguard foo(bar) when import(bar)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule RequireUsage do
          defguard foo(bar) when require(bar)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule SuperUsage do
          defguard foo(bar) when super(bar)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule SpawnUsage do
          defguard foo(bar) when spawn(& &1)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule ReceiveUsage do
          defguard foo(bar) when receive(do: (baz -> baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule CaseUsage do
          defguard foo(bar) when case(bar, do: (baz -> :baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule CondUsage do
          defguard foo(bar) when cond(do: (bar -> :baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule TryUsage do
          defguard foo(bar) when try(do: (baz -> baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule WithUsage do
          defguard foo(bar) when with(do: (baz -> baz))
        end
      end
    end
  end

  describe "defguard(p) expansion" do
    defguard with_unused_vars(foo, bar, _baz) when foo + bar

    test "doesn't obscure unused variables" do
      args = quote(do: [1 + 1, 2 + 2, 3 + 3])

      assert expand_defguard_to_string(:with_unused_vars, args, :guard) == """
             :erlang.+(1 + 1, 2 + 2)
             """

      assert expand_defguard_to_string(:with_unused_vars, args, nil) == """
             (
               {arg0, arg1} = {1 + 1, 2 + 2}
               :erlang.+(arg0, arg1)
             )
             """
    end

    defguard with_reused_vars(foo, bar, baz) when foo + foo + bar + baz

    test "handles re-used variables" do
      args = quote(do: [1 + 1, 2 + 2, 3 + 3])

      assert expand_defguard_to_string(:with_reused_vars, args, :guard) == """
             :erlang.+(:erlang.+(:erlang.+(1 + 1, 1 + 1), 2 + 2), 3 + 3)
             """

      assert expand_defguard_to_string(:with_reused_vars, args, nil) == """
             (
               {arg0, arg1, arg2} = {1 + 1, 2 + 2, 3 + 3}
               :erlang.+(:erlang.+(:erlang.+(arg0, arg0), arg1), arg2)
             )
             """
    end

    defp expand_defguard_to_string(fun, args, context) do
      {{:., [], [__MODULE__, fun]}, [], args}
      |> Macro.expand(%{__ENV__ | context: context})
      |> Macro.to_string()
      |> Kernel.<>("\n")
    end
  end
end
