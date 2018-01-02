Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.GuardTest do
  use ExUnit.Case, async: true

  describe "Kernel.defguard(p) usage" do
    defmodule Guards.In.Macros do
      defguard is_foo(atom) when atom == :foo

      defmacro is_compile_time_foo(atom) when is_foo(atom) do
        quote do: unquote(__MODULE__).is_foo(unquote(atom))
      end
    end

    test "guards can be used in other macros in the same module" do
      require Guards.In.Macros
      assert Guards.In.Macros.is_foo(:foo)
      refute Guards.In.Macros.is_foo(:baz)
      assert Guards.In.Macros.is_compile_time_foo(:foo)
    end

    defmodule Guards.In.Funs do
      defguard is_foo(atom) when atom == :foo

      def is_foobar(atom) when is_foo(atom) do
        is_foo(atom)
      end
    end

    test "guards can be used in other funs in the same module" do
      require Guards.In.Funs
      assert Guards.In.Funs.is_foo(:foo)
      refute Guards.In.Funs.is_foo(:bar)
    end

    defmodule Macros.In.Guards do
      defmacro is_foo(atom) do
        quote do
          unquote(atom) == :foo
        end
      end

      defguard is_foobar(atom) when is_foo(atom) or atom == :bar
    end

    test "macros can be used in other guards in the same module" do
      require Macros.In.Guards
      assert Macros.In.Guards.is_foobar(:foo)
      assert Macros.In.Guards.is_foobar(:bar)
      refute Macros.In.Guards.is_foobar(:baz)
    end

    defmodule Guards.In.Guards do
      defguard is_foo(atom) when atom == :foo
      defguard is_foobar(atom) when is_foo(atom) or atom == :bar
    end

    test "guards can be used in other guards in the same module" do
      require Guards.In.Guards
      assert Guards.In.Guards.is_foobar(:foo)
      assert Guards.In.Guards.is_foobar(:bar)
      refute Guards.In.Guards.is_foobar(:baz)
    end

    defmodule Default.Args do
      defguard is_divisible(value, remainder \\ 2)
               when is_integer(value) and rem(value, remainder) == 0
    end

    test "permits default values in args" do
      require Default.Args
      assert Default.Args.is_divisible(2)
      refute Default.Args.is_divisible(1)
      assert Default.Args.is_divisible(3, 3)
      refute Default.Args.is_divisible(3, 4)
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

    defmodule Integer.Private.Guards do
      defguardp is_even(value) when is_integer(value) and rem(value, 2) == 0

      def is_even_and_large?(value) when is_even(value) and value > 100, do: true
      def is_even_and_large?(_), do: false

      def is_even_and_small?(value) do
        if is_even(value) and value <= 100, do: true, else: false
      end
    end

    test "defguardp defines private guards that work inside and outside guard clauses" do
      assert Integer.Private.Guards.is_even_and_large?(102)
      refute Integer.Private.Guards.is_even_and_large?(98)
      refute Integer.Private.Guards.is_even_and_large?(99)
      refute Integer.Private.Guards.is_even_and_large?(103)

      assert Integer.Private.Guards.is_even_and_small?(98)
      refute Integer.Private.Guards.is_even_and_small?(99)
      refute Integer.Private.Guards.is_even_and_small?(102)
      refute Integer.Private.Guards.is_even_and_small?(103)

      assert_raise CompileError, ~r"cannot invoke local is_even/1 inside guard", fn ->
        defmodule Integer.Private.Guard.Utils do
          import Integer.Private.Guards

          def is_even_and_large?(value) when is_even(value) and value > 100, do: true
          def is_even_and_large?(_), do: false
        end
      end

      assert_raise CompileError, ~r"undefined function is_even/1", fn ->
        defmodule Integer.Private.Function.Utils do
          import Integer.Private.Guards

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
        defmodule OverridenFunUsage do
          def foo(bar), do: bar
          defguard foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacro (.*?) already defined as defp", fn ->
        defmodule OverridenPrivateFunUsage do
          defp foo(bar), do: bar
          defguard foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacro (.*?) already defined as defmacrop", fn ->
        defmodule OverridenPrivateFunUsage do
          defmacrop foo(bar), do: bar
          defguard foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacrop (.*?) already defined as def", fn ->
        defmodule OverridenFunUsage do
          def foo(bar), do: bar
          defguardp foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacrop (.*?) already defined as defp", fn ->
        defmodule OverridenPrivateFunUsage do
          defp foo(bar), do: bar
          defguardp foo(bar) when bar
        end
      end

      assert_raise CompileError, ~r"defmacrop (.*?) already defined as defmacro", fn ->
        defmodule OverridenPrivateFunUsage do
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

  describe "Kernel.defguard compilation" do
    test "refuses to compile non-sensical code" do
      assert_raise CompileError, ~r"cannot invoke local undefined/1 inside guard", fn ->
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

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule SoftNegationLogicUsage do
          defguard foo(logic) when !logic
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule SoftAndLogicUsage do
          defguard foo(soft, logic) when soft && logic
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
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

  describe "Kernel.Utils.defguard/2" do
    test "generates unquoted variables based on context" do
      args = quote(do: [foo, bar, baz])
      expr = quote(do: foo + bar + baz)

      {:ok, goal} =
        Code.string_to_quoted("""
        case Macro.Env.in_guard? __CALLER__ do
          true -> quote do
            :erlang.+(:erlang.+(unquote(foo), unquote(bar)), unquote(baz))
          end
          false -> quote do
            foo = unquote(foo)
            bar = unquote(bar)
            baz = unquote(baz)
            :erlang.+(:erlang.+(foo, bar), baz)
          end
        end
        """)

      assert expand_defguard_to_string(args, expr) == Macro.to_string(goal)
    end

    test "doesn't obscure unused variables" do
      args = quote(do: [foo, bar, baz])
      expr = quote(do: foo + bar)

      {:ok, goal} =
        Code.string_to_quoted("""
        case Macro.Env.in_guard? __CALLER__ do
          true -> quote do
            :erlang.+(unquote(foo), unquote(bar))
          end
          false -> quote do
            foo = unquote(foo)
            bar = unquote(bar)
            :erlang.+(foo, bar)
          end
        end
        """)

      assert expand_defguard_to_string(args, expr) == Macro.to_string(goal)
    end

    test "handles re-used variables" do
      args = quote(do: [foo, bar, baz])
      expr = quote(do: foo + foo + bar + baz)

      {:ok, goal} =
        Code.string_to_quoted("""
        case(Macro.Env.in_guard?(__CALLER__)) do
          true ->
            quote() do
              :erlang.+(:erlang.+(:erlang.+(unquote(foo), unquote(foo)), unquote(bar)), unquote(baz))
            end
          false ->
            quote() do
              foo = unquote(foo)
              bar = unquote(bar)
              baz = unquote(baz)
              :erlang.+(:erlang.+(:erlang.+(foo, foo), bar), baz)
            end
        end
        """)

      assert expand_defguard_to_string(args, expr) == Macro.to_string(goal)
    end

    defp expand_defguard_to_string(args, expr) do
      require Kernel.Utils

      quote(do: Kernel.Utils.defguard(unquote(args), unquote(expr)))
      |> Macro.expand(__ENV__)
      |> Macro.to_string()
    end
  end
end
