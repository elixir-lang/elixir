Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.GuardTest do
  use ExUnit.Case, async: true

  describe "Kernel.defguard(p) usage" do

    test "successfully defines guard" do
      defmodule Success, do: defguard foo(bar, baz) when bar + baz
    end

    test "successfully defines private guard" do
      defmodule PrivateSuccess do
        defguardp foo(bar, baz) when bar + baz
        def fizz(a, b) when foo(a, b), do: :buzz
      end
    end

    test "permits default values in args" do
      defmodule Default.Args do
        defguard is_divisible(value, remainder \\ 2)
          when is_integer(value) and rem(value, remainder) == 0
      end
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

    test "defguard defines guards that work inside and outside guard clauses" do
      defmodule Integer.Guards do
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0

        def is_even_and_large?(value) when is_even(value) and value > 100, do: true
        def is_even_and_large?(_), do: false
      end

      defmodule Integer.Utils do
        import Integer.Guards

        def is_even_and_small?(value) do
          if is_even(value) and value <= 100, do: true, else: false
        end
      end

      assert Integer.Guards.is_even_and_large?(102)
      refute Integer.Guards.is_even_and_large?(98)
      refute Integer.Guards.is_even_and_large?(99)
      refute Integer.Guards.is_even_and_large?(103)

      assert Integer.Utils.is_even_and_small?(98)
      refute Integer.Utils.is_even_and_small?(99)
      refute Integer.Utils.is_even_and_small?(102)
      refute Integer.Utils.is_even_and_small?(103)
    end

    test "defguardp defines private guards that work inside and outside guard clauses" do
      defmodule Integer.Private.Guards do
        defguardp is_even(value) when is_integer(value) and rem(value, 2) == 0

        def is_even_and_large?(value) when is_even(value) and value > 100, do: true
        def is_even_and_large?(_), do: false

        def is_even_and_small?(value) do
          if is_even(value) and value <= 100, do: true, else: false
        end
      end

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
        defmodule LiteralUsage, do: defguard "literal is bad"
      end

      assert_raise ArgumentError, ~r"invalid syntax in defguard", fn ->
        defmodule RemoteUsage, do: defguard Remote.call(is_bad)
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
          defguard foo(bar), do: one_liner
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
          defguard foo(bar) when both_given, do: error
        end
      end

    end

  end

  describe "Kernel.defguard compilation" do

    test "refuses to compile non-sensical code" do

      assert_raise CompileError, ~r"undefined function undefined", fn ->
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
          defguard foo(bar) when :erlang.binary_to_atom "foo"
        end
      end

      assert_raise CompileError, ~r"cannot invoke remote function", fn ->
        defmodule SendUsage do
          defguard foo(bar) when send self(), :baz
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
          defguard foo(bar) when (for x <- [1, 2, 3], do: (x*bar))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule AliasUsage do
          defguard foo(bar) when alias bar
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule ImportUsage do
          defguard foo(bar) when import bar
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule RequireUsage do
          defguard foo(bar) when require bar
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule SuperUsage do
          defguard foo(bar) when super bar
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule SpawnUsage do
          defguard foo(bar) when spawn &(&1)
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule ReceiveUsage do
          defguard foo(bar) when (receive do: (baz -> baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule CaseUsage do
          defguard foo(bar) when (case bar, do: (baz -> :baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule CondUsage do
          defguard foo(bar) when (cond do: (bar -> :baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule TryUsage do
          defguard foo(bar) when (try do: (baz -> baz))
        end
      end

      assert_raise CompileError, ~r"invalid expression in guard", fn ->
        defmodule WithUsage do
          defguard foo(bar) when (with do: (baz -> baz))
        end
      end

    end

  end

  describe "Kernel.Utils.defguard" do

    test "generates unquoted variables based on context" do
      refs = for ref <- ~w[foo bar baz]a, do: {ref, __MODULE__}
      expr = quote(do: foo + bar + baz)

      {:ok, goal} = Code.string_to_quoted("""
      case Macro.Env.in_guard? __CALLER__ do
        true -> quote do
          unquote(foo) + unquote(bar) + unquote(baz)
        end
        false -> quote do
          foo = unquote(foo)
          bar = unquote(bar)
          baz = unquote(baz)
          foo + bar + baz
        end
      end
      """)

      assert Macro.to_string(Kernel.Utils.defguard(expr, refs)) == Macro.to_string(goal)
    end

    test "doesn't obscure unused variables" do
      refs = for ref <- ~w[foo bar baz]a, do: {ref, __MODULE__}
      expr = quote(do: foo + bar)

      {:ok, goal} = Code.string_to_quoted("""
      case Macro.Env.in_guard? __CALLER__ do
        true -> quote do
          unquote(foo) + unquote(bar)
        end
        false -> quote do
          foo = unquote(foo)
          bar = unquote(bar)
          foo + bar
        end
      end
      """)

      assert Macro.to_string(Kernel.Utils.defguard(expr, refs)) == Macro.to_string(goal)
    end

    test "doesn't unquote unspecified variables" do
      refs = for ref <- ~w[foo bar baz]a, do: {ref, __MODULE__}
      expr = quote(do: foo + bar + baz + fizzbuzz)

      {:ok, goal} = Code.string_to_quoted("""
      case Macro.Env.in_guard? __CALLER__ do
        true -> quote do
          unquote(foo) + unquote(bar) + unquote(baz) + fizzbuzz
        end
        false -> quote do
          foo = unquote(foo)
          bar = unquote(bar)
          baz = unquote(baz)
          foo + bar + baz + fizzbuzz
        end
      end
      """)

      assert Macro.to_string(Kernel.Utils.defguard(expr, refs)) == Macro.to_string(goal)
    end

    test "handles re-used variables" do
      refs = for ref <- ~w[foo bar baz]a, do: {ref, __MODULE__}
      expr = quote(do: foo + foo + bar + baz)

      {:ok, goal} = Code.string_to_quoted("""
      case Macro.Env.in_guard? __CALLER__ do
        true -> quote do
          unquote(foo) + unquote(foo) + unquote(bar) + unquote(baz)
        end
        false -> quote do
          foo = unquote(foo)
          bar = unquote(bar)
          baz = unquote(baz)
          foo + foo + bar + baz
        end
      end
      """)

      assert Macro.to_string(Kernel.Utils.defguard(expr, refs)) == Macro.to_string(goal)
    end

  end

end
