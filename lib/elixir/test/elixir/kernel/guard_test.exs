Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.GuardTest do
  use ExUnit.Case, async: true

  describe "Kernel.defguard usage" do

    test "requires a proper name macro name" do

      assert_raise ArgumentError, ~r/could not extract a definition/, fn ->
        defmodule LiteralUsage, do: defguard "literal is bad"
      end

      assert_raise ArgumentError, ~r/could not extract a definition/, fn ->
        defmodule RemoteUsage, do: defguard Remote.call(is_bad)
      end

    end

    test "requires a unique name if guard name is already taken" do

      assert_raise ArgumentError, ~r/requires a unique name/, fn ->
        defmodule KernelGuardUsage, do: defguard is_integer(bar)
      end

      assert_raise ArgumentError, ~r/requires a unique name/, fn ->
        import Record, warn: false
        defmodule ImportedGuardUsage, do: defguard is_record(bar)
      end

    end

    test "requires an implementation" do
      assert_raise ArgumentError, ~r/requires an implementation/, fn ->
        defmodule ImplementationlessUsage, do: defguard foo(bar)
      end
    end

    test "does not accept a block" do

      assert_raise ArgumentError, ~r/does not accept a block/, fn ->
        defmodule OnelinerBlockUsage do
          defguard foo(bar), do: one_liner
        end
      end

      assert_raise ArgumentError, ~r/does not accept a block/, fn ->
        defmodule MultilineBlockUsage do
          defguard foo(bar) do
            multi
            liner
          end
        end
      end

      assert_raise ArgumentError, ~r/does not accept a block/, fn ->
        defmodule ImplAndBlockUsage do
          defguard foo(bar) when both_given, do: error
        end
      end

    end

  end

  describe "Kernel.defguard compilation" do

    test "refuses to compile non-sensical code" do

      assert_raise CompileError, ~r/undefined function undefined/, fn ->
        defmodule UndefinedUsage do
          defguard foo(function) when undefined(function)
        end
      end

    end

    test "fails on expressions not allowed in guard" do

      # Slightly different errors

      assert_raise ArgumentError, ~r/expects a compile-time list/, fn ->
        defmodule RuntimeListUsage do
          defguard foo(bar, baz) when bar in baz
        end
      end

      assert_raise CompileError, ~r/cannot invoke remote function/, fn ->
        defmodule BadErlangFunctionUsage do
          defguard foo(bar) when :erlang.binary_to_atom "foo"
        end
      end

      assert_raise CompileError, ~r/cannot invoke remote function/, fn ->
        defmodule SendUsage do
          defguard foo(bar) when send self(), :baz
        end
      end

      # Consistent errors

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule SoftNegationLogicUsage do
          defguard foo(logic) when !logic
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule SoftAndLogicUsage do
          defguard foo(soft, logic) when soft && logic
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule SoftOrLogicUsage do
          defguard foo(soft, logic) when soft || logic
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule LocalCallUsage do
          defguard foo(local, call) when local.(call)
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule ComprehensionUsage do
          defguard foo(bar) when (for x <- [1, 2, 3], do: (x*bar))
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule AliasUsage do
          defguard foo(bar) when alias bar
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule ImportUsage do
          defguard foo(bar) when import bar
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule RequireUsage do
          defguard foo(bar) when require bar
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule SuperUsage do
          defguard foo(bar) when super bar
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule SpawnUsage do
          defguard foo(bar) when spawn &(&1)
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule ReceiveUsage do
          defguard foo(bar) when (receive do: (baz -> baz))
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule CaseUsage do
          defguard foo(bar) when (case bar, do: (baz -> :baz))
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule CondUsage do
          defguard foo(bar) when (cond do: (bar -> :baz))
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
        defmodule TryUsage do
          defguard foo(bar) when (try do: (baz -> baz))
        end
      end

      assert_raise CompileError, ~r/invalid expression in guard/, fn ->
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
