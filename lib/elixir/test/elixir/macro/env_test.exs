Code.require_file("../test_helper.exs", __DIR__)

defmodule MacroEnvMacros do
  defmacro my_macro(arg), do: arg

  @deprecated "this is deprecated"
  defmacro my_deprecated_macro(arg), do: arg
end

defmodule Macro.EnvTest do
  use ExUnit.Case, async: true

  import Macro.Env
  import ExUnit.CaptureIO

  alias List, as: CustomList, warn: false
  import MacroEnvMacros, warn: false

  def trace(event, _env) do
    send(self(), event)
    :ok
  end

  def meta, do: [file: "some_file.exs", line: 123]
  def env, do: %{__ENV__ | tracers: [__MODULE__], line: 456}

  doctest Macro.Env

  test "inspect" do
    assert inspect(__ENV__) =~ "#Macro.Env<"
  end

  test "prune_compile_info" do
    assert %Macro.Env{lexical_tracker: nil, tracers: []} =
             Macro.Env.prune_compile_info(%{__ENV__ | lexical_tracker: self(), tracers: [Foo]})
  end

  test "stacktrace" do
    env = %{__ENV__ | file: "foo", line: 12}

    assert Macro.Env.stacktrace(env) ==
             [{__MODULE__, :"test stacktrace", 1, [file: ~c"foo", line: 12]}]

    env = %{env | function: nil}

    assert Macro.Env.stacktrace(env) == [
             {__MODULE__, :__MODULE__, 0, [file: ~c"foo", line: 12]}
           ]

    env = %{env | module: nil}

    assert Macro.Env.stacktrace(env) ==
             [{:elixir_compiler, :__FILE__, 1, [file: ~c"foo", line: 12]}]
  end

  test "context modules" do
    defmodule Foo.Bar do
      assert __MODULE__ in __ENV__.context_modules
    end

    assert Foo.Bar in __ENV__.context_modules

    Code.compile_string("""
    defmodule Foo.Bar.Compiled do
      true = __MODULE__ in __ENV__.context_modules
    end
    """)
  end

  test "to_match/1" do
    quote = quote(do: x in [])

    assert {:__block__, [], [{:=, [], [{:_, [], Kernel}, {:x, [], Macro.EnvTest}]}, false]} =
             Macro.expand_once(quote, __ENV__)

    assert Macro.expand_once(quote, Macro.Env.to_match(__ENV__)) == false
  end

  test "prepend_tracer" do
    assert %Macro.Env{tracers: [MyCustomTracer | _]} =
             Macro.Env.prepend_tracer(__ENV__, MyCustomTracer)
  end

  describe "define_import/4" do
    test "with tracing" do
      define_import(env(), meta(), List)
      assert_received {:import, _, List, []}

      define_import(env(), meta(), Integer, only: :macros, trace: false)
      refute_received {:import, _, Integer, _}
    end

    test "with errors" do
      message =
        "invalid :only option for import, expected value to be an atom :functions, :macros, " <>
          "or a literal keyword list of function names with arity as values, got: "

      assert define_import(env(), meta(), Integer, only: :unknown) ==
               {:error, message <> ":unknown"}

      assert define_import(env(), meta(), Integer, only: [:unknown]) ==
               {:error, message <> "[:unknown]"}
    end

    test "with warnings" do
      assert capture_io(:stderr, fn ->
               define_import(env(), meta(), Integer, only: [is_odd: 1, is_odd: 1])
             end) =~ "invalid :only option for import, is_odd/1 is duplicated"

      assert {:ok, _env} =
               define_import(env(), meta(), Integer,
                 only: [is_odd: 1, is_odd: 1],
                 emit_warnings: false
               )
    end
  end

  describe "expand_alias/4" do
    test "with tracing" do
      {:alias, List} = expand_alias(env(), meta(), [:CustomList])
      assert_received {:alias_expansion, _, Elixir.CustomList, List}

      {:alias, List} = expand_alias(env(), meta(), [:CustomList], trace: false)
      refute_received {:alias_expansion, _, Elixir.CustomList, List}

      {:alias, List.Continues} = expand_alias(env(), meta(), [:CustomList, :Continues])
      assert_received {:alias_expansion, _, Elixir.CustomList, List}
    end
  end

  describe "expand_require/6" do
    test "returns :error for functions and unknown modules" do
      assert :error = expand_require(env(), meta(), List, :flatten, 1)
      assert :error = expand_require(env(), meta(), Unknown, :flatten, 1)
    end

    test "returns :error for unrequired modules" do
      assert :error = expand_require(env(), meta(), Integer, :is_odd, 1)
    end

    test "expands required modules" do
      assert {:macro, Integer, _} =
               expand_require(env(), [required: true] ++ meta(), Integer, :is_odd, 1)

      assert {:macro, Integer, _} =
               expand_require(%{env() | requires: [Integer]}, meta(), Integer, :is_odd, 1)

      assert {:macro, Integer, _} =
               expand_require(%{env() | module: Integer}, meta(), Integer, :is_odd, 1)
    end

    test "expands with argument" do
      {:macro, MacroEnvMacros, fun} = expand_require(env(), meta(), MacroEnvMacros, :my_macro, 1)
      assert fun.([], [quote(do: hello())]) == quote(do: hello())
      assert fun.([line: 789], [quote(do: hello())]) == quote(line: 789, do: hello())
      assert fun.([generated: true], [quote(do: hello())]) == quote(generated: true, do: hello())
    end

    test "with tracing and deprecations" do
      message = "MacroEnvMacros.my_deprecated_macro/1 is deprecated"

      {:macro, MacroEnvMacros, fun} =
        expand_require(env(), meta(), MacroEnvMacros, :my_deprecated_macro, 1)

      assert capture_io(:stderr, fn -> fun.([], [quote(do: hello())]) end) =~ message
      assert_received {:remote_macro, _, MacroEnvMacros, :my_deprecated_macro, 1}

      {:macro, MacroEnvMacros, fun} =
        expand_require(env(), meta(), MacroEnvMacros, :my_deprecated_macro, 1,
          trace: false,
          check_deprecations: false
        )

      refute capture_io(:stderr, fn -> fun.([], [quote(do: hello())]) end) =~ message
      refute_received {:remote_macro, _, MacroEnvMacros, :my_deprecated_macro, 1}
    end
  end

  describe "expand_import/5" do
    test "returns tagged :error for unknown imports" do
      assert {:error, :not_found} = expand_import(env(), meta(), :flatten, 1)
    end

    test "returns tagged :error for special forms" do
      assert {:error, :not_found} = expand_import(env(), meta(), :case, 1)
    end

    test "returns tagged :error for ambiguous" do
      import Date, warn: false
      import Time, warn: false
      assert {:error, {:ambiguous, mods}} = expand_import(__ENV__, meta(), :new, 3)
      assert Enum.sort(mods) == [Date, Time]
    end

    test "returns :function tuple" do
      assert {:function, ExUnit.CaptureIO, :capture_io} =
               expand_import(env(), meta(), :capture_io, 1)
    end

    test "expands with argument" do
      {:macro, MacroEnvMacros, fun} = expand_import(env(), meta(), :my_macro, 1)
      assert fun.([], [quote(do: hello())]) == quote(do: hello())
      assert fun.([line: 789], [quote(do: hello())]) == quote(line: 789, do: hello())
      assert fun.([generated: true], [quote(do: hello())]) == quote(generated: true, do: hello())
    end

    test "with tracing and deprecations" do
      message = "MacroEnvMacros.my_deprecated_macro/1 is deprecated"

      {:macro, MacroEnvMacros, fun} = expand_import(env(), meta(), :my_deprecated_macro, 1)

      assert capture_io(:stderr, fn -> fun.([], [quote(do: hello())]) end) =~ message
      assert_received {:imported_macro, _, MacroEnvMacros, :my_deprecated_macro, 1}

      {:macro, MacroEnvMacros, fun} =
        expand_import(env(), meta(), :my_deprecated_macro, 1,
          trace: false,
          check_deprecations: false
        )

      refute capture_io(:stderr, fn -> fun.([], [quote(do: hello())]) end) =~ message
      refute_received {:imported_macro, _, MacroEnvMacros, :my_deprecated_macro, 1}
    end
  end
end
