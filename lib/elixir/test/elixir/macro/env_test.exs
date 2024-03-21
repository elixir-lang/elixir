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
  import MacroEnvMacros, warn: false

  def trace(event, _env) do
    send(self(), event)
    :ok
  end

  def meta, do: [file: "some_file.exs", line: 123]
  def env, do: %{__ENV__ | tracers: [__MODULE__], line: 456}

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
    test "returns :error for unknown imports" do
      assert :error = expand_import(env(), meta(), :flatten, 1)
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
