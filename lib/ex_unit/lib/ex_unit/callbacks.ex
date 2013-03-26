defmodule ExUnit.Callbacks do
  @moduledoc %B"""
  This module defines four callbacks: `setup_all`, `teardown_all`,
  `setup` and `teardown`. Those callbacks are defined via macros
  and receives a keyword list of metadata. The callback may
  optionally define extra data which will be available in the test
  cases.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        setup do
          IO.puts "This is a setup callback"

          # Returns extra metadata
          { :ok, [hello: "world"] }
        end

        setup context do
          # We can access the test name in the context
          IO.puts "Setting up: #{context[:test]}"

          # The metadata returned by the previous setup as well
          assert context[:hello] == "world"

          # No metadata
          :ok
        end

        test "always pass" do
          assert true
        end
      end

  """

  @doc false
  defmacro __using__(opts) do
    parent = opts[:parent]

    quote do
      @exunit_setup []
      @exunit_teardown []
      @exunit_setup_all []
      @exunit_teardown_all []

      @before_compile unquote(__MODULE__)
      @after_compile unquote(__MODULE__)
      import unquote(__MODULE__)

      def __exunit__(:parent) do
        unquote(parent)
      end

      def __exunit__(:setup, context) do
        context = __exunit_setup__ unquote(parent).__exunit__(:setup, context)
        ExUnit.Callbacks.__setup__(__MODULE__, context)
      end

      def __exunit__(:teardown, context) do
        context = unquote(parent).__exunit__(:teardown, __exunit_teardown__ context)
        ExUnit.Callbacks.__teardown__(__MODULE__, context)
      end

      def __exunit__(:setup_all, context) do
        context = __exunit_setup_all__ unquote(parent).__exunit__(:setup_all, context)
        ExUnit.Callbacks.__setup_all__(__MODULE__, context)
      end

      def __exunit__(:teardown_all, context) do
        context = unquote(parent).__exunit__(:teardown_all, __exunit_teardown_all__ context)
        ExUnit.Callbacks.__teardown_all__(__MODULE__, context)
      end
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    [ compile_callbacks(env, :exunit_setup),
      compile_callbacks(env, :exunit_teardown),
      compile_callbacks(env, :exunit_setup_all),
      compile_callbacks(env, :exunit_teardown_all) ]
  end

  defmacro setup(var // quote(do: _), block) do
    quote do
      name = :"__exunit_setup_#{length(@exunit_setup)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_setup [name|@exunit_setup]
    end
  end

  defmacro teardown(var // quote(do: _), block) do
    quote do
      name = :"__exunit_teardown_#{length(@exunit_teardown)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_teardown [name|@exunit_teardown]
    end
  end

  defmacro setup_all(var // quote(do: _), block) do
    quote do
      name = :"__exunit_setup_all_#{length(@exunit_setup_all)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_setup_all [name|@exunit_setup_all]
    end
  end

  defmacro teardown_all(var // quote(do: _), block) do
    quote do
      name = :"__exunit_teardown_all_#{length(@exunit_teardown_all)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_teardown_all [name|@exunit_teardown_all]
    end
  end

  ## Helpers

  def __merge__(_mod, other, :ok), do: other
  def __merge__(_mod, other, { :ok, data }), do: Keyword.merge(other, data)
  def __merge__(mod, _, failure) do
    raise "expected ExUnit callback in #{inspect mod} to return :ok " <>
          " or { :ok, data }, got #{inspect failure} instead"
  end

  defp escape(contents) do
    Macro.escape(contents, escape_unquoted: false)
  end

  defp compile_callbacks(env, kind) do
    callbacks = Module.get_attribute(env.module, kind) |> Enum.reverse

    acc =
      Enum.reduce callbacks, quote(do: context), fn(callback, acc) ->
        quote do
          context = unquote(acc)
          unquote(__MODULE__).__merge__(__MODULE__, context, unquote(callback)(context))
        end
      end

    quote do
      defp unquote(:"__#{kind}__")(context), do: unquote(acc)
    end
  end

  ## Deprecated handling

  @doc false
  def __after_compile__(env, _binary) do
    test_case = env.module

    Enum.each [:setup, :teardown, :setup_all, :teardown_all], fn(kind) ->
      if Enum.any? 0..2, function_exported?(test_case, kind, &1) do
        IO.puts "[Warning] #{kind} callback in #{inspect test_case} is deprecated. Please use the #{kind} macro instead."
      end
    end
  end

  @doc false
  def __setup__(test_case, context) do
    cond do
      function_exported?(test_case, :setup, 2) ->
        test_case.setup(context, context[:test])
      function_exported?(test_case, :setup, 1) ->
        test_case.setup(context)
      function_exported?(test_case, :setup, 0) ->
        test_case.setup
      true ->
        context
    end
  end

  @doc false
  def __teardown__(test_case, context) do
    cond do
      function_exported?(test_case, :teardown, 2) ->
        test_case.teardown(context, context[:test])
      function_exported?(test_case, :teardown, 1) ->
        test_case.teardown(context)
      function_exported?(test_case, :teardown, 0) ->
        test_case.teardown
      true ->
        context
    end
  end

  @doc false
  def __teardown_all__(test_case, context) do
    cond do
      function_exported?(test_case, :teardown_all, 1) ->
        test_case.teardown_all(context)
      function_exported?(test_case, :teardown_all, 0) ->
        test_case.teardown_all
      true ->
        context
    end
  end

  @doc false
  def __setup_all__(test_case, context) do
    cond do
      function_exported?(test_case, :setup_all, 0) ->
        test_case.setup_all
      true ->
        context
    end
  end
end
