defmodule ExUnit.Callbacks do
  @moduledoc %B"""
  This module defines four callbacks: `setup_all`, `teardown_all`,
  `setup` and `teardown`.

  Those callbacks are defined via macros and each one can optionally receive a
  keyword list with metadata, usually referred to as `context`. The callback
  may optionally put extra data into `context` to be used in the tests.

  **Note**: `setup` and `teardown` callbacks share the same context, it
  provides an ExUnit.Test record associated with the `:test` key. `setup_all`
  and `teardown_all` share their own context in a similar way, but this one
  provides an ExUnit.TestCase record associated with the `:case` key.

  If you return `{ :ok, <keyword list> }` from `setup` or `teardown`, the keyword
  list will get merged into the context that will be available in all
  subsequent `setup`, `test`, or `teardown` calls.

  Similarly, returning `{ :ok, <keyword list> }` from `setup_all` or
  `teardown_all` will merge the keyword list into the context that will be
  available in all subsequent `setup_all` or `teardown_all` calls.

  Returning :ok leaves the context unchanged in both cases.

  Returning anything else from `setup` or `teardown` will force the current
  test to fail, and subsequent `setup`, `test`, and `teardown` callbacks won't
  be called for it.

  Returning anything else from `setup_all` or `teardown_all` will force the
  whole case to fail, and no other callback will be called.

  It is allowed to define multiple `setup` or `teardown` callbacks, they will
  be called sequentially in the order of definition before each test. The
  returned keyword list from the last `setup` will be merged into the context passed to
  the `test` and `teardown` (if defined) callbacks.

  In the case of `setup_all` and `teardown_all` callbacks, each `setup_all`
  will be called only once before the first test's `setup` and each
  `teardown_all` will be called once after the last test. The returned keyword
  list from the last `setup_all` will get merged into the context passed to the
  `teardown_all` callbacks.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        # `setup` is called before each test is run
        setup do
          IO.puts "This is a setup callback"

          # Return extra metadata, it has to be a keyword list
          { :ok, [hello: "world"] }
        end

        # Same as `setup`, but receives the context for the current test
        setup context do
          # We can access the test record in the context
          IO.puts "Setting up: #{context[:test]}"

          # We can also access the data returned from `setup/0`
          assert context[:hello] == "world"

          # No metadata
          :ok
        end

        # This is called after each test finishes
        teardown context do
          assert context[:hello] == "world"
          :ok
        end

        test "always pass" do
          assert true
        end

        test "another one", context do
          assert context[:hello] == "world"
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
      import unquote(__MODULE__)

      def __exunit__(:parent) do
        unquote(parent)
      end

      def __exunit__(:setup, context) do
        __exunit_setup__ unquote(parent).__exunit__(:setup, context)
      end

      def __exunit__(:teardown, context) do
        unquote(parent).__exunit__(:teardown, __exunit_teardown__ context)
      end

      def __exunit__(:setup_all, context) do
        __exunit_setup_all__ unquote(parent).__exunit__(:setup_all, context)
      end

      def __exunit__(:teardown_all, context) do
        unquote(parent).__exunit__(:teardown_all, __exunit_teardown_all__ context)
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

  @doc """
  Called before the start of each test.
  """
  defmacro setup(var // quote(do: _), block) do
    quote do
      name = :"__exunit_setup_#{length(@exunit_setup)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_setup [name|@exunit_setup]
    end
  end

  @doc """
  Called after the finish of each test. Note that if the test crashed with an :exit
  message, `teardown` will not be run.
  """
  defmacro teardown(var // quote(do: _), block) do
    quote do
      name = :"__exunit_teardown_#{length(@exunit_teardown)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_teardown [name|@exunit_teardown]
    end
  end

  @doc """
  Called before the start of a case, i.e. called once before the first test in
  the current module and before any `setup` callbacks.
  """
  defmacro setup_all(var // quote(do: _), block) do
    quote do
      name = :"__exunit_setup_all_#{length(@exunit_setup_all)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_setup_all [name|@exunit_setup_all]
    end
  end

  @doc """
  Called once after the last test finishes without emitting an :exit message.
  """
  defmacro teardown_all(var // quote(do: _), block) do
    quote do
      name = :"__exunit_teardown_all_#{length(@exunit_teardown_all)}"
      defp name, [unquote(escape var)], [], unquote(escape block)
      @exunit_teardown_all [name|@exunit_teardown_all]
    end
  end

  ## Helpers

  @doc false
  def __merge__(_mod, other, :ok), do: other
  def __merge__(_mod, other, { :ok, data }) when is_list(data), do: Keyword.merge(other, data)
  def __merge__(mod, _, failure) do
    raise "expected ExUnit callback in #{inspect mod} to return :ok " <>
          " or { :ok, keywords }, got #{inspect failure} instead"
  end

  defp escape(contents) do
    Macro.escape_quoted(contents)
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
end
