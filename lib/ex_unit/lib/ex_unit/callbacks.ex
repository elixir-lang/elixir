defmodule ExUnit.Callbacks do
  @moduledoc ~S"""
  Defines ExUnit Callbacks.

  This module defines four callbacks: `setup_all`, `teardown_all`,
  `setup` and `teardown`.

  These callbacks are defined via macros and each one can optionally receive a
  keyword list with metadata, usually referred to as `context`. The callback
  may optionally put extra data into `context` to be used in the tests.

  If you return `{:ok, <keyword list>}` from `setup` or `teardown`, the keyword
  list will be merged into the context that will be available in all
  subsequent `setup`, `test` or `teardown` calls.

  Similarly, returning `{:ok, <keyword list>}` from `setup_all` or
  `teardown_all` will merge the keyword list into the context that will be
  available in all subsequent `setup_all` or `teardown_all` calls.

  Returning `:ok` leaves the context unchanged in both cases.

  Returning anything else from `setup` or `teardown` will force the current
  test to fail, and subsequent `setup`, `test` and `teardown` callbacks won't
  be called for it.

  Returning anything else from `setup_all` or `teardown_all` will force the
  whole case to fail, and no other callback will be called.

  It is possible to define multiple `setup` and `teardown` callbacks and they will
  be called sequentially. In the case of `setup_all` and `teardown_all` callbacks,
  each `setup_all` will be called only once before the first test's `setup` and each
  `teardown_all` will be called once after the last test. No callback runs if the
  test case has no tests or all tests were filtered out via `include`/`exclude`.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        # `setup` is called before each test is run
        setup do
          IO.puts "This is a setup callback"

          # Return extra metadata, it must be a keyword list
          {:ok, [hello: "world"]}
        end

        # Same as `setup`, but receives the context for the current test
        setup context do
          # We can access the current test in the context
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
  defmacro __using__(_) do
    quote do
      @ex_unit_setup []
      @ex_unit_teardown []
      @ex_unit_setup_all []
      @ex_unit_teardown_all []

      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    [ compile_callbacks(env, :setup),
      compile_callbacks(env, :teardown),
      compile_callbacks(env, :setup_all),
      compile_callbacks(env, :teardown_all) ]
  end

  @doc """
  Called before the start of each test.
  """
  defmacro setup(var \\ quote(do: _), block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_setup_#{length(@ex_unit_setup)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup [name|@ex_unit_setup]
    end
  end

  @doc """
  Called after the completion of each test. 
  
  Note that if the test crashed with an `:exit`
  message, `teardown` will not be run.
  """
  defmacro teardown(var \\ quote(do: _), block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_teardown_#{length(@ex_unit_teardown)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_teardown [name|@ex_unit_teardown]
    end
  end

  @doc """
  Called before the start of a case, i.e. called once before the first test in
  the current module and before any `setup` callbacks.
  """
  defmacro setup_all(var \\ quote(do: _), block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_setup_all_#{length(@ex_unit_setup_all)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup_all [name|@ex_unit_setup_all]
    end
  end

  @doc """
  Called once after the last test finishes without emitting an `:exit` message.
  """
  defmacro teardown_all(var \\ quote(do: _), block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_teardown_all_#{length(@ex_unit_teardown_all)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_teardown_all [name|@ex_unit_teardown_all]
    end
  end

  ## Helpers

  @doc false
  def __merge__(_mod, other, :ok) do
    {:ok, other}
  end

  def __merge__(_mod, other, {:ok, data}) when is_list(data) do
    {:ok, Keyword.merge(other, data)}
  end

  def __merge__(mod, _, failure) do
    raise "expected ExUnit callback in #{inspect mod} to return :ok " <>
          " or {:ok, keywords}, got #{inspect failure} instead"
  end

  defp escape(contents) do
    Macro.escape(contents, unquote: true)
  end

  defp compile_callbacks(env, kind) do
    callbacks = Module.get_attribute(env.module, :"ex_unit_#{kind}") |> Enum.reverse

    acc =
      case callbacks do
        [] ->
          quote do: {:ok, context}
        [h|t] ->
          Enum.reduce t, compile_merge(h), fn(callback, acc) ->
            quote do
              case unquote(acc) do
                {:ok, context} ->
                  unquote(compile_merge(callback))
                other ->
                  other
              end
            end
          end
      end

    quote do
      def __ex_unit__(unquote(kind), context), do: unquote(acc)
    end
  end

  defp compile_merge(callback) do
    quote do
      unquote(__MODULE__).__merge__(__MODULE__, context, unquote(callback)(context))
    end
  end
end
