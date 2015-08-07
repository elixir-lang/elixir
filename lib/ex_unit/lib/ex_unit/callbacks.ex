defmodule ExUnit.Callbacks do
  @moduledoc ~S"""
  Defines ExUnit Callbacks.

  This module defines both `setup_all` and `setup` callbacks, as well as
  the `on_exit` facility.

  The setup callbacks are defined via macros and each one can optionally
  receive a map with metadata, usually referred to as `context`. The
  callback may optionally put extra data into `context` to be used in
  the tests.

  The `setup_all` callbacks are invoked once to setup the test case before any
  test is run and all `setup` callbacks are run before each test. No callback
  runs if the test case has no tests or all tests were filtered out.

  `on_exit` callbacks are registered on demand, usually to undo an action
  performed by a setup callback. `on_exit` may also take a reference,
  allowing callback to be overridden in the future. A registered `on_exit`
  callback always runs, while failures in `setup` and `setup_all` will stop
  all remaining setup callbacks from executing.

  Finally, `setup_all` callbacks run in the test case process, while all
  `setup` callbacks run in the same process as the test itself. `on_exit`
  callbacks always run in a separate process than the test case or the
  test itself. Since the test process exits with reason `:shutdown`, most
  of times `on_exit/1` can be avoided as processes are going to clean
  up on their own.

  ## Context

  If you return `{:ok, <dict>}` from `setup_all`, the dictionary
  will be merged into the current context and be available in all
  subsequent `setup_all`, `setup` and the test itself.

  Similarly, returning `{:ok, <dict>}` from `setup`, the dict returned
  will be merged into the current context and be available in all
  subsequent `setup` and the `test` itself.

  Returning `:ok` leaves the context unchanged in both cases.

  Returning anything else from `setup_all` will force all tests to fail,
  while a bad response from `setup` causes the current test to fail.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        # `setup_all` is called once to setup the case before any test is run
        setup_all do
          IO.puts "Starting AssertionTest"

          # No metadata
          :ok
        end

        # `setup` is called before each test is run
        setup do
          IO.puts "This is a setup callback"

          on_exit fn ->
            IO.puts "This is invoked once the test is done"
          end

          # Returns extra metadata, it must be a dict
          {:ok, hello: "world"}
        end

        # Same as `setup`, but receives the context
        # for the current test
        setup context do
          IO.puts "Setting up: #{context[:test]}"
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
      @ex_unit_setup_all []

      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    [compile_callbacks(env, :setup),
     compile_callbacks(env, :setup_all)]
  end

  @doc """
  Defines a callback to be run before each test in a case.
  """
  defmacro setup(var \\ quote(do: _), block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_setup_#{length(@ex_unit_setup)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup [name|@ex_unit_setup]
    end
  end

  @doc """
  Defines a callback to be run before all tests in a case.
  """
  defmacro setup_all(var \\ quote(do: _), block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_setup_all_#{length(@ex_unit_setup_all)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup_all [name|@ex_unit_setup_all]
    end
  end

  @doc """
  Defines a callback that runs on the test (or test case) exit.

  An `on_exit` callback is a function that receives no arguments and
  runs in a separate process than the caller.

  `on_exit/2` is usually called from `setup` and `setup_all` callbacks,
  often to undo the action performed during `setup`. However, `on_exit`
  may also be called dynamically, where a reference can be used to
  guarantee the callback will be invoked only once.
  """
  @spec on_exit(term, (() -> term)) :: :ok
  def on_exit(ref \\ make_ref, callback) do
    case ExUnit.OnExitHandler.add(self, ref, callback) do
      :ok -> :ok
      :error ->
        raise ArgumentError, "on_exit/1 callback can only be invoked from the test process"
    end
  end

  ## Helpers

  @doc false
  def __merge__(_mod, other, :ok) do
    {:ok, other}
  end

  def __merge__(_mod, other, {:ok, data}) do
    {:ok, Dict.merge(other, data)}
  end

  def __merge__(mod, _, failure) do
    raise "expected ExUnit callback in #{inspect mod} to return :ok " <>
          " or {:ok, dict}, got #{inspect failure} instead"
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
