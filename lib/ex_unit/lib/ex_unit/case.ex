defmodule ExUnit.Case do
  @moduledoc """
  This module is meant to be used in other modules
  as a way to configure and prepare them for testing.

  When used, it allows the following options:

  * :async - configure Elixir to run that specific test case
             in parallel with others. Must be used for performance
             when your test cases do not change any global state;

  This module automatically includes all callbacks defined in
  `ExUnit.Callbacks`. See that module's documentation for more
  information.

  ## Examples

     defmodule AssertionTest do
       # Use the module
       use ExUnit.Case, async: true

       # The `test` macro is imported by ExUnit.Case
       test "always pass" do
         assert true
       end
     end

  """

  @doc false
  defmacro __using__(opts // []) do
    async = Keyword.get(opts, :async, false)

    unless Process.whereis(ExUnit.Server) do
      raise "cannot use ExUnit.Case without starting ExUnit application, " <>
            "please call ExUnit.start() or explicitly start the :ex_unit app"
    end

    quote do
      unless Module.get_attribute(__MODULE__, :ex_unit_case) do
        if unquote(async) do
          ExUnit.Server.add_async_case(__MODULE__)
        else
          ExUnit.Server.add_sync_case(__MODULE__)
        end

        use ExUnit.Callbacks
      end

      @ex_unit_case true

      import ExUnit.Callbacks
      import ExUnit.Assertions
      import ExUnit.Case
      import ExUnit.DocTest
    end
  end

  @doc """
  Provides a convenient macro that allows a test to be
  defined with a string. This macro automatically inserts
  the atom :ok as the last line of the test. That said,
  a passing test always returns :ok, but, more important,
  it forces Elixir to not tail call optimize the test and
  therefore avoiding hiding lines from the backtrace.

  ## Examples

      test "true is equal to true" do
        assert true == true
      end

  """
  defmacro test(message, var // quote(do: _), contents) do
    contents =
      case contents do
        [do: _] ->
          quote do
            unquote(contents)
            :ok
          end
        _ ->
          quote do
            try(unquote(contents))
            :ok
          end
      end

    var      = Macro.escape(var)
    contents = Macro.escape(contents, unquote: true)

    quote bind_quoted: binding do
      message = if is_binary(message) do
        :"test #{message}"
      else
        :"test_#{message}"
      end

      def unquote(message)(unquote(var)), do:
        unquote(contents)
    end
  end
end
