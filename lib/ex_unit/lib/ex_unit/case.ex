defmodule ExUnit.Case do
  @moduledoc """
  This module is meant to be used in other modules
  as a way to configure and prepare them for testing.

  When used, it allows the following options:

  * :async - configure Elixir to run that specific test case
             in parallel with others. Must be used for performance
             when your test cases do not change any global state;

  This module automatically includes all callbacks defined
  in `ExUnit.Callbacks`. Read it for more information.

 ## Examples

     defmodule AssertionTest do
       use ExUnit.Case, async: true

       def test_always_pass
         assert true
       end
     end

  """

  @doc false
  defmacro __using__(opts // []) do
    async  = Keyword.get(opts, :async, false)
    parent = Keyword.get(opts, :parent, __MODULE__)

    quote do
      if unquote(async) do
        ExUnit.Server.add_async_case(__MODULE__)
      else
        ExUnit.Server.add_sync_case(__MODULE__)
      end

      use ExUnit.Callbacks, parent: unquote(parent)

      import ExUnit.Assertions
      import ExUnit.Case
      import ExUnit.DocTest, only: [doctest: 1, doctest: 2]
    end
  end

  @doc false
  def __exunit__(kind, context) when kind in [:setup, :teardown, :setup_all, :teardown_all] do
    context
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

    quote do
      message = unquote(message)

      message = if is_binary(message) do
        :"test #{message}"
      else
        :"test_#{message}"
      end

      def message, [unquote(Macro.escape var)], [], do:
        unquote(Macro.escape_quoted contents)
    end
  end
end
