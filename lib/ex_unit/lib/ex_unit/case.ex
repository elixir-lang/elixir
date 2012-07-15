defmodule ExUnit.Case do
  @moduledoc """
  This module is meant to be used in other modules
  as a way to configure and prepare them for testing.

  When used, it allows the following options:

  * :async - configure Elixir to run that specific test case
             in parallel with others. Must be used for performance
             when your test cases do not change any global state;

  ## Callbacks

  This module defines two callbacks. `setup_all` and `teardown_all`
  which are executed before and after all tests respectively.
  Those callbacks needs to return :ok, otherwise we assume
  tests should not be run.

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
    if Keyword.get(opts, :async, false) do
      ExUnit.Server.add_async_case(__CALLER__.module)
    else
      ExUnit.Server.add_sync_case(__CALLER__.module)
    end

    quote do
      import ExUnit.Assertions
      import ExUnit.Case

      def setup(_),     do: :ok
      def teardown(_),  do: :ok
      def setup_all,    do: :ok
      def teardown_all, do: :ok

      defoverridable [setup: 1, teardown: 1, setup_all: 0, teardown_all: 0]
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
  defmacro test(message, contents) do
    contents =
      case contents do
        [do: block] ->
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
      def message, [], [], do: unquote(contents)
    end
  end
end
