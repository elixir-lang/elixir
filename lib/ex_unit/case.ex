defmodule ExUnit.Case do
  defmacro __using__(module, opts // []) do
    if Orddict.get(opts, :sync, false) do
      ExUnit.Server.add_sync_case(module)
    else:
      ExUnit.Server.add_case(module)
    end

    quote do
      import ExUnit.Assertions
      import ExUnit.Case

      @overridable true
      def setup_all, do: :ok

      @overridable true
      def teardown_all, do: :ok
    end
  end

  # Provides a convenient macro that allows a test to be
  # defined with a string. This macro automatically inserts
  # the atom :ok as the last line of the test. That said,
  # a passing test always returns :ok, but, more important,
  # it forces Elixir to not tail call optimize the test and
  # therefore avoiding hiding lines from the backtrace.
  #
  # ## Examples
  #
  #     test "true is equal to true" do
  #       assert_equal true, true
  #     end
  #
  defmacro test(message, contents) do
    contents =
      case contents do
      match: [do: block]
        quote do
          unquote(contents)
          :ok
        end
      else:
        quote do
          try(unquote(contents))
          :ok
        end
      end

    quote do
      message = unquote(message)
      message = if is_binary(message) do
        :"test #{message}"
      else:
        :"test_#{message}"
      end
      def message, [], true, do: unquote(contents)
    end
  end
end