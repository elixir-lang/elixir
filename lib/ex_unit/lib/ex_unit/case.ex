defmodule ExUnit.Case do
  @moduledoc """
  Sets up an ExUnit case.

  This module must be used in other modules as a way to configure
  and prepare them for testing.

  When used, it accepts the following options:

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

  ## Tags

  This module also supports test tags. A tag allows developers to mark
  tests so they can be customized in callbacks. Let's see an example:

      defmodule FileTest do
        # Changing directory cannot be async
        use ExUnit.Case, async: false

        setup config do
          if cd = config[:test].tags[:cd] do
            prev_cwd = File.cwd!
            File.cd!(cd)
            { :ok, [prev_cwd: prev_cwd] }
          else
            :ok
          end
        end

        teardown config do
          if cd = config[:prev_cwd] do
            File.cd!(cd)
          end
        end

        @tag cd: "fixtures"
        test "reads utf-8 fixtures" do
          File.read("hello")
        end
      end

  In the example above, we have defined a tag called `:cd` that is
  read in callbacks to configure the working directory the test is
  going to run on. We use then the test context to store the previous
  working directory that will be reverted to after the test.

  Tags provide an excellent mechanism to avoid duplication in tests
  or extract common functionality in a test suite with the help of
  case templates (`ExUnit.CaseTemplate`).

  Note a tag can be set in two different ways:

      @tag key: value
      @tag :key       # equivalent to setting @tag key: true

  If a tag is given more than once, the last value win.
  """

  @doc false
  defmacro __using__(opts) do
    async = Keyword.get(opts, :async, false)

    unless Process.whereis(ExUnit.Server) do
      raise "cannot use ExUnit.Case without starting ExUnit application, " <>
            "please call ExUnit.start() or explicitly start the :ex_unit app"
    end

    quote do
      unless Module.get_attribute(__MODULE__, :ex_unit_tests) do
        if unquote(async) do
          ExUnit.Server.add_async_case(__MODULE__)
        else
          ExUnit.Server.add_sync_case(__MODULE__)
        end

        Module.register_attribute(__MODULE__, :ex_unit_tests, accumulate: true)
        Module.register_attribute(__MODULE__, :tag, accumulate: true)

        @before_compile ExUnit.Case
        @on_definition ExUnit.Case

        use ExUnit.Callbacks
      end

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

      def unquote(message)(unquote(var)), do: unquote(contents)
    end
  end

  @doc false
  defmacro __before_compile__(_) do
    quote do
      def __ex_unit__(:case) do
        ExUnit.TestCase[name: __MODULE__, tests: @ex_unit_tests]
      end
    end
  end

  @doc false
  def __on_definition__(env, kind, name, args, _guards, _body) do
    if kind == :def and is_test?(atom_to_list(name)) and length(args) == 1 do
      tags = Module.get_attribute(env.module, :tag)
      Module.put_attribute(env.module, :ex_unit_tests,
        ExUnit.Test[name: name, case: env.module, tags: normalize_tags(tags)])
    end
  end

  defp is_test?('test ' ++ _), do: true
  defp is_test?('test_' ++ _), do: true
  defp is_test?(_), do: false

  defp normalize_tags(tags) do
    Enum.reduce tags, [], fn
      tag, acc when is_atom(tag) -> Keyword.put_new(acc, tag, true)
      tag, acc when is_list(tag) -> Keyword.merge(tag, acc)
    end
  end
end
