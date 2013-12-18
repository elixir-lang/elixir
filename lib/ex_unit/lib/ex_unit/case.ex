defmodule ExUnit.Case do
  @moduledoc """
  Sets up an ExUnit test case.

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

  ## Context

  All tests receive a context as an argument. The context is particularly
  useful for sharing information between callbacks and tests:

      defmodule KVTest do
        use ExUnit.Case

        setup do
          { :ok, pid } = KV.start_link
          { :ok, [pid: pid] }
        end

        test "stores key-values", context do
          assert KV.put(context[:pid], :hello, :world) == :ok
          assert KV.get(context[:pid], :hello) == :world
        end
      end

  ## Tags

  The context is used to pass information from the callbacks to
  the test. In order to pass information from the test to the
  callback, ExUnit provides tags.

  By tagging a test, the tag value can be accessed in the context,
  allowing the developer to customize the test. Let's see an
  example:

      defmodule FileTest do
        # Changing directory cannot be async
        use ExUnit.Case, async: false

        setup context do
          # Read the :cd tag value
          if cd = context[:cd] do
            prev_cwd = File.cwd!
            File.cd!(cd)
            { :ok, [prev_cd: prev_cd] }
          else
            :ok
          end
        end

        teardown context do
          # Revert to the previous working directory
          if cd = context[:prev_cd] do
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
  going to run on. We then use the same context to store the
  previous working directory that is reverted to after the test
  in a teardown callback.

  Tags are also very effective when used with case templates
  (`ExUnit.CaseTemplate`) allowing callbacks in the case template
  to customize the test behaviour.

  Note a tag can be set in two different ways:

      @tag key: value
      @tag :key       # equivalent to setting @tag key: true

  If a tag is given more than once, the last value wins.

  ### Reserved tags

  The following tags are set automatically by ExUnit and are
  therefore reserved:

  * `:case` - the test case module
  * `:test` - the test name
  * `:line` - the line the test was defined

  ## Filters

  Tags can also be used to identify specific tests, which can then be included
  or excluded using filters. Filters are defined as key-value pairs, similar to
  tags, and are used to match against the tags given for each test. For example
  the following command will skip any test that contains the `:os` tag but has
  a value other than `"unix"`.

      mix test --include os:unix

  If your tags are defined using boolean values, you can use the shorthand
  version by only specifying the tag name. The value will be automatically set
  to `true`. To skip all tests with a tag of `slow: true` run the following
  command:

      mix test --exclude slow

  Filters can also be combined to further limit the tests to be run. When
  defining filters with the same tag name, tests that match either filter will
  be run. The following command will skip any test that contains the `:os` tag
  but has a value other than `"unix"` or `"win32"`:

      mix test --include os:unix --include os:win32

  However, if multiple filters with different tag names are given, only tests
  that match the filter defined for each unique tag name will be run. This
  command will only run tests that have both `os: "unix"` and `type: "unit"`
  tags.

      mix test --include os:unix --include type:unit

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
  Define a test with a string.

  Provides a convenient macro that allows a test to be
  defined with a string. This macro automatically inserts
  the atom `:ok` as the last line of the test. That said,
  a passing test always returns `:ok`, but, more important,
  it forces Elixir to not tail call optimize the test and
  therefore avoids hiding lines from the backtrace.

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
             |> normalize_tags()
             |> Keyword.put(:line, env.line)
      Module.put_attribute(env.module, :ex_unit_tests,
        ExUnit.Test[name: name, case: env.module, tags: tags])
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
