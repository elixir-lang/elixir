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
  read in the setup callback to configure the working directory the test is
  going to run on. We then use the same context to store the
  previous working directory that is reverted to after the test
  in the teardown callback.

  Tags are also very effective when used with case templates
  (`ExUnit.CaseTemplate`) allowing callbacks in the case template
  to customize the test behaviour.

  Note a tag can be set in two different ways:

      @tag key: value
      @tag :key       # equivalent to setting @tag key: true

  If a tag is given more than once, the last value wins.

  ### Module tags

  A tag can be set for all tests in a module by setting `@moduletag`:

      @moduletag :external

  If the same key is set via `@tag`, the `@tag` value has higher
  precedence.

  ### Reserved tags

  The following tags are set automatically by ExUnit and are
  therefore reserved:

  * `:case` - the test case module
  * `:test` - the test name
  * `:line` - the line on which the test was defined

  ## Filters

  Tags can also be used to identify specific tests, which can then
  be included or excluded using filters. The most common functionality
  is to exclude some particular tests from running, which can be done
  via `ExUnit.configure/1`:

      # Exclude all external tests from running
      ExUnit.configure exclude: [external: true]

  From now on, ExUnit will not run any test that has the `external` flag
  set to true. This behaviour can be reversed with the `:include` option
  which is usually passed through the command line:

      mix test --include external:true

  Run `mix help test` for more information on how to run filters via Mix.

  Another use case for tags and filters is to exclude all tests that have
  a particular tag by default, regardless of its value, and include only
  a certain subset:

      ExUnit.configure exclude: :os, include: [os: :unix]

  Keep in mind that all tests are included by default, so unless they are
  excluded first, the `include` option has no effect.
  """

  @doc false
  defmacro __using__(opts) do
    async = Keyword.get(opts, :async, false)

    unless Process.whereis(ExUnit.Server) do
      raise "cannot use ExUnit.Case without starting the ExUnit application, " <>
            "please call ExUnit.start() or explicitly start the :ex_unit app"
    end

    quote do
      unless Module.get_attribute(__MODULE__, :ex_unit_tests) do
        if unquote(async) do
          ExUnit.Server.add_async_case(__MODULE__)
        else
          ExUnit.Server.add_sync_case(__MODULE__)
        end

        Enum.each [:ex_unit_tests, :tag, :moduletag],
          &Module.register_attribute(__MODULE__, &1, accumulate: true)

        @before_compile ExUnit.Case
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
  a passing test always returns `:ok`, but, more importantly,
  it forces Elixir to not tail call optimize the test and
  therefore avoids hiding lines from the backtrace.

  ## Examples

      test "true is equal to true" do
        assert true == true
      end

  """
  defmacro test(message, var \\ quote(do: _), contents) do
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

      ExUnit.Case.__on_definition__(__ENV__, message)
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
  def __on_definition__(env, name) do
    mod  = env.module
    tags = Module.get_attribute(mod, :tag) ++ Module.get_attribute(mod, :moduletag)
    tags = [line: env.line] ++ normalize_tags(tags)

    Module.put_attribute(mod, :ex_unit_tests,
      ExUnit.Test[name: name, case: mod, tags: tags])

    Module.delete_attribute(mod, :tag)
  end

  defp normalize_tags(tags) do
    Enum.reduce tags, [], fn
      tag, acc when is_atom(tag) -> Keyword.put_new(acc, tag, true)
      tag, acc when is_list(tag) -> Keyword.merge(tag, acc)
    end
  end
end
