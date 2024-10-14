defmodule ExUnit.Case do
  @moduledoc """
  Helpers for defining test cases.

  This module must be used in other modules as a way to configure
  and prepare them for testing.

  When used, it accepts the following options:

    * `:async` - configures tests in this module to run concurrently with
      tests in other modules. Tests in the same module never run concurrently.
      It should be enabled only if tests do not change any global state.
      Defaults to `false`.

    * `:group` (since v1.18.0) - configures the **group** this module belongs to.
      Tests in the same group never run concurrently. Tests from different
      groups (or with no groups) can run concurrently when `async: true`
      is given. By default, this module belongs to no group (defaults to `nil`).

    * `:register` - when `false`, does not register this module within
      ExUnit server. This means the module won't run when ExUnit suite runs.

    * `:parameterize` (since v1.18.0) - a list of maps to parameterize tests.
      If both `:async` and `:parameterize` are given, the different parameters
      run concurrently. See the "Parameterized tests" section below for more
      information.

  > #### `use ExUnit.Case` {: .info}
  >
  > When you `use ExUnit.Case`, it will import the functionality
  > from `ExUnit.Assertions`, `ExUnit.Callbacks`, `ExUnit.DocTest`,
  > and this module itself.

  ## Examples

      defmodule AssertionTest do
        # Use the module
        use ExUnit.Case, async: true

        # The "test" macro is imported by ExUnit.Case
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
          {:ok, pid} = KV.start_link()
          {:ok, pid: pid}
        end

        test "stores key-value pairs", context do
          assert KV.put(context[:pid], :hello, :world) == :ok
          assert KV.get(context[:pid], :hello) == :world
        end
      end

  As the context is a map, it can be pattern matched on to extract
  information:

      test "stores key-value pairs", %{pid: pid} = _context do
        assert KV.put(pid, :hello, :world) == :ok
        assert KV.get(pid, :hello) == :world
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
            prev_cd = File.cwd!()
            File.cd!(cd)
            on_exit(fn -> File.cd!(prev_cd) end)
          end

          :ok
        end

        @tag cd: "fixtures"
        test "reads UTF-8 fixtures" do
          File.read("README.md")
        end
      end

  In the example above, we have defined a tag called `:cd` that is
  read in the setup callback to configure the working directory the
  test is going to run on.

  Tags are also very effective when used with case templates
  (`ExUnit.CaseTemplate`) allowing callbacks in the case template
  to customize the test behaviour.

  Note a tag can be set in two different ways:

      @tag key: value
      @tag :key       # equivalent to setting @tag key: true

  If a tag is given more than once, the last value wins.

  ### Module and describe tags

  A tag can be set for all tests in a module or describe block by
  setting `@moduletag` or `@describetag` inside each context
  respectively:

      defmodule ApiTest do
        use ExUnit.Case
        @moduletag :external

        describe "makes calls to the right endpoint" do
          @describetag :endpoint

          # ...
        end
      end

  If you are setting a `@moduletag` or `@describetag` attribute, you must
  set them after your call to `use ExUnit.Case` otherwise you will see
  compilation errors.

  If the same key is set via `@tag`, the `@tag` value has higher
  precedence.

  The `setup_all` blocks only receive tags that are set using `@moduletag`.

  ### Known tags

  The following tags are set automatically by ExUnit and are
  therefore reserved:

    * `:module` - the module on which the test was defined

    * `:file` - the file on which the test was defined

    * `:line` - the line on which the test was defined

    * `:test` - the test name

    * `:async` - if the test case is in async mode

    * `:registered` - used for `ExUnit.Case.register_attribute/3` values

    * `:describe` - the describe block the test belongs to

    * `:describe_line` - the line the describe block begins on

    * `:doctest` - the module or the file being doctested (if a doctest)

    * `:doctest_line` - the line the doctest was defined (if a doctest)

    * `:doctest_data` - additional metadata about doctests (if a doctest)

    * `:test_type` - the test type used when printing test results.
      It is set by ExUnit to `:test`, `:doctest` and so on, but is customizable.

  The following tags customize how tests behave:

    * `:capture_log` - see the "Log Capture" section below

    * `:skip` - skips the test with the given reason

    * `:timeout` - customizes the test timeout in milliseconds (defaults to 60000).
      Accepts `:infinity` as a timeout value.

    * `:tmp_dir` - (since v1.11.0) see the "Tmp Dir" section below

  ## Parameterized tests

  Sometimes you want to run the same tests but with different parameters.
  In ExUnit, it is possible to do so by passing a `:parameterize` key to
  `ExUnit.Case`. The value must be a list of maps which will be the
  parameters merged into the test context.

  For example, Elixir has a module called `Registry`, which can have type
  `:unique` or `:duplicate`, and can control its concurrency factor using
  the `:partitions` option. If you have a number of tests that *behave the
  same* across all of those values, you can parameterize those tests with:

      use ExUnit.Case,
        async: true,
        parameterize:
          for(kind <- [:unique, :duplicate],
              partitions <- [1, 8],
              do: %{kind: kind, partitions: partitions})

  Then, in your tests, you can access the parameters as part of the context:

      test "starts a registry", %{kind: kind, partitions: partitions} do
        ...
      end

  Use parameterized tests with care:

    * Although parameterized tests run concurrently when `async: true` is also given,
      abuse of parameterized tests may make your test suite slower

    * If you use parameterized tests and then find yourself adding conditionals
      in your tests to deal with different parameters, then parameterized tests
      may be the wrong solution to your problem. Consider creating separated
      tests and sharing logic between them using regular functions

  ## Filters

  Tags can also be used to identify specific tests, which can then
  be included or excluded using filters. The most common functionality
  is to exclude some particular tests from running, which can be done
  via `ExUnit.configure/1`:

      # Exclude all external tests from running
      ExUnit.configure(exclude: [external: true])

  From now on, ExUnit will not run any test that has the `:external` option
  set to `true`. This behaviour can be reversed with the `:include` option
  which is usually passed through the command line:

      $ mix test --include external:true

  Run `mix help test` for more information on how to run filters via Mix.

  Another use case for tags and filters is to exclude all tests that have
  a particular tag by default, regardless of its value, and include only
  a certain subset:

      ExUnit.configure(exclude: :os, include: [os: :unix])

  A given include/exclude filter can be given more than once:

      ExUnit.configure(exclude: [os: :unix, os: :windows])

  Keep in mind that all tests are included by default, so unless they are
  excluded first, the `include` option has no effect.

  ## Log Capture

  ExUnit can optionally suppress printing of log messages that are generated
  during a test. Log messages generated while running a test are captured and
  only if the test fails are they printed to aid with debugging.

  You can opt into this behaviour for individual tests by tagging them with
  `:capture_log` or enable log capture for all tests in the ExUnit configuration:

      ExUnit.start(capture_log: true)

  This default can be overridden by `@tag capture_log: false` or
  `@moduletag capture_log: false`.

  Since `setup_all` blocks don't belong to a specific test, log messages generated
  in them (or between tests) are never captured. If you want to suppress these
  messages as well, remove the console backend globally by setting:

      config :logger, backends: []

  ## Tmp Dir

  ExUnit automatically creates a temporary directory for tests tagged with
  `:tmp_dir` and puts the path to that directory into the test context.
  The directory is removed before being created to ensure we start with a blank
  slate.

  The temporary directory path is unique (includes the test module and test name)
  and thus appropriate for running tests concurrently. You can customize the path
  further by setting the tag to a string, e.g.: `tmp_dir: "my_path"`, which would
  make the final path to be: `tmp/<module>/<test>/my_path`.

  Example:

      defmodule MyTest do
        use ExUnit.Case, async: true

        @tag :tmp_dir
        test "with tmp_dir", %{tmp_dir: tmp_dir} do
          assert tmp_dir =~ "with tmp_dir"
          assert File.dir?(tmp_dir)
        end
      end

  As with other tags, `:tmp_dir` can also be set as `@moduletag` and
  `@describetag`.
  """

  @type env :: module() | Macro.Env.t()
  @compile {:no_warn_undefined, [IEx.Pry]}
  @reserved [:module, :file, :line, :test, :async, :registered, :describe]

  @doc false
  defmacro __using__(opts) do
    quote do
      ExUnit.Case.__register__(__MODULE__, unquote(opts))

      import ExUnit.Callbacks
      import ExUnit.Assertions
      import ExUnit.Case, only: [describe: 2, test: 1, test: 2, test: 3]
      import ExUnit.DocTest
    end
  end

  @doc false
  def __register__(module, opts) do
    if not Keyword.keyword?(opts) do
      raise ArgumentError,
            ~s(the argument passed to "use ExUnit.Case" must be a list of options, ) <>
              ~s(got: #{inspect(opts)})
    end

    {register?, opts} = Keyword.pop(opts, :register, true)
    {next_opts, opts} = Keyword.split(opts, [:async, :group, :parameterize])

    if opts != [] do
      IO.warn("unknown options given to ExUnit.Case: #{inspect(opts)}")
    end

    if not Module.has_attribute?(module, :ex_unit_tests) do
      tag_check = Enum.any?([:moduletag, :describetag, :tag], &Module.has_attribute?(module, &1))

      if tag_check do
        raise "you must set @tag, @describetag, and @moduletag after the call to \"use ExUnit.Case\""
      end

      accumulate_attributes = [
        :ex_unit_tests,
        :tag,
        :describetag,
        :moduletag,
        :ex_unit_registered_test_attributes,
        :ex_unit_registered_describe_attributes,
        :ex_unit_registered_module_attributes
      ]

      Enum.each(accumulate_attributes, &Module.register_attribute(module, &1, accumulate: true))

      if register? do
        Module.put_attribute(module, :after_compile, ExUnit.Case)
      end

      ExUnit.Callbacks.__register__(module)
      Module.put_attribute(module, :before_compile, ExUnit.Case)
    end

    past_opts = Module.get_attribute(module, :ex_unit_module, [])
    Module.put_attribute(module, :ex_unit_module, Keyword.merge(past_opts, next_opts))
    :ok
  end

  @doc """
  Defines a test with `message`.

  The test may also define a pattern, which will be matched
  against the test context. For more information on contexts, see
  `ExUnit.Callbacks`.

  ## Examples

      test "true is equal to true" do
        assert true == true
      end

  """
  defmacro test(message, var \\ quote(do: _), contents) do
    if not is_tuple(var) do
      IO.warn(
        "test context is always a map. The pattern " <>
          "#{inspect(Macro.to_string(var))} will never match",
        Macro.Env.stacktrace(__CALLER__)
      )
    end

    contents =
      case annotate_test(contents, __CALLER__) do
        [do: block] ->
          quote do
            unquote(block)
            :ok
          end

        contents ->
          quote do
            try(unquote(contents))
            :ok
          end
      end

    %{module: mod, file: file, line: line} = __CALLER__

    name =
      quote do
        name =
          ExUnit.Case.register_test(
            unquote(mod),
            unquote(file),
            unquote(line),
            :test,
            unquote(message),
            []
          )
      end

    def =
      {:def, [],
       [
         {{:unquote, [], [quote(do: name)]}, [], [var]},
         [do: contents]
       ]}

    {:__block__, [], [name, def]}
  end

  defp annotate_test(contents, caller) do
    if Application.get_env(:ex_unit, :breakpoints, false) and Keyword.keyword?(contents) do
      for {key, expr} <- contents do
        {key, IEx.Pry.annotate_quoted(expr, true, caller)}
      end
    else
      contents
    end
  end

  @doc """
  Defines a not implemented test with a string.

  Provides a convenient macro that allows a test to be defined
  with a string, but not yet implemented. The resulting test will
  always fail and print a "Not implemented" error message. The
  resulting test case is also tagged with `:not_implemented`.

  ## Examples

      test "this will be a test in future"

  """
  defmacro test(message) do
    %{module: mod, file: file, line: line} = __CALLER__

    quote bind_quoted: binding() do
      name = ExUnit.Case.register_test(mod, file, line, :test, message, [:not_implemented])
      def unquote(name)(_), do: flunk("Not implemented")
    end
  end

  @doc """
  Describes tests together.

  Every describe block receives a name which is used as prefix for
  upcoming tests. Inside a block, `ExUnit.Callbacks.setup/1` may be
  invoked and it will define a setup callback to run only for the
  current block. The describe name is also added as a tag, allowing
  developers to run tests for specific blocks.

  ## Examples

      defmodule StringTest do
        use ExUnit.Case, async: true

        describe "String.downcase/1" do
          test "with ascii characters" do
            assert String.downcase("HELLO") == "hello"
          end

          test "with Unicode" do
            assert String.downcase("HÉLLÒ") == "héllò"
          end
        end
      end

  When using Mix, you can run all tests in a describe block by name:

      $ mix test --only describe:"String.downcase/1"

  or by passing the exact line the describe block starts on:

      $ mix test path/to/file:123

  Note describe blocks cannot be nested. Instead of relying on hierarchy
  for composition, developers should build on top of named setups. For
  example:

      defmodule UserManagementTest do
        use ExUnit.Case, async: true

        describe "when user is logged in and is an admin" do
          setup [:log_user_in, :set_type_to_admin]

          test ...
        end

        describe "when user is logged in and is a manager" do
          setup [:log_user_in, :set_type_to_manager]

          test ...
        end

        defp log_user_in(context) do
          # ...
        end
      end

  By forbidding hierarchies in favor of named setups, it is straightforward
  for the developer to glance at each describe block and know exactly the
  setup steps involved.
  """
  @doc since: "1.3.0"
  defmacro describe(message, do: block) do
    definition =
      quote unquote: false do
        defp unquote(name)(var!(context)), do: unquote(body)
      end

    quote do
      ExUnit.Callbacks.__describe__(__MODULE__, __ENV__.line, unquote(message), fn
        message, describes ->
          res = unquote(block)

          case ExUnit.Callbacks.__describe__(__MODULE__, message, describes) do
            {nil, nil} -> :ok
            {name, body} -> unquote(definition)
          end

          res
      end)
    end
  end

  @doc false
  defmacro __before_compile__(%{module: module}) do
    tests =
      module
      |> Module.get_attribute(:ex_unit_tests)
      |> Enum.reverse()
      |> Macro.escape()

    moduletag = Module.get_attribute(module, :moduletag)

    tags =
      moduletag
      |> normalize_tags()
      |> validate_tags()
      |> Map.new()

    opts = Module.get_attribute(module, :ex_unit_module, [])
    async? = Keyword.get(opts, :async, false)
    group = Keyword.get(opts, :group, nil)
    parameterize = Keyword.get(opts, :parameterize, nil)

    if not (parameterize == nil or (is_list(parameterize) and Enum.all?(parameterize, &is_map/1))) do
      raise ArgumentError, ":parameterize must be a list of maps, got: #{inspect(parameterize)}"
    end

    {setup_all?, callbacks} = ExUnit.Callbacks.__callbacks__(module)

    quote do
      unquote(callbacks)

      def __ex_unit__ do
        %ExUnit.TestModule{
          file: __ENV__.file,
          name: __MODULE__,
          setup_all?: unquote(setup_all?),
          tags: unquote(Macro.escape(tags)),
          tests: unquote(tests)
        }
      end

      def __ex_unit__(:config) do
        %{
          async?: unquote(async?),
          group: unquote(Macro.escape(group)),
          parameterize: unquote(Macro.escape(parameterize))
        }
      end
    end
  end

  @doc false
  def __after_compile__(%{module: module}, _) do
    cond do
      Process.whereis(ExUnit.Server) ->
        ExUnit.Server.add_module(module, module.__ex_unit__(:config))

      Code.can_await_module_compilation?() ->
        :ok

      true ->
        raise "cannot use ExUnit.Case without starting the ExUnit application, " <>
                "please call ExUnit.start() or explicitly start the :ex_unit app"
    end
  end

  @doc """
  Registers a function to run as part of this case.

  This is used by third-party projects, like QuickCheck, to
  implement macros like `property/3` that works like `test`
  but instead defines a property. See `test/3` implementation
  for an example of invoking this function.

  The test type will be converted to a string and pluralized for
  display. You can use `ExUnit.plural_rule/2` to set a custom
  pluralization.
  """
  @doc since: "1.11.0"
  def register_test(mod, file, line, test_type, name, tags) do
    if not Module.has_attribute?(mod, :ex_unit_tests) do
      raise "cannot define #{test_type}. Please make sure you have invoked " <>
              "\"use ExUnit.Case\" in the current module"
    end

    registered_attribute_keys = [
      :ex_unit_registered_module_attributes,
      :ex_unit_registered_describe_attributes,
      :ex_unit_registered_test_attributes
    ]

    registered =
      for key <- registered_attribute_keys,
          attribute <- Module.get_attribute(mod, key),
          into: %{} do
        {attribute, Module.get_attribute(mod, attribute)}
      end

    moduletag = Module.get_attribute(mod, :moduletag)
    tag = Module.delete_attribute(mod, :tag)

    {name, describe, describe_line, describetag} =
      case Module.get_attribute(mod, :ex_unit_describe) do
        {line, describe, _counter} ->
          test_name = validate_test_name("#{test_type} #{describe} #{name}")
          {test_name, describe, line, Module.get_attribute(mod, :describetag)}

        nil ->
          test_name = validate_test_name("#{test_type} #{name}")
          {test_name, nil, nil, []}
      end

    if Module.defines?(mod, {name, 1}) do
      raise ArgumentError, ~s("#{name}" is already defined in #{inspect(mod)})
    end

    tags =
      (tags ++ tag ++ describetag ++ moduletag)
      |> normalize_tags()
      |> validate_tags()
      |> Map.merge(%{
        line: line,
        file: file,
        registered: registered,
        describe: describe,
        describe_line: describe_line,
        test_type: test_type
      })

    test = %ExUnit.Test{name: name, case: mod, tags: tags, module: mod}
    Module.put_attribute(mod, :ex_unit_tests, test)

    for attribute <- Module.get_attribute(mod, :ex_unit_registered_test_attributes) do
      Module.delete_attribute(mod, attribute)
    end

    name
  end

  @doc """
  Registers a test with the given environment.

  This function is deprecated in favor of `register_test/6` which performs
  better under tight loops by avoiding `__ENV__`.
  """
  # TODO: Remove me Elixir v2.0
  @deprecated "Use register_test/6 instead"
  @doc since: "1.3.0"
  def register_test(%{module: mod, file: file, line: line}, test_type, name, tags) do
    register_test(mod, file, line, test_type, name, tags)
  end

  @doc """
  Registers a new attribute to be used during `ExUnit.Case` tests.

  The attribute values will be available through `context.registered`.
  Registered values are cleared after each `test/3` similar
  to `@tag`.

  This function takes the same options as `Module.register_attribute/3`.

  ## Examples

      defmodule MyTest do
        use ExUnit.Case

        ExUnit.Case.register_attribute(__MODULE__, :fixtures, accumulate: true)

        @fixtures :user
        @fixtures {:post, insert: false}
        test "using custom attribute", context do
          assert context.registered.fixtures == [{:post, insert: false}, :user]
        end

        test "custom attributes are cleared per test", context do
          assert context.registered.fixtures == []
        end
      end

  """
  @doc since: "1.3.0"
  @spec register_attribute(env, atom, keyword) :: :ok
  def register_attribute(env, name, opts \\ [])
  def register_attribute(%{module: mod}, name, opts), do: register_attribute(mod, name, opts)

  def register_attribute(mod, name, opts) when is_atom(mod) and is_atom(name) and is_list(opts) do
    register_attribute(:ex_unit_registered_test_attributes, mod, name, opts)
  end

  @doc """
  Registers a new describe attribute to be used during `ExUnit.Case` tests.

  The attribute values will be available through `context.registered`.
  Registered values are cleared after each `describe/2` similar
  to `@describetag`.

  This function takes the same options as `Module.register_attribute/3`.

  ## Examples

      defmodule MyTest do
        use ExUnit.Case

        ExUnit.Case.register_describe_attribute(__MODULE__, :describe_fixtures, accumulate: true)

        describe "using custom attribute" do
          @describe_fixtures :user
          @describe_fixtures {:post, insert: false}

          test "has attribute", context do
            assert context.registered.describe_fixtures == [{:post, insert: false}, :user]
          end
        end

        describe "custom attributes are cleared per describe" do
          test "doesn't have attributes", context do
            assert context.registered.describe_fixtures == []
          end
        end
      end

  """
  @doc since: "1.10.0"
  @spec register_describe_attribute(env, atom, keyword) :: :ok
  def register_describe_attribute(env, name, opts \\ [])

  def register_describe_attribute(%{module: mod}, name, opts) do
    register_describe_attribute(mod, name, opts)
  end

  def register_describe_attribute(mod, name, opts)
      when is_atom(mod) and is_atom(name) and is_list(opts) do
    register_attribute(:ex_unit_registered_describe_attributes, mod, name, opts)
  end

  @doc """
  Registers a new module attribute to be used during `ExUnit.Case` tests.

  The attribute values will be available through `context.registered`.

  This function takes the same options as `Module.register_attribute/3`.

  ## Examples

      defmodule MyTest do
        use ExUnit.Case

        ExUnit.Case.register_module_attribute(__MODULE__, :module_fixtures, accumulate: true)

        @module_fixtures :user
        @module_fixtures {:post, insert: false}

        test "using custom attribute", context do
          assert context.registered.module_fixtures == [{:post, insert: false}, :user]
        end

        test "still using custom attribute", context do
          assert context.registered.module_fixtures == [{:post, insert: false}, :user]
        end
      end

  """
  @doc since: "1.10.0"
  @spec register_module_attribute(env, atom, keyword) :: :ok
  def register_module_attribute(env, name, opts \\ [])

  def register_module_attribute(%{module: mod}, name, opts) do
    register_module_attribute(mod, name, opts)
  end

  def register_module_attribute(mod, name, opts)
      when is_atom(mod) and is_atom(name) and is_list(opts) do
    register_attribute(:ex_unit_registered_module_attributes, mod, name, opts)
  end

  defp register_attribute(type, mod, name, opts) do
    validate_registered_attribute!(type, mod, name)
    Module.register_attribute(mod, name, opts)
    Module.put_attribute(mod, type, name)
  end

  defp validate_registered_attribute!(type, mod, name) do
    registered_attribute_keys = [
      :ex_unit_registered_module_attributes,
      :ex_unit_registered_describe_attributes,
      :ex_unit_registered_test_attributes
    ]

    for key <- registered_attribute_keys,
        type != key and name in Module.get_attribute(mod, key) do
      raise ArgumentError, "cannot register attribute #{inspect(name)} multiple times"
    end

    if Module.has_attribute?(mod, name) do
      raise "you must set @#{name} after it has been registered"
    end
  end

  @doc """
  Returns the most recently registered test case as an `%ExUnit.Test{}`
  struct.

  This is used by third-party utilities to allow compile-time configuration
  using test tags without having to explicitly pass the test context at
  run-time. It is intended to be invoked in macros before the test module
  is compiled.

  Raises if called with a module that has already been compiled.
  """
  @doc since: "1.15.0"
  @spec get_last_registered_test(env) :: ExUnit.Test.t() | nil
  def get_last_registered_test(%{module: mod}) do
    get_last_registered_test(mod)
  end

  def get_last_registered_test(mod) when is_atom(mod) do
    Module.get_last_attribute(mod, :ex_unit_tests)
  end

  defp validate_tags(tags) do
    for tag <- @reserved, Map.has_key?(tags, tag) do
      raise "cannot set tag #{inspect(tag)} because it is reserved by ExUnit"
    end

    if not is_atom(tags[:test_type]) do
      raise("value for tag \":test_type\" must be an atom")
    end

    tags
  end

  defp validate_test_name(name) do
    try do
      String.to_atom(name)
    rescue
      SystemLimitError ->
        raise SystemLimitError, """
        the computed name of a test (which includes its type, the name of its parent describe \
        block if present, and the test name itself) must be shorter than 255 characters, \
        got: #{inspect(name)}
        """
    end
  end

  defp normalize_tags(tags) do
    Enum.reduce(Enum.reverse(tags), %{}, fn
      {key, value}, acc -> Map.put(acc, key, value)
      tag, acc when is_atom(tag) -> Map.put(acc, tag, true)
      tag, acc when is_list(tag) -> Enum.into(tag, acc)
    end)
  end
end
