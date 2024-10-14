defmodule ExUnit do
  @moduledoc """
  Unit testing framework for Elixir.

  ## Example

  A basic setup for ExUnit is shown below:

      # File: assertion_test.exs

      # 1) Start ExUnit.
      ExUnit.start()

      # 2) Create a new test module (test case) and use "ExUnit.Case".
      defmodule AssertionTest do
        # 3) Note that we pass "async: true", this runs the test case
        #    concurrently with other test cases. The individual tests
        #    within each test case are still run serially.
        use ExUnit.Case, async: true

        # 4) Use the "test" macro instead of "def" for clarity.
        test "the truth" do
          assert true
        end
      end

  To run the tests above, run the file using `elixir` from the
  command line. Assuming you named the file `assertion_test.exs`,
  you can run it as:

      $ elixir assertion_test.exs

  ## Case, Callbacks and Assertions

  See `ExUnit.Case` and `ExUnit.Callbacks` for more information
  about defining test cases and setting up callbacks.

  The `ExUnit.Assertions` module contains a set of macros to
  generate assertions with appropriate error messages.

  ## Integration with Mix

  Mix is the project management and build tool for Elixir. Invoking `mix test`
  from the command line will run the tests in each file matching the pattern
  `*_test.exs` found in the `test` directory of your project.

  You must create a `test_helper.exs` file inside the
  `test` directory and put the code common to all tests there.

  The minimum example of a `test_helper.exs` file would be:

      # test/test_helper.exs
      ExUnit.start()

  Mix will load the `test_helper.exs` file before executing the tests.
  It is not necessary to `require` the `test_helper.exs` file in your test
  files. Run `mix help test` for more information.
  """

  @typedoc """
  All tests start with a state of `nil`.

  A finished test can be in one of five states:

    1. Passed (also represented by `nil`)
    2. Failed
    3. Skipped (via @tag :skip)
    4. Excluded (via :exclude filters)
    5. Invalid (when setup_all fails)

  """
  @type state ::
          nil
          | {:excluded, binary}
          | {:failed, failed}
          | {:invalid, ExUnit.TestModule.t()}
          | {:skipped, binary}

  @typedoc "The error state returned by `ExUnit.Test` and `ExUnit.TestModule`"
  @type failed :: [{Exception.kind(), reason :: term, Exception.stacktrace()}]

  @typedoc "A map representing the results of running a test suite"
  @type suite_result :: %{
          excluded: non_neg_integer,
          failures: non_neg_integer,
          skipped: non_neg_integer,
          total: non_neg_integer
        }

  @type test_id :: {module, name :: atom}

  defmodule Test do
    @moduledoc """
    A struct that keeps information about the test.

    It is received by formatters and contains the following fields:

      * `:name` - the test name
      * `:module` - the test module
      * `:state` - the finished test state (see `t:ExUnit.state/0`)
      * `:time` - the duration in microseconds of the test's runtime
      * `:tags` - the test tags
      * `:logs` - the captured logs
      * `:parameters` - the test parameters

    """
    defstruct [:name, :case, :module, :state, time: 0, tags: %{}, logs: "", parameters: %{}]

    # TODO: Remove the `:case` field on v2.0
    @type t :: %__MODULE__{
            name: atom,
            case: module,
            module: module,
            state: ExUnit.state(),
            time: non_neg_integer,
            tags: map,
            logs: String.t()
          }
  end

  defmodule TestModule do
    @moduledoc """
    A struct that keeps information about the test module.

    It is received by formatters and contains the following fields:

      * `:file` - (since v1.11.0) the file of the test module

      * `:name` - the test module name

      * `:parameters` - (since v1.18.0) the test module parameters

      * `:setup_all?` - (since v1.18.0) if the test module requires a setup all

      * `:state` - the test error state (see `t:ExUnit.state/0`)

      * `:tags` - all tags in this module

      * `:tests` - all tests in this module

    """
    defstruct [:file, :name, :setup_all?, :state, parameters: %{}, tags: %{}, tests: []]

    @type t :: %__MODULE__{
            file: binary(),
            name: module,
            parameters: map(),
            setup_all?: boolean(),
            state: ExUnit.state(),
            tags: map,
            tests: [ExUnit.Test.t()]
          }
  end

  defmodule TestCase do
    # TODO: Remove this module on v2.0 (it has been replaced by TestModule)
    @moduledoc false
    defstruct [:name, :state, tests: []]

    @type t :: %__MODULE__{name: module, state: ExUnit.state(), tests: [ExUnit.Test.t()]}
  end

  defmodule TimeoutError do
    @moduledoc """
    Exception raised when a test times out.
    """

    @typedoc since: "1.16.0"
    @type t :: %__MODULE__{
            timeout: non_neg_integer,
            type: String.t()
          }

    defexception [:timeout, :type]

    @impl true
    def message(%{timeout: timeout, type: type}) do
      """
      #{type} timed out after #{timeout}ms. You can change the timeout:

        1. per test by setting "@tag timeout: x" (accepts :infinity)
        2. per test module by setting "@moduletag timeout: x" (accepts :infinity)
        3. globally via "ExUnit.start(timeout: x)" configuration
        4. by running "mix test --timeout x" which sets timeout
        5. or by running "mix test --trace" which sets timeout to infinity
           (useful when using IEx.pry/0)

      where "x" is the timeout given as integer in milliseconds (defaults to 60_000).
      """
    end
  end

  use Application

  @doc false
  def start(_type, []) do
    children = [
      ExUnit.Server,
      ExUnit.CaptureServer
    ]

    opts = [strategy: :one_for_one, name: ExUnit.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @doc """
  Starts ExUnit and automatically runs tests right before the
  VM terminates.

  It accepts a set of `options` to configure `ExUnit`
  (the same ones accepted by `configure/1`).

  If you want to run tests manually, you can set the `:autorun` option
  to `false` and use `run/0` to run tests.
  """
  @spec start(Keyword.t()) :: :ok
  def start(options \\ []) do
    {:ok, _} = Application.ensure_all_started(:ex_unit)

    configure(options)

    if Application.fetch_env!(:ex_unit, :autorun) do
      Application.put_env(:ex_unit, :autorun, false)

      System.at_exit(fn
        0 ->
          time = ExUnit.Server.modules_loaded(false)
          seed = Application.get_env(:ex_unit, :seed)
          options = persist_defaults(configuration())
          %{failures: failures} = maybe_repeated_run(options, seed, time)

          if failures > 0 do
            System.at_exit(fn _ -> exit({:shutdown, Keyword.fetch!(options, :exit_status)}) end)
          end

        _ ->
          :ok
      end)
    else
      :ok
    end
  end

  @doc """
  Configures ExUnit.

  ## Options

  ExUnit supports the following options:

    * `:assert_receive_timeout` - the timeout to be used on `assert_receive`
      calls in milliseconds, defaults to `100`;

    * `:autorun` - if ExUnit should run by default on exit. Defaults to `true`;

    * `:capture_log` - if ExUnit should default to keeping track of log messages
      and print them on test failure. Can be overridden for individual tests via
      `@tag capture_log: false`. This can also be configured to a specific level
      with `capture_log: [level: LEVEL]`, to capture all logs but only keep those
      above `LEVEL`. Note that `on_exit` and `setup_all` callbacks may still log,
      as they run outside of the testing process. To silent those, you can use
      `ExUnit.CaptureLog.capture_log/2` or consider disabling logging altogether.

    * `:colors` - a keyword list of color options to be used by some formatters:
      * `:enabled` - boolean option to enable colors, defaults to `IO.ANSI.enabled?/0`;

      * `:success` - success message (defaults to `:green`)
      * `:invalid` - invalid test message (defaults to `:yellow`)
      * `:skipped` - skipped test message (defaults to `:yellow`)
      * `:failure` - failed test message (defaults to `:red`)
      * `:error_info` - display of actual error (defaults to `:red`)
      * `:extra_info` - additional information (defaults to `:cyan`)
      * `:location_info` - filename and tags (defaults to `[:bright, :black]`)
      * `:diff_insert` - color of the insertions on diffs, defaults to `:green`;
      * `:diff_insert_whitespace` - color of the whitespace insertions on diffs,
        defaults to `IO.ANSI.color_background(2, 0, 0)`;
      * `:diff_delete` - color of the deletions on diffs, defaults to `:red`;
      * `:diff_delete_whitespace` - color of the whitespace deletions on diffs,
        defaults to `IO.ANSI.color_background(0, 2, 0)`;

    * `:exclude` - specifies which tests are run by skipping tests that match the
      filter. See the "Filters" section in the documentation for `ExUnit.Case`;

    * `:exit_status` - specifies an alternate exit status to use when the test
      suite fails. Defaults to 2;

    * `:failures_manifest_path` - specifies a path to the file used to store failures
      between runs;

    * `:formatters` - the formatters that will print results,
      defaults to `[ExUnit.CLIFormatter]`;

    * `:include` - specifies which tests are run by skipping tests that do not
      match the filter. Keep in mind that all tests are included by default, so unless they are
      excluded first, the `:include` option has no effect. To only run the tests
      that match the `:include` filter, exclude the `:test` tag first (see the
      documentation for `ExUnit.Case` for more information on tags and filters);

    * `:max_cases` - maximum number of tests to run in parallel. Only tests from
      different modules run in parallel. It defaults to `System.schedulers_online * 2`
      to optimize both CPU-bound and IO-bound tests;

    * `:max_failures` - the suite stops evaluating tests when this number of test failures
      is reached. All tests within a module that fail when using the
      [`setup_all/1,2`](`ExUnit.Callbacks.setup_all/1`) callbacks
      are counted as failures. Defaults to `:infinity`;

    * `:only_test_ids` - a list of `{module_name, test_name}` tuples that limits
      what tests get run. This is typically used by Mix to filter which tests
      should run;

    * `:rand_algorithm` - algorithm to be used when generating the test seed.
      Available algorithms can be found in Erlang's
      [`:rand`](https://www.erlang.org/doc/man/rand.html) documentation (see
      [`:rand.builting_arg/0`](https://www.erlang.org/doc/apps/stdlib/rand.html#t:builtin_alg/0)).
      Available since v1.16.0. Before v1.16.0, the algorithm was hard-coded to
      `:exs1024`. On Elixir v1.16.0 and after, the default changed to `:exsss`;

    * `:refute_receive_timeout` - the timeout to be used on `refute_receive`
      calls in milliseconds, defaults to `100`;

    * `:seed` - an integer seed value to randomize the test suite. This seed
      is also mixed with the test module and name to create a new unique seed
      on every test, which is automatically fed into the `:rand` module. This
      provides randomness between tests, but predictable and reproducible
      results. A `:seed` of `0` will disable randomization and the tests in each
      file will always run in the order that they were defined in;

    * `:slowest` - prints timing information for the N slowest tests. Running
      ExUnit with slow test reporting automatically runs in `trace` mode. It
      is disabled by default;

    * `:slowest_modules` - prints timing information for the N slowest test modules. Running
      ExUnit with slow test reporting automatically runs in `trace` mode. It
      is disabled by default;

    * `:stacktrace_depth` - configures the stacktrace depth to be used
      on formatting and reporters, defaults to `20`;

    * `:timeout` - sets the timeout for the tests in milliseconds, defaults to `60_000`;

    * `:trace` - sets ExUnit into trace mode, this sets `:max_cases` to `1` and
      prints each test case and test while running. Note that in trace mode test
      timeouts will be ignored as timeout is set to `:infinity`;

    * `:test_location_relative_path` - the test location is the file:line information
      printed by tests as a shortcut to run a given test. When this value is set,
      the value is used as a prefix for the test itself. This is typically used by
      Mix to properly set-up umbrella projects;

  Any arbitrary configuration can also be passed to `configure/1` or `start/1`,
  and these options can then be used in places such as custom formatters. These
  other options will be ignored by ExUnit itself.
  """
  @spec configure(Keyword.t()) :: :ok
  def configure(options) when is_list(options) do
    Enum.each(options, fn {k, v} ->
      Application.put_env(:ex_unit, k, v)
    end)
  end

  @doc """
  Returns ExUnit configuration.

  For the available configuration options, see `configure/1`.
  """
  @spec configuration() :: Keyword.t()
  def configuration do
    Application.get_all_env(:ex_unit)
    |> put_seed()
    |> put_slowest()
    |> put_max_cases()
  end

  @doc """
  Returns the pluralization for `word`.

  If one is not registered, returns the word appended with an "s".
  """
  @spec plural_rule(binary) :: binary
  def plural_rule(word) when is_binary(word) do
    Application.get_env(:ex_unit, :plural_rules, %{})
    |> Map.get(word, "#{word}s")
  end

  @doc """
  Registers a `pluralization` for `word`.

  If one is already registered, it is replaced.
  """
  @spec plural_rule(binary, binary) :: :ok
  def plural_rule(word, pluralization) when is_binary(word) and is_binary(pluralization) do
    plural_rules =
      Application.get_env(:ex_unit, :plural_rules, %{})
      |> Map.put(word, pluralization)

    configure(plural_rules: plural_rules)
  end

  @doc """
  Runs the tests. It is invoked automatically
  if ExUnit is started via `start/1`.

  From Elixir v1.14, it accepts an optional list of modules to run
  as part of the suite. This is often used to rerun modules already
  loaded in memory.

  Returns a map containing the total number of tests, the number
  of failures, the number of excluded tests and the number of skipped tests.
  """
  @spec run([module()]) :: suite_result()
  def run(additional_modules \\ []) do
    for module <- additional_modules do
      if Code.ensure_loaded?(module) and function_exported?(module, :__ex_unit__, 1) do
        ExUnit.Server.add_module(module, module.__ex_unit__(:config))
      else
        raise(ArgumentError, "#{inspect(module)} is not a ExUnit.Case module")
      end
    end

    _ = ExUnit.Server.modules_loaded(additional_modules != [])
    seed = Application.get_env(:ex_unit, :seed)
    options = persist_defaults(configuration())
    maybe_repeated_run(options, seed, nil)
  end

  @doc """
  Starts tests asynchronously while test cases are still loading.

  It returns a task that must be given to `await_run/0` when a result
  is desired.
  """
  @doc since: "1.12.0"
  @spec async_run() :: Task.t()
  def async_run() do
    seed = Application.get_env(:ex_unit, :seed)
    options = persist_defaults(configuration())

    Task.async(fn ->
      maybe_repeated_run(options, seed, nil)
    end)
  end

  @doc """
  Awaits for a test suite that has been started with `async_run/0`.
  """
  @doc since: "1.12.0"
  @spec await_run(Task.t()) :: suite_result()
  def await_run(task) do
    ExUnit.Server.modules_loaded(false)
    Task.await(task, :infinity)
  end

  @doc """
  Sets a callback to be executed after the completion of a test suite.

  Callbacks set with `after_suite/1` must accept a single argument, which is a
  map containing the results of the test suite's execution.

  If `after_suite/1` is called multiple times, the callbacks will be called in
  reverse order. In other words, the last callback set will be the first to be
  called.
  """
  @doc since: "1.8.0"
  @spec after_suite((suite_result() -> any)) :: :ok
  def after_suite(function) when is_function(function) do
    current_callbacks = Application.fetch_env!(:ex_unit, :after_suite)
    configure(after_suite: [function | current_callbacks])
  end

  @doc """
  Fetches the test supervisor for the current test.

  Returns `{:ok, supervisor_pid}` or `:error` if not called from the test process.

  This is the same supervisor as used by `ExUnit.Callbacks.start_supervised/2`
  and similar, see `ExUnit.Callbacks` module documentation for more information.
  """
  @doc since: "1.11.0"
  @spec fetch_test_supervisor() :: {:ok, pid()} | :error
  def fetch_test_supervisor() do
    case ExUnit.OnExitHandler.get_supervisor(self()) do
      {:ok, nil} ->
        {:ok, sup} = ExUnit.OnExitHandler.Supervisor.start_link([])
        ExUnit.OnExitHandler.put_supervisor(self(), sup)
        {:ok, sup}

      {:ok, _} = ok ->
        ok

      :error ->
        :error
    end
  end

  # Persists default values in application
  # environment before the test suite starts.
  defp persist_defaults(config) do
    config |> Keyword.take([:max_cases, :seed, :trace]) |> configure()
    config
  end

  defp maybe_repeated_run(options, seed, load_us) do
    repeat = Keyword.fetch!(options, :repeat_until_failure)
    maybe_repeated_run(options, seed, load_us, repeat)
  end

  defp maybe_repeated_run(options, seed, load_us, repeat) do
    case ExUnit.Runner.run(options, load_us) do
      {%{failures: 0}, {async_modules, sync_modules}}
      when repeat > 0 and (sync_modules != [] or async_modules != []) ->
        ExUnit.Server.restore_modules(async_modules, sync_modules)

        # Clear the seed if it was generated
        if seed == nil do
          Application.delete_env(:ex_unit, :seed)
        end

        # Re-run configuration
        options = persist_defaults(configuration())
        maybe_repeated_run(options, seed, load_us, repeat - 1)

      {stats, _} ->
        stats
    end
  end

  defp put_seed(opts) do
    Keyword.put_new_lazy(opts, :seed, fn ->
      # We're using `rem System.system_time()` here
      # instead of directly using :os.timestamp or using the
      # :microsecond argument because the VM on Windows has odd
      # precision. Calling with :microsecond will give us a multiple
      # of 1000. Calling without it gives actual microsecond precision.
      System.system_time()
      |> System.convert_time_unit(:native, :microsecond)
      |> rem(1_000_000)
    end)
  end

  defp put_max_cases(opts) do
    Keyword.put(opts, :max_cases, max_cases(opts))
  end

  defp put_slowest(opts) do
    if opts[:slowest] > 0 or opts[:slowest_modules] > 0 do
      Keyword.put(opts, :trace, true)
    else
      opts
    end
  end

  defp max_cases(opts) do
    cond do
      opts[:trace] -> 1
      max = opts[:max_cases] -> max
      true -> System.schedulers_online() * 2
    end
  end
end
