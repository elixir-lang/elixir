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
        # 3) Notice we pass "async: true", this runs the test case
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

      elixir assertion_test.exs

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
  files. See `Mix.Tasks.Test` for more information.
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
          nil | {:failed, failed} | {:skipped, binary} | {:excluded, binary} | {:invalid, module}

  @typedoc "The error state returned by `ExUnit.Test` and `ExUnit.TestModule`"
  @type failed :: [{Exception.kind(), reason :: term, Exception.stacktrace()}]

  @typedoc "A map representing the results of running a test suite"
  @type suite_result :: %{
          excluded: non_neg_integer,
          failures: non_neg_integer,
          skipped: non_neg_integer,
          total: non_neg_integer
        }

  defmodule Test do
    @moduledoc """
    A struct that keeps information about the test.

    It is received by formatters and contains the following fields:

      * `:name` - the test name
      * `:module` - the test module
      * `:state` - the finished test state (see `t:ExUnit.state/0`)
      * `:time` - the time to run the test
      * `:tags` - the test tags
      * `:logs` - the captured logs

    """
    defstruct [:name, :case, :module, :state, time: 0, tags: %{}, logs: ""]

    # TODO: Remove the `:case` field on Elixir v2.0
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
    A struct that keeps information about the test case.

    It is received by formatters and contains the following fields:

      * `:name`  - the test case name
      * `:state` - the test error state (see `t:ExUnit.state/0`)
      * `:tests` - all tests for this case

    """
    defstruct [:name, :state, tests: []]

    @type t :: %__MODULE__{name: module, state: ExUnit.state(), tests: [ExUnit.Test.t()]}
  end

  defmodule TestCase do
    # TODO: Remove this on Elixir v2.0 as well as uses of it.
    @moduledoc false
    defstruct [:name, :state, tests: []]

    @type t :: %__MODULE__{name: module, state: ExUnit.state(), tests: [ExUnit.Test.t()]}
  end

  defmodule TimeoutError do
    defexception [:timeout, :type]

    @impl true
    def message(%{timeout: timeout, type: type}) do
      """
      #{type} timed out after #{timeout}ms. You can change the timeout:

        1. per test by setting "@tag timeout: x"
        2. per case by setting "@moduletag timeout: x"
        3. globally via "ExUnit.start(timeout: x)" configuration
        4. or set it to infinity per run by calling "mix test --trace"
           (useful when using IEx.pry/0)

      Timeouts are given as integers in milliseconds.
      """
    end
  end

  use Application

  @doc false
  def start(_type, []) do
    children = [
      ExUnit.Server,
      ExUnit.CaptureServer,
      ExUnit.OnExitHandler
    ]

    opts = [strategy: :one_for_one, name: ExUnit.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @doc """
  Starts ExUnit and automatically runs tests right before the
  VM terminates.

  It accepts a set of options to configure `ExUnit`
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
          time = ExUnit.Server.modules_loaded()
          config = persist_defaults(configuration())
          %{failures: failures} = ExUnit.Runner.run(config, time)

          System.at_exit(fn _ ->
            if failures > 0, do: exit({:shutdown, 1})
          end)

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
      calls, defaults to `100` milliseconds;

    * `:autorun` - if ExUnit should run by default on exit. Defaults to `true`;

    * `:capture_log` - if ExUnit should default to keeping track of log messages
      and print them on test failure. Can be overridden for individual tests via
      `@tag capture_log: false`. Defaults to `false`;

    * `:colors` - a keyword list of colors to be used by some formatters.
      The only option so far is `[enabled: boolean]` which defaults to `IO.ANSI.enabled?/0`;

    * `:exclude` - specifies which tests are run by skipping tests that match the
      filter;

    * `:failures_manifest_file` - specifies a path to the file used to store failures
      between runs;

    * `:formatters` - the formatters that will print results,
      defaults to `[ExUnit.CLIFormatter]`;

    * `:include` - specifies which tests are run by skipping tests that do not
      match the filter. Keep in mind that all tests are included by default, so unless they are
      excluded first, the `:include` option has no effect. To only run the tests
      that match the `:include` filter, exclude the `:test` tag first (see the
      documentation for `ExUnit.Case` for more information on tags);

    * `:max_cases` - maximum number of tests to run in parallel. Only tests from
      different modules run in parallel. It defaults to `System.schedulers_online * 2`
      to optimize both CPU-bound and IO-bound tests;

    * `:module_load_timeout` - the timeout to be used when loading a test module,
      defaults to `60_000` milliseconds;

    * `:only_test_ids` - a list of `{module_name, test_name}` tuples that limits
      what tests get run;

    * `:refute_receive_timeout` - the timeout to be used on `refute_receive`
      calls, defaults to `100` milliseconds;

    * `:print_seed` - whether to print out the randomized seed when the test suite
      finishes. This is enabled by default;

    * `:seed` - an integer seed value to randomize the test suite. This seed
      is also mixed with the test module and name to create a new unique seed
      on every test, which is automatically fed into the `:rand` module. This
      provides randomness between tests, but predictable and reproducible results;

    * `:slowest` - prints timing information for the N slowest tests. Running
      ExUnit with slow test reporting automatically runs in `trace` mode. It
      is disabled by default;

    * `:stacktrace_depth` - configures the stacktrace depth to be used
      on formatting and reporters, defaults to `20`;

    * `:timeout` - sets the timeout for the tests, defaults to `60_000` milliseconds;

    * `:trace` - sets ExUnit into trace mode, this sets `:max_cases` to `1` and
      prints each test case and test while running. Note that in trace mode test timeouts
      will be ignored.

  Any arbitrary configuration can also be passed to `configure/1` or `start/1`,
  and these options can then be used in places such as custom formatters. These
  other options will be ignored by ExUnit itself.
  """
  @spec configure(Keyword.t()) :: :ok
  def configure(options) do
    Enum.each(options, fn {k, v} ->
      Application.put_env(:ex_unit, k, v)
    end)
  end

  @doc """
  Returns ExUnit configuration.
  """
  @spec configuration() :: Keyword.t()
  def configuration do
    Application.get_all_env(:ex_unit)
    |> put_seed()
    |> put_print_seed()
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

  Returns a map containing the total number of tests, the number
  of failures, the number of excluded tests and the number of skipped tests.
  """
  @spec run() :: suite_result()
  def run do
    config = persist_defaults(configuration())
    ExUnit.Runner.run(config, nil)
  end

  @doc """
  Sets a callback to be executed after the completion of a test suite.

  Callbacks set with `after_suite/1` must accept a single argument, which is a
  map containing the results of the test suite's execution.

  If `after_suite/1` is called multiple times, the callbacks will be called in
  reverse order. In other words, the last callback set will be the first to be
  called.
  """
  @spec after_suite((suite_result() -> any)) :: :ok
  def after_suite(function) when is_function(function) do
    current_callbacks = Application.fetch_env!(:ex_unit, :after_suite)
    configure(after_suite: [function | current_callbacks])
  end

  # Persists default values in application
  # environment before the test suite starts.
  defp persist_defaults(config) do
    config |> Keyword.take([:max_cases, :seed, :trace]) |> configure()
    config
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

  defp put_print_seed(opts) do
    Keyword.put_new(opts, :print_seed, true)
  end

  defp put_max_cases(opts) do
    Keyword.put(opts, :max_cases, max_cases(opts))
  end

  defp put_slowest(opts) do
    if opts[:slowest] > 0 do
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
