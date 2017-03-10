defmodule ExUnit do
  @moduledoc """
  Unit testing framework for Elixir.

  ## Example

  A basic setup for ExUnit is shown below:

      # File: assertion_test.exs

      # 1) Start ExUnit.
      ExUnit.start

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
      ExUnit.start

  Mix will load the `test_helper.exs` file before executing the tests.
  It is not necessary to `require` the `test_helper.exs` file in your test
  files. See `Mix.Tasks.Test` for more information.
  """

  @typedoc "The error state returned by ExUnit.Test and ExUnit.TestCase"
  @type state  :: nil | {:failed, failed} | {:skip, binary} | {:invalid, module}
  @type failed :: [{Exception.kind, reason :: term, stacktrace :: [tuple]}]

  defmodule Test do
    @moduledoc """
    A struct that keeps information about the test.

    It is received by formatters and contains the following fields:

      * `:name`  - the test name
      * `:case`  - the test case
      * `:state` - the test error state (see ExUnit.state)
      * `:time`  - the time to run the test
      * `:tags`  - the test tags
      * `:logs`  - the captured logs

    """
    defstruct [:name, :case, :state, time: 0, tags: %{}, logs: ""]

    @type t :: %__MODULE__{
                 name: atom,
                 case: module,
                 state: ExUnit.state,
                 time: non_neg_integer,
                 tags: map}
  end

  defmodule TestCase do
    @moduledoc """
    A struct that keeps information about the test case.

    It is received by formatters and contains the following fields:

      * `:name`  - the test case name
      * `:state` - the test error state (see ExUnit.state)
      * `:tests` - all tests for this case

    """
    defstruct [:name, :state, tests: []]

    @type t :: %__MODULE__{
                 name: module,
                 state: ExUnit.state,
                 tests: [ExUnit.Test.t]}
  end

  defmodule TimeoutError do
    defexception [:timeout, :type]

    def message(%{timeout: timeout, type: type}) do
      """
      #{type} timed out after #{timeout}ms. You can change the timeout:

        1. per test by setting "@tag timeout: x"
        2. per case by setting "@moduletag timeout: x"
        3. globally via "ExUnit.start(timeout: x)" configuration
        4. or set it to infinity per run by calling "mix test --trace"
           (useful when using IEx.pry)

      Timeouts are given as integers in milliseconds.
      """
    end
  end

  use Application

  @doc false
  def start(_type, []) do
    import Supervisor.Spec

    children = [
      worker(ExUnit.Server, []),
      worker(ExUnit.CaptureServer, []),
      worker(ExUnit.OnExitHandler, [])
    ]

    opts = [strategy: :one_for_one, name: ExUnit.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @doc """
  Starts ExUnit and automatically runs tests right before the
  VM terminates. It accepts a set of options to configure `ExUnit`
  (the same ones accepted by `configure/1`).

  If you want to run tests manually, you can set `:autorun` to `false`.
  """
  def start(options \\ []) do
    {:ok, _} = Application.ensure_all_started(:ex_unit)

    configure(options)
    configure_defaults(options)

    if Application.fetch_env!(:ex_unit, :autorun) do
      Application.put_env(:ex_unit, :autorun, false)

      System.at_exit fn
        0 ->
          time = ExUnit.Server.cases_loaded()
          %{failures: failures} = ExUnit.Runner.run(configuration(), time)
          System.at_exit fn _ ->
            if failures > 0, do: exit({:shutdown, 1})
          end
        _ ->
          :ok
      end
    end
  end

  @doc """
  Configures ExUnit.

  ## Options

  ExUnit supports the following options:

    * `:assert_receive_timeout` - the timeout to be used on `assert_receive`
      calls. Defaults to 100ms.

    * `:autorun` - if ExUnit should run by default on exit; defaults to `true`

    * `:capture_log` - if ExUnit should default to keeping track of log messages
      and print them on test failure. Can be overridden for individual tests via
      `@tag capture_log: false`. Defaults to `false`.

    * `:case_load_timeout` - the timeout to be used when loading a test case.
      Defaults to `60_000` milliseconds.

    * `:colors` - a keyword list of colors to be used by some formatters.
      The only option so far is `[enabled: boolean]` which defaults to `IO.ANSI.enabled?/0`

    * `:exclude` - specifies which tests are run by skipping tests that match the
      filter

    * `:formatters` - the formatters that will print results;
      defaults to `[ExUnit.CLIFormatter]`

    * `:include` - specifies which tests are run by skipping tests that do not
      match the filter. Keep in mind that all tests are included by default, so unless they are
      excluded first, the `:include` option has no effect.

    * `:max_cases` - maximum number of cases to run in parallel;
      defaults to `:erlang.system_info(:schedulers_online) * 2` to
      optimize both CPU-bound and IO-bound tests

    * `:refute_receive_timeout` - the timeout to be used on `refute_receive`
      calls (defaults to 100ms)

    * `:seed` - an integer seed value to randomize the test suite

    * `:stacktrace_depth` - configures the stacktrace depth to be used
      on formatting and reporters (defaults to 20)

    * `:timeout` - sets the timeout for the tests (default 60_000ms)

    * `:trace` - sets ExUnit into trace mode, this sets `:max_cases` to `1` and
      prints each test case and test while running
  """
  def configure(options) do
    Enum.each options, fn {k, v} ->
      Application.put_env(:ex_unit, k, v)
    end
  end

  @doc """
  Returns ExUnit configuration.
  """
  def configuration do
    Application.get_all_env(:ex_unit)
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
  API used to run the tests. It is invoked automatically
  if ExUnit is started via `ExUnit.start/1`.

  Returns a map containing the total number of tests, the number
  of failures and the number of skipped tests.
  """
  def run do
    ExUnit.Runner.run(configuration(), nil)
  end

  # Configures on demand defaults
  defp configure_defaults(options) do
    unless Keyword.has_key?(options, :seed) do
      Application.put_env(:ex_unit, :seed, :os.timestamp |> elem(2))
    end

    unless Keyword.has_key?(options, :max_cases) do
      Application.put_env(:ex_unit, :max_cases, max_cases(options))
    end
  end

  defp max_cases(opts) do
    if opts[:trace] do
      1
    else
      :erlang.system_info(:schedulers_online) * 2
    end
  end
end
