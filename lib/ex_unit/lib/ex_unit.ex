defmodule ExUnit do
  @moduledoc """
  Basic unit testing framework for Elixir.

  ## Example

  A basic setup for ExUnit is shown below:

      # File: assertion_test.exs

      # 1) Start ExUnit.
      ExUnit.start

      # 2) Create a new test module (test case) and use `ExUnit.Case`.
      defmodule AssertionTest do
        # 3) Notice we pass `async: true`, this runs the test case
        #    concurrently with other test cases
        use ExUnit.Case, async: true

        # 4) Use the `test` macro instead of `def` for clarity.
        test "the truth" do
          assert true
        end
      end

  To run the tests above, run the file
  using `elixir` from the command line. Assuming you named the file
  `assertion_test.exs`, you can run it as:

      bin/elixir assertion_test.exs

  ## Case, Callbacks and Assertions

  See `ExUnit.Case` and `ExUnit.Callbacks`
  for more information about defining test cases.

  The `ExUnit.Assertions` module contains
  a set of macros to easily generate assertions with appropriate
  error messages.

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

  @typedoc "The state returned by ExUnit.Test and ExUnit.TestCase"
  @type state  :: nil | {:failed, failed} | {:skip, binary} | {:invalid, module}
  @type failed :: {Exception.kind, reason :: term, stacktrace :: [tuple]}

  defmodule Test do
    @moduledoc """
    A struct that keeps information about the test.

    It is received by formatters and contains the following fields:

      * `:name`  - the test name
      * `:case`  - the test case
      * `:state` - the test state (see ExUnit.state)
      * `:time`  - the time to run the test
      * `:tags`  - the test tags

    """
    defstruct name: nil,
              case: nil,
              state: nil,
              time: 0,
              tags: %{}

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
      * `:state` - the test state (see ExUnit.state)
      * `:tests` - all tests for this case

    """
    defstruct name: nil,
              state: nil,
              tests: []

    @type t :: %__MODULE__{
                 name: module,
                 state: ExUnit.state,
                 tests: [ExUnit.Test.t]}
  end

  defmodule TimeoutError do
    defexception [:timeout]

    def message(timeout)
    def message(%{timeout: timeout}) do
      "test timed out after #{timeout}ms (you can change the test timeout " <>
        "by setting \"@tag timeout: x\" where x is an integer in milliseconds)"
    end
  end

  use Application

  @doc false
  def start(_type, []) do
    import Supervisor.Spec

    children = [
      worker(ExUnit.Server, []),
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

    if Application.get_env(:ex_unit, :autorun, true) do
      Application.put_env(:ex_unit, :autorun, false)

      System.at_exit fn
        0 ->
          %{failures: failures} = ExUnit.run
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

    * `:colors` - a keyword list of colors to be used by some formatters.
      The only option so far is `[enabled: boolean]` which defaults to `IO.ANSI.enabled?/1`

    * `:formatters` - the formatters that will print results;
      defaults to `[ExUnit.CLIFormatter]`

    * `:max_cases` - maximum number of cases to run in parallel;
      defaults to `:erlang.system_info(:schedulers_online)`

    * `:trace` - set ExUnit into trace mode, this sets `:max_cases` to `1` and
      prints each test case and test while running

    * `:autorun` - if ExUnit should run by default on exit; defaults to `true`

    * `:include` - specify which tests are run by skipping tests that do not
      match the filter

    * `:exclude` - specify which tests are run by skipping tests that match the
      filter

    * `:seed` - an integer seed value to randomize the test suite

    * `:timeout` - set the timeout for the tests
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
  API used to run the tests. It is invoked automatically
  if ExUnit is started via `ExUnit.start/1`.

  Returns a map containing the total number of tests, the number
  of failures and the number of skipped tests.
  """
  def run do
    {async, sync, load_us} = ExUnit.Server.start_run
    ExUnit.Runner.run async, sync, configuration, load_us
  end
end
