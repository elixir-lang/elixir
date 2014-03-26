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

        # 4) A test is a function whose name starts with
        #    `test` and receives a context.
        def test_always_pass(_) do
          assert true
        end

        # 5) Use the `test` macro instead of `def` for clarity.
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
  @type state   :: nil | :passed | { :failed, failed } | { :skip, binary } | { :invalid, invalid }
  @type failed  :: { :error | :exit | :throw | :EXIT, reason :: term, stacktrace :: [tuple] }
  @type invalid :: module

  defrecord Test, [:name, :case, :state, :time, :tags] do
    @moduledoc """
    A record that keeps information about the test.
    It is received by formatters and also accessible
    in the metadata under the key `:test`.
    """
    record_type name: atom, case: module, state: ExUnit.state,
                time: non_neg_integer, tags: Keyword.t
  end

  defrecord TestCase, [:name, :state, :tests] do
    @moduledoc """
    A record that keeps information about the test case.
    It is received by formatters and also accessible
    in the metadata under the key `:case`.
    """
    record_type name: module, state: ExUnit.state, tests: [ExUnit.Test.t]
  end

  use Application.Behaviour

  @doc false
  def start(_type, []) do
    pid = ExUnit.Sup.start_link
    ExUnit.Server.start_load
    pid
  end

  @doc """
  Starts ExUnit and automatically runs tests right before the
  VM terminates. It accepts a set of options to configure `ExUnit`
  (the same ones accepted by `configure/1`).

  If you want to run tests manually, you can set `:autorun` to `false`.
  """
  def start(options \\ []) do
    :application.start(:elixir)
    :application.start(:ex_unit)

    configure(options)

    if :application.get_env(:ex_unit, :autorun) != { :ok, false } do
      :application.set_env(:ex_unit, :autorun, false)

      System.at_exit fn
        0 ->
          %{failures: failures} = ExUnit.run
          System.at_exit fn _ ->
            if failures > 0, do: System.halt(1)
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

  * `:color` - When color should be used by specific formatters.
               Defaults to the result of `IO.ANSI.terminal?/1`;

  * `:formatters` - The formatters that will print results.
                    Defaults to `[ExUnit.CLIFormatter]`;

  * `:max_cases` - Maximum number of cases to run in parallel.
                   Defaults to `:erlang.system_info(:schedulers_online)`;

  * `:trace` - Set ExUnit into trace mode, this sets `:max_cases` to `1`
               and prints each test case and test while running;

  * `:autorun` - If ExUnit should run by default on exit, defaults to `true`;

  * `:include` - Specify which tests are run by skipping tests that do not match the filter

  * `:exclude` - Specify which tests are run by skipping tests that match the filter

  * `:seed` - An integer seed value to randomize the test suite
  """
  def configure(options) do
    Enum.each options, fn { k, v } ->
      :application.set_env(:ex_unit, k, v)
    end
  end

  @doc """
  Returns ExUnit configuration.
  """
  def configuration do
    :application.get_all_env(:ex_unit)
  end

  @doc """
  API used to run the tests. It is invoked automatically
  if ExUnit is started via `ExUnit.start/1`.

  Returns the number of failures.
  """
  def run do
    { async, sync, load_us } = ExUnit.Server.start_run
    ExUnit.Runner.run async, sync, configuration, load_us
  end
end
