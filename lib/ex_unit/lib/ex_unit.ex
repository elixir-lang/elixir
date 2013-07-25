defmodule ExUnit do
  defrecord Test, [:name, :case, :failure, :time] do
    @moduledoc """
    A record that keeps information about the test.
    It is received by formatters and also accessible
    in the metadata under the key `:test`.
    """
  end

  defrecord TestCase, [:name, :failure] do
    @moduledoc """
    A record that keeps information about the test case.
    It is received by formatters and also accessible
    in the metadata under the key `:case`.
    """
  end

  @moduledoc """
  Basic unit testing framework for Elixir.

  ## Example

  A basic setup for ExUnit is shown below:

      # File: assertion_test.exs

      # 1) Start ExUnit. You can pass some options
      #    (see `configure/1` for the list of options)
      ExUnit.start

      # 2) Create a new test module (test case) and use ExUnit.Case
      defmodule AssertionTest do
        # 3) Notice we pass async: true, this runs the test case
        #    concurrently with other test cases
        use ExUnit.Case, async: true

        # 4) A test is a function whose name starts with
        #    test and receives a context
        def test_always_pass(_) do
          assert true
        end

        # 5) It is recommended to use the "test" macro instead of def
        test "the truth" do
          assert true
        end
      end

  To run the test above, all you need to do is to run the file
  using `elixir` from the command line. Assuming you named your file
  `assertion_test.exs`, you can run it as:

      bin/elixir assertion_test.exs

  ## Case, callbacks and assertions

  See `ExUnit.Case` and `ExUnit.Callbacks` for more information about
  defining test cases.

  The `ExUnit.Assertions` module contains a set of macros to easily
  generate assertions with appropriate error messages.

  ## Integration with Mix

  Mix is the project management and build tool for Elixir. Invoking `mix test`
  from the command line will run tests in each file matching the pattern
  `*_test.exs` found in the `test` directory of your project.

  By convention, you could also create a `test_helper.exs` file inside the
  `test` directory and put the code common to all tests there.

  The minimum example of a `test_helper.exs` file would be:

      # test/test_helper.exs
      ExUnit.start

  Then, in each test file, require `test_helper.exs` before defining test modules
  (or cases):

      # test/myproject_test.exs
      Code.require_file "test_helper.exs", __DIR__

      # ... test cases follow

  """

  use Application.Behaviour

  @doc false
  def start(_type, []) do
    pid = ExUnit.Sup.start_link
    ExUnit.Server.start_load
    pid
  end

  @doc """
  Starts up ExUnit and automatically runs tests right before the
  VM terminates. It accepts a set of options to configure `ExUnit`
  (the same ones accepted by `configure/1`).

  If you want to run tests manually, skip calling this
  function and rely on `configure/1` and `run/0` instead.
  """
  def start(options // []) do
    :application.start(:elixir)
    :application.start(:ex_unit)

    configure(options)

    if :application.get_env(:ex_unit, :started) != { :ok, true } do
      :application.set_env(:ex_unit, :started, true)

      System.at_exit fn
        0 ->
          failures = ExUnit.run
          System.at_exit fn _ ->
            if failures > 0, do: System.halt(1), else: System.halt(0)
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

  * `:color` - When color should be used by specific formatters;

  * `:formatter` - The formatter that will print results.
                   Defaults to `ExUnit.CLIFormatter`;

  * `:max_cases` - Maximum number of cases to run in parallel.
                   Defaults to `:erlang.system_info(:schedulers_online)`;

  * `:trace` - Set ExUnit into trace mode, this sets `:max_cases` to 1
               and prints each test case and test while running;

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
  if ExUnit is started via `ExUnit.start`.

  Returns the number of failures.
  """
  def run do
    { async, sync, load_us } = ExUnit.Server.start_run
    ExUnit.Runner.run async, sync, configuration, load_us
  end
end
