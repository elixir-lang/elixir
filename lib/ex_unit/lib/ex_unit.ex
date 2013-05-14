defmodule ExUnit do
  defrecord Test, [:name, :case, :failure, :invalid] do
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
  Basic unit test structure for Elixir.

  ## Example

  A basic setup for ExUnit is shown below:

      # File: assertion_test.exs

      # 1) Start ExUnit. You can pass some options as argument (list below)
      ExUnit.start

      # 2) Next we create a new TestCase and use ExUnit.Case
      defmodule AssertionTest do
        # 3) Notice we pass async: true, this runs the test case
        #    concurrently with other test cases
        use ExUnit.Case, async: true

        # 4) A test is a function whose name starts with
        #    test and receives a context
        def test_always_pass(_) do
          assert true
        end

        # 5) It is recommended to use the test macro instead of def
        test "the truth" do
          assert true
        end
      end

  To run the test above, all you need to to is to run the file
  using elixir from command line. Assuming you named your file
  assertion_test.exs, you can run it as:

      bin/elixir assertion_test.exs

  ## Case, callbacks and assertions

  Check `ExUnit.Case` and `ExUnit.Callbacks` for more information about
  defining test cases.

  The `ExUnit.Assertions` module contains a set of macros to easily
  generate assertions with appropriate error messages.

  """

  use Application.Behaviour

  @doc false
  def start(_type, []) do
    ExUnit.Sup.start_link([])
  end

  @doc """
  Starts up ExUnit and automatically set it up to run
  tests at the VM exit. It accepts a set of options to
  configure `ExUnit` (the same ones accepted by `configure/1`).

  In case you want to run tests manually, skip calling this
  function and rely on `configure/1` and `run/0` instead.
  """
  def start(options // []) do
    :application.start(:elixir)
    :application.start(:ex_unit)

    configure(options)

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

  @doc """
  Configures ExUnit.

  ## Options

  ExUnit supports the following options given to start:

  * `:formatter` - The formatter that will print results.
                   Defaults to `ExUnit.CLIFormatter`;

  * `:max_cases` - Maximum number of cases to run in parallel.
                   Defaults to `:erlang.system_info(:schedulers_online)`;

  """
  def configure(options) do
    ExUnit.Server.merge_options(options)
  end

  @doc """
  API used to run the tests. It is invoked automatically
  if ExUnit is started via `ExUnit.start`.

  Returns the number of failures.
  """
  def run do
    { async, sync } = ExUnit.Server.cases
    ExUnit.Runner.run async, sync, ExUnit.Server.options
  end
end
