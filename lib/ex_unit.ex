defmodule ExUnit do
  @moduledoc """
  Basic unit test structure for Elixir.

  ## Example

  A basic setup for ExUnit is shown below:

      # File: assertion_test.exs

      # 1) Start ExUnit. You can pass some options as argument (list below)
      ExUnit.start

      # 2) Next we create a new TestCase and use ExUnit.Case
      defmodule AssertionTest do
        use ExUnit.Case

        # 3) A test is a method which name finishes with _test
        def test_always_pass
          assert true
        end
      end

  To run the test above, all you need to to is to run the file
  using elixir from command line. Assuming you named your file
  assertion_test.exs, you can run it as:

      bin/elixir assertion_test.exs

  ## Assertions

  Check ExUnit.Assertions for assertions documentation.

  """

  @doc """
  Start ExUnit. Required to be invoked before loading
  any file that uses ExUnit.Case. Check `configure/1`
  to see the supported options.
  """
  def start(options // []) do
    ExUnit.Server.start_link
    configure(options)
    System.at_exit fn(status) ->
      if status == 0, do: ExUnit.run
    end
  end

  @doc """
  Configure ExUnit.

  ## Options

  ExUnit supports the following options given to start:

  * `:formatter` - The formatter that will print results
  * `:max_cases` - Maximum number of cases to run in parallel

  """
  def configure(options) do
    ExUnit.Server.merge_options(options)
  end

  @doc """
  API used to run the tests. A developer does not
  need to call it directly.
  """
  def run do
    config = ExUnit.Runner.Config.new ExUnit.Server.options
    config = config.formatter(config.formatter.start)
    failures = ExUnit.Runner.start config
    if failures > 0, do: halt(1), else: halt(0)
  end
end
