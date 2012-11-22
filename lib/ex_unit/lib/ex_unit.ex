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
        # 3) Notice we pass async: true, this runs the test case in parallel
        use ExUnit.Case, async: true

        # 4) A test is a method which name finishes with _test
        def test_always_pass do
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

  ## Assertions

  Check ExUnit.Assertions for assertions documentation.

  """

  @doc """
  Start ExUnit. Required to be invoked before loading
  any file that uses ExUnit.Case. Check `configure/1`
  to see the supported options.

  This function will also try to read a user config from the following
  locations, in this order:

  * $EXUNIT_CONFIG environment variable
  * $HOME/.ex_unit.exs

  If none found, no user config will be read. 

  User config is an elixir file which should return a keyword list
  with ex_unit options. Please note that explicit options passed to start/1 
  will take precedence over user options.

  # User config example (~/.ex_unit.exs)

    [formatter: ExUnit.Formatter.ANSI]

  """
  def start(options // []) do
    options = Keyword.merge(user_options, options)    
    ExUnit.Server.start_link
    configure(options)
    System.at_exit fn status ->
      if status == 0 do
        failures = ExUnit.run
        if failures > 0, do: System.halt(1), else: System.halt(0)
      end
    end
  end

  @doc """
  Returns the configured user options.
  """
  def user_options(user_config // nil) do
    user_config = user_config ||
      System.get_env("EXUNIT_CONFIG") ||
      File.join(System.get_env("HOME"), ".ex_unit.exs")

    case File.read(user_config) do
      { :ok, contents } ->
        { config, _ } = Code.eval(contents, [], file: user_config)
        config
      _ ->
        []
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
  Register a callback to be invoked every time a
  new ExUnit process is spawned.
  """
  def after_spawn(callback) do
    ExUnit.Server.add_after_spawn(callback)
  end

  @doc """
  API used to run the tests. A developer does not
  need to call it directly.
  """
  def run do
    ExUnit.Runner.run ExUnit.Server.options
  end
end
