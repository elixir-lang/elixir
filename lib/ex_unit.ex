# Basic unit test structure for Elixir.
#
# ## Example
#
# A basic setup for ExUnit is shown below:
#
#     # File: assertion_test.exs
#
#     # 1) Start ExUnit. You can pass some options as argument (list below)
#     ExUnit.start
#
#     # 2) Next we create a new TestCase and add ExUnit.Case to it
#     defmodule AssertionTest do
#       use ExUnit.Case
#
#       # 3) A test is a method which name finishes with _test
#       def test_always_pass
#         true = true
#       end
#     end
#
# To run the test above, all you need to to is to use the bin/exunit
# script that ships with Elixir. Assuming you named your file
# assertion_test.exs, you can run it as:
#
#     bin/elixir assertion_test.exs
#
# ## Assertions
#
# Most of ExUnit assertions can be done with pattern matching.
# However, there are a few assertions over ExUnit.Assertions to aid testing.
#
# ## Options
#
# ExUnit supports the following options given to configure:
#
# * `:formatter` - The formatter that will print results
# * `:max_cases` - Maximum number of cases to run in parallel
#
defmodule ExUnit do
  def start(options // []) do
    ExUnit.Server.start_link
    configure(options)
    Code.at_exit fn(status) ->
      if status == 0, do: ExUnit.run
    end
  end

  def configure(options) do
    ExUnit.Server.merge_options(options)
  end

  def run do
    config = ExUnit.Runner.Config.new ExUnit.Server.options
    config = config.formatter(config.formatter.start)
    failures = ExUnit.Runner.start config
    if failures > 0, do: halt(1), else: halt(0)
  end
end