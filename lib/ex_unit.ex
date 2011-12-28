# Basic unit test structure for Elixir.
#
# ## Example
#
# A basic setup for ExUnit is shown below:
#
#     # File: assertion_test.exs
#
#     # 1) If you wish to configure ExUnit. See a list of options below.
#     ExUnit.configure
#
#     # 2) Next we create a new TestCase and add ExUnit::Case to it
#     module AssertionTest
#     ExUnit::Case.prepare
#
#     # 3) A test is a method which name finishes with _test
#     def test_always_pass
#       true = true
#     end
#
# To run the test above, all you need to to is to use the bin/exunit
# script that ships with Elixir. Assuming you named your file
# assertion_test.exs, you can run it as:
#
#     bin/exunit assertion_test.exs
#
# ## Assertions
#
# Most of ExUnit assertions can be done with pattern matching.
# However, there are a few assertions over ExUnit::Assertions to aid testing.
#
# ## Options
#
# ExUnit supports the following options given to configure:
#
# * `:formatter` - The formatter that will print results
# * `:max_cases` - Maximum number of cases to run in parallel
#
module ExUnit

def start do
  ExUnit::Server.start_link
end

def configure(options) do
  ExUnit::Server.merge_options(options)
end

def run do
  config = ExUnit::Runner::Config.new ExUnit::Server.options
  config = config.formatter(config.formatter.start)
  ExUnit::Runner.start config
end