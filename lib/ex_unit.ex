% Basic unit test structure for Elixir.
%
% ## Example
%
% A basic setup for ExUnit is shown below:
%
%     % File: assertion_test.exs
%
%     % 1) If you wish to configure ExUnit. See a list of options below.
%     ExUnit.configure
%
%     % 2) Next we create a new TestCase and add ExUnit::Case to it
%     module AssertionTest
%       mixin ExUnit::Case
%
%       % 3) A test is a method which name finishes with _test
%       def always_pass_test
%         % 4) You can get most of your tests done with pattern matching
%         true = true
%       end
%    end
%
% To run the test above, all you need to to is to use the bin/exunit script that ships with Elixir.
% Assuming you named your file assertion_test.ex, you can run it as:
%
%     bin/exunit assertion_test.exs
%
% ## Assertions
%
% Most of ExUnit assertions can be done with pattern matching. However, there are
% a few assertions over ExUnit::Assertions to aid testing.
%
% ## Callbacks
%
% ExUnit provides `setup` and `teardown` callbacks before and after running each test.
%
% For instance, imagine you have to connection to a database before each test and
% disconnect from it after each test is executed, regardless if it failed or not.
% You can do it as follow:
%
%     module QueryTest
%       mixin ExUnit::Case
%
%       def setup(_)
%         @('connection, Database.connection)
%       end
%
%       def query_test
%         "2" = @connection.query("SELECT 1 + 1")
%       end
%
%       def teardown(_)
%         @connection.disconnect
%       end
%     end
%
% It is important to notice two things:
%
% 1) Both `setup` and `teardown` methods receives an atom with the name of test
%    begin executed. This allows you to specialize the behavior for one specific test.
%
% 2) The `setup` method needs to necessarily return a data type of the same kind
%    of the test case (self). For instance, the following is wrong:
%
%     def setup
%       MyModule.something
%     end
%
%    However the following works:
%
%     def setup
%       MyModule.something
%       self
%     end
%
% ## Options
%
% ExUnit supports the following options given to configure:
%
% * `'formatter` - The formatter that will print results
% * `'max_cases` - Maximum number of cases to run in parallel
%
module ExUnit
  def start
    ExUnit::Server.start
  end

  def configure(options)
    ExUnit::Server.merge_options(options)
  end

  def run
    cases     = ExUnit::Server.cases
    options   = ExUnit::Server.options
    formatter = options['formatter] || #ExUnit::Formatter()
    max_cases = options['max_cases] || 4
    #ExUnit::Runner(formatter, cases, max_cases).start
  end
end