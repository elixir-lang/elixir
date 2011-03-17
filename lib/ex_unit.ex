% Basic unit test structure for Elixir.
%
% ## Example
%
% A basic setup for ExUnit is shown below:
%
%     % 1) Require to configure ExUnit. See a list of options below.
%     ExUnit.configure
%
%     % 2) Next we create a new TestCase and add ExUnit::Case to it
%     object AssertionTest
%       proto ExUnit::Case
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
%     bin/exunit assertion_test.ex
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
%     object QueryTest
%       proto ExUnit::Case
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
% 2) The `setup` method needs to necessarily return an object of the same kind
%    of the test case. For instance, the following is wrong:
%
%     def setup
%       [1,2,3]
%     end
%
% ## Options
%
% ExUnit supports the following options given to configure:
%
% * `:formatter` - The formatter that will print results
%
module ExUnit
  proto Code::Formatter

  def configure(options)
    ExUnit::Server.start(options)
  end

  def run
    cases = ExUnit::Server.cases
    options = ExUnit::Server.options
    formatter = options['formatter] || ExUnit::Formatter.new
    new_formatter = run(formatter, cases)
    new_formatter.finish
  end

  private

  % run/3 instantiates each object given in the list and execute
  % each test in these objects. counter keeps all tests executed
  % and failures all failures with their backtrace.
  def run(formatter, [])
    formatter
  end

  def run(formatter, [object|t])
    instance = object.to_constant.new
    new_formatter = run_tests(formatter, object, instance, instance.__tests__)
    run(new_formatter, t)
  end

  % For each instanciated object, dispatch each test in it.
  def run_tests(formatter, object, instance, [test|t])
    final = try
      subject = instance.setup(test)

      partial = try
        subject.__send__(test)
        nil
      catch kind1: error1
        {kind1, error1, self.__stacktrace__}
      end

      subject.teardown(test)
      partial
    catch kind2: error2
      {kind2, error2, self.__stacktrace__}
    end

    new_formatter = formatter.each(object, test, final)
    run_tests(new_formatter, object, instance, t)
  end

  def run_tests(formatter, _, _, [])
    formatter
  end
end

Code.require "ex_unit/assertions"
Code.require "ex_unit/case"
Code.require "ex_unit/server"
Code.require "ex_unit/formatter"