% Basic unit test structure for Elixir.
%
% ## Example
%
% A basic setup for ExUnit is shown below:
%
%     % 1) Require to configure ExUnit
%     % Right now there are no configuration options yet, so we pass an empty dict
%     ExUnit.configure {:}
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
module ExUnit
  proto Code::Formatter

  def configure(options)
    ExUnit::Server.start(options)
  end

  def run
    cases = ExUnit::Server.cases
    { failures, counter } = run(cases, [], 0)
    print_results(failures, counter)
  end

  private

  % run/3 instantiates each object given in the list and execute
  % each test in these objects. counter keeps all tests executed
  % and failures all failures with their backtrace.
  def run([], failures, counter)
    { failures, counter }
  end

  def run([object|t], failures, counter)
    instance = object.to_constant.new
    { new_failures, new_counter } = run_tests(object, instance, instance.__tests__, failures, counter)
    run(t, new_failures, new_counter)
  end

  % For each instanciated object, dispatch each test in it.
  def run_tests(object, instance, [test|t], failures, counter)
    final_failures = try
      result = instance.setup(test)

      new_failures = try
        result.__send__(test)
        failures
      catch kind1: error1
        [{object, test, kind1, error1, self.__stacktrace__}|failures]
      end

      result.teardown(test)
      new_failures
    catch kind2: error2
      [{object, test, kind2, error2, self.__stacktrace__}|failures]
    end

    print_partial(failures, final_failures)

    run_tests(object, instance, t, final_failures, counter + 1)
  end

  def run_tests(_, _, [], failures, counter)
    { failures, counter }
  end

  def print_partial(failures, final_failures)
    if final_failures.size > failures.size
      IO.write "F"
    else
      IO.write "."
    end
  end

  def print_results(failures, counter)
    IO.puts "\n"
    failures.foldl 1, -> (x, acc) print_failure(x, acc)
    IO.puts "#{counter} tests, #{failures.length} failures."
  end

  % Print each failure that occurred.
  def print_failure({object, test, kind, reason, stacktrace}, acc)
    IO.puts "#{acc}) #{test}(#{object})\n  #{kind} #{self.format_catch(kind, reason)}\n  stacktrace:"
    stacktrace.each -> (s) IO.puts "    #{self.format_stacktrace(s)}"
    IO.puts
    acc + 1
  end
end

Code.require "ex_unit/assertions"
Code.require "ex_unit/case"
Code.require "ex_unit/server"