% Basic unit test structure for Elixir.
module ExUnit
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
    new_failures = try
      instance.__send__(test)
      failures
    catch kind: error
      [{object, test, kind, error, self.__stacktrace__}|failures]
    end

    run_tests(object, instance, t, new_failures, counter + 1)
  end

  def run_tests(_, _, [], failures, counter)
    { failures, counter }
  end

  % Prints results after the execution of the test suite.
  def print_results([], counter)
    IO.puts "  All #{counter} tests passed."
  end

  def print_results(failures, counter)
    failures.foldl 1, -> (x, acc) print_failure(x, acc)
    total_failures = failures.length
    total_passed = counter - total_failures
    IO.puts "  Failed: #{total_failures}.  Passed: #{total_passed}."
  end

  % Print each failure that occurred.
  def print_failure({object, test, kind, reason, stacktrace}, acc)
    IO.puts "#{acc}) #{test}(#{object})\n  #{kind}: #{reason}\n  stacktrace:"
    stacktrace.each -> (b) print_backtrace(b)
    IO.puts
    acc + 1
  end

  % Get each item in the backtrace and print them nicely.
  def print_backtrace({module, method, arity})
    IO.puts "    #{module}##{method}/#{arity}"
  end
end

Code.require "ex_unit/case"
Code.require "ex_unit/server"