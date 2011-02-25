module ExUnit
  def run(list)
    { failures, counter } = run(list, [], 0)
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
    instance = object.new
    { new_failures, new_counter } = run_tests(object, instance, instance.__tests__, failures, counter)
    run(t, new_failures, new_counter)
  end

  % For each instanciated object, dispatch each test in it.
  def run_tests(object, instance, [test|t], failures, counter)
    result = self.catch -> instance.__send__(test)
    new_failures = parse_result(object, test, result, failures)
    run_tests(object, instance, t, new_failures, counter + 1)
  end

  def run_tests(_, _, [], failures, counter)
    { failures, counter }
  end

  % Parse results after executing the test. If a failure happened,
  % result will be a tuple starting with 'EXIT.
  def parse_result(object, test, {'EXIT, reason}, failures)
    [{object, test, reason}|failures]
  end

  def parse_result(_, _, _, failures)
    failures
  end

  % Prints results after the execution of the test suite.
  def print_results([], counter)
    IO.puts("  All #{counter} tests passed.")
  end

  def print_results(failures, counter)
    failures.foldl 1, -> (x, acc) print_failure(x, acc)
    total_failures = failures.length
    total_passed = counter - total_failures
    IO.puts("\n  Failed: #{total_failures}.  Passed: #{total_passed}.")
  end

  % Print each failued that occurred.
  def print_failure({object, test, {reason, backtrace}}, acc)
    IO.puts("#{acc}) #{test}(#{object})\n  Reason: #{reason}\n  Backtrace:")
    backtrace.each -> (b) print_backtrace(b)
    acc + 1
  end

  % Get each item in the backtrace and print them nicely.
  def print_backtrace({module, method, arity})
    IO.puts("    #{module}##{method}/#{arity}")
  end
end

Code.require "ex_unit/case"