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
    { new_failures, new_counter } = run_tests(instance, instance.__tests__, failures, counter)
    run(t, new_failures, new_counter)
  end

  % For each instanciated object, dispatch each test in it.
  def run_tests(object, [test|t], failures, counter)
    result = self.catch -> object.send(test)
    new_failures = parse_result(test, result, failures)
    run_tests(object, t, new_failures, counter + 1)
  end

  def run_tests(_, [], failures, counter)
    { failures, counter }
  end

  % Parse results after executing the test. If a failure happened,
  % result will be a tuple starting with 'EXIT.
  def parse_result(test, {'EXIT, reason}, failures)
    [{test, reason}|failures]
  end

  def parse_result(_, _, failures)
    failures
  end

  % Prints results after the execution of the test suite.
  def print_results([], counter)
    IO.puts("  All ~w tests passed.\n", [counter])
  end

  def print_results(failures, counter)
    IO.puts("  Failed: ~w.  Passed: ~w.\n", [failures.length, counter - failures.length])
  end
end

Code.require "ex_unit/case"