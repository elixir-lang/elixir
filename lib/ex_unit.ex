% Basic unit test structure for Elixir.
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
    new_failures = try
      instance.__send__(test)
      IO.write "."
      failures
    catch kind: error
      IO.write "F"
      [{object, test, kind, error, self.__stacktrace__}|failures]
    end

    run_tests(object, instance, t, new_failures, counter + 1)
  end

  def run_tests(_, _, [], failures, counter)
    { failures, counter }
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