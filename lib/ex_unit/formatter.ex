module ExUnit::Formatter
  mixin Code::Formatter

  def __bound__()
    @('counter: 0, 'failures: [])
  end

  def each(_object, _test, nil)
    IO.write "."
    @('counter, @counter + 1)
  end

  def each(object, test, failure)
    IO.write "F"
    @('counter: @counter + 1, 'failures: [{object, test, failure}|@failures])
  end

  def each_case(_object)
    self
  end

  def finish
    IO.puts "\n"
    @failures.foldl 1, -> (x, acc) print_failure(x, acc)
    IO.puts "#{@counter} tests, #{@failures.length} failures."
  end

  private

  def print_failure({object, test, {kind, reason, stacktrace}}, acc)
    IO.puts "#{acc}) #{test}(#{object})\n  #{kind} #{format_catch(kind, reason)}\n  stacktrace:"
    stacktrace.each -> (s) IO.puts "    #{format_stacktrace(s)}"
    IO.puts
    acc + 1
  end
end