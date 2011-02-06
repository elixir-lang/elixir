module ExUnit
  def run(list)
    counter = run(list, 0)
    IO.puts("  All ~w tests passed.\n", [counter])
  end

  def run([], counter)
    counter
  end

  def run([object|t], counter)
    instance = object.new
    new_counter = run_tests(instance, instance.__tests__, counter)
    run(t, new_counter)
  end

  def run_tests(object, [test|t], counter)
    object.send(test)
    run_tests(object, t, counter + 1)
  end

  def run_tests(_, [], counter)
    counter
  end
end

Code.require "ex_unit/case"