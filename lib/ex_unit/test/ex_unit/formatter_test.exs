Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.FormatterTest do
  use ExUnit.Case, async: true

  import ExUnit.Formatter
  doctest ExUnit.Formatter

  def falsy, do: false

  defmacrop catch_expectation(expr) do
    quote do
      try do
        unquote(expr)
      rescue
        e -> e
      end
    end
  end

  test "formats test errors" do
    failure = { :error, catch_error(raise "oops"), [] }
    assert format_test_failure(Hello, :world, failure, 1, nil) =~ """
      1) world (Hello)
         ** (RuntimeError) oops
         stacktrace:
    """
  end

  test "formats test exits" do
    failure = { :exit, 1, [] }
    assert format_test_failure(Hello, :world, failure, 1, nil) == """
      1) world (Hello)
         ** (exit) 1
         stacktrace:
    """
  end

  test "formats test throws" do
    failure = { :throw, 1, [] }
    assert format_test_failure(Hello, :world, failure, 1, nil) == """
      1) world (Hello)
         ** (throw) 1
         stacktrace:
    """
  end

  test "formats test expectations" do
    failure = { :error, catch_expectation(assert 1 == 2), [] }
    assert format_test_failure(Hello, :world, failure, 1, nil) =~ """
      1) world (Hello)
         ** (ExUnit.ExpectationError)
                      expected: 1
           to be equal to (==): 2
         stacktrace:
    """
  end

  test "formats test expectations with prelude" do
    failure = { :error, catch_expectation(assert ExUnit.FormatterTest.falsy), [] }
    assert format_test_failure(Hello, :world, failure, 1, nil) =~ """
      1) world (Hello)
         ** (ExUnit.ExpectationError)
              expected: ExUnit.FormatterTest.falsy()
                 to be: true
           instead got: false
         stacktrace:
    """
  end
  
  test "formats test expectations inside property test" do
    test = test { :error, ExUnit.Prop.PropertyError[shrinks: 5, input: "blaat",
                            wrapped: { :error, catch_expectation(assert 1 == 2) }],
                  [] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (ExUnit.ExpectationError in ExUnit.Prop.PropertyError)
                 after shrinks: 5
               generated input: "blaat"
                      expected: 1
           to be equal to (==): 2
         stacktrace:
    """
  end
  
  test "formats exceptions inside property test" do
    test = test { :error, ExUnit.Prop.PropertyError[shrinks: 5, input: "blaat",
                            wrapped: { :error, CaseClauseError[actual: "moo"] }],
                  [] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (ExUnit.Prop.PropertyError)
             after shrinks: 5
           generated input: "blaat"
                 exception: (CaseClauseError) no case clause matching: "moo"
         stacktrace:
    """
  end
  
  test "formats Erlang errors inside property test" do
    test = test { :error, ExUnit.Prop.PropertyError[shrinks: 5, input: "blaat",
                            wrapped: { :error, :moo }],
                  [] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (ExUnit.Prop.PropertyError)
             after shrinks: 5
           generated input: "blaat"
                    caught: (error) moo
         stacktrace:
    """
  end

  test "formats stacktraces with test location" do
    failure = { :error, catch_error(raise "oops"), [{ Hello, :world, 1, [file: __FILE__, line: 1]}] }
    assert format_test_failure(Hello, :world, failure, 1, nil) =~ """
      1) world (Hello)
         ** (RuntimeError) oops
         at test/ex_unit/formatter_test.exs:1
    """
  end

  test "formats stacktraces without test location" do
    failure = { :error, catch_error(raise "oops"), [{ Oops, :wrong, 1, [file: __FILE__, line: 1]}] }
    assert format_test_failure(Hello, :world, failure, 1, nil) =~ """
      1) world (Hello)
         ** (RuntimeError) oops
         stacktrace:
           test/ex_unit/formatter_test.exs:1: Oops.wrong/1
    """
  end

  test "formats test case errors" do
    failure = { :error, catch_error(raise "oops"), [] }
    assert format_test_case_failure(Hello, failure, 1, nil) =~ """
      1) Hello: failure on setup_all/teardown_all callback, tests invalidated
         ** (RuntimeError) oops
         stacktrace:
    """
  end
end
