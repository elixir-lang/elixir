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
    test = test { :error, catch_error(raise "oops"), [] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (RuntimeError) oops
         stacktrace:
    """
  end

  test "formats test exits" do
    test = test { :exit, 1, [] }
    assert format_test_failure(test, 1, nil) == """
      1) world (Hello)
         ** (exit) 1
         stacktrace:
    """
  end

  test "formats test throws" do
    test = test { :throw, 1, [] }
    assert format_test_failure(test, 1, nil) == """
      1) world (Hello)
         ** (throw) 1
         stacktrace:
    """
  end

  test "formats test expectations" do
    test = test { :error, catch_expectation(assert 1 == 2), [] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (ExUnit.ExpectationError)
                      expected: 1
           to be equal to (==): 2
         stacktrace:
    """
  end

  test "formats test expectations with prelude" do
    test = test { :error, catch_expectation(assert ExUnit.FormatterTest.falsy), [] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (ExUnit.ExpectationError)
              expected: ExUnit.FormatterTest.falsy()
                 to be: true
           instead got: false
         stacktrace:
    """
  end

  test "formats stacktraces with test location" do
    test = test { :error, catch_error(raise "oops"), [{ Hello, :world, 1, [file: __FILE__, line: 1]}] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (RuntimeError) oops
         at test/ex_unit/formatter_test.exs:1
    """
  end

  test "formats stacktraces without test location" do
    test = test { :error, catch_error(raise "oops"), [{ Oops, :wrong, 1, [file: __FILE__, line: 1]}] }
    assert format_test_failure(test, 1, nil) =~ """
      1) world (Hello)
         ** (RuntimeError) oops
         stacktrace:
           test/ex_unit/formatter_test.exs:1: Oops.wrong/1
    """
  end

  test "formats test case errors" do
    test_case = test_case { :error, catch_error(raise "oops"), [] }
    assert format_test_case_failure(test_case, 1, nil) =~ """
      1) Hello: failure on setup_all/teardown_all callback, tests invalidated
         ** (RuntimeError) oops
         stacktrace:
    """
  end

  defp test(failure) do
    ExUnit.Test[case: Hello, name: :world, failure: failure]
  end

  defp test_case(failure) do
    ExUnit.TestCase[name: Hello, failure: failure]
  end
end
