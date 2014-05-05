Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.FormatterTest do
  use ExUnit.Case, async: true

  import ExUnit.Formatter
  doctest ExUnit.Formatter

  def falsy, do: false
  def formatter(_color, msg), do: msg

  defmacrop catch_assertion(expr) do
    quote do
      try do
        unquote(expr)
      rescue
        e -> e
      end
    end
  end

  defp case do
    ExUnit.TestCase[name: Hello]
  end

  defp test do
    ExUnit.Test[name: :world, case: Hello, tags: [file: __ENV__.file, line: 1]]
  end

  test "formats test case filters" do
    filters = [run: true, slow: false]
    assert format_filters(filters, :include) =~ "Including tags: [run: true, slow: false]"
    assert format_filters(filters, :exclude) =~ "Excluding tags: [run: true, slow: false]"
  end

  test "formats test errors" do
    failure = {:error, catch_error(raise "oops"), []}
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (RuntimeError) oops
    """
  end

  test "formats test exits" do
    failure = {:exit, 1, []}
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (exit) 1
    """
  end

  test "formats test throws" do
    failure = {:throw, 1, []}
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (throw) 1
    """
  end

  test "formats stacktraces" do
    failure = {:error, catch_error(raise "oops"), [{Oops, :wrong, 1, [file: "formatter_test.exs", line: 1]}]}
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (RuntimeError) oops
         stacktrace:
           formatter_test.exs:1: Oops.wrong/1
    """
  end

  test "formats assertions" do
    failure = {:error, catch_assertion(assert ExUnit.FormatterTest.falsy), []}
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         Expected truthy, got false
         code: ExUnit.FormatterTest.falsy()
    """
  end

  test "formats test case errors" do
    failure = {:error, catch_error(raise "oops"), []}
    assert format_test_case_failure(case(), failure, 1, 80, &formatter/2) =~ """
      1) Hello: failure on setup_all/teardown_all callback, tests invalidated
         ** (RuntimeError) oops
    """
  end

  test "formats assertions with operators with no limit" do
    failure = {:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}
    assert format_test_case_failure(case(), failure, 1, :infinity, &formatter/2) =~ """
      1) Hello: failure on setup_all/teardown_all callback, tests invalidated
         Assertion with == failed
         code: [1, 2, 3] == [4, 5, 6]
         lhs:  [1, 2, 3]
         rhs:  [4, 5, 6]
    """
  end

  test "formats assertions with operators with column limit" do
    failure = {:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}
    assert format_test_case_failure(case(), failure, 1, 15, &formatter/2) =~ """
      1) Hello: failure on setup_all/teardown_all callback, tests invalidated
         Assertion with == failed
         code: [1, 2, 3] == [4, 5, 6]
         lhs:  [1,
                2,
                3]
         rhs:  [4,
                5,
                6]
    """
  end
end
