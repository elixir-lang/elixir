Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.FormatterTest do
  use ExUnit.Case, async: true

  import ExUnit.Formatter
  doctest ExUnit.Formatter

  def falsy(), do: false

  defp formatter(:diff_insert, message) do
    "[" <> message <> "]"
  end

  defp formatter(:diff_delete, message) do
    "{" <> message <> "}"
  end

  defp formatter(_kind, message), do: message

  defmacrop catch_assertion(expr) do
    quote do
      try do
        unquote(expr)
      rescue
        ex -> ex
      end
    end
  end

  defp test_case do
    %ExUnit.TestCase{name: Hello}
  end

  defp test do
    %ExUnit.Test{name: :world, case: Hello, tags: %{file: __ENV__.file, line: 1}}
  end

  test "formats test case filters" do
    filters = [run: true, slow: false]
    assert format_filters(filters, :include) =~ "Including tags: [run: true, slow: false]"
    assert format_filters(filters, :exclude) =~ "Excluding tags: [run: true, slow: false]"
  end

  test "formats test errors" do
    failure = [{:error, catch_error(raise "oops"), []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (RuntimeError) oops
    """
  end

  test "formats test exits" do
    failure = [{:exit, 1, []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (exit) 1
    """
  end

  test "formats test exits with mfa" do
    failure = [{:exit, {:bye, {:mod, :fun, []}}, []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (exit) exited in: :mod.fun()
             ** (EXIT) :bye
    """
  end

  test "formats test throws" do
    failure = [{:throw, 1, []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (throw) 1
    """
  end

  test "formats test EXITs" do
    failure = [{{:EXIT, self}, 1, []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (EXIT from #{inspect self}) 1
    """
  end

  test "formats reports" do
    test = test()
    failure = [{:error, catch_error(raise "oops"), []}]

    test = update_in test.tags,
           &Map.merge(&1, %{user_id: 1, report: :user_id})
    assert format_test_failure(test, failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (RuntimeError) oops
         tags:
           user_id: 1
    """

    test = update_in test.tags,
           &Map.merge(&1, %{many_ids: Enum.to_list(1..30), report: [:user_id, :many_ids]})
    assert format_test_failure(test, failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (RuntimeError) oops
         tags:
           many_ids: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
                      19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
           user_id: 1
    """
  end

  test "formats stacktraces" do
    failure = [{:error, catch_error(raise "oops"), [{Oops, :wrong, 1, [file: "formatter_test.exs", line: 1]}]}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (RuntimeError) oops
         stacktrace:
           formatter_test.exs:1: Oops.wrong/1
    """
  end

  test "formats assertions" do
    failure = [{:error, catch_assertion(assert ExUnit.FormatterTest.falsy), []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         Expected truthy, got false
         code: ExUnit.FormatterTest.falsy()
    """
  end

  test "formats multiple assertions" do
    failure = [{:error, catch_assertion(assert ExUnit.FormatterTest.falsy), []},
               {:error, catch_assertion(assert 1 == 2), []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1

         Failure #1
         Expected truthy, got false
         code: ExUnit.FormatterTest.falsy()

         Failure #2
         Assertion with == failed
         code: 1 == 2
         lhs:  1
         rhs:  2
    """
  end

  test "formats test case errors" do
    failure = [{:error, catch_error(raise "oops"), []}]
    assert format_test_case_failure(test_case(), failure, 1, 80, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, tests invalidated
         ** (RuntimeError) oops
    """
  end

  test "formats assertions with operators with no limit" do
    failure = [{:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}]
    assert format_test_case_failure(test_case(), failure, 1, :infinity, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, tests invalidated
         Assertion with == failed
         code: [1, 2, 3] == [4, 5, 6]
         lhs:  [1, 2, 3]
         rhs:  [4, 5, 6]
    """
  end

  test "formats assertions with operators with column limit" do
    failure = [{:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}]
    assert format_test_case_failure(test_case(), failure, 1, 15, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, tests invalidated
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

  test "formats assertions with message with multiple lines" do
    message = "Some meaningful error:\nuseful info\nanother useful info"
    failure = [{:error, catch_assertion(assert(false, message)), []}]
    assert format_test_case_failure(test_case(), failure, 1, :infinity, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, tests invalidated
         Some meaningful error:
         useful info
         another useful info
    """
  end

  defmodule BadInspect do
    defstruct key: 0

    defimpl Inspect do
      def inspect(struct, opts) when is_atom(opts) do
        struct.unknown
      end
    end
  end

  test "inspect failure" do
    failure = [{:error, catch_assertion(assert :will_fail == %BadInspect{}), []}]

    message = "got FunctionClauseError with message \"no function clause matching " <>
              "in Inspect.ExUnit.FormatterTest.BadInspect.inspect/2\" while inspecting " <>
              "%{__struct__: ExUnit.FormatterTest.BadInspect, key: 0}"

    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         Assertion with == failed
         code: :will_fail == %BadInspect{}
         lhs:  :will_fail
         rhs:  %Inspect.Error{message: #{inspect message}}
    """
  end

  defmodule BadMessage do
    defexception key: 0

    def message(_message) do
      raise "oops"
    end
  end

  test "message failure" do
    failure = [{:error, catch_error(raise BadMessage), []}]
    message = "got RuntimeError with message \"oops\" while retrieving Exception.message/1 " <>
              "for %ExUnit.FormatterTest.BadMessage{key: 0}"
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (ExUnit.FormatterTest.BadMessage) #{message}
    """
  end

  defmodule User do
    defstruct [:age]
  end

  test "difference formatting" do
    int1 = 491512235
    int2 = 490512035
    assert format_diff(int1, int2, &formatter/2) == "49{1}[0]512{2}[0]35 {(off by -1000200)}"
    assert format_diff(42.0, 43.0, &formatter/2) == "4{2}[3].0 [(off by +1.0)]"

    string1 = "fox hops over the dog"
    string2 = "fox jumps over the lazy cat"
    assert format_diff(string1, string2, &formatter/2) == "fox {ho}[jum]ps over the {dog}[lazy cat]"

    list1 = [{"Content-type", "text/plain"}, {"etag", "One"}]
    list2 = [{"content-type", "text/html"}, {"Etag", "Two"}]
    expected = ~S([{"{C}[c]ontent-type", "text/{p}[htm]l{ain}"}, {"{e}[E]tag", "{One}[Two]"}])
    assert format_diff(list1, list2, &formatter/2) == expected

    tuple1 = {:yes, 'ject', []}
    tuple2 = {:yes, 'lter', []}
    assert format_diff(tuple1, tuple2, &formatter/2) == "{:yes, '{jec}[l]t[er]', []}"

    map1 = Enum.into(1..40, %{}, &{&1, &1}) |> Map.delete(33)
    map2 = Enum.reduce(5..10, map1, &Map.delete(&2, &1)) |> Map.put(33, 33) |> Map.put(23, 32)
    expected = "%{23 => {2}3[2] [(off by +9)], {8 => 8}, {7 => 7}, {6 => 6}, {10 => 10}, {9 => 9}, {5 => 5}, [33 => 33], ...}"
    assert format_diff(map1, map2, &formatter/2) == expected

    map1 = %{baz: 12}
    map2 = %{foo: 12, bar: 12, baz: 12}
    assert format_diff(map1, map2, &formatter/2) == "%{[bar: 12], [foo: 12], ...}"
    assert format_diff(map2, map1, &formatter/2) == "%{{bar: 12}, {foo: 12}, ...}"
    assert format_diff(map1, %{}, &formatter/2) == "%{{baz: 12}}"
    assert format_diff(%{}, map1, &formatter/2) == "%{[baz: 12]}"

    user1 = %User{age: 16}
    user2 = %User{age: 21}
    assert format_diff(user1, user2, &formatter/2) == "%ExUnit.FormatterTest.User{age: [2]1{6} [(off by +5)]}"
    assert format_diff(%User{}, %ExUnit.Test{}, &formatter/2) == nil

    bin1 = <<147, 1, 2, 31>>
    bin2 = <<193, 1, 31>>
    assert format_diff(bin1, bin2, &formatter/2) == nil

    assert format_diff(:foo, :bar, &formatter/2) == nil
    assert format_diff(12, "foo", &formatter/2) == nil
  end
end
