Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.FormatterTest do
  use ExUnit.Case, async: true

  import ExUnit.Formatter
  doctest ExUnit.Formatter

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

  def falsy() do
    false
  end

  defp formatter(_kind, message) do
    message
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
    failure = [{{:EXIT, self()}, 1, []}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) == """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (EXIT from #{inspect self()}) 1
    """
  end

  test "formats test errors with code snippets" do
    stack = {Hello, :world, 1, [file: __ENV__.file, line: 3]}
    failure = [{:error, catch_error(raise "oops"), [stack]}]
    assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
      1) world (Hello)
         test/ex_unit/formatter_test.exs:1
         ** (RuntimeError) oops
         code: defmodule ExUnit.FormatterTest do
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
         code: assert ExUnit.FormatterTest.falsy()
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
         code: assert ExUnit.FormatterTest.falsy()

         Failure #2
         Assertion with == failed
         code:  assert 1 == 2
         left:  1
         right: 2
    """
  end

  # TODO: Remove this check once we depend only on 20
  if :erlang.system_info(:otp_release) >= '20' do
    defp trim_multiline_whitespace(string) do
      String.replace(string, ~r"\n\s+\n", "\n\n")
    end

    test "blames function clause error" do
      {error, stack} =
        try do
          Access.fetch(:foo, :bar)
        rescue
          exception -> {exception, System.stacktrace()}
        end

      failure = format_test_failure(test(), [{:error, error, [hd(stack)]}], 1, 80, &formatter/2)

      assert trim_multiline_whitespace(failure) =~ """
        1) world (Hello)
           test/ex_unit/formatter_test.exs:1
           ** (FunctionClauseError) no function clause matching in Access.fetch/2

           The following arguments were given to Access.fetch/2:

               # 1
               :foo

               # 2
               :bar

           Attempted function clauses (showing 5 out of 5):

               def fetch(%module{} = container, key)
      """

      assert failure =~ ~r"\(elixir\) lib/access\.ex:\d+: Access\.fetch/2"
    end
  end

  test "formats setup all errors" do
    failure = [{:error, catch_error(raise "oops"), []}]
    assert format_test_case_failure(test_case(), failure, 1, 80, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, test invalidated
         ** (RuntimeError) oops
    """
  end

  test "formats assertions with operators with no limit" do
    failure = [{:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}]
    assert format_test_case_failure(test_case(), failure, 1, :infinity, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, test invalidated
         Assertion with == failed
         code:  assert [1, 2, 3] == [4, 5, 6]
         left:  [1, 2, 3]
         right: [4, 5, 6]
    """
  end

  test "formats assertions with operators with column limit" do
    failure = [{:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}]
    assert format_test_case_failure(test_case(), failure, 1, 15, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, test invalidated
         Assertion with == failed
         code:  assert [1, 2, 3] == [4, 5, 6]
         left:  [1,
                 2,
                 3]
         right: [4,
                 5,
                 6]
    """
  end

  test "formats assertions with message with multiple lines" do
    message = "Some meaningful error:\nuseful info\nanother useful info"
    failure = [{:error, catch_assertion(assert(false, message)), []}]
    assert format_test_case_failure(test_case(), failure, 1, :infinity, &formatter/2) =~ """
      1) Hello: failure on setup_all callback, test invalidated
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
         code:  assert :will_fail == %BadInspect{}
         left:  :will_fail
         right: %Inspect.Error{message: #{inspect message}}
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
end
