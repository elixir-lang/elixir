Code.require_file("../test_helper.exs", __DIR__)

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

  defp test_module do
    %ExUnit.TestModule{name: Hello}
  end

  defp test do
    %ExUnit.Test{name: :world, module: Hello, tags: %{file: __ENV__.file, line: 1}}
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

  describe "format_test_failure/5" do
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
                  ** (EXIT from #{inspect(self())}) 1
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

    test "formats stacktraces" do
      stacktrace = [{Oops, :wrong, 1, [file: "formatter_test.exs", line: 1]}]
      failure = [{:error, catch_error(raise "oops"), stacktrace}]

      assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
               1) world (Hello)
                  test/ex_unit/formatter_test.exs:1
                  ** (RuntimeError) oops
                  stacktrace:
                    formatter_test.exs:1: Oops.wrong/1
             """
    end

    test "formats assertions" do
      failure = [{:error, catch_assertion(assert ExUnit.FormatterTest.falsy()), []}]

      assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
               1) world (Hello)
                  test/ex_unit/formatter_test.exs:1
                  Expected truthy, got false
                  code: assert ExUnit.FormatterTest.falsy()
             """
    end

    test "formats multiple assertions" do
      failure = [
        {:error, catch_assertion(assert ExUnit.FormatterTest.falsy()), []},
        {:error, catch_assertion(assert 1 == 2), []}
      ]

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

    test "omits the code example if that code is invalid UTF-8" do
      # The binary in this expr corresponds to `String.valid?("invalid utf8 \xFF")`
      failure = [
        {:error,
         %ExUnit.AssertionError{
           expr:
             <<83, 116, 114, 105, 110, 103, 46, 118, 97, 108, 105, 100, 63, 40, 34, 105, 110, 118,
               97, 108, 105, 100, 32, 117, 116, 102, 56, 32, 255, 34, 41>>,
           message:
             "Doctest did not compile, got: (UnicodeConversionError) invalid encoding starting at <<255, 34, 41>>"
         },
         [
           {ExUnit.DocTestTest.Invalid, :__MODULE__, 0,
            [line: 195, file: "test/ex_unit/doc_test_test.exs"]}
         ]}
      ]

      formatted = format_test_failure(test(), failure, 1, 80, &formatter/2)
      assert String.valid?(formatted)

      assert formatted =~ """
               1) world (Hello)
                  test/ex_unit/formatter_test.exs:1
                  Doctest did not compile, got: (UnicodeConversionError) invalid encoding starting at <<255, 34, 41>>
                  stacktrace:
                    test/ex_unit/doc_test_test.exs:195: ExUnit.DocTestTest.Invalid (module)
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

    defmodule BadInspect do
      defstruct key: 0

      defimpl Inspect do
        def inspect(struct, opts) when is_atom(opts) do
          struct.unknown
        end
      end
    end

    defmodule BadMessage do
      defexception key: 0

      def message(_message) do
        raise "oops"
      end
    end

    test "inspect failure" do
      failure = [{:error, catch_assertion(assert :will_fail == %BadInspect{}), []}]

      message =
        "got FunctionClauseError with message \"no function clause matching " <>
          "in Inspect.ExUnit.FormatterTest.BadInspect.inspect/2\" while inspecting " <>
          "%{__struct__: ExUnit.FormatterTest.BadInspect, key: 0}"

      assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
               1) world (Hello)
                  test/ex_unit/formatter_test.exs:1
                  Assertion with == failed
                  code:  assert :will_fail == %BadInspect{}
                  left:  :will_fail
                  right: %Inspect.Error{
                           message: #{inspect(message)}
                         }
             """
    end

    test "message failure" do
      failure = [{:error, catch_error(raise BadMessage), []}]

      message =
        "got RuntimeError with message \"oops\" while retrieving Exception.message/1 " <>
          "for %ExUnit.FormatterTest.BadMessage{key: 0}"

      assert format_test_failure(test(), failure, 1, 80, &formatter/2) =~ """
               1) world (Hello)
                  test/ex_unit/formatter_test.exs:1
                  ** (ExUnit.FormatterTest.BadMessage) #{message}
             """
    end
  end

  describe "format_test_all_failure/5" do
    test "formats setup all errors" do
      failure = [{:error, catch_error(raise "oops"), []}]

      assert format_test_all_failure(test_module(), failure, 1, 80, &formatter/2) =~ """
               1) Hello: failure on setup_all callback, test invalidated
                  ** (RuntimeError) oops
             """
    end

    test "formats assertions with operators with no limit" do
      failure = [{:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}]

      assert format_test_all_failure(test_module(), failure, 1, :infinity, &formatter/2) =~ """
               1) Hello: failure on setup_all callback, test invalidated
                  Assertion with == failed
                  code:  assert [1, 2, 3] == [4, 5, 6]
                  left:  [1, 2, 3]
                  right: [4, 5, 6]
             """
    end

    test "formats assertions with operators with column limit" do
      failure = [{:error, catch_assertion(assert [1, 2, 3] == [4, 5, 6]), []}]

      assert format_test_all_failure(test_module(), failure, 1, 15, &formatter/2) =~ """
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

    test "formats assertions with function call arguments" do
      failure = [{:error, catch_assertion(assert is_list({1, 2, 3})), []}]

      assert format_test_all_failure(test_module(), failure, 1, 80, &formatter/2) =~ """
               1) Hello: failure on setup_all callback, test invalidated
                  Expected truthy, got false
                  code: assert is_list({1, 2, 3})
                  arguments:

                      # 1
                      {1, 2, 3}
             """
    end

    test "formats assertions with message with multiple lines" do
      message = "Some meaningful error:\nuseful info\nanother useful info"
      failure = [{:error, catch_assertion(assert(false, message)), []}]

      assert format_test_all_failure(test_module(), failure, 1, :infinity, &formatter/2) =~ """
               1) Hello: failure on setup_all callback, test invalidated
                  Some meaningful error:
                  useful info
                  another useful info
             """
    end
  end
end
