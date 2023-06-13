Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.FancyDiagnosticsTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  setup do
    Code.put_compiler_option(:fancy_diagnostics, true)
    on_exit(fn -> Code.put_compiler_option(:fancy_diagnostics, false) end)
  end

  describe "compile-time exceptions" do
    test "SyntaxError (snippet)" do
      expected = """
      ** (SyntaxError) invalid syntax found on nofile:1:17:

         ┌─ error: nofile:1:17
         │
       1 │ [1, 2, 3, 4, 5, *]
         │                 ^
         │
         │ syntax error before: '*'
         │
      """

      output =
        capture_raise(
          """
          [1, 2, 3, 4, 5, *]
          """,
          SyntaxError
        )

      assert ansi_error?(output)
      assert strip_ansi(output) == expected
    end

    test "TokenMissingError (snippet)" do
      expected = """
      ** (TokenMissingError) token missing on nofile:1:4:

         ┌─ error: nofile:1:4
         │
       1 │ 1 +
         │    ^
         │
         │ syntax error: expression is incomplete
         │
      """

      output =
        capture_raise(
          """
          1 +
          """,
          TokenMissingError
        )

      assert ansi_error?(output)
      assert strip_ansi(output) == expected
    end

    test "TokenMissingError (line only)" do
      expected = """
      ** (TokenMissingError) token missing on nofile:2:1:

         ┌─ error: nofile:2
         │
       2 │ missing terminator: end (for "fn" starting at line 1)
         │
      """

      output =
        capture_raise(
          """
          fn a
          """,
          TokenMissingError
        )

      assert ansi_error?(output)
      assert strip_ansi(output) == expected
    end

    test "shows stacktrace if present" do
      fake_stacktrace = [
        {:fake, :fun, 3, [file: "nofile", line: 10]},
        {:real, :fun, 2, [file: "nofile", line: 10]}
      ]

      expected = """
      ** (TokenMissingError) token missing on nofile:1:4:

         ┌─ error: nofile:1:4
         │
       1 │ 1 -
         │    ^
         │
         │ syntax error: expression is incomplete
         │

          nofile:10: :fake.fun/3
          nofile:10: :real.fun/2
      """

      output =
        capture_raise(
          """
          1 -
          """,
          TokenMissingError,
          fake_stacktrace
        )

      assert ansi_error?(output)
      assert strip_ansi(output) == expected
    end

    test "respects disabled ansi" do
      Application.put_env(:elixir, :ansi_enabled, false)

      expected = """
      ** (SyntaxError) invalid syntax found on nofile:1:8:

         ┌─ error: nofile:1:8
         │
       1 │ [:a, :b}
         │        ^
         │
         │ unexpected token: }
         │ 
         │     HINT: the "[" on line 1 is missing terminator "]"
         │ 
         │
      """

      output =
        capture_raise(
          """
          [:a, :b}
          """,
          SyntaxError
        )

      refute ansi_error?(output)
      assert output == expected
    after
      Application.put_env(:elixir, :ansi_enabled, true)
      purge(Sample)
    end
  end

  describe "diagnostics" do
    test "grouped warnings (with file)" do
      expected = """
         ┌─ warning: test/elixir/fixtures/fancy_diagnostics/grouped_warnings.ex:5:6
         │
       5 │ A.bar()
         │ ~~~~~~~
         │
         │ A.bar/0 is undefined or private
         │
         │ Invalid call also found at 3 other locations:
         │   test/elixir/fixtures/fancy_diagnostics/grouped_warnings.ex:6:6: Sample.a/0
         │   test/elixir/fixtures/fancy_diagnostics/grouped_warnings.ex:7:6: Sample.a/0
         │   test/elixir/fixtures/fancy_diagnostics/grouped_warnings.ex:8:6: Sample.a/0
         │ 

      """

      source = read_fixture("grouped_warnings.ex")
      output = capture_eval(source)

      assert ansi_warning?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "grouped warnings (no file)" do
      expected = """
         ┌─ warning: nofile:3:6
         │
         │ A.bar/0 is undefined or private
         │
         │ Invalid call also found at 3 other locations:
         │   nofile:4:6: Sample.a/0
         │   nofile:5:6: Sample.a/0
         │   nofile:6:6: Sample.a/0
         │ 

      """

      output =
        capture_eval("""
        defmodule Sample do
          def a do 
            A.bar()
            A.bar()
            A.bar()
            A.bar()
          end
        end
        """)

      assert ansi_warning?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "warning (line only)" do
      expected = """
         ┌─ warning: nofile:4
         │
       4 │ function hello/0 is unused
         │

      """

      source = read_fixture("warn_line.ex")
      output = capture_eval(source)

      assert ansi_warning?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "warning (line+column)" do
      expected = """
         ┌─ warning: test/elixir/fixtures/fancy_diagnostics/warn_line_column.ex:6:5
         │
       6 │ @foo
         │ ~~~~
         │
         │ module attribute @foo in code block has no effect as it is never returned (remove
         │ the attribute or assign it to _ to avoid warnings)
         │

      """

      source = read_fixture("warn_line_column.ex")
      output = capture_eval(source)

      assert ansi_warning?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "warning (no file)" do
      expected = """
         ┌─ warning: nofile:2:3
         │
       2 │ unused alias List
         │

      """

      output =
        capture_eval("""
        defmodule Sample do
          alias :lists, as: List
          import MapSet
          new()
        end
        """)

      assert ansi_warning?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "warning (long message)" do
      expected = """
         ┌─ warning: nofile:4:12
         │
       4 │ variable "compare_local" is unused (there is a variable with the same name in the
         │ context, use the pin operator (^) to match on it or prefix this variable with
         │ underscore if it is not meant to be used)
         │

         ┌─ warning: nofile:3:5
         │
       3 │ variable "compare_local" is unused (if the variable is not meant to be used, prefix
         │ it with an underscore)
         │

      """

      output =
        capture_eval("""
        defmodule Sample do
          def test do
            compare_local = "hello"
            match?(compare_local, "hello")
          end
        end
        """)

      assert ansi_warning?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "error (line+column)" do
      expected = """
         ┌─ error: test/elixir/fixtures/fancy_diagnostics/error_line_column.ex:5:13
         │
       5 │ IO.puts bar
         │         ^^^
         │
         │ undefined variable "bar"
         │

      """

      source = read_fixture("error_line_column.ex")
      output = capture_compile(source)

      assert ansi_error?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "error (line only)" do
      expected = """
         ┌─ error: test/elixir/fixtures/fancy_diagnostics/error_line.ex:4
         │
       4 │ def CamelCase do
         │ ^^^^^^^^^^^^^^^^
         │
         │ function names should start with lowercase characters or underscore, invalid name
         │ CamelCase
         │

      """

      source = read_fixture("error_line.ex")
      output = capture_compile(source)

      assert ansi_error?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "error (long message)" do
      expected = """
         ┌─ error: nofile:2:16
         │
       2 │ undefined function module_info/0 (this function is auto-generated by the compiler
         │ and must always be called as a remote, as in __MODULE__.module_info/0)
         │

      """

      output =
        capture_compile("""
        defmodule Sample do
          def foo, do: module_info()
        end
        """)

      assert ansi_error?(output)
      assert strip_ansi(output) == expected
    after
      purge(Sample)
    end

    test "respects disabled ansi" do
      Application.put_env(:elixir, :ansi_enabled, false)

      expected = """
         ┌─ error: nofile:1:1
         │
       1 │ undefined variable "foo"
         │

      """

      output =
        capture_compile("""
        foo
        """)

      refute ansi_error?(output)
      assert output == expected
    after
      Application.put_env(:elixir, :ansi_enabled, true)
    end
  end

  defp capture_eval(source) do
    capture_io(:stderr, fn ->
      quoted = Code.string_to_quoted!(source, columns: true)
      Code.eval_quoted(quoted)
    end)
  end

  defp capture_compile(source) do
    capture_io(:stderr, fn ->
      assert_raise CompileError, fn ->
        ast = Code.string_to_quoted!(source, columns: true)
        Code.eval_quoted(ast)
      end
    end)
  end

  defp capture_raise(source, exception, mock_stacktrace \\ []) do
    e =
      assert_raise exception, fn ->
        ast = Code.string_to_quoted!(source, columns: true)
        Code.eval_quoted(ast)
      end

    Exception.format(:error, e, mock_stacktrace)
  end

  defp read_fixture(name) do 
    fixture = "fancy_diagnostics/" <> name

    fixture
    |> PathHelpers.fixture_path()
    |> File.read!()
  end

  defp purge(module) when is_atom(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defp ansi_warning?(output) do
    pattern = :binary.compile_pattern(["\e[33m", "\e[0m"])
    String.contains?(output, pattern)
  end

  defp ansi_error?(output) do
    pattern = :binary.compile_pattern(["\e[31m", "\e[0m"])
    String.contains?(output, pattern)
  end

  defp strip_ansi(output) do
    ansi_pattern = :binary.compile_pattern(["\e[33m", "\e[31m", "\e[0m"])
    String.replace(output, ansi_pattern, "")
  end
end
