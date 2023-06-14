Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.FancyDiagnosticsTest do
  use ExUnit.Case, async: false

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
         syntax error before: '*'
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
         syntax error: expression is incomplete
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
          missing terminator: end (for "fn" starting at line 1)
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
         syntax error: expression is incomplete

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

    test "handles unicode" do
      source = read_fixture("unicode_error._ex")
      output = capture_raise(source, SyntaxError)

      assert ansi_error?(output)
      assert strip_ansi(output) =~ "😎"
    after
      purge(Sample)
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
         unexpected token: }
         
             HINT: the "[" on line 1 is missing terminator "]"
         
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

  defp ansi_error?(output) do
    pattern = :binary.compile_pattern(["\e[31m", "\e[0m"])
    String.contains?(output, pattern)
  end

  defp strip_ansi(output) do
    ansi_pattern = :binary.compile_pattern(["\e[33m", "\e[31m", "\e[0m"])
    String.replace(output, ansi_pattern, "")
  end
end
