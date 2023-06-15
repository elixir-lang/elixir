Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DiagnosticsTest do
  use ExUnit.Case, async: false

  setup do
    Code.put_compiler_option(:fancy_diagnostics, true)
    on_exit(fn -> Code.put_compiler_option(:fancy_diagnostics, false) end)

    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit(fn -> Application.put_env(:elixir, :ansi_enabled, true) end)
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

      assert output == expected
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

      assert output == expected
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

      assert output == expected
    end

    test "trims leading spaces on snippet" do
      expected = """
      ** (SyntaxError) invalid syntax found on nofile:5:11:
         ┌─ error: nofile:5:11
         │
       5 │ end
         │ ^
         │
         unexpected reserved word: end
      """

      output =
        capture_raise(
          ~S("""
          if true do 
              """
                
          end
          """),
          SyntaxError
        )

      assert output == expected
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

      assert output == expected
    end

    test "handles unicode" do
      source = read_fixture("unicode_error._ex")
      output = capture_raise(source, SyntaxError)

      assert output =~ "😎"
    after
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
    fixture = "diagnostics/" <> name

    fixture
    |> PathHelpers.fixture_path()
    |> File.read!()
  end

  defp purge(module) when is_atom(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
