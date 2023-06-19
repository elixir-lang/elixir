Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DiagnosticsTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  setup do
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

    test "TokenMissingError (no snippet)" do
      expected = """
      ** (TokenMissingError) token missing on nofile:2:1:
          ┌─ error: nofile:2:1
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

    test "keeps trailing whitespace if under threshold" do
      expected = """
      ** (SyntaxError) invalid syntax found on nofile:1:23:
         ┌─ error: nofile:1:23
         │
       1 │                   a + 😎
         │                       ^
         │
         unexpected token: "😎" (column 23, code point U+****)
      """

      output =
        capture_raise(
          """
                            a + 😎
          """,
          SyntaxError
        )

      assert output == expected
    end

    test "limits trailing whitespace if too many" do
      expected = """
      ** (SyntaxError) invalid syntax found on nofile:1:43:
         ┌─ error: nofile:1:43
         │
       1 │ ...                   a + 😎
         │                           ^
         │
         unexpected token: "😎" (column 43, code point U+****)
      """

      output =
        capture_raise(
          """
                                                a + 😎
          """,
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
      source = """
      defmodule Sample do
        def a do
          10 + 😎
        end
      end
      """

      output = capture_raise(source, SyntaxError)

      assert output =~ "😎"
    after
      purge(Sample)
    end
  end

  describe "compiler warnings" do
    # handles unicode
    # nofile
    # file + line
    # file + line + column
  end

  describe "warning groups" do
    test "no file" do
      source = """
      defmodule Sample do
        def a do
          A.bar()
          A.bar()
          A.bar()
          A.bar()
        end
      end
      """

      expected = """
         ┌─ warning: nofile:3:6
         A.bar/0 is undefined (module A is not available or is yet to be defined)

         Invalid call also found at 3 other locations:
           nofile:4:6: Sample.a/0
           nofile:5:6: Sample.a/0
           nofile:6:6: Sample.a/0

      """

      assert capture_eval(source) == expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "file + line + column", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "warning_group_nofile.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def a do
          A.bar()
          A.bar()
          A.bar()
          A.bar()
        end
      end
      """

      File.write!(path, source)

      expected = """
         ┌─ warning: #{path}:5:6
         │
       5 │     A.bar()
         │     ~~~~~~~
         │
         A.bar/0 is undefined (module A is not available or is yet to be defined)

         Invalid call also found at 3 other locations:
           #{path}:6:6: Sample.a/0
           #{path}:7:6: Sample.a/0
           #{path}:8:6: Sample.a/0

      """

      assert capture_eval(source) == expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "file + line", %{tmp_dir: _tmp_dir} do
      # TODO: this
      assert true
    end

    @tag :tmp_dir
    test "handles unicode", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "warning_group_unicode.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def a do
          A.bar("😎")
          A.bar("😎")
        end
      end
      """

      File.write!(path, source)

      assert capture_eval(source) =~ "😎"
    after
      purge(Sample)
    end
  end

  defp make_relative_tmp(tmp_dir, filename) do
    # Compiler outputs relative, so we just grab the tmp dir
    tmp_dir
    |> Path.join(filename)
    |> Path.split()
    |> Enum.drop_while(& &1 != "tmp")
    |> Path.join()
  end

  defp capture_eval(source) do
    capture_io(:stderr, fn ->
      quoted = Code.string_to_quoted!(source, columns: true)
      Code.eval_quoted(quoted)
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

  defp purge(module) when is_atom(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
