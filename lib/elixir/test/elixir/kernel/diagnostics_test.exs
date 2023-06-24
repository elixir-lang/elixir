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
    @tag :tmp_dir
    test "simple warning (line + column + file)", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "long-warning.ex")

      source = """
      defmodule Sample do
        @file "#{path}"
        defp a, do: A.b()
      end
      """

      File.write!(path, source)

      expected = """
         ┌─ warning: #{path}:3:16: Sample.a/0
         │
       3 │   defp a, do: A.b()
         │                ~
         │
         A.b/0 is undefined or private

      """

      assert capture_eval(source) =~ expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "simple warning (line + file)", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "long-warning.ex")

      source = """
      defmodule Sample do
        @file "#{path}"
        defp a, do: A.b()
      end
      """

      File.write!(path, source)

      expected = """
         ┌─ warning: #{path}:3: Sample.a/0
         │
       3 │   defp a, do: A.b()
         │   ~~~~~~~~~~~~~~~~~
         │
         A.b/0 is undefined or private

      """

      assert capture_eval(source, false) =~ expected
    after
      purge(Sample)
    end

    test "simple warning (no file)" do
      source = """
      defmodule Sample do
        defp a, do: A.b()
      end
      """

      expected = """
       ┌─ warning: nofile:2:16: Sample.a/0
       A.b/0 is undefined or private

      """

      assert capture_eval(source) =~ expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "long message (file)", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "long-warning.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def atom_case do
          v = "bc"

          case v do
            _ when is_atom(v) -> :ok
            _ -> :fail
          end
        end
      end
      """

      File.write!(path, source)

      expected = """
         ┌─ warning: #{path}:8:14: Sample.atom_case/0
         │
       8 │       _ when is_atom(v) -> :ok
         │              ~
         │
         incompatible types:
         
             binary() !~ atom()
         
         in expression:
         
             # #{path}:8
             is_atom(v)
         
         where "v" was given the type binary() in:
         
             # #{path}:5
             v = "bc"
         
         where "v" was given the type atom() in:
         
             # #{path}:8
             is_atom(v)
         
         Conflict found at

      """

      assert capture_eval(source) =~ expected
    after
      purge(Sample)
    end

    test "long message (nofile)" do
      source = """
      defmodule Sample do
        def atom_case do
          v = "bc"

          case v do
            _ when is_atom(v) -> :ok
            _ -> :fail
          end
        end
      end
      """

      expected = """
       ┌─ warning: nofile:6:14: Sample.atom_case/0
       incompatible types:
       
           binary() !~ atom()
       
       in expression:
       
           # nofile:6
           is_atom(v)
       
       where "v" was given the type binary() in:
       
           # nofile:3
           v = "bc"
       
       where "v" was given the type atom() in:
       
           # nofile:6
           is_atom(v)
       
       Conflict found at

      """

      assert capture_eval(source) =~ expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "trims lines if too many whitespaces", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "trim_warning_line.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def a do
                                                  A.bar(:test)
        end
      end
      """

      File.write!(path, source)

      expected = """
         ┌─ warning: #{path}:5:46: Sample.a/0
         │
       5 │ ...                   A.bar(:test)
         │                        ~
         │
         A.bar/1 is undefined or private

      """

      assert capture_eval(source) == expected
    after
      purge(Sample)
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
       ┌─ warning: nofile:3:6: Sample.a/0
       A.bar/0 is undefined or private
       
       Invalid call also found at 3 other locations:
         nofile:4:6: Sample.a/0
         nofile:5:6: Sample.a/0
         nofile:6:6: Sample.a/0

      """

      assert capture_eval(source) =~ expected
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
         ┌─ warning: #{path}:5:6: Sample.a/0
         │
       5 │     A.bar()
         │      ~
         │
         A.bar/0 is undefined or private
         
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
    test "file + line", %{tmp_dir: tmp_dir} do
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
         ┌─ warning: #{path}:5: Sample.a/0
         │
       5 │     A.bar()
         │     ~~~~~~~
         │
         A.bar/0 is undefined or private
         
         Invalid call also found at 3 other locations:
           #{path}:6: Sample.a/0
           #{path}:7: Sample.a/0
           #{path}:8: Sample.a/0

      """

      assert capture_eval(source, false) == expected
    after
      purge(Sample)
    end
  end

  defp make_relative_tmp(tmp_dir, filename) do
    # Compiler outputs relative, so we just grab the tmp dir
    tmp_dir
    |> Path.join(filename)
    |> Path.relative_to_cwd()
  end

  defp capture_eval(source, columns? \\ true) do
    capture_io(:stderr, fn ->
      quoted = Code.string_to_quoted!(source, columns: columns?)
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
