Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DiagnosticsTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  setup do
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit(fn -> Application.put_env(:elixir, :ansi_enabled, true) end)
  end

  describe "mismatched delimiter" do
    test "same line - handles unicode input" do
      output =
        capture_raise(
          """
          [1, 2, 3, 4, 5, 6) <- 😎
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:1:18:
                 error: unexpected token: )
                 │
               1 │ [1, 2, 3, 4, 5, 6) <- 😎
                 │ │                └ mismatched closing delimiter (expected "]")
                 │ └ unclosed delimiter
                 │
                 └─ nofile:1:18\
             """
    end

    test "same line" do
      output =
        capture_raise(
          """
          [1, 2, 3, 4, 5, 6)
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:1:18:
                 error: unexpected token: )
                 │
               1 │ [1, 2, 3, 4, 5, 6)
                 │ │                └ mismatched closing delimiter (expected "]")
                 │ └ unclosed delimiter
                 │
                 └─ nofile:1:18\
             """
    end

    test "same line with offset" do
      output =
        capture_raise(
          """
          [1, 2, 3, 4, 5, 6)
          """,
          MismatchedDelimiterError,
          line: 3
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:3:18:
                 error: unexpected token: )
                 │
               3 │ [1, 2, 3, 4, 5, 6)
                 │ │                └ mismatched closing delimiter (expected "]")
                 │ └ unclosed delimiter
                 │
                 └─ nofile:3:18\
             """
    end

    test "two-line span" do
      output =
        capture_raise(
          """
          [a, b, c
           d, f, g}
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:2:9:
                 error: unexpected token: }
                 │
               1 │ [a, b, c
                 │ └ unclosed delimiter
               2 │  d, f, g}
                 │         └ mismatched closing delimiter (expected "]")
                 │
                 └─ nofile:2:9\
             """
    end

    test "two-line span with offset" do
      output =
        capture_raise(
          """
          [a, b, c
           d, f, g}
          """,
          MismatchedDelimiterError,
          line: 3
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:4:9:
                 error: unexpected token: }
                 │
               3 │ [a, b, c
                 │ └ unclosed delimiter
               4 │  d, f, g}
                 │         └ mismatched closing delimiter (expected "]")
                 │
                 └─ nofile:4:9\
             """
    end

    test "many-line span" do
      output =
        capture_raise(
          """
              [ a,
            b,
            c,
            d
            e )
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:5:5:
                 error: unexpected token: )
                 │
               1 │     [ a,
                 │     └ unclosed delimiter
               2 │   b,
               3 │   c,
               4 │   d
               5 │   e )
                 │     └ mismatched closing delimiter (expected "]")
                 │
                 └─ nofile:5:5\
             """
    end

    test "many-line span with offset" do
      output =
        capture_raise(
          """
          fn always_forget_end ->
            IO.inspect(2 + 2) + 2
          )
          """,
          MismatchedDelimiterError,
          line: 3
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:5:1:
                 error: unexpected token: )
                 │
               3 │ fn always_forget_end ->
                 │ └ unclosed delimiter
               4 │   IO.inspect(2 + 2) + 2
               5 │ )
                 │ └ mismatched closing delimiter (expected "end")
                 │
                 └─ nofile:5:1\
             """
    end

    test "line range - handles unicode input" do
      output =
        capture_raise(
          """
          defmodule A do
            IO.inspect(2 + 2)
          ) <- 😎
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:3:1:
                 error: unexpected token: )
                 │
               1 │ defmodule A do
                 │             └ unclosed delimiter
               2 │   IO.inspect(2 + 2)
               3 │ ) <- 😎
                 │ └ mismatched closing delimiter (expected "end")
                 │
                 └─ nofile:3:1\
             """
    end

    test "trim in between lines if too many" do
      output =
        capture_raise(
          """
          [ :a,
            :b,
            :c,
            :d,
            :e,
            :f,
            :g,
            :h
          )
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:9:1:
                 error: unexpected token: )
                 │
               1 │ [ :a,
                 │ └ unclosed delimiter
              ...
               9 │ )
                 │ └ mismatched closing delimiter (expected "]")
                 │
                 └─ nofile:9:1\
             """
    end

    test "trimmed line range - handles unicode input" do
      output =
        capture_raise(
          """
          [ :a,
            :b,
            :c,
            :d,
            :e,
            :f,
            :g,
            :h
          ) <- 😎
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:9:1:
                 error: unexpected token: )
                 │
               1 │ [ :a,
                 │ └ unclosed delimiter
              ...
               9 │ ) <- 😎
                 │ └ mismatched closing delimiter (expected "]")
                 │
                 └─ nofile:9:1\
             """
    end

    test "pads according to line number digits" do
      output =
        capture_raise(
          """
          [ a,
          #{String.duplicate("\n", 10)}
            b )
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:13:5:
                 error: unexpected token: )
                 │
               1 │ [ a,
                 │ └ unclosed delimiter
              ...
              13 │   b )
                 │     └ mismatched closing delimiter (expected "]")
                 │
                 └─ nofile:13:5\
             """

      output =
        capture_raise(
          """
          [ a,
          #{String.duplicate("\n", 400)}
            b )
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:403:5:
                  error: unexpected token: )
                  │
                1 │ [ a,
                  │ └ unclosed delimiter
              ...
              403 │   b )
                  │     └ mismatched closing delimiter (expected "]")
                  │
                  └─ nofile:403:5\
             """

      output =
        capture_raise(
          """
          #{String.duplicate("\n", 97)}
          [ a,
          #{String.duplicate("\n", 6)}
            b )
          """,
          MismatchedDelimiterError
        )

      assert output == """
             ** (MismatchedDelimiterError) mismatched delimiter found on nofile:107:5:
                  error: unexpected token: )
                  │
               99 │ [ a,
                  │ └ unclosed delimiter
              ...
              107 │   b )
                  │     └ mismatched closing delimiter (expected "]")
                  │
                  └─ nofile:107:5\
             """
    end
  end

  describe "token missing error" do
    test "missing parens terminator" do
      output =
        capture_raise(
          """
          my_numbers = [1, 2, 3, 4, 5, 6
          IO.inspect(my_numbers)
          """,
          TokenMissingError
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:2:23:
                 error: missing terminator: ]
                 │
               1 │ my_numbers = [1, 2, 3, 4, 5, 6
                 │              └ unclosed delimiter
               2 │ IO.inspect(my_numbers)
                 │                       └ missing closing delimiter (expected "]")
                 │
                 └─ nofile:2:23\
             """
    end

    test "missing heredoc terminator" do
      output =
        capture_raise(
          """
          a = \"""
          test string

          IO.inspect(10 + 20)
          """,
          TokenMissingError
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:4:20:
                 error: missing terminator: \""" (for heredoc starting at line 1)
                 │
               1 │ a = \"""
                 │     └ unclosed delimiter
               2 │ test string
               3 │\s
               4 │ IO.inspect(10 + 20)
                 │                    └ missing closing delimiter (expected \""")
                 │
                 └─ nofile:4:20\
             """
    end

    test "missing sigil terminator" do
      output =
        capture_raise("~s<foobar", TokenMissingError)

      assert output == """
             ** (TokenMissingError) token missing on nofile:1:10:
                 error: missing terminator: > (for sigil ~s< starting at line 1)
                 │
               1 │ ~s<foobar
                 │   │      └ missing closing delimiter (expected ">")
                 │   └ unclosed delimiter
                 │
                 └─ nofile:1:10\
             """

      output =
        capture_raise("~s|foobar", TokenMissingError)

      assert output == """
             ** (TokenMissingError) token missing on nofile:1:10:
                 error: missing terminator: | (for sigil ~s| starting at line 1)
                 │
               1 │ ~s|foobar
                 │   │      └ missing closing delimiter (expected "|")
                 │   └ unclosed delimiter
                 │
                 └─ nofile:1:10\
             """
    end

    test "missing string terminator" do
      output =
        capture_raise("\"foobar", TokenMissingError)

      assert output == """
             ** (TokenMissingError) token missing on nofile:1:8:
                 error: missing terminator: " (for string starting at line 1)
                 │
               1 │ "foobar
                 │ │      └ missing closing delimiter (expected ")
                 │ └ unclosed delimiter
                 │
                 └─ nofile:1:8\
             """
    end

    test "missing atom terminator" do
      output =
        capture_raise(":\"foobar", TokenMissingError)

      assert output == """
             ** (TokenMissingError) token missing on nofile:1:9:
                 error: missing terminator: " (for atom starting at line 1)
                 │
               1 │ :"foobar
                 │  │      └ missing closing delimiter (expected ")
                 │  └ unclosed delimiter
                 │
                 └─ nofile:1:9\
             """
    end

    test "missing function terminator" do
      output =
        capture_raise("K.\"foobar", TokenMissingError)

      assert output == """
             ** (TokenMissingError) token missing on nofile:1:10:
                 error: missing terminator: " (for function name starting at line 1)
                 │
               1 │ K."foobar
                 │   │      └ missing closing delimiter (expected ")
                 │   └ unclosed delimiter
                 │
                 └─ nofile:1:10\
             """
    end

    test "shows in between lines if EOL is not far below" do
      output =
        capture_raise(
          """
          my_numbers = [1, 2, 3, 4, 5, 6
          my_numbers
          |> Enum.map(&(&1 + 1))
          |> Enum.map(&(&1 * &1))
          |> IO.inspect()
          """,
          TokenMissingError
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:5:16:
                 error: missing terminator: ]
                 │
               1 │ my_numbers = [1, 2, 3, 4, 5, 6
                 │              └ unclosed delimiter
               2 │ my_numbers
               3 │ |> Enum.map(&(&1 + 1))
               4 │ |> Enum.map(&(&1 * &1))
               5 │ |> IO.inspect()
                 │                └ missing closing delimiter (expected "]")
                 │
                 └─ nofile:5:16\
             """
    end

    test "trims lines" do
      output =
        capture_raise(
          """
          my_numbers = (1, 2, 3, 4, 5, 6






          IO.inspect(my_numbers)
          """,
          TokenMissingError
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:8:23:
                 error: missing terminator: )
                 │
               1 │ my_numbers = (1, 2, 3, 4, 5, 6
                 │              └ unclosed delimiter
              ...
               8 │ IO.inspect(my_numbers)
                 │                       └ missing closing delimiter (expected ")")
                 │
                 └─ nofile:8:23\
             """
    end

    test "shows the last non-empty line of a file" do
      output =
        capture_raise(
          """
          my_numbers = {1, 2, 3, 4, 5, 6
          IO.inspect(my_numbers)




          """,
          TokenMissingError
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:2:23:
                 error: missing terminator: }
                 │
               1 │ my_numbers = {1, 2, 3, 4, 5, 6
                 │              └ unclosed delimiter
               2 │ IO.inspect(my_numbers)
                 │                       └ missing closing delimiter (expected "}")
                 │
                 └─ nofile:2:23\
             """
    end

    test "supports unicode" do
      output =
        capture_raise(
          """
          my_emojis = [1, 2, 3, 4 # ⚗️
          IO.inspect(my_numbers)
          """,
          TokenMissingError
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:2:23:
                 error: missing terminator: ]
                 │
               1 │ my_emojis = [1, 2, 3, 4 # ⚗️
                 │             └ unclosed delimiter
               2 │ IO.inspect(my_numbers)
                 │                       └ missing closing delimiter (expected "]")
                 │
                 └─ nofile:2:23\
             """
    end
  end

  describe "compile-time exceptions" do
    test "SyntaxError (snippet)" do
      output =
        capture_raise(
          """
          [1, 2, 3, 4, 5, *]
          """,
          SyntaxError
        )

      assert output == """
             ** (SyntaxError) invalid syntax found on nofile:1:17:
                 error: syntax error before: '*'
                 │
               1 │ [1, 2, 3, 4, 5, *]
                 │                 ^
                 │
                 └─ nofile:1:17\
             """
    end

    test "SyntaxError (snippet) with offset" do
      output =
        capture_raise(
          """
          [1, 2, 3, 4, 5, *]
          """,
          SyntaxError,
          line: 3
        )

      assert output == """
             ** (SyntaxError) invalid syntax found on nofile:3:17:
                 error: syntax error before: '*'
                 │
               3 │ [1, 2, 3, 4, 5, *]
                 │                 ^
                 │
                 └─ nofile:3:17\
             """
    end

    test "TokenMissingError (snippet)" do
      output =
        capture_raise(
          """
          1 +
          """,
          TokenMissingError
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:1:4:
                 error: syntax error: expression is incomplete
                 │
               1 │ 1 +
                 │    ^
                 │
                 └─ nofile:1:4\
             """
    end

    test "TokenMissingError (snippet) with offset and column" do
      output =
        capture_raise(
          """
          1 +
          """,
          TokenMissingError,
          line: 3,
          column: 3
        )

      assert output == """
             ** (TokenMissingError) token missing on nofile:3:6:
                 error: syntax error: expression is incomplete
                 │
               3 │   1 +
                 │      ^
                 │
                 └─ nofile:3:6\
             """
    end

    test "TokenMissingError (unclosed delimiter)" do
      expected = """
      ** (TokenMissingError) token missing on nofile:1:5:
          error: missing terminator: end
          │
        1 │ fn a
          │ │   └ missing closing delimiter (expected "end")
          │ └ unclosed delimiter
          │
          └─ nofile:1:5\
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
          error: unexpected token: "😎" (column 23, code point U+****)
          │
        1 │                   a + 😎
          │                       ^
          │
          └─ nofile:1:23\
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
          error: unexpected token: "😎" (column 43, code point U+****)
          │
        1 │ ...                   a + 😎
          │                           ^
          │
          └─ nofile:1:43\
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
          error: syntax error: expression is incomplete
          │
        1 │ 1 -
          │    ^
          │
          └─ nofile:1:4
          nofile:10: :fake.fun/3
          nofile:10: :real.fun/2
      """

      output =
        capture_raise(
          """
          1 -
          """,
          TokenMissingError,
          stacktrace: fake_stacktrace
        )

      assert output == expected
    end

    test "2-digit line errors stay aligned 1-digit line errors" do
      fake_stacktrace = [
        {:fake, :fun, 3, [file: "nofile", line: 10]}
      ]

      expected = """
      ** (TokenMissingError) token missing on nofile:12:4:
          error: syntax error: expression is incomplete
          │
       12 │ 1 -
          │    ^
          │
          └─ nofile:12:4
          nofile:10: :fake.fun/3
      """

      output =
        capture_raise(
          """
          #{String.duplicate("\n", 10)}
          1 -
          """,
          TokenMissingError,
          stacktrace: fake_stacktrace
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
        defp a, do: Unknown.b()
      end
      """

      File.write!(path, source)

      expected = """
          warning: Unknown.b/0 is undefined (module Unknown is not available or is yet to be defined)
          │
        3 │   defp a, do: Unknown.b()
          │                       ~
          │
          └─ #{path}:3:23: Sample.a/0
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
        defp a, do: Unknown.b()
      end
      """

      File.write!(path, source)

      expected = """
          warning: Unknown.b/0 is undefined (module Unknown is not available or is yet to be defined)
          │
        3 │   defp a, do: Unknown.b()
          │   ~~~~~~~~~~~~~~~~~~~~~~~
          │
          └─ #{path}:3: Sample.a/0
      """

      assert capture_eval(source, columns: false) =~ expected
    after
      purge(Sample)
    end

    test "simple warning (no file)" do
      source = """
      defmodule Sample do
        defp a, do: Unknown.b()
      end
      """

      expected = """
      warning: Unknown.b/0 is undefined (module Unknown is not available or is yet to be defined)
      └─ nofile:2:23: Sample.a/0
      """

      assert capture_eval(source) =~ expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "IO.warn file+line", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "io-warn-file-line.ex")

      source = """
      IO.warn("oops\\nmulti\\nline", file: __ENV__.file, line: __ENV__.line)
      """

      File.write!(path, source)

      expected = """
          warning: oops
          multi
          line
          │
        1 │ IO.warn("oops\\nmulti\\nline", file: __ENV__.file, line: __ENV__.line)
          │ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          │
          └─ tmp\
      """

      assert capture_io(:stderr, fn -> Code.eval_file(path) end) =~ expected
    end

    @tag :tmp_dir
    test "IO.warn file+line+column", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "io-warn-file-line-column.ex")

      source = """
      IO.warn("oops\\nmulti\\nline", file: __ENV__.file, line: __ENV__.line, column: 4)
      """

      File.write!(path, source)

      expected = """
          warning: oops
          multi
          line
          │
        1 │ IO.warn("oops\\nmulti\\nline", file: __ENV__.file, line: __ENV__.line, column: 4)
          │    ~
          │
          └─ tmp\
      """

      assert capture_io(:stderr, fn -> Code.eval_file(path) end) =~ expected
    end

    test "IO.warn with missing data" do
      assert capture_eval("""
             IO.warn("oops-bad", file: #{inspect(__ENV__.file)}, line: 3, column: nil)
             """) =~ "warning: oops-bad"

      assert capture_eval("""
             IO.warn("oops-bad", file: #{inspect(__ENV__.file)}, line: nil)
             """) =~ "oops-bad"

      assert capture_eval("""
             IO.warn("oops-bad", file: nil)
             """) =~ "oops-bad"
    end

    @tag :tmp_dir
    test "trims lines if too many whitespaces", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "trim_warning_line.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def a do
                                                  Unknown.bar(:test)
        end
      end
      """

      File.write!(path, source)

      expected = """
          warning: Unknown.bar/1 is undefined (module Unknown is not available or is yet to be defined)
          │
        5 │ ...                   Unknown.bar(:test)
          │                               ~
          │
          └─ #{path}:5:53: Sample.a/0

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
          Unknown.bar("😎")
          Unknown.bar("😎")
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
          Unknown.bar()
          Unknown.bar()
          Unknown.bar()
          Unknown.bar()
        end
      end
      """

      expected = """
      warning: Unknown.bar/0 is undefined (module Unknown is not available or is yet to be defined)
      └─ nofile:3:13: Sample.a/0
      └─ nofile:4:13: Sample.a/0
      └─ nofile:5:13: Sample.a/0
      └─ nofile:6:13: Sample.a/0

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
          Unknown.bar()
          Unknown.bar()
          Unknown.bar()
          Unknown.bar()
        end
      end
      """

      File.write!(path, source)

      expected = """
          warning: Unknown.bar/0 is undefined (module Unknown is not available or is yet to be defined)
          │
        5 │     Unknown.bar()
          │             ~
          │
          └─ #{path}:5:13: Sample.a/0
          └─ #{path}:6:13: Sample.a/0
          └─ #{path}:7:13: Sample.a/0
          └─ #{path}:8:13: Sample.a/0

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
          Unknown.bar()
          Unknown.bar()
          Unknown.bar()
          Unknown.bar()
        end
      end
      """

      File.write!(path, source)

      expected = """
          warning: Unknown.bar/0 is undefined (module Unknown is not available or is yet to be defined)
          │
        5 │     Unknown.bar()
          │     ~~~~~~~~~~~~~
          │
          └─ #{path}:5: Sample.a/0
          └─ #{path}:6: Sample.a/0
          └─ #{path}:7: Sample.a/0
          └─ #{path}:8: Sample.a/0

      """

      assert capture_eval(source, columns: false) == expected
    after
      purge(Sample)
    end
  end

  describe "error diagnostics" do
    @tag :tmp_dir
    test "line only", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "error_line_only.ex")

      source = """
      defmodule Sample do
        @file "#{path}"
        def CamelCase do
        end
      end
      """

      File.write!(path, source)

      expected = """
          error: function names should start with lowercase characters or underscore, invalid name CamelCase
          │
        3 │   def CamelCase do
          │   ^^^^^^^^^^^^^^^^
          │
          └─ #{path}:3

      """

      assert capture_compile(source, columns: false) == expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "shows span for unused variables", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "error_line_column.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def foo(unused_param) do
          :constant
        end
      end
      """

      File.write!(path, source)

      expected = """
          warning: variable "unused_param" is unused (if the variable is not meant to be used, prefix it with an underscore)
          │
        4 │   def foo(unused_param) do
          │           ~~~~~~~~~~~~
          │
          └─ #{path}:4:11: Sample.foo/1

      """

      assert capture_eval(source) == expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "shows span for undefined variables", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "undefined_variable_span.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def foo(a) do
          a - unknown_var
        end
      end
      """

      File.write!(path, source)

      expected = """
          error: undefined variable "unknown_var"
          │
        5 │     a - unknown_var
          │         ^^^^^^^^^^^
          │
          └─ #{path}:5:9: Sample.foo/1

      """

      assert capture_compile(source) == expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "shows span for unknown local function calls", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "unknown_local_function_call.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def foo do
          _result = unknown_func_call!(:hello!)
        end
      end
      """

      File.write!(path, source)

      expected = """
          error: undefined function unknown_func_call!/1 (expected Sample to define such a function or for it to be imported, but none are available)
          │
        5 │     _result = unknown_func_call!(:hello!)
          │               ^^^^^^^^^^^^^^^^^^
          │
          └─ #{path}:5:15: Sample.foo/0

      """

      assert capture_compile(source) == expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "line + column", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "error_line_column.ex")

      source = """
      defmodule Sample do
        @file "#{path}"

        def foo do
          IO.puts(bar)
        end
      end
      """

      File.write!(path, source)

      expected = """
          error: undefined variable "bar"
          │
        5 │     IO.puts(bar)
          │             ^^^
          │
          └─ #{path}:5:13: Sample.foo/0

      """

      assert capture_compile(source) == expected
    after
      purge(Sample)
    end

    test "no file" do
      expected = """
      error: undefined function module_info/0 (this function is auto-generated by the compiler and must always be called as a remote, as in __MODULE__.module_info/0)
      └─ nofile:2:16: Sample.foo/0

      """

      output =
        capture_compile("""
        defmodule Sample do
          def foo, do: module_info()
        end
        """)

      assert expected == output
    after
      purge(Sample)
    end
  end

  describe "warning diagnostics" do
    @tag :tmp_dir
    test "line only", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "warn_line.ex")

      source = """
      defmodule Sample do
        @file "#{path}"
        def a(unused), do: 1
      end
      """

      File.write!(path, source)

      expected = """
          warning: variable "unused" is unused (if the variable is not meant to be used, prefix it with an underscore)
          │
        3 │   def a(unused), do: 1
          │   ~~~~~~~~~~~~~~~~~~~~
          │
          └─ #{path}:3: Sample.a/1

      """

      assert capture_eval(source, columns: false) == expected
    after
      purge(Sample)
    end

    @tag :tmp_dir
    test "line + column", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "warn_line_column.ex")

      source = """
      defmodule Sample do
        @file "#{path}"
        @foo 1

        def bar do
          @foo
          :ok
        end
      end
      """

      File.write!(path, source)

      expected = """
          warning: module attribute @foo in code block has no effect as it is never returned (remove the attribute or assign it to _ to avoid warnings)
          │
        6 │     @foo
          │     ~
          │
          └─ #{path}:6:5: Sample.bar/0

      """

      assert capture_eval(source) == expected
    after
      purge(Sample)
    end

    test "no file" do
      expected = """
      warning: unused alias List
      └─ nofile:2:3

      """

      output =
        capture_eval("""
        defmodule Sample do
          alias :lists, as: List
          import MapSet
          new()
        end
        """)

      assert output == expected
    after
      purge(Sample)
    end
  end

  describe "Code.print_diagnostic" do
    @tag :tmp_dir
    test "handles diagnostic with span", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "diagnostic_length.ex")

      diagnostic = %{
        file: path,
        severity: :error,
        message: "Diagnostic span test",
        stacktrace: [],
        position: {4, 7},
        span: {4, 10},
        compiler_name: "Elixir"
      }

      source = """
      defmodule Sample do
        @file "#{path}"

        def bar do
          nil
        end
      end
      """

      File.write!(path, source)

      result =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          Code.print_diagnostic(diagnostic)
        end)

      assert result == """
                 error: Diagnostic span test
                 │
               4 │   def bar do
                 │       ^^^
                 │
                 └─ #{path}:4:7

             """
    end

    @tag :tmp_dir
    test "prints single marker for multiline diagnostic", %{tmp_dir: tmp_dir} do
      path = make_relative_tmp(tmp_dir, "diagnostic_length.ex")

      diagnostic = %{
        file: path,
        severity: :error,
        message: "Diagnostic span test",
        stacktrace: [],
        position: {4, 7},
        span: {5, 2},
        compiler_name: "Elixir"
      }

      source = """
      defmodule Sample do
        @file "#{path}"

        def bar do
          nil
        end
      end
      """

      File.write!(path, source)

      result =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          Code.print_diagnostic(diagnostic)
        end)

      assert result == """
                 error: Diagnostic span test
                 │
               4 │   def bar do
                 │       ^
                 │
                 └─ #{path}:4:7

             """
    end
  end

  defp make_relative_tmp(tmp_dir, filename) do
    # Compiler outputs relative, so we just grab the tmp dir
    tmp_dir
    |> Path.join(filename)
    |> Path.relative_to_cwd()
  end

  defp capture_eval(source, opts \\ [columns: true]) do
    capture_io(:stderr, fn ->
      quoted = Code.string_to_quoted!(source, opts)
      Code.eval_quoted(quoted)
    end)
  end

  defp capture_compile(source, opts \\ [columns: true]) do
    capture_io(:stderr, fn ->
      assert_raise CompileError, fn ->
        ast = Code.string_to_quoted!(source, opts)
        Code.eval_quoted(ast)
      end
    end)
  end

  defp capture_raise(source, exception, opts \\ []) do
    {stacktrace, opts} = Keyword.pop(opts, :stacktrace, [])

    e =
      assert_raise exception, fn ->
        ast = Code.string_to_quoted!(source, [columns: true] ++ opts)
        Code.eval_quoted(ast)
      end

    Exception.format(:error, e, stacktrace)
  end

  defp purge(module) when is_atom(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
