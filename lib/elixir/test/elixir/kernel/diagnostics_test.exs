Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DiagnosticsTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  setup do
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit(fn -> Application.put_env(:elixir, :ansi_enabled, true) end)
  end

  describe "mismatched delimiter" do
    test "trims start if distance exceeds threshold in same line" do
      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:1:77:
          error: unexpected token: }
          HINT: the "[" on line 1 is missing terminator "]"
          │
        1 │ ...long_long_name = [1, 2, 3}
          │                     │       └ mismatched closing delimiter
          │                     └ unclosed delimiter
          │
          └─ nofile:1:77\
      """

      output =
        capture_raise(
          """
          long_long_long_long_long_long_long_variable_with_a_long_long_name = [1, 2, 3}
          """,
          MismatchedDelimiterError
        )

      assert output == expected
    end

    test "trims in between if distance exceeds threshold in same line" do
      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:1:91:
          error: unexpected token: )
          HINT: the "[" on line 1 is missing terminator "]"
          │
        1 │ [1, MyLong.ReallyLong...function(), 200)
          │ │                                      └ mismatched closing delimiter
          │ └ unclosed delimiter
          │
          └─ nofile:1:91\
      """

      output =
        capture_raise(
          """
          [1, MyLong.ReallyLongModule.FromALongNamespace.calling_a_really_long_named_function(), 200)
          """,
          MismatchedDelimiterError
        )

      assert output == expected
    end

    test "same line" do
      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:1:18:
          error: unexpected token: )
          HINT: the "[" on line 1 is missing terminator "]"
          │
        1 │ [1, 2, 3, 4, 5, 6)
          │ │                └ mismatched closing delimiter
          │ └ unclosed delimiter
          │
          └─ nofile:1:18\
      """

      output =
        capture_raise(
          """
          [1, 2, 3, 4, 5, 6)
          """,
          MismatchedDelimiterError
        )

      assert output == expected
    end

    test "two-line span" do
      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:2:9:
          error: unexpected token: }
          HINT: the "[" on line 1 is missing terminator "]"
          │
        1 │ [a, b, c
          │ └ unclosed delimiter
        2 │  d, f, g}
          │         └ mismatched closing delimiter
          │
          └─ nofile:2:9\
      """

      output =
        capture_raise(
          """
          [a, b, c
           d, f, g}
          """,
          MismatchedDelimiterError
        )

      assert output == expected
    end

    test "many-line span" do
      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:5:5:
          error: unexpected token: )
          HINT: the "[" on line 1 is missing terminator "]"
          │
        1 │ [ a,
          │ └ unclosed delimiter
        2 │   b,
        3 │   c,
        4 │   d
        5 │   e )
          │     └ mismatched closing delimiter
          │
          └─ nofile:5:5\
      """

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

      assert output == expected
    end

    test "trim inbetween lines if too many" do
      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:9:5:
          error: unexpected token: )
          HINT: the "[" on line 1 is missing terminator "]"
          │
        1 │ [ a,
          │ └ unclosed delimiter
          │ ...
        9 │   i )
          │     └ mismatched closing delimiter
          │
          └─ nofile:9:5\
      """

      output =
        capture_raise(
          """
          [ a,
            b,
            c,
            d,
            e,
            f,
            g,
            h
            i )
          """,
          MismatchedDelimiterError
        )

      assert output == expected
    end

    test "pads according to line number digits" do
      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:13:5:
          error: unexpected token: )
          HINT: the "[" on line 1 is missing terminator "]"
          │
        1 │ [ a,
          │ └ unclosed delimiter
          │ ...
       13 │   b )
          │     └ mismatched closing delimiter
          │
          └─ nofile:13:5\
      """

      output =
        capture_raise(
          """
          [ a,
          #{String.duplicate("\n", 10)}
            b )
          """,
          MismatchedDelimiterError
        )

      assert output == expected

      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:403:5:
           error: unexpected token: )
           HINT: the "[" on line 1 is missing terminator "]"
           │
         1 │ [ a,
           │ └ unclosed delimiter
           │ ...
       403 │   b )
           │     └ mismatched closing delimiter
           │
           └─ nofile:403:5\
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

      assert output == expected

      expected = """
      ** (MismatchedDelimiterError) mismatched delimiter found on nofile:107:5:
           error: unexpected token: )
           HINT: the "[" on line 99 is missing terminator "]"
           │
        99 │ [ a,
           │ └ unclosed delimiter
           │ ...
       107 │   b )
           │     └ mismatched closing delimiter
           │
           └─ nofile:107:5\
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

      assert output == expected
    end

    test "trims lines that are too long (> 60 chars)" do
      expected = """
      ** (MismatchedDelimiterError) invalid syntax found on nofile:1:17:
          error: syntax error before: '*'
          │
        1 │ ...= { a,
          │      └ unclosed delimiter
        2 │ ...
        3 │ ...    b )
          │          └ mismatched closing delimiter
          │
          └─ nofile:1:17\
      """

      output =
        capture_raise(
          """
          a                                                           = { a,

                                                                         b )
          """,
          MismatchedDelimiterError
        )

      assert output == expected

      expected = """
      ** (MismatchedDelimiterError) invalid syntax found on nofile:1:17:
          error: syntax error before: '*'
          │
        1 │ ...aaaa = [ 1,
          │           └ unclosed delimiter
          │ ...
        3 │ ...aaaa" )
          │          └ mismatched closing delimiter
          │
          └─ nofile:1:17\
      """

      output =
        capture_raise(
          """
          aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = [ 1,

          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" )
          """,
          MismatchedDelimiterError
        )

      assert output == expected
    end
  end

  describe "compile-time exceptions" do
    test "SyntaxError (snippet)" do
      expected = """
      ** (SyntaxError) invalid syntax found on nofile:1:17:
          error: syntax error before: '*'
          │
        1 │ [1, 2, 3, 4, 5, *]
          │                 ^
          │
          └─ nofile:1:17\
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
          error: syntax error: expression is incomplete
          │
        1 │ 1 +
          │    ^
          │
          └─ nofile:1:4\
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
         error: missing terminator: end (for "fn" starting at line 1)
         └─ nofile:2:1\
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
          fake_stacktrace
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
        defp a, do: Unknown.b()
      end
      """

      File.write!(path, source)

      expected = """
          warning: Unknown.b/0 is undefined (module Unknown is not available or is yet to be defined)
          │
        3 │   defp a, do: Unknown.b()
          │                      ~
          │
          └─ #{path}:3:22: Sample.a/0
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

      assert capture_eval(source, false) =~ expected
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
      └─ nofile:2:22: Sample.a/0
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
          warning: incompatible types:

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
          │
        8 │       _ when is_atom(v) -> :ok
          │              ~
          │
          └─ #{path}:8:14: Sample.atom_case/0
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
      warning: incompatible types:

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
      └─ nofile:6:14: Sample.atom_case/0

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
                                                  Unknown.bar(:test)
        end
      end
      """

      File.write!(path, source)

      expected = """
          warning: Unknown.bar/1 is undefined (module Unknown is not available or is yet to be defined)
          │
        5 │ ...                   Unknown.bar(:test)
          │                              ~
          │
          └─ #{path}:5:52: Sample.a/0

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
      └─ nofile:3:12: Sample.a/0
      └─ nofile:4:12: Sample.a/0
      └─ nofile:5:12: Sample.a/0
      └─ nofile:6:12: Sample.a/0

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
          │            ~
          │
          └─ #{path}:5:12: Sample.a/0
          └─ #{path}:6:12: Sample.a/0
          └─ #{path}:7:12: Sample.a/0
          └─ #{path}:8:12: Sample.a/0

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

      assert capture_eval(source, false) == expected
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

      assert capture_compile(source, false) == expected
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
          │             ^
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

      assert capture_eval(source, false) == expected
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

  defp capture_eval(source, columns? \\ true) do
    capture_io(:stderr, fn ->
      quoted = Code.string_to_quoted!(source, columns: columns?)
      Code.eval_quoted(quoted)
    end)
  end

  defp capture_compile(source, columns? \\ true) do
    capture_io(:stderr, fn ->
      assert_raise CompileError, fn ->
        ast = Code.string_to_quoted!(source, columns: columns?)
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

  defp purge(module) when is_atom(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
