Code.require_file("../test_helper.exs", __DIR__)

import ExUnit.TestHelpers

defmodule ExUnit.DocTestTest.GoodModule do
  @doc """
  iex> one()
  1
  iex> one() + 1
  2
  """
  def one, do: 1

  @doc ~S"""
  iex> ~S(f#{o}o)
  "f\#{o}o"
  """
  def test_sigil, do: :ok

  @doc "    iex>1 + 2\n    3"
  def no_trailing_new_line, do: :ok

  @doc """
  iex> a = 1
  iex> b = a + 2
  3
  iex> a + b
  4
  """
  def single_context, do: :ok

  @doc """
  iex> 1 + (fn() -> "" end).()
  ** (ArithmeticError) bad argument in arithmetic expression

  iex> 2 + (fn() -> :a end).()
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def two_exceptions, do: :ok

  @doc """
  iex> 1 + (fn() -> :a end).()
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def exception_test, do: :ok

  @doc ~S"""
  iex> raise "foo\nbar"
  ** (RuntimeError) foo
  bar
  """
  def multiline_exception_test, do: :ok

  @doc """
  iex> Enum.into([:a, :b, :c], MapSet.new)
  #MapSet<[:a, :b, :c]>
  """
  def inspect1_test, do: :ok

  @doc """
  iex> x = Enum.into([:a, :b, :c], MapSet.new)
  ...> x
  #MapSet<[:a, :b, :c]>
  """
  def inspect2_test, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.MultipleExceptions do
  @doc """
  iex> 1 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  iex> 2 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def two_exceptions, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.SomewhatGoodModuleWithOnly do
  @doc """
  iex> one()
  1
  iex> one() + 1
  2
  """
  def one, do: 1

  @doc """
  iex> two()
  2
  iex> two() + 1
  100
  """
  def two, do: 2
end
|> write_beam

defmodule ExUnit.DocTestTest.SomewhatGoodModuleWithExcept do
  @moduledoc """
  iex> 1 + 1
  1
  """

  @doc """
  iex> one()
  1
  iex> one() + 1
  2
  """
  def one, do: 1

  @doc """
  iex> two()
  2
  iex> two() + 1
  100
  """
  def two, do: 2
end
|> write_beam

defmodule ExUnit.DocTestTest.NoImport do
  @doc """
  iex> ExUnit.DocTestTest.NoImport.max(1, 2)
  {:ok, 2}

  iex> max(1, 2)
  2
  """
  def max(a, b), do: {:ok, Kernel.max(a, b)}
end
|> write_beam

defmodule ExUnit.DocTestTest.Invalid do
  @moduledoc """

      iex> 1 + * 1
      1

      iex> 1 + hd(List.flatten([1]))
      3

      iex> a = "This is an egregiously long text string."
      iex> b = ~r{an egregiously long}
      iex> c = "a slightly shorter"
      iex> String.replace(a, b, c)
      "This is a much shorter text string."

      iex> :oops
      #MapSet<[]>

      iex> Hello.world
      :world

      iex> raise "oops"
      ** (WhatIsThis) oops

      iex> raise "oops"
      ** (RuntimeError) hello

  """

  @doc """
      iex> 1 + * 1
      1
  """
  def a(), do: :ok

  @doc """
      iex> 1 + * 1
      1
  """
  defmacro b(), do: :ok

  @doc """
    ```
    iex> 1 + 2
    3
  ```
  """
  def indented_not_enough, do: :ok

  @doc ~S'''
  ```
  iex> 1 + 2
  3
    ```
  '''
  def indented_too_much, do: :ok

  @doc """
      ```
  iex> 1 + 2
  3
      ```
  """
  def dedented_past_fence, do: :ok

  @doc """
  iex> String.valid?("invalid utf8 \xFF")
  false
  """
  def invalid_utf8, do: :ok

  @doc """
      iex> {:ok, MapSet.new([1, 2, 3])}
      {:ok, #MapSet<[1, 2, 3]>}
  """
  def misplaced_opaque_type, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.IndentationHeredocs do
  @doc ~S'''
  Receives a test and formats its failure.

  ## Examples

      iex> "  1\n  2\n"
      """
        1
        2
      """

  '''
  def heredocs, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.IndentationMismatchedPrompt do
  @doc ~S'''
    iex> foo = 1
     iex> bar = 2
    iex> foo + bar
    3
  '''
  def mismatched, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.IndentationTooMuch do
  @doc ~S'''
    iex> 1 + 2
      3
  '''
  def too_much, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.IndentationNotEnough do
  @doc ~S'''
      iex> 1 + 2
    3
  '''
  def test_fun, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.FencedHeredocs do
  @doc ~S'''
  Receives a test and formats its failure.

  ## Examples

  ```
  iex> 1 + 2
  3
  ```

      ```
      iex> 1 + 2
      3
      ```

  ```
      iex> 1 + 2
      3
  ```
  '''
  def heredocs, do: :ok

  @doc ~S'''
  ```
  iex> 1 + 2
  3
  '''
  def incomplete, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.Incomplete do
  @doc ~S'''
      iex> 1 + 2

  '''
  def test_fun, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.IncompleteNoTrailingNewLine do
  @doc "iex> 3 + 4"
  def test_fun, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.FenceIncomplete do
  @doc ~S'''
  ```
  iex> 1 + 2
  3
  '''
  def test_fun, do: :ok
end
|> write_beam

defmodule ExUnit.DocTestTest.Numbered do
  @doc """
  iex(1)> 1 +
  ...(1)> 2
  3
  """
  def test_fun(), do: :ok
end
|> write_beam()

defmodule ExUnit.DocTestTest.Host do
  @doc """
      iex(foo@bar)1> 1 +
      ...(foo@bar)1> 2
      3
  """
  def test_fun(), do: :ok
end
|> write_beam()

defmodule ExUnit.DocTestTest.Haiku do
  @moduledoc """
  This module describes the ancient Japanese poem form known as Haiku.

  The Inspect protocol has been overridden for `%Haiku{}`
  so that Haikus are shown in a pretty-printed fashion.

  This module is part of the DocTest test suite,
  to ensure that DocTest can handle opaque inspect types
  which contain unicode and possibly consist of multiple lines.
  """

  defstruct [:first_phrase, :second_phrase, :third_phrase, :author]

  @doc """
  Creates a new Haiku.
  Optionally pass in the `author` as fourth argument.

  ## Examples:

      # Simple Haiku, inspect output consists of multiple lines.
      iex> ExUnit.DocTestTest.Haiku.new("Haikus are easy", "But sometimes they don't make sense", "Refrigerator")
      #Haiku<
        Haikus are easy
        But sometimes they don't make sense
        Refrigerator
      >

      # Haiku with Unicode characters (Japanese Kanji, em-dash).
      iex> ExUnit.DocTestTest.Haiku.new("古池や", "蛙飛びこむ", "水の音", "Matsuo Basho")
      #Haiku<
        古池や
        蛙飛びこむ
        水の音
        -- Matsuo Basho
      >

  """
  def new(first, second, third, author \\ "")
      when is_binary(first) and is_binary(second) and is_binary(third) and is_binary(author) do
    %__MODULE__{
      first_phrase: first,
      second_phrase: second,
      third_phrase: third,
      author: author
    }
  end

  defimpl Inspect do
    def inspect(haiku, _opts) do
      author = if haiku.author == "", do: "", else: "\n  -- #{haiku.author}"

      """
      #Haiku<
        #{haiku.first_phrase}
        #{haiku.second_phrase}
        #{haiku.third_phrase}#{author}
      >
      """
      |> String.trim_trailing("\n")
    end
  end
end
|> write_beam

defmodule ExUnit.DocTestTest do
  use ExUnit.Case

  # This is intentional. The doctests in DocTest's docs
  # fail for demonstration purposes.
  # doctest ExUnit.DocTest

  doctest ExUnit.DocTestTest.GoodModule, import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithOnly, only: [one: 0], import: true

  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithExcept,
    except: [:moduledoc, two: 0],
    import: true

  doctest ExUnit.DocTestTest.NoImport
  doctest ExUnit.DocTestTest.IndentationHeredocs
  doctest ExUnit.DocTestTest.FencedHeredocs
  doctest ExUnit.DocTestTest.Haiku

  import ExUnit.CaptureIO

  test "multiple functions filtered with :only" do
    defmodule MultipleOnly do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.SomewhatGoodModuleWithOnly, only: [one: 0, two: 0], import: true
    end

    ExUnit.Server.modules_loaded()
    assert capture_io(fn -> ExUnit.run() end) =~ "2 doctests, 1 failure"
  end

  test "empty :only" do
    defmodule EmptyOnly do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.SomewhatGoodModuleWithOnly, only: [], import: true
    end

    ExUnit.Server.modules_loaded()
    output = capture_io(fn -> ExUnit.run() end)

    assert output =~ "0 failures"
    refute output =~ "doctest"
  end

  test "doctest failures" do
    # When adding or removing lines above this line, the tests below will
    # fail because we are explicitly asserting some doctest lines from
    # ActuallyCompiled in the format of "test/ex_unit/doc_test_test.exs:<LINE>".
    defmodule ActuallyCompiled do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.Invalid
    end

    doctest_line = __ENV__.line - 3

    ExUnit.configure(seed: 0, colors: [enabled: false])
    ExUnit.Server.modules_loaded()
    output = capture_io(fn -> ExUnit.run() end)

    assert output =~ """
             1) doctest module ExUnit.DocTestTest.Invalid (1) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:138: syntax error before: '*'
                code: 1 + * 1
                stacktrace:
                  test/ex_unit/doc_test_test.exs:138: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
             2) doctest module ExUnit.DocTestTest.Invalid (2) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest failed
                doctest:
                       iex> 1 + hd(List.flatten([1]))
                       3
                code:  1 + hd(List.flatten([1])) === 3
                left:  2
                right: 3
                stacktrace:
                  test/ex_unit/doc_test_test.exs:141: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
             3) doctest module ExUnit.DocTestTest.Invalid (3) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest failed
                doctest:
                       iex> a = "This is an egregiously long text string."
                       iex> b = ~r{an egregiously long}
                       iex> c = "a slightly shorter"
                       iex> String.replace(a, b, c)
                       "This is a much shorter text string."
                code:  a = "This is an egregiously long text string."
                        b = ~r{an egregiously long}
                        c = "a slightly shorter"
                        String.replace(a, b, c) === "This is a much shorter text string."
                left:  "This is a slightly shorter text string."
                right: "This is a much shorter text string."
                stacktrace:
                  test/ex_unit/doc_test_test.exs:144: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
             4) doctest module ExUnit.DocTestTest.Invalid (4) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest failed
                doctest:
                       iex> :oops
                       "#MapSet<[]>"
                code:  inspect(:oops) === "#MapSet<[]>"
                left:  ":oops"
                right: "#MapSet<[]>"
                stacktrace:
                  test/ex_unit/doc_test_test.exs:150: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
             5) doctest module ExUnit.DocTestTest.Invalid (5) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                ** (UndefinedFunctionError) function Hello.world/0 is undefined (module Hello is not available)
                stacktrace:
                  Hello.world()
                  (for doctest at) test/ex_unit/doc_test_test.exs:153: (test)
           """

    assert output =~ """
             6) doctest module ExUnit.DocTestTest.Invalid (6) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest failed: expected exception WhatIsThis but got RuntimeError with message "oops"
                doctest:
                      iex> raise "oops"
                      ** (WhatIsThis) "oops"
                code: raise "oops"
                stacktrace:
                  test/ex_unit/doc_test_test.exs:156: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
             7) doctest module ExUnit.DocTestTest.Invalid (7) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest failed: wrong message for RuntimeError
                expected:
                  "hello"
                actual:
                  "oops"
                doctest:
                      iex> raise "oops"
                      ** (RuntimeError) "hello"
                code: raise "oops"
                stacktrace:
                  test/ex_unit/doc_test_test.exs:159: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
             8) doctest ExUnit.DocTestTest.Invalid.a/0 (8) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:165: syntax error before: '*'
                code: 1 + * 1
                stacktrace:
                  test/ex_unit/doc_test_test.exs:165: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
             9) doctest ExUnit.DocTestTest.Invalid.dedented_past_fence/0 (9) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:195: unexpected token: "`" (column 5, code point U+0060)
                code: 3
                          ```
                stacktrace:
                  test/ex_unit/doc_test_test.exs:194: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
            10) doctest ExUnit.DocTestTest.Invalid.indented_not_enough/0 (10) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:179: unexpected token: "`" (column 1, code point U+0060)
                code: 3
                      `
                stacktrace:
                  test/ex_unit/doc_test_test.exs:178: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
            11) doctest ExUnit.DocTestTest.Invalid.indented_too_much/0 (11) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:187: unexpected token: "`" (column 3, code point U+0060)
                code: 3
                        ```
                stacktrace:
                  test/ex_unit/doc_test_test.exs:186: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
            12) doctest ExUnit.DocTestTest.Invalid.invalid_utf8/0 (12) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (UnicodeConversionError) invalid encoding starting at <<255, 34, 41>>
                stacktrace:
                  test/ex_unit/doc_test_test.exs:201: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
            13) doctest ExUnit.DocTestTest.Invalid.misplaced_opaque_type/0 (13) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (TokenMissingError) test/ex_unit/doc_test_test.exs:207: missing terminator: } (for "{" starting at line 207). If you are planning to assert on the result of an iex> expression which contains a value inspected as #Name<...>, please make sure the inspected value is placed at the beginning of the expression; otherwise Elixir will treat it as a comment due to the leading sign #.
                code: {:ok, #MapSet<[1, 2, 3]>}
                stacktrace:
                  test/ex_unit/doc_test_test.exs:207: ExUnit.DocTestTest.Invalid (module)
           """

    assert output =~ """
            14) doctest ExUnit.DocTestTest.Invalid.b/0 (14) (ExUnit.DocTestTest.ActuallyCompiled)
                test/ex_unit/doc_test_test.exs:#{doctest_line}
                Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:171: syntax error before: '*'
                code: 1 + * 1
                stacktrace:
                  test/ex_unit/doc_test_test.exs:171: ExUnit.DocTestTest.Invalid (module)
           """
  end

  test "IEx prefix contains a number" do
    defmodule NumberedUsage do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.Numbered
    end

    ExUnit.Server.modules_loaded()
    assert capture_io(fn -> ExUnit.run() end) =~ "1 doctest, 0 failures"
  end

  test "IEx prompt contains host" do
    message =
      ~s[unknown IEx prompt: "iex(foo@bar)1> 1 +".\nAccepted formats are: iex>, iex(1)>, ...>, ...(1)>]

    regex = ~r[test/ex_unit/doc_test_test\.exs:\d+: #{Regex.escape(message)}]

    assert_raise ExUnit.DocTest.Error, regex, fn ->
      defmodule HostUsage do
        use ExUnit.Case
        doctest ExUnit.DocTestTest.Host
      end
    end
  end

  test "doctests type" do
    defmodule DoctestType do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.NoImport

      setup test do
        assert test.test_type == :doctest
        :ok
      end
    end

    ExUnit.Server.modules_loaded()
    assert capture_io(fn -> ExUnit.run() end) =~ "2 doctests, 0 failures"
  end

  test "multiple exceptions in one test case is not supported" do
    message = ~r"multiple exceptions in one doctest case are not supported"

    assert_raise ExUnit.DocTest.Error, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.MultipleExceptions
      end
    end
  end

  test "fails on invalid module" do
    message = ~r"module ExUnit\.DocTestTest\.Unknown is not loaded and could not be found"

    assert_raise CompileError, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Unknown
      end
    end
  end

  test "fails when there are no docs" do
    message = ~r"could not retrieve the documentation for module ExUnit\.DocTestTest"

    assert_raise ExUnit.DocTest.Error, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest
      end
    end
  end

  test "fails in indentation mismatch" do
    message =
      ~r[test/ex_unit/doc_test_test\.exs:\d+: indentation level mismatch on doctest line: \"   iex> bar = 2\".*is exactly 2 spaces]s

    assert_raise ExUnit.DocTest.Error, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationMismatchedPrompt
      end
    end

    message =
      ~r[test/ex_unit/doc_test_test\.exs:\d+: indentation level mismatch on doctest line: \"    3\".*is exactly 2 spaces]s

    assert_raise ExUnit.DocTest.Error, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationTooMuch
      end
    end

    message =
      ~r[test/ex_unit/doc_test_test\.exs:\d+: indentation level mismatch on doctest line: \"  3\".*is exactly 4 spaces]s

    assert_raise ExUnit.DocTest.Error, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationNotEnough
      end
    end
  end

  test "fails with improper termination" do
    message =
      ~r[test/ex_unit/doc_test_test\.exs:\d+: expected non-blank line to follow iex> prompt]

    assert_raise ExUnit.DocTest.Error, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Incomplete
      end
    end
  end

  test "fails with improper termination not ending in new line" do
    message =
      ~r[test/ex_unit/doc_test_test\.exs:\d+: expected non-blank line to follow iex> prompt]

    assert_raise ExUnit.DocTest.Error, message, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IncompleteNoTrailingNewLine
      end
    end
  end

  test "fails on invalid use" do
    assert_raise RuntimeError, ~r"cannot define doctest", fn ->
      defmodule FunctionClashFail do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Invalid
      end
    end
  end
end
