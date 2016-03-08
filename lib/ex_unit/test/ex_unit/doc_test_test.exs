Code.require_file "../test_helper.exs", __DIR__

import ExUnit.TestHelpers

defmodule ExUnit.DocTestTest.GoodModule do
  @doc """
  iex> test_fun
  1
  iex> test_fun + 1
  2
  """
  def test_fun, do: 1

  @doc ~S"""
  iex> ~S(f#{o}o)
  "f\#{o}o"
  """
  def test_sigil, do: :ok

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
end |> write_beam

defmodule ExUnit.DocTestTest.MultipleExceptions do
  @doc """
  iex> 1 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  iex> 2 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def two_exceptions, do: :ok
end |> write_beam

defmodule ExUnit.DocTestTest.SomewhatGoodModuleWithOnly do
  @doc """
  iex> test_fun1
  1
  iex> test_fun1 + 1
  2
  """
  def test_fun1, do: 1

  @doc """
  iex> test_fun2
  1
  iex> test_fun2 + 1
  1
  """
  def test_fun2, do: 1
end |> write_beam

defmodule ExUnit.DocTestTest.SomewhatGoodModuleWithExcept do
  @moduledoc """
  iex> 1 + 1
  1
  """

  @doc """
  iex> test_fun1
  1
  iex> test_fun1 + 1
  2
  """
  def test_fun1, do: 1

  @doc """
  iex> test_fun2
  1
  iex> test_fun2 + 1
  1
  """
  def test_fun2, do: 1
end |> write_beam

defmodule ExUnit.DocTestTest.NoImport do
  @doc """
  iex> ExUnit.DocTestTest.NoImport.min(1, 2)
  2
  """
  def min(a, b), do: max(a, b)
end |> write_beam

defmodule ExUnit.DocTestTest.Invalid do
  @moduledoc """

      iex> 1 + * 1
      1

      iex> 1 + hd(List.flatten([1]))
      3

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

end |> write_beam

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
end |> write_beam

defmodule ExUnit.DocTestTest.IndentationMismatchedPrompt do
  @doc ~S'''
    iex> foo = 1
     iex> bar = 2
    iex> foo + bar
    3
  '''
  def mismatched, do: :ok
end |> write_beam

defmodule ExUnit.DocTestTest.IndentationTooMuch do
  @doc ~S'''
    iex> 1 + 2
      3
  '''
  def too_much, do: :ok
end |> write_beam

defmodule ExUnit.DocTestTest.IndentationNotEnough do
  @doc ~S'''
      iex> 1 + 2
    3
  '''
  def test_fun, do: :ok
end |> write_beam

defmodule ExUnit.DocTestTest.Incomplete do
  @doc ~S'''
      iex> 1 + 2

  '''
  def test_fun, do: :ok
end |> write_beam

defmodule ExUnit.DocTestTest.Numbered do
  @doc """
  iex(1)> 1 +
  ...(1)> 2
  3
  """
  def test_fun(), do: :ok
end |> write_beam()

defmodule ExUnit.DocTestTest do
  use ExUnit.Case

  # This is intentional. The doctests in DocTest's docs
  # fail for demonstration purposes.
  # doctest ExUnit.DocTest

  doctest ExUnit.DocTestTest.GoodModule, import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithOnly, only: [test_fun1: 0], import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithExcept, except: [:moduledoc, test_fun2: 0], import: true
  doctest ExUnit.DocTestTest.NoImport
  doctest ExUnit.DocTestTest.IndentationHeredocs

  import ExUnit.CaptureIO

  test "multiple functions filtered with :only" do
    defmodule MultipleOnly do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.SomewhatGoodModuleWithOnly, only: [test_fun1: 0, test_fun2: 0], import: true
    end

    assert capture_io(fn -> ExUnit.run end) =~ "2 tests, 1 failure"
  end

  test "doctest failures" do
    defmodule ActuallyCompiled do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.Invalid
    end

    ExUnit.configure(seed: 0, colors: [enabled: false])
    output = capture_io(fn -> ExUnit.run end)

    # Test order is not guaranteed, we can't match this as a string for each failing doctest
    assert output =~ ~r/\d+\) test moduledoc at ExUnit\.DocTestTest\.Invalid \(\d+\) \(ExUnit\.DocTestTest\.ActuallyCompiled\)/

    assert output =~ """
      1) test moduledoc at ExUnit.DocTestTest.Invalid (1) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:120: syntax error before: '*'
         code: 1 + * 1
         stacktrace:
           test/ex_unit/doc_test_test.exs:120: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      2) test moduledoc at ExUnit.DocTestTest.Invalid (2) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest failed
         code: 1 + hd(List.flatten([1])) === 3
         lhs:  2
         stacktrace:
           test/ex_unit/doc_test_test.exs:123: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      3) test moduledoc at ExUnit.DocTestTest.Invalid (3) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest failed
         code: inspect(:oops) === "#MapSet<[]>"
         lhs:  ":oops"
         stacktrace:
           test/ex_unit/doc_test_test.exs:126: ExUnit.DocTestTest.Invalid (module)
    """

    # The stacktrace points to the cause of the error
    assert output =~ """
      4) test moduledoc at ExUnit.DocTestTest.Invalid (4) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest failed: got UndefinedFunctionError with message undefined function Hello.world/0 (module Hello is not available)
         code:  Hello.world
         stacktrace:
           Hello.world()
           (for doctest at) test/ex_unit/doc_test_test.exs:129
    """

    assert output =~ """
      5) test moduledoc at ExUnit.DocTestTest.Invalid (5) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest failed: expected exception WhatIsThis with message "oops" but got RuntimeError with message "oops"
         code: raise "oops"
         stacktrace:
           test/ex_unit/doc_test_test.exs:132: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      6) test moduledoc at ExUnit.DocTestTest.Invalid (6) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest failed: expected exception RuntimeError with message "hello" but got RuntimeError with message "oops"
         code: raise "oops"
         stacktrace:
           test/ex_unit/doc_test_test.exs:135: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      7) test doc at ExUnit.DocTestTest.Invalid.a/0 (7) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:141: syntax error before: '*'
         code: 1 + * 1
         stacktrace:
           test/ex_unit/doc_test_test.exs:141: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      8) test doc at ExUnit.DocTestTest.Invalid.b/0 (8) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:240
         Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:147: syntax error before: '*'
         code: 1 + * 1
         stacktrace:
           test/ex_unit/doc_test_test.exs:147: ExUnit.DocTestTest.Invalid (module)
    """
  end

  test "iex prefix contains a number" do
    defmodule NumberedUsage do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.Numbered
    end

    assert capture_io(fn -> ExUnit.run end) =~ "1 test, 0 failures"
  end

  test "tags tests as doctests" do
    defmodule DoctestTag do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.NoImport

      setup test do
        assert test.doctest
        :ok
      end
    end

    assert capture_io(fn -> ExUnit.run end) =~ "1 test, 0 failures"
  end

  test "multiple exceptions in one test case is not supported" do
    assert_raise ExUnit.DocTest.Error, ~r"multiple exceptions in one doctest case are not supported", fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.MultipleExceptions
      end
    end
  end

  test "fails on invalid module" do
    assert_raise CompileError, ~r"module ExUnit.DocTestTest.Unknown is not loaded and could not be found", fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Unknown
      end
    end
  end

  test "fails when there are no docs" do
    assert_raise ExUnit.DocTest.Error, ~r"could not retrieve the documentation for module ExUnit.DocTestTest", fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest
      end
    end
  end

  test "fails in indentation mismatch" do
    assert_raise ExUnit.DocTest.Error,
      ~r[test/ex_unit/doc_test_test.exs:\d+: indentation level mismatch: "   iex> bar = 2", should have been 2 spaces], fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationMismatchedPrompt
      end
    end

    assert_raise ExUnit.DocTest.Error,
      ~r[test/ex_unit/doc_test_test.exs:\d+: indentation level mismatch: "    3", should have been 2 spaces], fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationTooMuch
      end
    end

    assert_raise ExUnit.DocTest.Error,
      ~r[test/ex_unit/doc_test_test.exs:\d+: indentation level mismatch: \"  3\", should have been 4 spaces], fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationNotEnough
      end
    end

    assert_raise ExUnit.DocTest.Error,
      ~r[test/ex_unit/doc_test_test.exs:\d+: expected non-blank line to follow iex> prompt], fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Incomplete
      end
    end
  end

  test "fails on invalid use" do
    assert_raise RuntimeError, ~r"cannot define test", fn ->
      defmodule FunctionClashFail do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Invalid
      end
    end
  end
end
