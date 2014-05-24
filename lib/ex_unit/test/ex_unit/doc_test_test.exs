Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule ExUnit.DocTestTest.GoodModule do
  @doc """
  iex> test_fun
  1
  iex> test_fun + 1
  2
  """
  def test_fun, do: 1

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
  iex> Enum.into([a: 0, b: 1, c: 2], HashDict.new)
  #HashDict<[c: 2, b: 1, a: 0]>
  """
  def inspect1_test, do: :ok

  @doc """
  iex> x = Enum.into([a: 0, b: 1, c: 2], HashDict.new)
  ...> x
  #HashDict<[c: 2, b: 1, a: 0]>
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
  iex> test_fun
  1
  iex> test_fun + 1
  2
  """
  def test_fun, do: 1

  @doc """
  iex> test_fun
  1
  iex> test_fun + 1
  1
  """
  def test_fun1, do: 1
end |> write_beam

defmodule ExUnit.DocTestTest.SomewhatGoodModuleWithExcept do
  @doc """
  iex> test_fun
  1
  iex> test_fun + 1
  2
  """
  def test_fun, do: 1

  @doc """
  iex> test_fun
  1
  iex> test_fun + 1
  1
  """
  def test_fun1, do: 1
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
      #HashDict<[]>

      iex> Hello.world
      :world

      iex> raise "oops"
      ** (WhatIsThis) oops

      iex> raise "oops"
      ** (RuntimeError) hello

  """
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
  def not_enough, do: :ok
end |> write_beam

defmodule ExUnit.DocTestTest.Incomplete do
  @doc ~S'''
      iex> 1 + 2

  '''
  def not_enough, do: :ok
end |> write_beam

defmodule ExUnit.DocTestTest do
  use ExUnit.Case

  # This is intentional. The doctests in DocTest's docs
  # fail for demonstration purposes.
  # doctest ExUnit.DocTest

  doctest ExUnit.DocTestTest.GoodModule, import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithOnly, only: [test_fun: 0], import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithExcept, except: [test_fun1: 0], import: true
  doctest ExUnit.DocTestTest.NoImport
  doctest ExUnit.DocTestTest.IndentationHeredocs

  import ExUnit.CaptureIO

  test "doctest failures" do
    defmodule ActuallyCompiled do
      use ExUnit.Case
      doctest ExUnit.DocTestTest.Invalid
    end

    ExUnit.configure(seed: 0)
    output = capture_io(fn -> ExUnit.run end)

    assert output =~ """
      1) test moduledoc at ExUnit.DocTestTest.Invalid (1) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:198
         Doctest did not compile, got: (SyntaxError) test/ex_unit/doc_test_test.exs:106: syntax error before: '*'
         code: 1 + * 1
         stacktrace:
           test/ex_unit/doc_test_test.exs:106: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      2) test moduledoc at ExUnit.DocTestTest.Invalid (2) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:198
         Doctest failed
         code: 1 + hd(List.flatten([1])) === 3
         lhs:  2
         stacktrace:
           test/ex_unit/doc_test_test.exs:106: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      3) test moduledoc at ExUnit.DocTestTest.Invalid (3) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:198
         Doctest failed
         code: inspect(:oops) === "#HashDict<[]>"
         lhs:  ":oops"
         stacktrace:
           test/ex_unit/doc_test_test.exs:106: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      4) test moduledoc at ExUnit.DocTestTest.Invalid (4) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:198
         Doctest failed: got UndefinedFunctionError with message undefined function: Hello.world/0
         code:  Hello.world
         stacktrace:
           test/ex_unit/doc_test_test.exs:106: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      5) test moduledoc at ExUnit.DocTestTest.Invalid (5) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:198
         Doctest failed: expected exception WhatIsThis with message "oops" but got RuntimeError with message "oops"
         code: raise "oops"
         stacktrace:
           test/ex_unit/doc_test_test.exs:106: ExUnit.DocTestTest.Invalid (module)
    """

    assert output =~ """
      6) test moduledoc at ExUnit.DocTestTest.Invalid (6) (ExUnit.DocTestTest.ActuallyCompiled)
         test/ex_unit/doc_test_test.exs:198
         Doctest failed: expected exception RuntimeError with message "hello" but got RuntimeError with message "oops"
         code: raise "oops"
         stacktrace:
           test/ex_unit/doc_test_test.exs:106: ExUnit.DocTestTest.Invalid (module)
    """
  end

  test "multiple exceptions in one test case is not supported" do
    assert_raise ExUnit.DocTest.Error, ~r"multiple exceptions in one doctest case are not supported", fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.MultipleExceptions
      end
    end
  end

  test "fails in indentation mismatch" do
    assert_raise ExUnit.DocTest.Error, ~r/indentation level mismatch: "   iex> bar = 2", should have been 2 spaces/, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationMismatchedPrompt
      end
    end

    assert_raise ExUnit.DocTest.Error, ~r/indentation level mismatch: "    3", should have been 2 spaces/, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationTooMuch
      end
    end

    assert_raise ExUnit.DocTest.Error, ~r/indentation level mismatch: \"  3\", should have been 4 spaces/, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.IndentationNotEnough
      end
    end

    assert_raise ExUnit.DocTest.Error, ~r/expected non-blank line to follow iex> prompt/, fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Incomplete
      end
    end
  end
end
