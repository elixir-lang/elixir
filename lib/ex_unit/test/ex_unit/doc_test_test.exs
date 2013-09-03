Code.require_file "../test_helper.exs", __DIR__

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
end

defmodule ExUnit.DocTestTest.ExceptionModule do
  @doc """
  iex> 1 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  iex> 2 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def two_exceptions, do: :ok
end

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
end

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
end

defmodule ExUnit.DocTestTest.NoImport do
  @doc """
  iex> ExUnit.DocTestTest.NoImport.min(1, 2)
  2
  """
  def min(a, b), do: max(a, b)
end

defmodule ExUnit.DocTestTest.Invalid do
  @doc """
  iex> _a = 1
  1

  iex> _a + 1
  2
  """
  def no_leak, do: :ok
end

defmodule ExUnit.DocTestTest.IndentationHeredocs do
  @doc %S'''
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

defmodule ExUnit.DocTestTest.IndentationMismatchedPrompt do
  @doc %S'''
    iex> foo = 1
     iex> bar = 2
    iex> foo + bar
    3
  '''
  def mismatched, do: :ok
end

defmodule ExUnit.DocTestTest.IndentationTooMuch do
  @doc %S'''
    iex> 1 + 2
      3
  '''
  def too_much, do: :ok
end

defmodule ExUnit.DocTestTest.IndentationNotEnough do
  @doc %S'''
      iex> 1 + 2
    3
  '''
  def not_enough, do: :ok
end


defmodule ExUnit.DocTestTest do
  use ExUnit.Case

  # This is intentional. The doctests in DocTest's docs fail
  # for demonstration purposes.
  # doctest ExUnit.DocTest

  doctest ExUnit.DocTestTest.GoodModule, import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithOnly, only: [test_fun: 0], import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModuleWithExcept, except: [test_fun1: 0], import: true
  doctest ExUnit.DocTestTest.NoImport

  assert_raise ExUnit.DocTest.Error, fn ->
    doctest ExUnit.DocTestTest.ExceptionModule
  end

  test :no_var_leak do
    assert_raise CompileError, %r"function '_a'/0 undefined", fn ->
      defmodule NeverCompiled do
        import ExUnit.DocTest
        doctest ExUnit.DocTestTest.Invalid
      end
    end
  end

  doctest ExUnit.DocTestTest.IndentationHeredocs

  assert_raise ExUnit.DocTest.Error, fn ->
    doctest ExUnit.DocTestTest.IndentationMismatchedPrompt
  end

  assert_raise ExUnit.DocTest.Error, fn ->
    doctest ExUnit.DocTestTest.IndentationTooMuch
  end
end
