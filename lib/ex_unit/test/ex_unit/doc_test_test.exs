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
  def single_context

  @doc """
  iex> 1 + (fn() -> "" end).()
  ** (ArithmeticError) bad argument in arithmetic expression

  iex> 2 + (fn() -> :a end).()
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def two_exceptions

  @doc """
  iex> 1 + (fn() -> :a end).()
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def exception_test, do: 1
end

defmodule ExUnit.DocTestTest.ExceptionModule do
  @doc """
  iex> 1 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  iex> 2 + ""
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def two_exceptions_in_single_context
end

defmodule ExUnit.DocTestTest.SomewhatGoodModule do
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

defmodule ExUnit.DocTestTest.SomewhatGoodModule1 do
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
  iex> ExUnit.DocTestTest.NoImport.min(1,2)
  2
  """
  def min(a,b), do: max(a,b)
end
defmodule ExUnit.DocTestTest do
  use ExUnit.Case

  # This is intentional. The doctests in DocTest's docs fail for demonstration
  # purposes.
  #doctest ExUnit.DocTest
  doctest ExUnit.DocTestTest.GoodModule, import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModule, only: [test_fun: 0], import: true
  doctest ExUnit.DocTestTest.SomewhatGoodModule1, except: [test_fun1: 0], import: true
  doctest ExUnit.DocTestTest.NoImport

  assert_raise ExUnit.DocTest.Error, "Multiple exceptions in one doctest case are not supported", fn ->
    doctest ExUnit.DocTestTest.ExceptionModule
  end

  test :var_leak do
    assert "nofile:9: function '_a'/0 undefined" = format_rescue("""
      defmodule M do
      @doc \"\"\"
      iex> _a = 1
      1

      iex> _a + 1
      2
      \"\"\"
      def no_leak
      end

      defmodule M.Test do
        require ExUnit.DocTest

        ExUnit.DocTest.doctest M
      end
      """
      )
  end

  defp format_rescue(expr) do
    result = try do
      :elixir.eval(to_char_list(expr), [])
      nil
    rescue
      error -> error.message
    end

    result || raise(ExUnit.AssertionError, message: "Expected function given to format_rescue to fail")
  end
end
