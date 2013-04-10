Code.require_file "../../test_helper.exs", __FILE__

defmodule ExUnit.DocTestTest.GoodModule do
  @doc """
  iex> test_fun
  1
  iex> test_fun + 1
  2
  """
  def test_fun, do: 1

  @doc """
  iex> 1 + (fn() -> :a end).()
  ** (ArithmeticError) bad argument in arithmetic expression
  """
  def exception_test, do: 1

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
defmodule ExUnit.DocTestTest do
  use ExUnit.Case

  defmodule T do
    @moduledoc """
    iex> 1+1
    2
    """

    @doc """
    iex> test_fun
    1
    """
    def test_fun, do: 1

    @doc """

    iex> test_fun1
    1

    """
    def test_fun1, do: 2

    @doc """

        iex> test_fun2
        2

    """
    def test_fun2, do: 2

    @doc """
    iex> hd(
    ...> [1]
    ...> )
    1
    """
    def multiline, do: nil

    @doc """
    iex> hd(
    iex> [1]
    iex> )
    1
    """
    def multiline1, do: nil

    @doc """
    iex> 1
    1

    Test

    iex> 2
    3
    """
    def multiple, do: nil

    @doc """
    iex> 1
    1
    iex> 2
    3
    """
    def multiple1, do: nil

    @doc """
    iex> [1,2,3]
    [1,
    2,
    3]
    """
    def multiline_expectation, do: nil

    @doc """
    iex(1)> 1+
    ...(1)> 2
    3
    """
    def iex_number, do: nil

  end


  test "extraction" do
    assert [_test] = ExUnit.DocTest.extract(T, :test_fun, 0)
    assert [_test] = ExUnit.DocTest.extract(T, :test_fun)

    assert [_test] = ExUnit.DocTest.extract(T, :test_fun1, 0)
    assert [_test] = ExUnit.DocTest.extract(T, :test_fun1)

    assert [_test] = ExUnit.DocTest.extract(T, :test_fun2, 0)
    assert [_test] = ExUnit.DocTest.extract(T, :test_fun2)

    assert [_test] = ExUnit.DocTest.extract(T, :multiline, 0)
    assert [_test] = ExUnit.DocTest.extract(T, :multiline)

    assert [_test] = ExUnit.DocTest.extract(T, :multiline1, 0)
    assert [_test] = ExUnit.DocTest.extract(T, :multiline1)

    assert [_, _] = ExUnit.DocTest.extract(T, :multiple, 0)
    assert [_, _] = ExUnit.DocTest.extract(T, :multiple)

    assert [_, _] = ExUnit.DocTest.extract(T, :multiple1, 0)
    assert [_, _] = ExUnit.DocTest.extract(T, :multiple1)

    assert [_test] = ExUnit.DocTest.extract(T, :multiline_expectation, 0)
    assert [_test] = ExUnit.DocTest.extract(T, :multiline_expectation)

    assert [_test] = ExUnit.DocTest.extract(T, :iex_number, 0)
    assert [_test] = ExUnit.DocTest.extract(T, :iex_number)

    assert length(ExUnit.DocTest.extract(T)) == 12
  end

  doctest ExUnit.DocTest
  doctest ExUnit.DocTestTest.GoodModule
  doctest ExUnit.DocTestTest.SomewhatGoodModule, only: [test_fun: 0]
  doctest ExUnit.DocTestTest.SomewhatGoodModule1, except: [test_fun1: 0]

end
