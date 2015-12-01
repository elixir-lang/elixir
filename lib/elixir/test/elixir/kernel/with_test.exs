Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.WithTest do
  use ExUnit.Case, async: true

  test "basic with" do
    assert with(res <- 41, do: res + 1) == 42
  end

  test "matching with" do
    assert with(_..42 <- 1..42, do: :ok) == :ok
    assert with({:ok, res} <- :error, do: res) == :error
    assert with({:ok, _} = res <- ok(42), do: elem(res, 1)) == 42
  end

  test "pin matching with" do
    key = :ok
    assert with({^key, res} <- ok(42), do: res) == 42
  end

  test "two levels with" do
    result = with(n1 <- 11, n2 <- 22, do: n1 + n2)
    assert result == 33

    result = with(n1 <- 11, {:ok, n2} <- :error, do: n1 + n2)
    assert result == :error
  end

  test "binding inside with" do
    result =
      with {:ok, n1} <- ok(11),
           n2 = n1 + 10,
           {:ok, n3} <- ok(22), do: n2 + n3
    assert result == 43

    result =
      with {:ok, n1} <- ok(11),
           n2 = n1 + 10,
           {:ok, n3} <- error(), do: n2 + n3
    assert result == :error
  end

  test "errors in with" do
    assert_raise RuntimeError, fn ->
      with({:ok, res} <- oops(), do: res)
    end

    assert_raise RuntimeError, fn ->
      with({:ok, res} <- ok(42), res = res + oops(), do: res)
    end
  end

  defp error() do
    :error
  end

  defp ok(num) do
    {:ok, num}
  end

  defp oops() do
    raise("oops")
  end
end
