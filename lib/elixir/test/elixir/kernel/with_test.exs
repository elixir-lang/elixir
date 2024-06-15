Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.WithTest do
  use ExUnit.Case, async: true

  test "basic with" do
    assert with({:ok, res} <- ok(41), do: res) == 41
    assert with(res <- four(), do: res + 10) == 14
  end

  test "matching with" do
    assert with(_..42//_ <- 1..42, do: :ok) == :ok
    assert with({:ok, res} <- error(), do: res) == :error
    assert with({:ok, _} = res <- ok(42), do: elem(res, 1)) == 42
  end

  test "with guards" do
    assert with(x when x < 2 <- four(), do: :ok) == 4
    assert with(x when x > 2 <- four(), do: :ok) == :ok
    assert with(x when x < 2 when x == 4 <- four(), do: :ok) == :ok
  end

  test "pin matching with" do
    key = :ok
    assert with({^key, res} <- ok(42), do: res) == 42
  end

  test "pin matching with multiple else" do
    key = :error

    first_else =
      with nil <- error() do
        :ok
      else
        ^key -> :pinned
        _other -> :other
      end

    assert first_else == :pinned

    second_else =
      with nil <- ok(42) do
        :ok
      else
        ^key -> :pinned
        _other -> :other
      end

    assert second_else == :other
  end

  test "two levels with" do
    result =
      with {:ok, n1} <- ok(11),
           n2 <- 22,
           do: n1 + n2

    assert result == 33

    result =
      with n1 <- 11,
           {:ok, n2} <- error(),
           do: n1 + n2

    assert result == :error
  end

  test "binding inside with" do
    result =
      with {:ok, n1} <- ok(11),
           n2 = n1 + 10,
           {:ok, n3} <- ok(22),
           do: n2 + n3

    assert result == 43

    result =
      with {:ok, n1} <- ok(11),
           n2 = n1 + 10,
           {:ok, n3} <- error(),
           do: n2 + n3

    assert result == :error
  end

  test "does not leak variables to else" do
    state = 1

    result =
      with 1 <- state,
           state = 2,
           :ok <- error(),
           do: state,
           else: (_ -> state)

    assert result == 1
    assert state == 1
  end

  test "with shadowing" do
    assert with(
             a <-
               (
                 b = 1
                 _ = b
                 1
               ),
             b <- 2,
             do: a + b
           ) == 3
  end

  test "with extra guards" do
    var =
      with %_{} = a <- struct(URI),
           %_{} <- a do
        :ok
      end

    assert var == :ok
  end

  test "errors in with" do
    assert_raise RuntimeError, fn ->
      with({:ok, res} <- oops(), do: res)
    end

    assert_raise RuntimeError, fn ->
      with({:ok, res} <- ok(42), res = res + oops(), do: res)
    end
  end

  test "else conditions" do
    assert (with {:ok, res} <- four() do
              res
            else
              {:error, error} -> error
              res -> res + 1
            end) == 5

    assert (with {:ok, res} <- four() do
              res
            else
              res when res == 4 -> res + 1
              res -> res
            end) == 5

    assert with({:ok, res} <- four(), do: res, else: (_ -> :error)) == :error
  end

  test "else conditions with match error" do
    assert_raise WithClauseError, "no with clause matching: :error", fn ->
      with({:ok, res} <- error(), do: res, else: ({:error, error} -> error))
    end
  end

  defp four() do
    4
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
