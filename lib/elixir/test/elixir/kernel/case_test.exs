Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.CaseTest do
  use ExUnit.Case, async: true

  test "inline case" do
    assert (case 1, do: (1 -> :ok; 2 -> :wrong)) == :ok
  end

  test "nested variables" do
    assert vars_case(400, 1) == {400, 1}
    assert vars_case(401, 1) == {400, -1}
    assert vars_case(0, -1)  == {0, -1}
    assert vars_case(-1, -1) == {0, 1}
  end

  test "nested vars match" do
    x = {:error, {:ok, :done}}
    assert (case x do
      {:ok, right} ->
        right
      {_left, right} ->
        case right do
          {:ok, right}  -> right
        end
    end) == :done
  end

  test "in operator outside case" do
    x = 1
    y = 4
    assert x in [1, 2, 3], "in assertion"
    assert not y in [1, 2, 3], "not in assertion"
  end

  test "in with match" do
    refute 1.0 in [1, 2, 3], "not in assertion"
  end

  test "in cond clause" do
    assert (cond do
      format() && (f = format()) ->
        f
      true ->
        :text
    end) == :html
  end

  defp format, do: :html

  defp vars_case(x, vx) do
    case x > 400 do
      true ->
        x = 400
        vx = -vx
      _ ->
        case x < 0 do
          true ->
            x = 0
            vx = -vx
          _ -> nil
        end
    end
    {x, vx}
  end
end
