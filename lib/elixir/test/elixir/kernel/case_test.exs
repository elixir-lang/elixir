Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.CaseTest do
  use ExUnit.Case, async: true

  test :inline_case do
    assert (case 1, do: (1 -> :ok; 2 -> :wrong)) == :ok
  end

  test :nested_case do
    assert get_case == 2
  end

  test :nested_variables do
    assert vars_case(400, 1) == { 400, 1 }
    assert vars_case(401, 1) == { 400, -1 }
    assert vars_case(0, -1)  == { 0, -1 }
    assert vars_case(-1, -1) == { 0, 1 }
  end

  test :nested_vars_match do
    x = { :error, { :ok, :done } }
    assert (case x do
      { :ok, right } ->
        right
      { _left, right } ->
        case right do
          { :ok, right }  -> right
        end
    end) == :done
  end

  test :in_operator_outside_case do
    x = 1
    y = 4
    assert x in [1, 2, 3], "in assertion"
    assert not y in [1, 2, 3], "not in assertion"
  end

  test :in_with_match do
    refute 1.0 in [1, 2, 3], "not in assertion"
  end

  defp get_case do
    case internal do
      :invalid ->
        status = :fail
      1 ->
        case other_internal do
          status ->
            status
        end
    end
    status
  end

  defp internal do
    1
  end

  defp other_internal do
    2
  end

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
