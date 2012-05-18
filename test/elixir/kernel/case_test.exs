Code.require_file "../../test_helper", __FILE__

defmodule Kernel.CaseTest do
  use ExUnit.Case

  test :nested_case do
    assert get_case == 2
  end

  test :nested_variables do
    assert vars_case(400, 1) == { 400, 1 }
    assert vars_case(401, 1) == { 400, -1 }
    assert vars_case(0, -1)  == { 0, -1 }
    assert vars_case(-1, -1) == { 0, 1 }
  end

  test :match_with_in do
    assert(case 3 do
      x in [1,2,3] ->
        true
    end)

    y = 3

    assert(case 3 do
      x in [1,2,3] when y == 2 ->
        false
      x in [1,2,3] when y == 1 when y == 3 ->
        true
    end)
  end

  test :in_operator_outside_case do
    x = 1
    y = 4
    assert x in [1,2,3]
    assert not y in [1, 2, 3]
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