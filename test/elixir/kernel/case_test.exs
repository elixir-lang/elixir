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

  defp get_case do
    case internal do
    match: :invalid
      status = :fail
    match: 1
      case other_internal do
      match: status
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
    match: true
      x = 400
      vx = -vx
    else:
      case x < 0 do
      match: true
        x = 0
        vx = -vx
      else:
        nil
      end
    end
    {x, vx}
  end
end