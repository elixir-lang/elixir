Code.require_file "../../test_helper", __FILE__

defmodule Kernek.CaseTest do
  use ExUnit.Case

  test :nested_case do
    assert_equal 2, get_case
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
end