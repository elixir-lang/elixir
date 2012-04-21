Code.require_file "../../test_helper", __FILE__

defmodule Kernel.DefaultsTest do
  use ExUnit.Case

  test :clauses_without_implementation_can_have_default_args do
    assert_equal 13, a_number
    assert_equal 17, a_number(4)
  end

  defp a_number(x // 0)
  defp a_number(x), do: x + 13
end
