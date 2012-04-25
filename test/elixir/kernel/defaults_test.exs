Code.require_file "../../test_helper", __FILE__

defmodule Kernel.DefaultsTest do
  use ExUnit.Case

  test :clauses_without_implementation_can_have_default_args do
    assert a_number == 13
    assert a_number(4) == 17
  end

  defp a_number(x // 0)
  defp a_number(x), do: x + 13
end
