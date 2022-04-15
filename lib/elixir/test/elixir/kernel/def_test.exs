Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DefTest do
  use ExUnit.Case, async: true

  def fun_with_block_defaults(
        x,
        y \\ (
          default = "y"
          default
        ),
        z \\ (
          default = "z"
          default
        )
      ) do
    {x, y, z}
  end

  test "function with block defaults" do
    assert {1, "y", "z"} = fun_with_block_defaults(1)
    assert {1, 2, "z"} = fun_with_block_defaults(1, 2)
    assert {1, 2, 3} = fun_with_block_defaults(1, 2, 3)
  end
end
