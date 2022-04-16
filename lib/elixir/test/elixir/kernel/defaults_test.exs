Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.DefaultsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

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

  test "with block defaults" do
    assert {1, "y", "z"} = fun_with_block_defaults(1)
    assert {1, 2, "z"} = fun_with_block_defaults(1, 2)
    assert {1, 2, 3} = fun_with_block_defaults(1, 2, 3)
  end

  test "accessing variable from default block" do
    message = "variable \"default\" does not exist"

    assert capture_io(:stderr, fn ->
             assert_raise CompileError, ~r/undefined function default\/0/, fn ->
               defmodule VarDefaultScope do
                 def test(_ \\ default = 1),
                   do: default
               end
             end
           end) =~ message
  end
end
