defmodule OnlyFailingTest do
  use ExUnit.Case

  test "fails", do: assert(System.get_env("PASS_FAILING_TESTS"))
end
