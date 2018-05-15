defmodule PassingAndFailingTest do
  use ExUnit.Case

  @tag :foo
  test "fails", do: assert(System.get_env("PASS_FAILING_TESTS"))

  @tag :foo
  test "passes", do: assert(1 == 1)
end
