defmodule OnlyFailingTest do
  use ExUnit.Case

  test "test number #{:random.uniform()} failed at #{DateTime.utc_now()}",
    do: assert(System.get_env("PASS_FAILING_TESTS"))
end
