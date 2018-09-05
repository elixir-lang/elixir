Code.require_file("test_helper.exs", __DIR__)

defmodule MaxFailuresSequentialTest do
  use ExUnit.Case, async: false
  import MaxFailuresHelper

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(false)
  test "fail - #{__ENV__.line}", do: assert(false)
  test "fail - #{__ENV__.line}", do: assert(false)
  test "pass - #{__ENV__.line}", do: assert(true)
end
