Code.require_file("test_helper.exs", __DIR__)

defmodule MaxFailuresReachedDuringSetupAll1Test do
  use ExUnit.Case, async: true
  import MaxFailuresHelper

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end

defmodule MaxFailuresReachedDuringSetupAll2Test do
  use ExUnit.Case, async: true
  import MaxFailuresHelper

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end

defmodule MaxFailuresReachedDuringSetupAll3Test do
  use ExUnit.Case, async: true
  import MaxFailuresHelper

  def fail(_), do: raise("fail")
  setup_all :fail

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end
