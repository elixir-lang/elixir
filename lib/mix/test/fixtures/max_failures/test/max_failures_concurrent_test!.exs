Code.require_file("test_helper.exs", __DIR__)

defmodule MaxFailuresConcurrent1Test do
  use ExUnit.Case, async: true
  import MaxFailuresHelper

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end

defmodule MaxFailuresConcurrent2Test do
  use ExUnit.Case, async: true
  import MaxFailuresHelper

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end

defmodule MaxFailuresConcurrent3Test do
  use ExUnit.Case, async: true
  import MaxFailuresHelper

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end

defmodule MaxFailuresConcurrent4Test do
  use ExUnit.Case, async: true
  import MaxFailuresHelper

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(1000, false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end

defmodule MaxFailuresConcurrent5Test do
  def fail(_) do
    raise("fail")
  end

  use ExUnit.Case, async: true
  import MaxFailuresHelper

  setup_all :fail

  test "pass - #{__ENV__.line}", do: assert(sleep(true))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "fail - #{__ENV__.line}", do: assert(sleep(false))
  test "pass - #{__ENV__.line}", do: assert(sleep(true))
end
