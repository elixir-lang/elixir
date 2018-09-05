Code.require_file("test_helper.exs", __DIR__)

defmodule MaxFailuresDuringSpawnModules1Test do
  use ExUnit.Case, async: true

  def fail(_), do: raise("fail")
  setup_all :fail

  test "pass - #{__ENV__.line}", do: assert(true)
end

defmodule MaxFailuresDuringSpawnModules2Test do
  use ExUnit.Case, async: true
  test "pass - #{__ENV__.line}", do: assert(true)
end

defmodule MaxFailuresDuringSpawnModules3Test do
  use ExUnit.Case, async: true
  test "pass - #{__ENV__.line}", do: assert(true)
end
