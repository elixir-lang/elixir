IO.puts("loading OnlyPassingTest")

defmodule OnlyPassingTest do
  use ExUnit.Case

  test "passes", do: assert(1 == 1)
end
