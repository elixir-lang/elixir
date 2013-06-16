Code.require_file "../test_helper.exs", __DIR__

# If it compiles, we are good.
defmodule Kernel.NestedModuleTest do
  defrecord State, a: nil

  defmodule B do
    defrecord State, b: nil
    def get(State[b: b]), do: b
  end

  def get(State[a: a]), do: a
end
