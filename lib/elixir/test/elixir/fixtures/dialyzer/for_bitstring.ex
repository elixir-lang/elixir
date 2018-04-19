defmodule Dialyzer.ForBitstring do
  def foo() do
    for a <- 1..3, into: "", do: <<a>>
  end
end
