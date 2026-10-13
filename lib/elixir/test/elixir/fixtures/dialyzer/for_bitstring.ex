defmodule Dialyzer.ForBitstring do
  def foo() do
    for a <- 1..3, into: "", do: <<a>>
  end

  def bar() do
    for a <- 1..3, into: "prefix", do: <<a>>
  end
end
