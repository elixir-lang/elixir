defmodule Dialyzer.OpaqueInline do
  @spec bar(MapSet.t()) :: term()
  def bar(set) do
    set
  end

  def foo() do
    bar(MapSet.new([1, 2, 3]))
  end
end
