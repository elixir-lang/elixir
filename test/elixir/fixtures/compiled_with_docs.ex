defmodule CompiledWithDocs do
  @moduledoc "moduledoc"

  @doc "Some example"
  def example(true),  do: 1
  def example(false), do: 0

  def nodoc, do: 2
end