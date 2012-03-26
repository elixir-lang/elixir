defmodule CompiledWithDocs do
  @moduledoc "moduledoc"

  @doc "Some example"
  def example, do: 1

  def nodoc, do: 2
end