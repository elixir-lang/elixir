defmodule CompiledWithDocs do
  @moduledoc "moduledoc"

  @doc "Some example"
  def example(false), do: 0
  def example(var),   do: var && private

  def nodoc, do: 2
  defp private, do: 1
end