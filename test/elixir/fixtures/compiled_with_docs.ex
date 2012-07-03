defmodule CompiledWithDocs do
  @moduledoc "moduledoc"

  @doc "Some example"
  def example(false), do: 0
  def example(var),   do: var && private

  def nodoc(var // 0)
  def nodoc(_), do: 2

  defp private, do: 1
end