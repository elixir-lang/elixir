defmodule CompiledWithDocs do
  @moduledoc "moduledoc"

  @doc "Some example"
  def example(true),  do: private
  def example(false), do: 0

  def nodoc, do: 2
  defp private, do: 1
end