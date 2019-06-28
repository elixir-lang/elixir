defmodule Mix.Tasks.Compile.Xref do
  use Mix.Task.Compiler

  @moduledoc false

  # TODO: Deprecate in v1.11
  @impl true
  def run(_args), do: {:noop, []}

  @impl true
  def manifests, do: []
end
